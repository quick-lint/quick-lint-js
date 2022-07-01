// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

package main

import (
	"context"
	"database/sql"
	"fmt"
	_ "github.com/mattn/go-sqlite3"
	"sync"
	"time"
)

type Storage struct {
	db    *sql.DB
	mutex sync.Mutex

	// Lazily created:
	insertGitHubHTTPCacheStatement *sql.Stmt
	insertWorkflowJobStatement     *sql.Stmt
	insertWorkflowJobStepStatement *sql.Stmt
	insertWorkflowRunStatement     *sql.Stmt
}

func OpenStorage(sqliteDatabasePath string) (*Storage, error) {
	db, err := sql.Open("sqlite3", sqliteDatabasePath)
	if err != nil {
		return nil, err
	}
	return &Storage{
		db: db,
	}, nil
}

func (storage *Storage) Close() {
	if storage.insertWorkflowRunStatement != nil {
		storage.insertWorkflowRunStatement.Close()
	}
	storage.db.Close()
}

func fieldsCreateTableSQL(fields []Field) string {
	fieldKindToSQLType := map[FieldKind]string{
		FieldKindString:    "TEXT",
		FieldKindInteger:   "INTEGER",
		FieldKindBoolean:   "INTEGER",
		FieldKindTimestamp: "INTEGER",
	}
	sql := ""
	for _, field := range fields {
		sql += ", " + field.SQLName() + " " + fieldKindToSQLType[field.Kind]
	}
	return sql
}

func (storage *Storage) CreateTables() error {
	if _, err := storage.db.Exec(fmt.Sprintf(`CREATE TABLE IF NOT EXISTS github_workflow_run (
		id INTEGER PRIMARY KEY NOT NULL
		%s
	)`, fieldsCreateTableSQL(WorkflowRunFields))); err != nil {
		return err
	}

	if _, err := storage.db.Exec(fmt.Sprintf(`CREATE TABLE IF NOT EXISTS github_workflow_job (
		id INTEGER PRIMARY KEY NOT NULL
		%s
	)`, fieldsCreateTableSQL(WorkflowJobFields))); err != nil {
		return err
	}

	if _, err := storage.db.Exec(fmt.Sprintf(`CREATE TABLE IF NOT EXISTS github_workflow_job_step (
		job_id INTEGER NOT NULL,
		step_index INTEGER NOT NULL
		%s
	)`, fieldsCreateTableSQL(WorkflowJobStepFields))); err != nil {
		return err
	}
	if _, err := storage.db.Exec(`CREATE UNIQUE INDEX IF NOT EXISTS github_workflow_job_step_id
		ON github_workflow_job_step (
			job_id,
			step_index
		)`); err != nil {
		return err
	}

	if _, err := storage.db.Exec(`CREATE TABLE IF NOT EXISTS github_http_cache (
	    url TEXT,
	    etag TEXT
	)`); err != nil {
		return err
	}

	return nil
}

// Thread-safe.
func (storage *Storage) GetAllRunIDs() ([]int64, error) {
	storage.mutex.Lock()
	defer storage.mutex.Unlock()

	rows, err := storage.db.Query("SELECT id FROM github_workflow_run")
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	runIDs := []int64{}
	for rows.Next() {
		var runID int64
		if err := rows.Scan(&runID); err != nil {
			return runIDs, err
		}
		runIDs = append(runIDs, runID)
	}
	if err = rows.Err(); err != nil {
		return runIDs, err
	}
	return runIDs, nil
}

// Thread-safe.
func (storage *Storage) SaveRuns(runs []WorkflowRun) error {
	var err error
	storage.mutex.Lock()
	defer storage.mutex.Unlock()

	if storage.insertWorkflowRunStatement == nil {
		storage.insertWorkflowRunStatement, err = storage.db.Prepare(fmt.Sprintf(
			"INSERT OR REPLACE INTO github_workflow_run (id %s) VALUES (? %s)",
			fieldNamesForInsertSQL(WorkflowRunFields),
			placeholdersSQL(len(WorkflowRunFields))))
		if err != nil {
			return err
		}
	}

	// Insert in a transaction for better performance.
	transaction, err := storage.db.BeginTx(context.TODO(), nil)
	if err != nil {
		return err
	}
	insertWorkflowRunStatement := transaction.Stmt(storage.insertWorkflowRunStatement)

	values := make([]interface{}, 1+len(WorkflowRunFields))
	for _, run := range runs {
		values[0] = run["id"]
		for fieldIndex, field := range WorkflowRunFields {
			value, err := extractField(run, &field)
			if err != nil {
				return err
			}
			values[fieldIndex+1] = value
		}
		if _, err := insertWorkflowRunStatement.Exec(values...); err != nil {
			return err
		}
	}

	if err := transaction.Commit(); err != nil {
		return err
	}

	return nil
}

// Thread-safe.
func (storage *Storage) SaveJobs(jobs []WorkflowJob) error {
	var err error
	storage.mutex.Lock()
	defer storage.mutex.Unlock()

	if storage.insertWorkflowJobStatement == nil {
		storage.insertWorkflowJobStatement, err = storage.db.Prepare(fmt.Sprintf(
			"INSERT OR REPLACE INTO github_workflow_job (id %s) VALUES (? %s)",
			fieldNamesForInsertSQL(WorkflowJobFields),
			placeholdersSQL(len(WorkflowJobFields))))
	}
	if storage.insertWorkflowJobStepStatement == nil {
		storage.insertWorkflowJobStepStatement, err = storage.db.Prepare(fmt.Sprintf(
			"INSERT OR REPLACE INTO github_workflow_job_step (job_id, step_index %s) VALUES (?, ? %s)",
			fieldNamesForInsertSQL(WorkflowJobStepFields),
			placeholdersSQL(len(WorkflowJobStepFields))))
	}

	// Insert in a transaction for better performance.
	transaction, err := storage.db.BeginTx(context.TODO(), nil)
	if err != nil {
		return err
	}
	insertWorkflowJobStatement := transaction.Stmt(storage.insertWorkflowJobStatement)
	insertWorkflowJobStepStatement := transaction.Stmt(storage.insertWorkflowJobStepStatement)

	values := make([]interface{}, 1+len(WorkflowJobFields))
	for _, job := range jobs {
		values[0] = job["id"]
		for fieldIndex, field := range WorkflowJobFields {
			value, err := extractField(job, &field)
			if err != nil {
				return err
			}
			values[fieldIndex+1] = value
		}
		if _, err := insertWorkflowJobStatement.Exec(values...); err != nil {
			return err
		}
	}

	values = make([]interface{}, 2+len(WorkflowJobStepFields))
	for _, job := range jobs {
		values[0] = job["id"]
		for stepIndex, step := range job["steps"].([]interface{}) {
			values[1] = stepIndex
			for fieldIndex, field := range WorkflowJobStepFields {
				value, err := extractField(step, &field)
				if err != nil {
					return err
				}
				values[fieldIndex+2] = value
			}
			if _, err := insertWorkflowJobStepStatement.Exec(values...); err != nil {
				return err
			}
		}
	}

	if err := transaction.Commit(); err != nil {
		return err
	}

	return nil
}

// Returns an empty string if there is no ETag in the cache.
//
// Thread-safe.
func (storage *Storage) GetGitHubHTTPCacheETag(url string) (string, error) {
	storage.mutex.Lock()
	defer storage.mutex.Unlock()

	var etag string
	// TODO(strager): Prepared statement.
	if err := storage.db.QueryRow("SELECT etag FROM github_http_cache WHERE url = ?", url).Scan(&etag); err != nil {
		if err == sql.ErrNoRows {
			return "", nil
		}
		return "", err
	}
	return etag, nil
}

// Thread-safe.
func (storage *Storage) SetGitHubHTTPCacheETag(url string, etag string) error {
	var err error

	storage.mutex.Lock()
	defer storage.mutex.Unlock()

	if storage.insertGitHubHTTPCacheStatement == nil {
		storage.insertGitHubHTTPCacheStatement, err = storage.db.Prepare(
			"INSERT OR REPLACE INTO github_http_cache (url, etag) VALUES (?, ?)")
		if err != nil {
			return err
		}
	}

	if _, err := storage.insertGitHubHTTPCacheStatement.Exec(url, etag); err != nil {
		return err
	}
	return nil
}

func fieldNamesForInsertSQL(fields []Field) string {
	sql := ""
	for _, field := range fields {
		sql += ", " + field.SQLName()
	}
	return sql
}

func placeholdersSQL(count int) string {
	sql := ""
	for i := 0; i < count; i += 1 {
		sql += ", ?"
	}
	return sql
}

func extractField(raw interface{}, field *Field) (interface{}, error) {
	for _, namePart := range field.NameParts {
		if namePart == "" {
			break
		}
		raw = raw.(map[string]interface{})[namePart]
		if raw == nil {
			break
		}
	}

	if raw == nil {
		return raw, nil
	} else if field.Kind == FieldKindTimestamp {
		timestamp, err := time.Parse(time.RFC3339, raw.(string))
		if err != nil {
			return nil, err
		}
		return timestamp.Unix(), nil
	} else {
		return raw, nil
	}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew "strager" Glazar
//
// This file is part of quick-lint-js.
//
// quick-lint-js is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// quick-lint-js is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
