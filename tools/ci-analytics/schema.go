// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

package main

import "log"
import "strings"

type FieldKind int

const (
	FieldKindString FieldKind = iota
	FieldKindInteger
	FieldKindBoolean
	FieldKindTimestamp
)

type Field struct {
	NameParts [2]string
	Kind      FieldKind
}

func (field *Field) SQLName() string {
	sqlName := field.NameParts[0]
	for i := 1; i < len(field.NameParts); i += 1 {
		if field.NameParts[i] == "" {
			break
		}
		sqlName += "__" + field.NameParts[i]
	}
	return sqlName
}

func MakeField(dottedName string, kind FieldKind) Field {
	field := Field{Kind: kind}
	parts := strings.Split(dottedName, ".")
	if len(parts) > len(field.NameParts) {
		log.Fatalf("field name %s has too many dotted parts\n", dottedName)
	}
	for i, part := range parts {
		field.NameParts[i] = part
	}
	return field
}

var WorkflowRunFields []Field = []Field{
	MakeField("name", FieldKindString),
	MakeField("head_branch", FieldKindString),
	MakeField("head_sha", FieldKindString),
	MakeField("path", FieldKindString),
	MakeField("run_number", FieldKindInteger),
	MakeField("event", FieldKindString),
	MakeField("status", FieldKindString),
	MakeField("conclusion", FieldKindString),
	MakeField("workflow_id", FieldKindInteger),
	MakeField("check_suite_id", FieldKindInteger),
	MakeField("created_at", FieldKindTimestamp),
	MakeField("updated_at", FieldKindTimestamp),
	MakeField("actor.login", FieldKindString),
	MakeField("actor.id", FieldKindInteger),
	MakeField("actor.type", FieldKindString),
	MakeField("run_attempt", FieldKindInteger),
	MakeField("run_started_at", FieldKindTimestamp),
	MakeField("triggering_actor.login", FieldKindString),
	MakeField("triggering_actor.id", FieldKindInteger),
	MakeField("triggering_actor.type", FieldKindString),
	MakeField("head_commit.id", FieldKindString),
	MakeField("repository.id", FieldKindInteger),
	MakeField("repository.name", FieldKindString),
	MakeField("repository.full_name", FieldKindString),
	MakeField("repository.private", FieldKindBoolean),
	MakeField("repository.fork", FieldKindBoolean),
	MakeField("head_repository.id", FieldKindInteger),
	MakeField("head_repository.name", FieldKindString),
	MakeField("head_repository.full_name", FieldKindString),
	MakeField("head_repository.private", FieldKindBoolean),
	MakeField("head_repository.fork", FieldKindBoolean),
}

var WorkflowJobFields []Field = []Field{
	MakeField("run_id", FieldKindInteger),
	MakeField("run_attempt", FieldKindInteger),
	MakeField("head_sha", FieldKindString),
	MakeField("status", FieldKindString),
	MakeField("conclusion", FieldKindString),
	MakeField("started_at", FieldKindTimestamp),
	MakeField("completed_at", FieldKindTimestamp),
	MakeField("name", FieldKindString),
	MakeField("runner_id", FieldKindInteger),
	MakeField("runner_name", FieldKindString),
	MakeField("runner_group_id", FieldKindInteger),
	MakeField("runner_group_name", FieldKindString),
}

var WorkflowJobStepFields []Field = []Field{
	MakeField("name", FieldKindString),
	MakeField("status", FieldKindString),
	MakeField("conclusion", FieldKindString),
	MakeField("number", FieldKindInteger),
	MakeField("started_at", FieldKindTimestamp),
	MakeField("completed_at", FieldKindTimestamp),
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
