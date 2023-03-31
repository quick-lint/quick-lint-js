# CI analytics

This directory contains scripts for analyzing build performance.

## Collecting data

The `load_github_actions.go` script queries GitHub's API for CI job details and
saves the information in a SQLite database called `analytics.sqlite3`.

1. Create a Personal Access Token on GitHub. Save the token in a file called
   `token` in the `tools/ci-analytics/` directory.
2. Run: `go run github_api.go load_github_actions.go schema.go storage.go`

## Processing data

Use [Apache Superset][] or another SQL tool to load `analytics.sqlite3` and
query and graph data.

[Apache Superset]: https://superset.apache.org/
