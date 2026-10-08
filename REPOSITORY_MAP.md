# Repository Map

Snapshot date: September 1, 2026.

## Runtime shape

- `app.R` is the only Shiny entrypoint.
- `R/data_fetch.R` owns runtime configuration checks, API requests, pagination, and payload transformation into the canonical long-format dataset.
- `R/utils_scoring.R` contains the shared business logic for composite scoring, rolling calculations, filtering, and alerts.
- `R/utils_theme.R` contains the shared visual helpers, asset lookup, DataTables localization, and Czech date-formatting helpers.
- `R/mod_sidebar_filters.R`, `R/mod_overview.R`, `R/mod_entity.R`, `R/mod_comments.R`, and `R/mod_alerts.R` are the active dashboard modules.
- `www/` contains the only runtime assets: `app.css`, `fonts/`, and `logo/`.

## Data flow

1. `app.R` sources the runtime files from `R/`.
2. `fetch_feedback()` validates `FEEDBACK_API_BASE_URL`, `FEEDBACK_API_TOKEN`, and `FORM_ID`.
3. The fetch layer calls `FEEDBACK_API_BASE_URL/forms/FORM_ID/responses`, paginates with the `before` token, and transforms the payload into the canonical feedback tibble.
4. `app.R` stores the current dataset, loading state, error state, and last update timestamp in one `reactiveValues` object.
5. Sidebar filters operate on the in-memory dataset only; changing tabs or filters does not refetch API data.
6. The modules render the Czech UI from filtered data and shared scoring helpers.

## Important files

- `app.R`: app shell, initial load, retry and refresh behavior, and module wiring
- `R/data_fetch.R`: live API contract and canonical transformation
- `R/utils_scoring.R`: scoring, aggregation, deltas, response rate, and alert rules
- `R/utils_theme.R`: theme, fonts, logo, plot helpers, table localization, and Czech date formatting
- `R/mod_entity.R`: shared teacher and client detail module
- `tests/testthat/test-data_fetch.R`: offline tests for config validation, pagination, and payload transformation
- `tests/testthat/test-utils_scoring.R`: offline tests for scoring and alert behavior
- `tests/testthat/test-utils_theme.R`: deterministic tests for Czech date helpers
- `.rscignore`: deployment exclusions for Posit Connect Cloud

## Local-only or excluded content

- `.Renviron`, `.env`, `.Rhistory`, `.Rproj.user/`, and `.agents/` are local state.
- `renv.lock` remains a local reproducibility aid.
- `config.toml` is not runtime code and stays out of deployment.
- `tests/`, docs, and other non-runtime files are excluded from the deployment bundle through `.rscignore`.

## September 1, 2026 contract update

- The live canonical payload is now `state$data$responses` plus `state$data$scores_long`, not one legacy long tibble.
- `overall_score` is the only headline KPI across overview, trends, summaries, and alerts.
- Workshop topics replace clients throughout the active UI and aggregation layer.
- The three free-text prompts stay separate in the runtime contract and comments UI.
- `workshop_instance_id` is the workshop-level grouping key and is built from workshop date, topic, and teacher.
