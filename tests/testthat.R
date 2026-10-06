source(file.path("R", "utils_scoring.R"), local = FALSE)
source(file.path("R", "utils_theme.R"), local = FALSE)
source(file.path("R", "data_fetch.R"), local = FALSE)
source(file.path("R", "mod_sidebar_filters.R"), local = FALSE)
source(file.path("R", "mod_overview.R"), local = FALSE)
source(file.path("R", "mod_entity.R"), local = FALSE)
source(file.path("R", "mod_compare.R"), local = FALSE)
source(file.path("R", "mod_comments.R"), local = FALSE)
source(file.path("R", "mod_alerts.R"), local = FALSE)

library(testthat)

testthat::test_dir("tests/testthat", reporter = "summary")
