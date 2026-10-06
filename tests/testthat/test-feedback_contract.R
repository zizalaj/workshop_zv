copy_mock_fixture <- function(request, fixture_name, mock_dir) {
  fixture_path <- file.path("tests", "testthat", "fixtures", fixture_name)
  target_file <- file.path(
    mock_dir,
    paste0(httptest2::build_mock_url(request), ".json")
  )

  dir.create(dirname(target_file), recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(
    fixture_path,
    target_file,
    overwrite = TRUE
  )

  testthat::expect_true(ok)
}

restore_env_values <- function(values) {
  for (name in names(values)) {
    if (is.na(values[[name]])) {
      Sys.unsetenv(name)
    } else {
      do.call(Sys.setenv, stats::setNames(list(values[[name]]), name))
    }
  }
}

test_that("validate_feedback_configuration errors when required values are missing", {
  old_env <- Sys.getenv(
    c("FEEDBACK_API_BASE_URL", "FEEDBACK_API_TOKEN", "FORM_ID"),
    unset = NA_character_
  )
  on.exit(restore_env_values(old_env), add = TRUE)

  Sys.setenv(
    FEEDBACK_API_BASE_URL = "",
    FEEDBACK_API_TOKEN = "",
    FORM_ID = ""
  )

  expect_error(
    validate_feedback_configuration(),
    "Chybí povinné proměnné prostředí"
  )
})

test_that("transform_feedback_payload builds response and score tables from field ids", {
  form_definition <- jsonlite::read_json(
    file.path("tests", "testthat", "fixtures", "typeform_form.json"),
    simplifyVector = FALSE
  )
  payload <- jsonlite::read_json(
    file.path("tests", "testthat", "fixtures", "typeform_responses_page1.json"),
    simplifyVector = FALSE
  )

  expect_warning(
    result <- transform_feedback_payload(form_definition, payload),
    "více lektory"
  )

  expect_named(result, c("responses", "scores_long"))
  expect_equal(nrow(result$responses), 2L)
  expect_equal(nrow(result$scores_long), 8L)
  expect_equal(result$responses$submission_key, c("tok-1", "tok-2"))
  expect_equal(result$responses$teacher_id, c("teacher-1", "teacher-2"))
  expect_equal(result$responses$teacher_name, c("Aneta Haramijová", "Barbora Hailandová"))
  expect_equal(result$responses$workshop_topic_id, c("topic-1", "topic-2"))
  expect_equal(
    sort(unique(result$scores_long$metric_key)),
    sort(names(score_area_labels_cs))
  )
  expect_equal(
    result$responses$workshop_instance_id[[1]],
    "2026-08-15::topic-1::teacher-1"
  )
})

test_that("fetch_feedback paginates, loads form definition, and keeps duplicated raw response ids", {
  mock_dir <- tempfile("httptest2-mocks-")
  dir.create(mock_dir, recursive = TRUE)

  old_mock_paths <- getOption("httptest2.mock.paths")
  on.exit(options(httptest2.mock.paths = old_mock_paths), add = TRUE)
  options(httptest2.mock.paths = c(mock_dir, old_mock_paths))

  old_env <- Sys.getenv(
    c("FEEDBACK_API_BASE_URL", "FEEDBACK_API_TOKEN", "FORM_ID"),
    unset = NA_character_
  )
  on.exit(restore_env_values(old_env), add = TRUE)

  config_env <- environment(fetch_feedback)
  old_page_size <- get("feedback_page_size", envir = config_env)
  on.exit(assign("feedback_page_size", old_page_size, envir = config_env), add = TRUE)
  assign("feedback_page_size", 2L, envir = config_env)

  Sys.setenv(
    FEEDBACK_API_BASE_URL = "https://feedback.example.test",
    FEEDBACK_API_TOKEN = "test-token",
    FORM_ID = "form-123"
  )

  config <- validate_feedback_configuration()
  form_request <- build_feedback_form_request(config)
  request_one <- build_feedback_request(config)
  request_two <- build_feedback_request(config, before = "tok-2")

  copy_mock_fixture(form_request, "typeform_form.json", mock_dir)
  copy_mock_fixture(request_one, "typeform_responses_page1.json", mock_dir)
  copy_mock_fixture(request_two, "typeform_responses_page2.json", mock_dir)

  result <- expect_warning(
    httptest2::with_mock_api({
      fetch_feedback()
    }),
    "více lektory"
  )

  expect_equal(nrow(result$responses), 3L)
  expect_equal(nrow(result$scores_long), 12L)
  expect_equal(dplyr::n_distinct(result$responses$submission_key), 3L)
  expect_equal(dplyr::n_distinct(result$responses$response_id), 2L)
  expect_equal(
    sort(unique(result$responses$workshop_topic_label)),
    sort(c("Delegování", "Práce se změnou"))
  )
})
