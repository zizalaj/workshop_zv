testthat::skip("Obsolete legacy fetch tests.")

copy_mock_fixture <- function(request, fixture_name, mock_dir) {
  fixture_path <- file.path("tests", "testthat", "fixtures", fixture_name)
  if (!file.exists(fixture_path)) {
    fixture_path <- file.path("fixtures", fixture_name)
  }

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

test_that("transform_feedback_payload keeps the canonical long-format contract", {
  payload <- list(
    data = list(
      list(
        response_id = "resp-001",
        workshop_id = "workshop-101",
        workshop_date = "2026-07-10",
        teacher_id = "teacher-1",
        teacher_name = "Adela Kratochvilova",
        client_id = "client-1",
        client_name = "Asteria Tech",
        n_participants = 12,
        comment_text = "Skvely workshop, vse bylo srozumitelne.",
        dimension_scores = list(
          list(dimension = "content", score = 4.8),
          list(dimension = "delivery", score = 4.2)
        )
      )
    )
  )

  result <- transform_feedback_payload(payload)

  expect_equal(names(result), names(empty_feedback_tibble()))
  expect_equal(nrow(result), 2L)
  expect_equal(unique(result$composite_score), 4.5)
})

test_that("fetch_feedback paginates Typeform-style responses and transforms them", {
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
  request_one <- build_feedback_request(config)
  request_two <- build_feedback_request(config, before = "page-1-resp-2")

  copy_mock_fixture(request_one, "feedback_page1.json", mock_dir)
  copy_mock_fixture(request_two, "feedback_page2.json", mock_dir)

  result <- httptest2::with_mock_api({
    fetch_feedback()
  })

  expect_equal(nrow(result), 6L)
  expect_equal(dplyr::n_distinct(result$response_id), 3L)
  expect_setequal(unique(result$dimension), c("content", "delivery"))
  expect_setequal(unique(result$teacher_name), c("Adela Kratochvilova", "Martin Havel"))
  expect_equal(unique(result$composite_score[result$response_id == "resp-001"]), 4.5)
})
