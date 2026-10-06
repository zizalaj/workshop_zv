testthat::skip("Obsolete legacy scoring tests.")

make_feedback_rows <- function(scores,
                               teacher_id = "teacher-1",
                               teacher_name = "Teacher One",
                               client_id = "client-1",
                               client_name = "Client One",
                               start_date = as.Date("2026-01-01")) {
  dimensions <- c("content", "delivery", "engagement", "materials")

  feedback <- purrr::imap_dfr(scores, function(score, index) {
    tibble::tibble(
      response_id = paste0(teacher_id, "-r", index),
      workshop_id = paste0(teacher_id, "-w", index),
      workshop_date = start_date + index - 1L,
      teacher_id = teacher_id,
      teacher_name = teacher_name,
      client_id = client_id,
      client_name = client_name,
      n_participants = 10L,
      dimension = dimensions,
      dimension_score = rep(score, length(dimensions)),
      comment_text = NA_character_
    )
  })

  append_composite_score(feedback)
}

test_that("append_composite_score calculates response means", {
  feedback <- tibble::tibble(
    response_id = c("resp-1", "resp-1", "resp-1", "resp-1"),
    workshop_id = "workshop-1",
    workshop_date = as.Date("2026-05-10"),
    teacher_id = "teacher-1",
    teacher_name = "Teacher One",
    client_id = "client-1",
    client_name = "Client One",
    n_participants = 12L,
    dimension = c("content", "delivery", "engagement", "materials"),
    dimension_score = c(4, 5, 3, 4),
    comment_text = "Skvely workshop."
  )

  scored <- append_composite_score(feedback)

  expect_equal(unique(scored$composite_score), 4)
})

test_that("compute_rolling_average uses available workshops for short histories", {
  feedback <- make_feedback_rows(c(4, 2))

  rolling <- compute_rolling_average(feedback, entity = "teacher", window = 3)

  expect_equal(rolling$rolling_average, c(4, 3))
})

test_that("flag_teacher_alerts captures absolute floor and self-history decline branches", {
  floor_teacher <- make_feedback_rows(
    c(3.3, 3.2, 3.1),
    teacher_id = "teacher-floor",
    teacher_name = "Low Floor"
  )

  declining_teacher <- make_feedback_rows(
    c(rep(5, 10), rep(3.6, 3)),
    teacher_id = "teacher-decline",
    teacher_name = "Declining Strongly"
  )

  stable_teacher <- make_feedback_rows(
    c(4.2, 4.1, 4.3, 4.2),
    teacher_id = "teacher-stable",
    teacher_name = "Stable Teacher"
  )

  alerts <- dplyr::bind_rows(floor_teacher, declining_teacher, stable_teacher) |>
    flag_teacher_alerts(window = 3, floor_score = 3.5, decline_delta = 1.0)

  expect_true(any(alerts$teacher_id == "teacher-floor" & alerts$triggered_absolute_floor))
  expect_true(any(alerts$teacher_id == "teacher-decline" & alerts$triggered_self_decline))
  expect_false(any(alerts$teacher_id == "teacher-stable"))
})
