test_that("apply_feedback_filters keeps responses on overall score and narrows only score areas", {
  responses <- tibble::tibble(
    submission_key = c("sub-1", "sub-2"),
    response_id = c("resp-1", "resp-2"),
    response_token = c("tok-1", "tok-2"),
    submitted_at = as.POSIXct(c("2026-08-10 09:00:00", "2026-08-15 09:00:00"), tz = "UTC"),
    landed_at = as.POSIXct(c("2026-08-10 08:55:00", "2026-08-15 08:55:00"), tz = "UTC"),
    workshop_date = as.Date(c("2026-08-10", "2026-08-15")),
    workshop_topic_id = c("topic-1", "topic-2"),
    workshop_topic_label = c("Delegování", "Práce se změnou"),
    teacher_id = c("teacher-1", "teacher-2"),
    teacher_name = c("Aneta Haramijová", "Barbora Hailandová"),
    overall_score = c(4, 2),
    professional_benefit_score = c(4, 2),
    personal_benefit_score = c(3, 1),
    lecturer_score = c(4, 2),
    organization_score = c(3, 1),
    comment_positive = c("A", "B"),
    comment_change = c("C", "D"),
    comment_for_teacher = c("E", "F"),
    response_status = c("final", "final"),
    workshop_instance_id = c("2026-08-10::topic-1::teacher-1", "2026-08-15::topic-2::teacher-2")
  )
  feedback <- make_test_feedback_data(responses)

  filtered <- apply_feedback_filters(
    feedback,
    list(
      date_range = as.Date(c("2026-08-01", "2026-08-31")),
      teacher_ids = "teacher-1",
      workshop_topic_ids = "topic-1",
      metric_keys = "lecturer"
    )
  )

  expect_equal(nrow(filtered$responses), 1L)
  expect_equal(unique(filtered$responses$overall_score), 4)
  expect_equal(unique(filtered$scores_long$metric_key), "lecturer")
})

test_that("compute_rolling_average uses overall score at workshop grain", {
  responses <- tibble::tibble(
    submission_key = c("sub-1", "sub-2"),
    response_id = c("resp-1", "resp-2"),
    response_token = c("tok-1", "tok-2"),
    submitted_at = as.POSIXct(c("2026-08-10 09:00:00", "2026-08-15 09:00:00"), tz = "UTC"),
    landed_at = as.POSIXct(c("2026-08-10 08:55:00", "2026-08-15 08:55:00"), tz = "UTC"),
    workshop_date = as.Date(c("2026-08-10", "2026-08-15")),
    workshop_topic_id = c("topic-1", "topic-1"),
    workshop_topic_label = c("Delegování", "Delegování"),
    teacher_id = c("teacher-1", "teacher-1"),
    teacher_name = c("Teacher One", "Teacher One"),
    overall_score = c(4, 2),
    professional_benefit_score = c(4, 2),
    personal_benefit_score = c(4, 2),
    lecturer_score = c(4, 2),
    organization_score = c(4, 2),
    comment_positive = c(NA_character_, NA_character_),
    comment_change = c(NA_character_, NA_character_),
    comment_for_teacher = c(NA_character_, NA_character_),
    response_status = c("final", "final"),
    workshop_instance_id = c("2026-08-10::topic-1::teacher-1", "2026-08-15::topic-1::teacher-1")
  )
  feedback <- make_test_feedback_data(responses)

  rolling <- compute_rolling_average(feedback, entity = "teacher", window = 3)

  expect_equal(rolling$rolling_average, c(4, 3))
})

test_that("flag_teacher_alerts uses overall score and topic summaries use workshop topics", {
  floor_teacher <- tibble::tibble(
    submission_key = c("floor-1", "floor-2", "floor-3"),
    response_id = c("resp-1", "resp-2", "resp-3"),
    response_token = c("tok-1", "tok-2", "tok-3"),
    submitted_at = as.POSIXct(c("2026-08-01 09:00:00", "2026-08-08 09:00:00", "2026-08-15 09:00:00"), tz = "UTC"),
    landed_at = as.POSIXct(c("2026-08-01 08:55:00", "2026-08-08 08:55:00", "2026-08-15 08:55:00"), tz = "UTC"),
    workshop_date = as.Date(c("2026-08-01", "2026-08-08", "2026-08-15")),
    workshop_topic_id = c("topic-1", "topic-1", "topic-1"),
    workshop_topic_label = c("Delegování", "Delegování", "Delegování"),
    teacher_id = c("teacher-floor", "teacher-floor", "teacher-floor"),
    teacher_name = c("Low Floor", "Low Floor", "Low Floor"),
    overall_score = c(3.3, 3.2, 3.1),
    professional_benefit_score = c(3, 3, 3),
    personal_benefit_score = c(3, 3, 3),
    lecturer_score = c(3, 3, 3),
    organization_score = c(3, 3, 3),
    comment_positive = c(NA_character_, NA_character_, NA_character_),
    comment_change = c(NA_character_, NA_character_, NA_character_),
    comment_for_teacher = c(NA_character_, NA_character_, NA_character_),
    response_status = c("final", "final", "final"),
    workshop_instance_id = c("2026-08-01::topic-1::teacher-floor", "2026-08-08::topic-1::teacher-floor", "2026-08-15::topic-1::teacher-floor")
  )
  stable_topic <- tibble::tibble(
    submission_key = c("topic-1", "topic-2"),
    response_id = c("topic-r1", "topic-r2"),
    response_token = c("topic-t1", "topic-t2"),
    submitted_at = as.POSIXct(c("2026-08-02 09:00:00", "2026-08-09 09:00:00"), tz = "UTC"),
    landed_at = as.POSIXct(c("2026-08-02 08:55:00", "2026-08-09 08:55:00"), tz = "UTC"),
    workshop_date = as.Date(c("2026-08-02", "2026-08-09")),
    workshop_topic_id = c("topic-2", "topic-2"),
    workshop_topic_label = c("Práce se změnou", "Práce se změnou"),
    teacher_id = c("teacher-1", "teacher-2"),
    teacher_name = c("Teacher One", "Teacher Two"),
    overall_score = c(4.2, 4.1),
    professional_benefit_score = c(4, 4),
    personal_benefit_score = c(4, 4),
    lecturer_score = c(4, 4),
    organization_score = c(4, 4),
    comment_positive = c(NA_character_, NA_character_),
    comment_change = c(NA_character_, NA_character_),
    comment_for_teacher = c(NA_character_, NA_character_),
    response_status = c("final", "final"),
    workshop_instance_id = c("2026-08-02::topic-2::teacher-1", "2026-08-09::topic-2::teacher-2")
  )
  feedback <- make_test_feedback_data(dplyr::bind_rows(floor_teacher, stable_topic))

  alerts <- flag_teacher_alerts(feedback, window = 3, floor_score = 3.5, decline_delta = 1.0)
  topic_summary <- summarise_score_areas(feedback, entity = "topic")

  expect_true(any(alerts$teacher_id == "teacher-floor" & alerts$triggered_absolute_floor))
  expect_true(any(topic_summary$entity_id == "topic-2"))
})
