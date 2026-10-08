test_that("sidebar UI replaces client with workshop topic and score areas", {
  html <- htmltools::renderTags(mod_sidebar_filters_ui("sidebar"))$html

  expect_match(html, "Téma workshopu")
  expect_match(html, "Hodnocené oblasti")
})

test_that("overview UI shows response-count KPI", {
  html <- htmltools::renderTags(mod_overview_ui("overview"))$html

  expect_match(html, "Počet odpovědí")
})

test_that("comments table data keeps separate comment prompts", {
  responses <- tibble::tibble(
    submission_key = "sub-1",
    response_id = "resp-1",
    response_token = "tok-1",
    submitted_at = as.POSIXct("2026-08-10 09:00:00", tz = "UTC"),
    landed_at = as.POSIXct("2026-08-10 08:55:00", tz = "UTC"),
    workshop_date = as.Date("2026-08-10"),
    workshop_topic_id = "topic-1",
    workshop_topic_label = "Delegování",
    teacher_id = "teacher-1",
    teacher_name = "Aneta Haramijová",
    overall_score = 4,
    professional_benefit_score = 4,
    personal_benefit_score = 3,
    lecturer_score = 4,
    organization_score = 3,
    comment_positive = "Silná facilitace.",
    comment_change = "Více času na cvičení.",
    comment_for_teacher = "Děkuji za energii.",
    response_status = "final",
    workshop_instance_id = "2026-08-10::topic-1::teacher-1"
  )

  table_data <- build_comments_table_data(make_test_feedback_data(responses))

  expect_true(all(c("Co jste ocenili", "Co změnit", "Zpráva pro lektora") %in% names(table_data)))
})
