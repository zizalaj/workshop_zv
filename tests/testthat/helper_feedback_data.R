build_test_scores_long <- function(responses) {
  purrr::imap_dfr(score_area_labels_cs, function(metric_label_cs, metric_key) {
    score_column <- paste0(metric_key, "_score")
    responses |>
      dplyr::transmute(
        submission_key = .data$submission_key,
        workshop_instance_id = .data$workshop_instance_id,
        teacher_id = .data$teacher_id,
        teacher_name = .data$teacher_name,
        workshop_topic_id = .data$workshop_topic_id,
        workshop_topic_label = .data$workshop_topic_label,
        workshop_date = .data$workshop_date,
        metric_key = metric_key,
        metric_label_cs = metric_label_cs,
        score = .data[[score_column]]
      )
  })
}

make_test_feedback_data <- function(responses) {
  list(
    responses = responses,
    scores_long = build_test_scores_long(responses)
  )
}
