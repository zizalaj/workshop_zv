`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) {
    return(y)
  }

  x
}

dimension_weights <- c(
  content = 1,
  delivery = 1,
  engagement = 1,
  materials = 1
)

rolling_window_size <- 3L
alert_floor_score <- 3.5
alert_decline_delta <- 1.0

empty_feedback_tibble <- function() {
  tibble::tibble(
    response_id = character(),
    workshop_id = character(),
    workshop_date = as.Date(character()),
    teacher_id = character(),
    teacher_name = character(),
    client_id = character(),
    client_name = character(),
    n_participants = integer(),
    dimension = character(),
    dimension_score = double(),
    comment_text = character(),
    composite_score = double()
  )
}

ordered_dimensions <- function(dimensions) {
  known <- names(dimension_weights)
  extras <- sort(setdiff(unique(dimensions), known))
  unique(c(known, extras))
}

append_composite_score <- function(feedback, weights = dimension_weights) {
  if (!nrow(feedback)) {
    feedback[["composite_score"]] <- numeric(0)
    return(feedback)
  }

  weight_lookup <- unname(weights[match(feedback$dimension, names(weights))])
  weight_lookup[is.na(weight_lookup)] <- 1

  response_scores <- feedback |>
    dplyr::mutate(
      dimension_weight = weight_lookup,
      weighted_score = .data$dimension_score * .data$dimension_weight
    ) |>
    dplyr::group_by(.data$response_id) |>
    dplyr::summarise(
      composite_score = sum(.data$weighted_score, na.rm = TRUE) /
        sum(.data$dimension_weight, na.rm = TRUE),
      .groups = "drop"
    )

  feedback |>
    dplyr::left_join(response_scores, by = "response_id")
}

moving_average <- function(values, window = rolling_window_size) {
  if (!length(values)) {
    return(double())
  }

  purrr::map_dbl(seq_along(values), function(index) {
    start_index <- max(1L, index - window + 1L)
    mean(values[start_index:index], na.rm = TRUE)
  })
}

summarise_dimension_scores <- function(feedback, entity = c("teacher", "client")) {
  entity <- match.arg(entity)
  group_cols <- entity_columns(entity)

  if (is.null(feedback) || !nrow(feedback)) {
    return(tibble::tibble(
      entity_id = character(),
      entity_name = character(),
      dimension = character(),
      average_score = double(),
      response_n = integer()
    ))
  }

  feedback |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_cols)),
      .data$dimension
    ) |>
    dplyr::summarise(
      average_score = mean(.data$dimension_score, na.rm = TRUE),
      response_n = dplyr::n_distinct(.data$response_id),
      .groups = "drop"
    ) |>
    dplyr::rename(
      entity_id = !!group_cols[[1]],
      entity_name = !!group_cols[[2]]
    )
}

compute_prior_period_bounds <- function(start_date, end_date) {
  range_days <- as.integer(as.Date(end_date) - as.Date(start_date)) + 1L
  prior_end <- as.Date(start_date) - 1L
  prior_start <- prior_end - (range_days - 1L)

  list(
    current_start = as.Date(start_date),
    current_end = as.Date(end_date),
    prior_start = prior_start,
    prior_end = prior_end
  )
}

format_score_with_n <- function(score, n, accuracy = 0.1) {
  vapply(seq_along(score), function(index) {
    if (is.na(score[[index]])) {
      return("N/A")
    }

    paste0(
      scales::number(score[[index]], accuracy = accuracy),
      " (n=",
      scales::comma(n[[index]] %||% 0),
      ")"
    )
  }, character(1))
}

compute_response_rate <- function(feedback) {
  responses <- summarise_responses(feedback)
  if (!nrow(responses)) {
    return(NA_real_)
  }

  workshop_participants <- responses |>
    dplyr::group_by(.data$workshop_id) |>
    dplyr::summarise(
      n_participants = dplyr::first(.data$n_participants),
      responses_n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(.data$n_participants), .data$n_participants > 0)

  if (!nrow(workshop_participants)) {
    return(NA_real_)
  }

  sum(workshop_participants$responses_n) / sum(workshop_participants$n_participants)
}

score_area_labels_cs <- c(
  professional_benefit = "Profesní přínos",
  personal_benefit = "Osobní přínos",
  lecturer = "Lektoři",
  organization = "Organizace"
)

empty_responses_tibble <- function() {
  tibble::tibble(
    submission_key = character(),
    response_id = character(),
    response_token = character(),
    submitted_at = as.POSIXct(character(), tz = "UTC"),
    landed_at = as.POSIXct(character(), tz = "UTC"),
    workshop_date = as.Date(character()),
    workshop_topic_id = character(),
    workshop_topic_label = character(),
    teacher_id = character(),
    teacher_name = character(),
    overall_score = double(),
    professional_benefit_score = double(),
    personal_benefit_score = double(),
    lecturer_score = double(),
    organization_score = double(),
    comment_positive = character(),
    comment_change = character(),
    comment_for_teacher = character(),
    response_status = character(),
    workshop_instance_id = character()
  )
}

empty_scores_tibble <- function() {
  tibble::tibble(
    submission_key = character(),
    workshop_instance_id = character(),
    teacher_id = character(),
    teacher_name = character(),
    workshop_topic_id = character(),
    workshop_topic_label = character(),
    workshop_date = as.Date(character()),
    metric_key = character(),
    metric_label_cs = character(),
    score = double()
  )
}

empty_feedback_data <- function() {
  list(
    responses = empty_responses_tibble(),
    scores_long = empty_scores_tibble()
  )
}

feedback_responses <- function(feedback) {
  if (is.null(feedback)) {
    return(empty_responses_tibble())
  }

  if (is.data.frame(feedback) && "overall_score" %in% names(feedback)) {
    return(feedback)
  }

  if (is.list(feedback) && !is.null(feedback$responses)) {
    return(feedback$responses)
  }

  empty_responses_tibble()
}

feedback_scores_long <- function(feedback) {
  if (is.null(feedback)) {
    return(empty_scores_tibble())
  }

  if (is.data.frame(feedback) && "metric_key" %in% names(feedback)) {
    return(feedback)
  }

  if (is.list(feedback) && !is.null(feedback$scores_long)) {
    return(feedback$scores_long)
  }

  empty_scores_tibble()
}

ordered_score_area_keys <- function(metric_keys = NULL) {
  known <- names(score_area_labels_cs)
  extras <- sort(setdiff(unique(metric_keys %||% character()), known))
  unique(c(known, extras))
}

ordered_score_area_labels <- function(metric_keys = NULL) {
  keys <- ordered_score_area_keys(metric_keys)
  labels <- unname(score_area_labels_cs[keys])
  labels[is.na(labels)] <- keys[is.na(labels)]
  labels
}

score_area_label <- function(metric_key) {
  score_area_labels_cs[[metric_key]] %||% metric_key
}

detect_score_scale_max <- function(values, minimum = 4) {
  if (is.list(values) && !is.data.frame(values)) {
    values <- feedback_responses(values)$overall_score
  }

  if (is.data.frame(values)) {
    values <- if ("overall_score" %in% names(values)) {
      values$overall_score
    } else if ("score" %in% names(values)) {
      values$score
    } else {
      numeric()
    }
  }

  if (!length(values) || all(is.na(values))) {
    return(minimum)
  }

  max(minimum, ceiling(max(values, na.rm = TRUE)))
}

entity_columns <- function(entity = c("teacher", "topic")) {
  entity <- match.arg(entity)

  if (identical(entity, "teacher")) {
    return(c("teacher_id", "teacher_name"))
  }

  c("workshop_topic_id", "workshop_topic_label")
}

filter_feedback_by_entity <- function(feedback, entity = c("teacher", "topic"), entity_ids = character()) {
  entity <- match.arg(entity)
  if (!length(entity_ids)) {
    return(feedback)
  }

  responses <- feedback_responses(feedback)
  scores_long <- feedback_scores_long(feedback)
  id_col <- entity_columns(entity)[[1]]

  filtered_responses <- responses |>
    dplyr::filter(.data[[id_col]] %in% entity_ids)

  filtered_scores <- scores_long |>
    dplyr::filter(.data$submission_key %in% filtered_responses$submission_key)

  list(
    responses = filtered_responses,
    scores_long = filtered_scores
  )
}

apply_feedback_filters <- function(feedback, filters, include_date = TRUE) {
  responses <- feedback_responses(feedback)
  scores_long <- feedback_scores_long(feedback)

  if (!nrow(responses)) {
    return(empty_feedback_data())
  }

  filtered_responses <- responses

  if (length(filters$teacher_ids %||% character())) {
    filtered_responses <- filtered_responses |>
      dplyr::filter(.data$teacher_id %in% filters$teacher_ids)
  }

  if (length(filters$workshop_topic_ids %||% character())) {
    filtered_responses <- filtered_responses |>
      dplyr::filter(.data$workshop_topic_id %in% filters$workshop_topic_ids)
  }

  if (isTRUE(include_date) &&
      length(filters$date_range %||% as.Date(character())) == 2L &&
      all(!is.na(filters$date_range))) {
    filtered_responses <- filtered_responses |>
      dplyr::filter(
        .data$workshop_date >= as.Date(filters$date_range[[1]]) &
          .data$workshop_date <= as.Date(filters$date_range[[2]])
      )
  }

  filtered_scores <- scores_long |>
    dplyr::filter(.data$submission_key %in% filtered_responses$submission_key)

  if (length(filters$metric_keys %||% character())) {
    filtered_scores <- filtered_scores |>
      dplyr::filter(.data$metric_key %in% filters$metric_keys)
  }

  list(
    responses = filtered_responses,
    scores_long = filtered_scores
  )
}

summarise_responses <- function(feedback) {
  feedback_responses(feedback)
}

summarise_workshops <- function(feedback, entity = c("teacher", "topic")) {
  entity <- match.arg(entity)
  group_cols <- entity_columns(entity)
  responses <- feedback_responses(feedback)

  if (!nrow(responses)) {
    return(tibble::tibble(
      workshop_instance_id = character(),
      workshop_date = as.Date(character()),
      teacher_name = character(),
      workshop_topic_label = character(),
      entity_id = character(),
      entity_name = character(),
      workshop_score = double(),
      response_n = integer()
    ))
  }

  responses |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_cols)),
      .data$workshop_instance_id,
      .data$workshop_date,
      .data$teacher_name,
      .data$workshop_topic_label
    ) |>
    dplyr::summarise(
      workshop_score = mean(.data$overall_score, na.rm = TRUE),
      response_n = dplyr::n_distinct(.data$submission_key),
      .groups = "drop"
    ) |>
    dplyr::rename(
      entity_id = !!group_cols[[1]],
      entity_name = !!group_cols[[2]]
    )
}

compute_rolling_average <- function(feedback, entity = c("teacher", "topic"), window = rolling_window_size) {
  entity <- match.arg(entity)
  workshop_summary <- summarise_workshops(feedback, entity)

  if (!nrow(workshop_summary)) {
    return(tibble::tibble(
      entity_id = character(),
      entity_name = character(),
      workshop_instance_id = character(),
      workshop_date = as.Date(character()),
      teacher_name = character(),
      workshop_topic_label = character(),
      workshop_score = double(),
      response_n = integer(),
      rolling_average = double()
    ))
  }

  workshop_summary |>
    dplyr::arrange(.data$entity_name, .data$workshop_date, .data$workshop_instance_id) |>
    dplyr::group_by(.data$entity_id, .data$entity_name) |>
    dplyr::mutate(
      rolling_average = moving_average(.data$workshop_score, window = window)
    ) |>
    dplyr::ungroup()
}

summarise_score_areas <- function(feedback, entity = c("teacher", "topic")) {
  entity <- match.arg(entity)
  group_cols <- entity_columns(entity)
  scores_long <- feedback_scores_long(feedback)

  if (!nrow(scores_long)) {
    return(tibble::tibble(
      entity_id = character(),
      entity_name = character(),
      metric_key = character(),
      metric_label_cs = character(),
      average_score = double(),
      response_n = integer()
    ))
  }

  scores_long |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_cols)),
      .data$metric_key,
      .data$metric_label_cs
    ) |>
    dplyr::summarise(
      average_score = mean(.data$score, na.rm = TRUE),
      response_n = dplyr::n_distinct(.data$submission_key),
      .groups = "drop"
    ) |>
    dplyr::rename(
      entity_id = !!group_cols[[1]],
      entity_name = !!group_cols[[2]]
    )
}

split_period_data <- function(feedback, start_date, end_date) {
  bounds <- compute_prior_period_bounds(start_date, end_date)
  responses <- feedback_responses(feedback)

  list(
    current = responses |>
      dplyr::filter(
        .data$workshop_date >= bounds$current_start &
          .data$workshop_date <= bounds$current_end
      ),
    prior = responses |>
      dplyr::filter(
        .data$workshop_date >= bounds$prior_start &
          .data$workshop_date <= bounds$prior_end
      ),
    bounds = bounds
  )
}

compute_overall_delta <- function(feedback, start_date, end_date) {
  periods <- split_period_data(feedback, start_date, end_date)

  current_average <- if (nrow(periods$current)) {
    mean(periods$current$overall_score, na.rm = TRUE)
  } else {
    NA_real_
  }

  prior_average <- if (nrow(periods$prior)) {
    mean(periods$prior$overall_score, na.rm = TRUE)
  } else {
    NA_real_
  }

  list(
    current_average = current_average,
    prior_average = prior_average,
    current_n = nrow(periods$current),
    prior_n = nrow(periods$prior),
    delta = current_average - prior_average
  )
}

compute_entity_period_delta <- function(feedback, entity = c("teacher", "topic"), start_date, end_date) {
  entity <- match.arg(entity)
  group_cols <- entity_columns(entity)
  periods <- split_period_data(feedback, start_date, end_date)

  current_metrics <- periods$current |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      current_average = mean(.data$overall_score, na.rm = TRUE),
      current_n = dplyr::n(),
      .groups = "drop"
    )

  prior_metrics <- periods$prior |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      prior_average = mean(.data$overall_score, na.rm = TRUE),
      prior_n = dplyr::n(),
      .groups = "drop"
    )

  dplyr::full_join(current_metrics, prior_metrics, by = group_cols) |>
    dplyr::mutate(
      delta = .data$current_average - .data$prior_average
    )
}

trend_arrow <- function(delta, tolerance = 0.01) {
  vapply(delta, function(value) {
    if (is.na(value)) {
      return("–")
    }

    if (value > tolerance) {
      return("▲")
    }

    if (value < (-1 * tolerance)) {
      return("▼")
    }

    "→"
  }, character(1))
}

flag_teacher_alerts <- function(feedback, window = rolling_window_size, floor_score = alert_floor_score, decline_delta = alert_decline_delta) {
  rolling <- compute_rolling_average(feedback, entity = "teacher", window = window)
  responses <- feedback_responses(feedback)

  if (!nrow(rolling) || !nrow(responses)) {
    return(tibble::tibble(
      teacher_id = character(),
      teacher_name = character(),
      current_rolling_average = double(),
      current_response_n = integer(),
      all_time_average = double(),
      effective_threshold = double(),
      below_threshold_n = integer(),
      triggered_absolute_floor = logical(),
      triggered_self_decline = logical(),
      sparkline_values = list()
    ))
  }

  all_time_average <- responses |>
    dplyr::group_by(.data$teacher_id, .data$teacher_name) |>
    dplyr::summarise(
      all_time_average = mean(.data$overall_score, na.rm = TRUE),
      .groups = "drop"
    )

  rolling |>
    dplyr::left_join(all_time_average, by = c("entity_id" = "teacher_id", "entity_name" = "teacher_name")) |>
    dplyr::group_by(.data$entity_id, .data$entity_name, .data$all_time_average) |>
    dplyr::summarise(
      current_rolling_average = dplyr::last(.data$rolling_average),
      current_response_n = dplyr::last(.data$response_n),
      effective_threshold = dplyr::last(
        pmax(floor_score, .data$all_time_average - decline_delta)
      ),
      below_threshold_n = sum(
        .data$rolling_average < pmax(floor_score, .data$all_time_average - decline_delta),
        na.rm = TRUE
      ),
      triggered_absolute_floor = dplyr::last(.data$rolling_average) < floor_score,
      triggered_self_decline = dplyr::last(.data$rolling_average) <
        (dplyr::last(.data$all_time_average) - decline_delta),
      sparkline_values = list(.data$rolling_average),
      .groups = "drop"
    ) |>
    dplyr::rename(
      teacher_id = entity_id,
      teacher_name = entity_name
    ) |>
    dplyr::filter(.data$triggered_absolute_floor | .data$triggered_self_decline) |>
    dplyr::arrange(.data$current_rolling_average)
}
