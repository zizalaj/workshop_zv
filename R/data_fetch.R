feedback_auth_header <- "Authorization"
feedback_auth_prefix <- "Bearer"
feedback_page_size <- 1000L
feedback_since_query_param <- "since"
feedback_before_query_param <- "before"
feedback_page_size_query_param <- "page_size"
feedback_records_path <- c("items")
required_feedback_env_vars <- c(
  "FEEDBACK_API_BASE_URL",
  "FEEDBACK_API_TOKEN",
  "FORM_ID"
)
DEFAULT_LOOKBACK_MONTHS <- 12L

abort_feedback_error <- function(message, class_name = "feedback_request_error") {
  stop(
    structure(
      list(message = message),
      class = c(class_name, "feedback_error", "error", "condition")
    )
  )
}

feedback_since_timestamp <- function(months = DEFAULT_LOOKBACK_MONTHS) {
  lookback_date <- lubridate::`%m-%`(
    Sys.Date(),
    lubridate::period(months = months)
  )

  format(
    as.POSIXct(lookback_date, tz = "UTC"),
    "%Y-%m-%dT00:00:00Z",
    tz = "UTC"
  )
}

pluck_path <- function(x, path, default = NULL) {
  purrr::pluck(x, !!!path, .default = default)
}

extract_named_value <- function(values, candidates) {
  if (is.null(values)) {
    return(NULL)
  }

  value_names <- names(values) %||% character()
  if (!length(value_names)) {
    return(NULL)
  }

  normalized_names <- vapply(value_names, normalize_feedback_key, character(1))
  for (candidate in candidates) {
    match_index <- match(candidate, normalized_names)
    if (!is.na(match_index)) {
      return(values[[match_index]])
    }
  }

  NULL
}

typeform_answer_value <- function(answer) {
  answer[["text"]] %||%
    answer[["number"]] %||%
    pluck_path(answer, c("choice", "label"), default = NULL) %||%
    pluck_path(answer, c("choices", "labels"), default = NULL) %||%
    answer[["date"]] %||%
    answer[["boolean"]] %||%
    answer[["email"]] %||%
    answer[["phone_number"]] %||%
    answer[["url"]] %||%
    answer[["file_url"]]
}

guess_dimension_name <- function(answer) {
  candidate_names <- c(
    pluck_path(answer, c("field", "ref"), default = NULL),
    pluck_path(answer, c("field", "title"), default = NULL)
  )
  candidate_names <- candidate_names[!vapply(candidate_names, is.null, logical(1))]
  if (!length(candidate_names)) {
    return(NULL)
  }

  normalized_candidates <- unique(vapply(candidate_names, normalize_feedback_key, character(1)))
  for (candidate in normalized_candidates) {
    if (candidate %in% names(dimension_weights)) {
      return(candidate)
    }
    if (grepl("dimension_", candidate)) {
      stripped <- sub("^dimension_", "", candidate)
      if (stripped %in% names(dimension_weights)) {
        return(stripped)
      }
    }
    if (grepl("content|obsah", candidate)) {
      return("content")
    }
    if (grepl("delivery|facilitation|lektor|prednes", candidate)) {
      return("delivery")
    }
    if (grepl("engagement|zapoj", candidate)) {
      return("engagement")
    }
    if (grepl("materials|material", candidate)) {
      return("materials")
    }
  }

  NULL
}

extract_comment_from_answers <- function(answers) {
  if (is.null(answers) || !length(answers)) {
    return(NA_character_)
  }

  for (answer in answers) {
    answer_value <- typeform_answer_value(answer)
    if (!is.character(answer_value) || !length(answer_value)) {
      next
    }

    candidate_names <- c(
      pluck_path(answer, c("field", "ref"), default = NULL),
      pluck_path(answer, c("field", "title"), default = NULL)
    )
    candidate_names <- candidate_names[!vapply(candidate_names, is.null, logical(1))]
    if (!length(candidate_names)) {
      next
    }

    normalized_names <- vapply(candidate_names, normalize_feedback_key, character(1))
    if (any(grepl("comment|komentar|poznam", normalized_names))) {
      return(enc2utf8(as.character(answer_value[[1]])))
    }
  }

  NA_character_
}

extract_dimension_rows <- function(record) {
  dimensions <- record[["dimension_scores"]] %||% record[["dimensions"]]
  if (length(dimensions)) {
    return(dimensions)
  }

  if (!is.null(record[["dimension"]]) && !is.null(record[["dimension_score"]])) {
    return(list(list(
      dimension = record[["dimension"]],
      score = record[["dimension_score"]]
    )))
  }

  answers <- record[["answers"]] %||% list()
  purrr::compact(purrr::map(answers, function(answer) {
    dimension_name <- guess_dimension_name(answer)
    dimension_score <- suppressWarnings(as.numeric(typeform_answer_value(answer)))

    if (is.null(dimension_name) || is.na(dimension_score)) {
      return(NULL)
    }

    list(
      dimension = dimension_name,
      score = dimension_score
    )
  }))
}

flatten_feedback_record <- function(record) {
  dimension_rows <- extract_dimension_rows(record)
  if (!length(dimension_rows)) {
    return(empty_feedback_tibble())
  }

  hidden_values <- record[["hidden"]] %||% list()
  response_id <- as.character(record[["response_id"]] %||% record[["id"]] %||% NA_character_)
  workshop_id <- as.character(
    record[["workshop_id"]] %||%
      extract_named_value(hidden_values, c("workshop_id", "workshop")) %||%
      response_id
  )
  workshop_date <- as.Date(
    record[["workshop_date"]] %||%
      record[["date"]] %||%
      extract_named_value(hidden_values, c("workshop_date", "date")) %||%
      substr(record[["submitted_at"]] %||% "", 1L, 10L)
  )
  teacher_id <- as.character(
    record[["teacher_id"]] %||%
      extract_named_value(hidden_values, c("teacher_id", "lecturer_id", "lektor_id")) %||%
      NA_character_
  )
  teacher_name <- enc2utf8(as.character(
    record[["teacher_name"]] %||%
      extract_named_value(hidden_values, c("teacher_name", "lecturer_name", "lektor_name")) %||%
      NA_character_
  ))
  client_id <- as.character(
    record[["client_id"]] %||%
      extract_named_value(hidden_values, c("client_id")) %||%
      NA_character_
  )
  client_name <- enc2utf8(as.character(
    record[["client_name"]] %||%
      extract_named_value(hidden_values, c("client_name", "company_name", "firma")) %||%
      NA_character_
  ))
  n_participants <- suppressWarnings(as.integer(
    record[["n_participants"]] %||%
      record[["participants"]] %||%
      extract_named_value(hidden_values, c("n_participants", "participants", "participant_count"))
  ))
  comment_text <- enc2utf8(as.character(
    record[["comment_text"]] %||%
      record[["comment"]] %||%
      extract_named_value(hidden_values, c("comment_text", "comment")) %||%
      extract_comment_from_answers(record[["answers"]] %||% list()) %||%
      NA_character_
  ))

  purrr::map_dfr(dimension_rows, function(dimension_row) {
    tibble::tibble(
      response_id = response_id,
      workshop_id = workshop_id,
      workshop_date = workshop_date,
      teacher_id = teacher_id,
      teacher_name = teacher_name,
      client_id = client_id,
      client_name = client_name,
      n_participants = n_participants,
      dimension = enc2utf8(as.character(
        dimension_row[["dimension"]] %||%
          dimension_row[["name"]] %||%
          dimension_row[["key"]] %||%
          NA_character_
      )),
      dimension_score = as.numeric(
        dimension_row[["dimension_score"]] %||%
          dimension_row[["score"]] %||%
          dimension_row[["value"]] %||%
          NA_real_
      ),
      comment_text = comment_text
    )
  })
}

build_feedback_request <- function(config, before = NULL) {
  request <- httr2::request(
    feedback_responses_url(config$base_url, config$form_id)
  ) |>
    httr2::req_headers(
      !!feedback_auth_header := paste(feedback_auth_prefix, config$token)
    ) |>
    httr2::req_url_query(
      !!feedback_page_size_query_param := feedback_page_size,
      !!feedback_since_query_param := feedback_since_timestamp()
    ) |>
    httr2::req_retry(max_tries = 5) |>
    httr2::req_error(is_error = function(resp) FALSE)

  if (!is.null(before) && nzchar(before)) {
    request <- request |>
      httr2::req_url_query(!!feedback_before_query_param := before)
  }

  request
}

fetch_feedback_pages <- function(config) {
  pages <- list()
  before_token <- NULL

  for (page_index in seq_len(1000L)) {
    page_payload <- request_feedback_page(config, before = before_token)
    records <- pluck_path(page_payload, feedback_records_path, default = NULL) %||%
      pluck_path(page_payload, c("data"), default = list())

    if (!length(records)) {
      break
    }

    pages[[length(pages) + 1L]] <- page_payload

    last_token <- as.character(records[[length(records)]][["token"]] %||% "")
    if (length(records) < feedback_page_size || !nzchar(last_token)) {
      break
    }

    before_token <- last_token

    if (page_index == 1000L) {
      abort_feedback_error("Načítání Feedback API překročilo limit 1000 stránek.")
    }
  }

  pages
}

feedback_field_specs <- list(
  workshop_date = list(type = "date", patterns = c("datum_workshopu")),
  workshop_topic = list(type = "multiple_choice", patterns = c("tema_workshopu")),
  overall_score = list(type = "rating", patterns = c("celkove", "workshop")),
  professional_benefit_score = list(type = "rating", patterns = c("profesni_prinos")),
  personal_benefit_score = list(type = "rating", patterns = c("osobni_prinos")),
  lecturer_score = list(type = "rating", patterns = c("^lektori$")),
  teacher = list(type = "multiple_choice", patterns = c("jmeno_lektora")),
  organization_score = list(type = "rating", patterns = c("^organizace$")),
  comment_positive = list(type = "long_text", patterns = c("nejvice_ocenili")),
  comment_change = list(type = "long_text", patterns = c("zmenili", "chybelo")),
  comment_for_teacher = list(type = "long_text", patterns = c("tip_pro_lektora", "zprava"))
)

score_area_specs <- list(
  professional_benefit = list(
    source_name = "professional_benefit_score",
    label = "Profesní přínos"
  ),
  personal_benefit = list(
    source_name = "personal_benefit_score",
    label = "Osobní přínos"
  ),
  lecturer = list(
    source_name = "lecturer_score",
    label = "Lektoři"
  ),
  organization = list(
    source_name = "organization_score",
    label = "Organizace"
  )
)

validate_feedback_configuration <- function() {
  missing_vars <- required_feedback_env_vars[
    trimws(Sys.getenv(required_feedback_env_vars, unset = "")) == ""
  ]

  if (length(missing_vars)) {
    abort_feedback_error(
      paste(
        "Chybí povinné proměnné prostředí:",
        paste(missing_vars, collapse = ", ")
      ),
      class_name = "feedback_config_error"
    )
  }

  list(
    base_url = trimws(Sys.getenv("FEEDBACK_API_BASE_URL")),
    token = Sys.getenv("FEEDBACK_API_TOKEN"),
    form_id = trimws(Sys.getenv("FORM_ID"))
  )
}

feedback_form_url <- function(base_url, form_id) {
  paste0(sub("/+$", "", base_url), "/forms/", form_id)
}

feedback_responses_url <- function(base_url, form_id) {
  paste0(feedback_form_url(base_url, form_id), "/responses")
}

normalize_feedback_key <- function(value) {
  normalized <- enc2utf8(as.character(value))
  normalized <- iconv(normalized, to = "ASCII//TRANSLIT")
  normalized <- tolower(normalized)
  normalized <- gsub("[^a-z0-9]+", "_", normalized)
  normalized <- gsub("^_|_$", "", normalized)
  normalized
}

as_scalar_character <- function(value) {
  if (is.null(value) || !length(value) || is.na(value[[1]])) {
    return(NA_character_)
  }

  enc2utf8(as.character(value[[1]]))
}

as_scalar_numeric <- function(value) {
  if (is.null(value) || !length(value) || is.na(value[[1]])) {
    return(NA_real_)
  }

  suppressWarnings(as.numeric(value[[1]]))
}

as_scalar_datetime <- function(value) {
  if (is.null(value) || !length(value) || is.na(value[[1]]) || !nzchar(as.character(value[[1]]))) {
    return(as.POSIXct(NA))
  }

  as.POSIXct(as.character(value[[1]]), tz = "UTC")
}

match_feedback_field <- function(form_fields, role_name, field_spec) {
  candidates <- purrr::keep(form_fields, function(field) {
    normalized_title <- normalize_feedback_key(field[["title"]] %||% "")
    identical(field[["type"]], field_spec$type) &&
      any(vapply(
        field_spec$patterns,
        function(pattern) grepl(pattern, normalized_title),
        logical(1)
      ))
  })

  if (!length(candidates)) {
    abort_feedback_error(
      paste("Ve formuláři se nepodařilo najít pole pro roli", shQuote(role_name)),
      class_name = "feedback_schema_error"
    )
  }

  if (length(candidates) > 1L) {
    abort_feedback_error(
      paste("Ve formuláři existuje více kandidátů pro roli", shQuote(role_name)),
      class_name = "feedback_schema_error"
    )
  }

  candidates[[1]]
}

resolve_feedback_fields <- function(form_definition) {
  form_fields <- form_definition[["fields"]] %||% list()

  purrr::imap(
    feedback_field_specs,
    function(field_spec, role_name) match_feedback_field(form_fields, role_name, field_spec)
  )
}

build_feedback_form_request <- function(config) {
  httr2::request(feedback_form_url(config$base_url, config$form_id)) |>
    httr2::req_headers(
      !!feedback_auth_header := paste(feedback_auth_prefix, config$token)
    ) |>
    httr2::req_retry(max_tries = 5) |>
    httr2::req_error(is_error = function(resp) FALSE)
}

perform_feedback_request <- function(request, error_prefix) {
  response <- tryCatch(
    httr2::req_perform(request),
    error = function(error) {
      abort_feedback_error(
        paste(error_prefix, conditionMessage(error))
      )
    }
  )

  response_status <- httr2::resp_status(response)
  if (response_status >= 400) {
    abort_feedback_error(
      paste("Feedback API vrátila chybu se stavem", response_status)
    )
  }

  tryCatch(
    httr2::resp_body_json(response, simplifyVector = FALSE),
    error = function(error) {
      abort_feedback_error(
        paste("Odpověď Feedback API nešlo zpracovat:", conditionMessage(error))
      )
    }
  )
}

request_feedback_form <- function(config) {
  perform_feedback_request(
    build_feedback_form_request(config),
    error_prefix = "Načtení definice formuláře selhalo:"
  )
}

request_feedback_page <- function(config, before = NULL) {
  perform_feedback_request(
    build_feedback_request(config, before = before),
    error_prefix = "Volání Feedback API selhalo:"
  )
}

extract_answer_map <- function(record) {
  answers <- record[["answers"]] %||% list()
  answer_ids <- vapply(
    answers,
    function(answer) as_scalar_character(pluck_path(answer, c("field", "id"), default = NA_character_)),
    character(1)
  )
  stats::setNames(answers, answer_ids)
}

extract_choice_selection <- function(answer) {
  list(
    ref = as_scalar_character(pluck_path(answer, c("choice", "ref"), default = NA_character_)),
    id = as_scalar_character(pluck_path(answer, c("choice", "id"), default = NA_character_)),
    label = as_scalar_character(pluck_path(answer, c("choice", "label"), default = NA_character_)),
    refs = enc2utf8(as.character(pluck_path(answer, c("choices", "refs"), default = character()))),
    ids = enc2utf8(as.character(pluck_path(answer, c("choices", "ids"), default = character()))),
    labels = enc2utf8(as.character(pluck_path(answer, c("choices", "labels"), default = character())))
  )
}

derive_choice_id <- function(choice_selection, allow_multiple = FALSE) {
  if (allow_multiple) {
    return(as_scalar_character(
      choice_selection$refs[[1]] %||%
        choice_selection$ids[[1]] %||%
        normalize_feedback_key(choice_selection$labels[[1]] %||% NA_character_)
    ))
  }

  as_scalar_character(
    choice_selection$ref %||%
      choice_selection$id %||%
      normalize_feedback_key(choice_selection$label)
  )
}

derive_choice_label <- function(choice_selection, allow_multiple = FALSE) {
  if (allow_multiple) {
    return(as_scalar_character(choice_selection$labels[[1]] %||% NA_character_))
  }

  as_scalar_character(choice_selection$label)
}

response_status_value <- function(record) {
  as_scalar_character(record[["status"]] %||% record[["response_type"]] %||% NA_character_)
}

is_submitted_response <- function(record) {
  identical(response_status_value(record), "final") ||
    identical(as_scalar_character(record[["response_type"]] %||% NA_character_), "completed")
}

build_submission_key <- function(record) {
  response_token <- as_scalar_character(record[["token"]] %||% NA_character_)
  response_id <- as_scalar_character(record[["response_id"]] %||% record[["id"]] %||% NA_character_)

  if (!is.na(response_token) && nzchar(response_token)) {
    return(response_token)
  }

  if (!is.na(response_id) && nzchar(response_id)) {
    return(response_id)
  }

  paste(
    as_scalar_character(record[["submitted_at"]] %||% NA_character_),
    as_scalar_character(record[["landed_at"]] %||% NA_character_),
    sep = "::"
  )
}

build_workshop_instance_id <- function(workshop_date, workshop_topic_id, teacher_id) {
  if (is.na(workshop_date) || is.na(workshop_topic_id) || is.na(teacher_id) ||
      !nzchar(as.character(workshop_topic_id)) || !nzchar(as.character(teacher_id))) {
    return(NA_character_)
  }

  paste(as.character(workshop_date), workshop_topic_id, teacher_id, sep = "::")
}

extract_response_row <- function(record, field_lookup) {
  answer_map <- extract_answer_map(record)
  topic_selection <- extract_choice_selection(answer_map[[field_lookup$workshop_topic$id]])
  teacher_selection <- extract_choice_selection(answer_map[[field_lookup$teacher$id]])

  response_row <- tibble::tibble(
    submission_key = build_submission_key(record),
    response_id = as_scalar_character(record[["response_id"]] %||% record[["id"]] %||% NA_character_),
    response_token = as_scalar_character(record[["token"]] %||% NA_character_),
    submitted_at = as_scalar_datetime(record[["submitted_at"]] %||% NA_character_),
    landed_at = as_scalar_datetime(record[["landed_at"]] %||% NA_character_),
    workshop_date = as.Date(as_scalar_character(answer_map[[field_lookup$workshop_date$id]][["date"]] %||% NA_character_)),
    workshop_topic_id = derive_choice_id(topic_selection),
    workshop_topic_label = derive_choice_label(topic_selection),
    teacher_id = derive_choice_id(teacher_selection, allow_multiple = TRUE),
    teacher_name = derive_choice_label(teacher_selection, allow_multiple = TRUE),
    overall_score = as_scalar_numeric(answer_map[[field_lookup$overall_score$id]][["number"]] %||% NA_real_),
    professional_benefit_score = as_scalar_numeric(answer_map[[field_lookup$professional_benefit_score$id]][["number"]] %||% NA_real_),
    personal_benefit_score = as_scalar_numeric(answer_map[[field_lookup$personal_benefit_score$id]][["number"]] %||% NA_real_),
    lecturer_score = as_scalar_numeric(answer_map[[field_lookup$lecturer_score$id]][["number"]] %||% NA_real_),
    organization_score = as_scalar_numeric(answer_map[[field_lookup$organization_score$id]][["number"]] %||% NA_real_),
    comment_positive = as_scalar_character(answer_map[[field_lookup$comment_positive$id]][["text"]] %||% NA_character_),
    comment_change = as_scalar_character(answer_map[[field_lookup$comment_change$id]][["text"]] %||% NA_character_),
    comment_for_teacher = as_scalar_character(answer_map[[field_lookup$comment_for_teacher$id]][["text"]] %||% NA_character_),
    response_status = response_status_value(record)
  ) |>
    dplyr::mutate(
      workshop_instance_id = build_workshop_instance_id(
        .data$workshop_date,
        .data$workshop_topic_id,
        .data$teacher_id
      )
    )

  list(
    response_row = response_row,
    multi_teacher_selected = length(teacher_selection$labels) > 1L
  )
}

build_scores_long <- function(response_rows) {
  if (!nrow(response_rows)) {
    return(empty_scores_tibble())
  }

  purrr::imap_dfr(score_area_specs, function(score_spec, metric_key) {
    response_rows |>
      dplyr::transmute(
        submission_key = .data$submission_key,
        workshop_instance_id = .data$workshop_instance_id,
        teacher_id = .data$teacher_id,
        teacher_name = .data$teacher_name,
        workshop_topic_id = .data$workshop_topic_id,
        workshop_topic_label = .data$workshop_topic_label,
        workshop_date = .data$workshop_date,
        metric_key = metric_key,
        metric_label_cs = score_spec$label,
        score = .data[[score_spec$source_name]]
      )
  }) |>
    dplyr::filter(!is.na(.data$score))
}

transform_feedback_payload <- function(form_definition, payload_pages) {
  if (is.null(payload_pages)) {
    return(empty_feedback_data())
  }

  pages <- payload_pages
  if (!is.null(pluck_path(payload_pages, feedback_records_path, default = NULL)) ||
      !is.null(pluck_path(payload_pages, c("data"), default = NULL))) {
    pages <- list(payload_pages)
  }

  field_lookup <- resolve_feedback_fields(form_definition)
  extracted_rows <- purrr::map(
    purrr::flatten(
      purrr::map(
        pages,
        ~ pluck_path(.x, feedback_records_path, default = NULL) %||%
          pluck_path(.x, c("data"), default = list())
      )
    ),
    function(record) {
      if (!is_submitted_response(record)) {
        return(NULL)
      }

      extract_response_row(record, field_lookup)
    }
  ) |>
    purrr::compact()

  if (!length(extracted_rows)) {
    return(empty_feedback_data())
  }

  multi_teacher_count <- sum(vapply(extracted_rows, `[[`, logical(1), "multi_teacher_selected"))
  if (multi_teacher_count > 0L) {
    warning(
      sprintf(
        "Typeform vrátil %s odpovědí s více lektory. Používám vždy první zvolenou možnost.",
        multi_teacher_count
      ),
      call. = FALSE
    )
  }

  response_rows <- purrr::map_dfr(extracted_rows, "response_row") |>
    dplyr::mutate(
      submission_key = as.character(.data$submission_key),
      response_id = as.character(.data$response_id),
      response_token = as.character(.data$response_token),
      submitted_at = as.POSIXct(.data$submitted_at, tz = "UTC"),
      landed_at = as.POSIXct(.data$landed_at, tz = "UTC"),
      workshop_date = as.Date(.data$workshop_date),
      workshop_topic_id = enc2utf8(as.character(.data$workshop_topic_id)),
      workshop_topic_label = enc2utf8(as.character(.data$workshop_topic_label)),
      teacher_id = enc2utf8(as.character(.data$teacher_id)),
      teacher_name = enc2utf8(as.character(.data$teacher_name)),
      overall_score = as.numeric(.data$overall_score),
      professional_benefit_score = as.numeric(.data$professional_benefit_score),
      personal_benefit_score = as.numeric(.data$personal_benefit_score),
      lecturer_score = as.numeric(.data$lecturer_score),
      organization_score = as.numeric(.data$organization_score),
      comment_positive = enc2utf8(as.character(.data$comment_positive)),
      comment_change = enc2utf8(as.character(.data$comment_change)),
      comment_for_teacher = enc2utf8(as.character(.data$comment_for_teacher)),
      response_status = enc2utf8(as.character(.data$response_status)),
      workshop_instance_id = enc2utf8(as.character(.data$workshop_instance_id))
    )

  list(
    responses = response_rows,
    scores_long = build_scores_long(response_rows)
  )
}

fetch_feedback <- function() {
  config <- validate_feedback_configuration()
  form_definition <- request_feedback_form(config)
  transform_feedback_payload(form_definition, fetch_feedback_pages(config))
}
