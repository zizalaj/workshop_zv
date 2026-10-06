sidebar_default_date_range <- function(dates) {
  date_max <- max(dates, na.rm = TRUE)
  date_min <- min(dates, na.rm = TRUE)
  default_start <- max(
    date_min,
    lubridate::`%m-%`(date_max, lubridate::period(months = 6))
  )
  c(default_start, date_max)
}

mod_sidebar_filters_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$p(
      class = "ju-filter-note",
      "Tyto filtry se propisují do všech záložek. Tlačítko aktualizace vždy spustí nové načtení z API."
    ),
    shiny::dateRangeInput(
      ns("date_range"),
      "Rozsah dat",
      format = "dd. mm. yyyy",
      language = "cs"
    ),
    shiny::selectizeInput(
      ns("teacher_ids"),
      "Lektor",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "Všichni lektoři")
    ),
    shiny::selectizeInput(
      ns("client_ids"),
      "Klient",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "Všichni klienti")
    ),
    shiny::checkboxGroupInput(
      ns("dimensions"),
      "Dimenze",
      choices = NULL
    ),
    shiny::actionButton(
      ns("refresh_data"),
      "Aktualizovat data",
      class = "btn ju-primary-button"
    ),
    shiny::uiOutput(ns("refresh_state"))
  )
}

mod_sidebar_filters_server <- function(id, dataset_reactive, app_state_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    observeEvent(dataset_reactive(), {
      dataset <- dataset_reactive()
      if (is.null(dataset) || !nrow(dataset)) {
        return(invisible(NULL))
      }

      teacher_choices <- dataset |>
        dplyr::distinct(.data$teacher_id, .data$teacher_name) |>
        dplyr::arrange(.data$teacher_name)
      teacher_selected <- input$teacher_ids %||% teacher_choices$teacher_id
      teacher_selected <- intersect(teacher_selected, teacher_choices$teacher_id)
      if (!length(teacher_selected)) {
        teacher_selected <- teacher_choices$teacher_id
      }

      client_choices <- dataset |>
        dplyr::distinct(.data$client_id, .data$client_name) |>
        dplyr::arrange(.data$client_name)
      client_selected <- input$client_ids %||% client_choices$client_id
      client_selected <- intersect(client_selected, client_choices$client_id)
      if (!length(client_selected)) {
        client_selected <- client_choices$client_id
      }

      dimension_choices <- ordered_dimensions(dataset$dimension)
      dimension_selected <- input$dimensions %||% dimension_choices
      dimension_selected <- intersect(dimension_selected, dimension_choices)
      if (!length(dimension_selected)) {
        dimension_selected <- dimension_choices
      }

      updateSelectizeInput(
        session,
        "teacher_ids",
        choices = stats::setNames(teacher_choices$teacher_id, teacher_choices$teacher_name),
        selected = teacher_selected,
        server = TRUE
      )

      updateSelectizeInput(
        session,
        "client_ids",
        choices = stats::setNames(client_choices$client_id, client_choices$client_name),
        selected = client_selected,
        server = TRUE
      )

      updateCheckboxGroupInput(
        session,
        "dimensions",
        choices = dimension_choices,
        selected = dimension_selected
      )

      bounds <- range(dataset$workshop_date, na.rm = TRUE)
      current_range <- input$date_range
      default_range <- sidebar_default_date_range(dataset$workshop_date)

      if (length(current_range %||% c()) == 2L) {
        start_date <- max(as.Date(current_range[[1]]), bounds[[1]])
        end_date <- min(as.Date(current_range[[2]]), bounds[[2]])
        selected_range <- c(start_date, end_date)
      } else {
        selected_range <- default_range
      }

      updateDateRangeInput(
        session,
        "date_range",
        start = selected_range[[1]],
        end = selected_range[[2]],
        min = bounds[[1]],
        max = bounds[[2]]
      )
    }, ignoreInit = FALSE)

    output$refresh_state <- shiny::renderUI({
      state <- app_state_reactive()
      last_loaded <- if (!is.null(state$last_loaded_at)) {
        paste("Naposledy načteno:", format_cz_datetime(state$last_loaded_at))
      } else {
        "Data ještě nebyla načtena."
      }

      status_class <- if (!is.null(state$error)) {
        "ju-inline-status is-error"
      } else {
        "ju-inline-status"
      }

      status_message <- if (isTRUE(state$loading) && isTRUE(state$has_data)) {
        "Probíhá aktualizace dat z API."
      } else if (!is.null(state$error)) {
        state$error$message
      } else {
        last_loaded
      }

      shiny::tags$div(
        class = status_class,
        status_message
      )
    })

    list(
      filters = shiny::reactive({
        list(
          date_range = as.Date(input$date_range),
          teacher_ids = input$teacher_ids %||% character(),
          client_ids = input$client_ids %||% character(),
          dimensions = input$dimensions %||% character()
        )
      }),
      refresh_trigger = shiny::reactive(input$refresh_data)
    )
  })
}

mod_sidebar_filters_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$p(
      class = "ju-filter-note",
      "Tyto filtry se propisují do všech záložek. Tlačítko aktualizace vždy spustí nové načtení z API."
    ),
    shiny::dateRangeInput(
      ns("date_range"),
      "Rozsah dat",
      format = "dd. mm. yyyy",
      language = "cs"
    ),
    shiny::selectizeInput(
      ns("teacher_ids"),
      "Lektoři",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "Všichni lektoři")
    ),
    shiny::selectizeInput(
      ns("workshop_topic_ids"),
      "Téma workshopu",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "Všechna témata workshopů")
    ),
    shiny::checkboxGroupInput(
      ns("metric_keys"),
      "Hodnocené oblasti",
      choices = NULL
    ),
    shiny::actionButton(
      ns("refresh_data"),
      "Aktualizovat data",
      class = "btn ju-primary-button"
    ),
    shiny::uiOutput(ns("refresh_state"))
  )
}

mod_sidebar_filters_server <- function(id, dataset_reactive, app_state_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    observeEvent(dataset_reactive(), {
      dataset <- dataset_reactive()
      responses <- feedback_responses(dataset)
      scores_long <- feedback_scores_long(dataset)

      if (!nrow(responses)) {
        return(invisible(NULL))
      }

      teacher_choices <- responses |>
        dplyr::distinct(.data$teacher_id, .data$teacher_name) |>
        dplyr::arrange(.data$teacher_name)
      teacher_selected <- input$teacher_ids %||% teacher_choices$teacher_id
      teacher_selected <- intersect(teacher_selected, teacher_choices$teacher_id)
      if (!length(teacher_selected)) {
        teacher_selected <- teacher_choices$teacher_id
      }

      topic_choices <- responses |>
        dplyr::distinct(.data$workshop_topic_id, .data$workshop_topic_label) |>
        dplyr::arrange(.data$workshop_topic_label)
      topic_selected <- input$workshop_topic_ids %||% topic_choices$workshop_topic_id
      topic_selected <- intersect(topic_selected, topic_choices$workshop_topic_id)
      if (!length(topic_selected)) {
        topic_selected <- topic_choices$workshop_topic_id
      }

      metric_choices <- ordered_score_area_keys(scores_long$metric_key)
      metric_selected <- input$metric_keys %||% metric_choices
      metric_selected <- intersect(metric_selected, metric_choices)
      if (!length(metric_selected)) {
        metric_selected <- metric_choices
      }

      updateSelectizeInput(
        session,
        "teacher_ids",
        choices = stats::setNames(teacher_choices$teacher_id, teacher_choices$teacher_name),
        selected = teacher_selected,
        server = TRUE
      )

      updateSelectizeInput(
        session,
        "workshop_topic_ids",
        choices = stats::setNames(topic_choices$workshop_topic_id, topic_choices$workshop_topic_label),
        selected = topic_selected,
        server = TRUE
      )

      updateCheckboxGroupInput(
        session,
        "metric_keys",
        choices = stats::setNames(metric_choices, ordered_score_area_labels(metric_choices)),
        selected = metric_selected
      )

      bounds <- range(responses$workshop_date, na.rm = TRUE)
      current_range <- input$date_range
      default_range <- sidebar_default_date_range(responses$workshop_date)

      if (length(current_range %||% c()) == 2L) {
        start_date <- max(as.Date(current_range[[1]]), bounds[[1]])
        end_date <- min(as.Date(current_range[[2]]), bounds[[2]])
        selected_range <- c(start_date, end_date)
      } else {
        selected_range <- default_range
      }

      updateDateRangeInput(
        session,
        "date_range",
        start = selected_range[[1]],
        end = selected_range[[2]],
        min = bounds[[1]],
        max = bounds[[2]]
      )
    }, ignoreInit = FALSE)

    output$refresh_state <- shiny::renderUI({
      state <- app_state_reactive()
      last_loaded <- if (!is.null(state$last_loaded_at)) {
        paste("Naposledy načteno:", format_cz_datetime(state$last_loaded_at))
      } else {
        "Data ještě nebyla načtena."
      }

      status_class <- if (!is.null(state$error)) {
        "ju-inline-status is-error"
      } else {
        "ju-inline-status"
      }

      status_message <- if (isTRUE(state$loading) && isTRUE(state$has_data)) {
        "Probíhá aktualizace dat z API."
      } else if (!is.null(state$error)) {
        state$error$message
      } else {
        last_loaded
      }

      shiny::tags$div(
        class = status_class,
        status_message
      )
    })

    list(
      filters = shiny::reactive({
        list(
          date_range = as.Date(input$date_range),
          teacher_ids = input$teacher_ids %||% character(),
          workshop_topic_ids = input$workshop_topic_ids %||% character(),
          metric_keys = input$metric_keys %||% character()
        )
      }),
      refresh_trigger = shiny::reactive(input$refresh_data)
    )
  })
}
