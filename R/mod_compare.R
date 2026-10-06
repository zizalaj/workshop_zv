build_comparison_subplot <- function(dataset, entity = c("teacher", "topic")) {
  entity <- match.arg(entity)
  summary_data <- summarise_score_areas(dataset, entity)

  if (!nrow(summary_data) || dplyr::n_distinct(summary_data$entity_id) < 2L) {
    label <- if (identical(entity, "teacher")) "lektory" else "témata workshopů"
    return(ju_empty_plotly(paste("Vyberte alespoň dva", label, "pro porovnání.")))
  }

  metric_order <- ordered_score_area_keys(summary_data$metric_key)
  benchmark <- feedback_scores_long(dataset) |>
    dplyr::group_by(.data$metric_key, .data$metric_label_cs) |>
    dplyr::summarise(
      average_score = mean(.data$score, na.rm = TRUE),
      response_n = dplyr::n_distinct(.data$submission_key),
      .groups = "drop"
    ) |>
    dplyr::mutate(metric_label_cs = factor(.data$metric_label_cs, levels = ordered_score_area_labels(metric_order))) |>
    dplyr::arrange(.data$metric_label_cs)

  entities <- summary_data |>
    dplyr::distinct(.data$entity_id, .data$entity_name) |>
    dplyr::arrange(.data$entity_name)

  figures <- purrr::map(seq_len(nrow(entities)), function(index) {
    entity_id <- entities$entity_id[[index]]
    entity_name <- entities$entity_name[[index]]

    entity_profile <- summary_data |>
      dplyr::filter(.data$entity_id == entity_id) |>
      dplyr::mutate(metric_label_cs = factor(.data$metric_label_cs, levels = ordered_score_area_labels(metric_order))) |>
      dplyr::arrange(.data$metric_label_cs)

    fig <- plotly::plot_ly()
    fig <- fig |>
      plotly::add_trace(
        type = "scatterpolar",
        mode = "lines+markers",
        fill = "toself",
        r = entity_profile$average_score,
        theta = as.character(entity_profile$metric_label_cs),
        name = entity_name,
        text = paste0(
          entity_name,
          "<br>",
          entity_profile$metric_label_cs,
          ": ",
          scales::number(entity_profile$average_score, accuracy = 0.1),
          "<br>n=",
          scales::comma(entity_profile$response_n)
        ),
        hovertemplate = "%{text}<extra></extra>",
        line = list(color = ju_series_colors[[(index - 1L) %% length(ju_series_colors) + 1L]], width = 3),
        marker = list(color = ju_series_colors[[(index - 1L) %% length(ju_series_colors) + 1L]]),
        showlegend = index == 1L
      ) |>
      plotly::add_trace(
        type = "scatterpolar",
        mode = "lines+markers",
        fill = "toself",
        r = benchmark$average_score,
        theta = as.character(benchmark$metric_label_cs),
        name = "Průměr skupiny",
        text = paste0(
          "Průměr skupiny<br>",
          benchmark$metric_label_cs,
          ": ",
          scales::number(benchmark$average_score, accuracy = 0.1),
          "<br>n=",
          scales::comma(benchmark$response_n)
        ),
        hovertemplate = "%{text}<extra></extra>",
        line = list(color = ju_palette$light_gray, width = 2),
        marker = list(color = ju_palette$light_gray),
        showlegend = index == 1L
      )

    ju_apply_plotly_theme(fig) |>
      plotly::layout(
        title = list(text = entity_name),
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(1, detect_score_scale_max(c(entity_profile$average_score, benchmark$average_score))),
            gridcolor = ju_palette$light_gray
          )
        )
      )
  })

  subplot_args <- c(
    figures,
    list(
      nrows = ceiling(length(figures) / 2),
      margin = 0.05,
      shareX = FALSE,
      shareY = FALSE,
      titleX = FALSE,
      titleY = FALSE
    )
  )

  do.call(plotly::subplot, subplot_args) |>
    ju_apply_plotly_theme()
}

mod_compare_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    "Porovnání",
    bslib::card(
      bslib::card_header("Porovnání napříč entitami"),
      shiny::radioButtons(
        ns("comparison_mode"),
        "Porovnat vybrané entity podle",
        choices = c("Lektoři" = "teacher", "Témata workshopů" = "topic"),
        selected = "teacher",
        inline = TRUE
      ),
      shiny::uiOutput(ns("comparison_note")),
      ju_with_spinner(
        plotly::plotlyOutput(ns("comparison_plot"), height = "640px"),
        type = 6,
        color = ju_palette$dark_teal
      )
    )
  )
}

mod_compare_server <- function(id, filtered_data) {
  shiny::moduleServer(id, function(input, output, session) {
    comparison_dataset <- shiny::reactive({
      dataset <- filtered_data()
      if (!nrow(summarise_responses(dataset))) {
        return(empty_feedback_data())
      }
      dataset
    })

    output$comparison_note <- shiny::renderUI({
      responses <- summarise_responses(comparison_dataset())
      mode <- input$comparison_mode %||% "teacher"

      entity_count <- if (identical(mode, "teacher")) {
        dplyr::n_distinct(responses$teacher_id)
      } else {
        dplyr::n_distinct(responses$workshop_topic_id)
      }

      label <- if (identical(mode, "teacher")) "lektorů" else "témat workshopů"
      shiny::tags$div(
        class = "ju-inline-status",
        paste("Zobrazuji profily hodnocených oblastí vedle sebe pro", entity_count, label, "v aktuálním globálním filtru.")
      )
    })

    output$comparison_plot <- plotly::renderPlotly({
      dataset <- comparison_dataset()
      if (!nrow(feedback_scores_long(dataset))) {
        return(ju_empty_plotly("Aktuálním filtrům neodpovídá žádná zpětná vazba."))
      }

      build_comparison_subplot(
        dataset,
        entity = input$comparison_mode %||% "teacher"
      )
    })
  })
}
