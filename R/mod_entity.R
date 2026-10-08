entity_module_config <- function(entity_type = c("teacher", "topic")) {
  entity_type <- match.arg(entity_type)

  if (identical(entity_type, "teacher")) {
    return(list(
      entity_type = "teacher",
      id_col = "teacher_id",
      name_col = "teacher_name",
      tab_title = "Lektoři",
      focus_title = "Zaměření na lektory",
      select_label = "Lektoři v tomto pohledu",
      select_placeholder = "Všichni lektoři v aktuálním filtru",
      single_note = "Vybrán jeden lektor. Radar porovnává jeho profil se všemi lektory.",
      multi_label = "lektory",
      trend_title = "Trend lektora v čase",
      summary_title = "Souhrn lektorů",
      benchmark_label = "Všichni lektoři",
      radar_empty = "Vyberte jednoho lektora pro zobrazení radarového grafu.",
      grouped_empty = "Vyberte alespoň dva lektory pro porovnání hodnocených oblastí.",
      trend_empty = "Pro aktuální filtry nejsou dostupná trendová data lektorů.",
      axis_entity = "Lektor",
      primary_color = ju_palette$primary_mint,
      benchmark_color = ju_palette$purple
    ))
  }

  list(
    entity_type = "topic",
    id_col = "workshop_topic_id",
    name_col = "workshop_topic_label",
    tab_title = "Témata workshopů",
    focus_title = "Zaměření na témata workshopů",
    select_label = "Témata workshopů v tomto pohledu",
    select_placeholder = "Všechna témata workshopů v aktuálním filtru",
    single_note = "Vybráno jedno téma workshopu. Radar porovnává jeho profil se všemi tématy workshopů.",
    multi_label = "témata workshopů",
    trend_title = "Trend tématu workshopu v čase",
    summary_title = "Souhrn témat workshopů",
    benchmark_label = "Všechna témata workshopů",
    radar_empty = "Vyberte jedno téma workshopu pro zobrazení radarového grafu.",
    grouped_empty = "Vyberte alespoň dvě témata workshopů pro porovnání hodnocených oblastí.",
    trend_empty = "Pro aktuální filtry nejsou dostupná trendová data témat workshopů.",
    axis_entity = "Téma workshopu",
    primary_color = ju_palette$dark_teal,
    benchmark_color = ju_palette$yellow
  )
}

build_entity_response_summary <- function(dataset, entity_type) {
  responses <- summarise_responses(dataset)

  if (identical(entity_type, "teacher")) {
    return(
      responses |>
        dplyr::group_by(.data$teacher_id, .data$teacher_name) |>
        dplyr::summarise(
          workshops = dplyr::n_distinct(.data$workshop_instance_id),
          responses = dplyr::n(),
          overall_average = mean(.data$overall_score, na.rm = TRUE),
          .groups = "drop"
        )
    )
  }

  responses |>
    dplyr::group_by(.data$workshop_topic_id, .data$workshop_topic_label) |>
    dplyr::summarise(
      teachers = paste(sort(unique(.data$teacher_name)), collapse = ", "),
      workshops = dplyr::n_distinct(.data$workshop_instance_id),
      responses = dplyr::n(),
      overall_average = mean(.data$overall_score, na.rm = TRUE),
      .groups = "drop"
    )
}

build_entity_summary_table <- function(table_data, entity_type) {
  score_columns <- ordered_score_area_labels()

  if (identical(entity_type, "teacher")) {
    return(
      table_data |>
        dplyr::select(
          Lektor = .data$teacher_name,
          Workshopy = .data$workshops,
          Odpovědi = .data$responses,
          dplyr::all_of(intersect(score_columns, names(.))),
          `Hlavní hodnocení` = .data$`Hlavní hodnocení`,
          Trend
        )
    )
  }

  table_data |>
    dplyr::select(
      `Téma workshopu` = .data$workshop_topic_label,
      Lektoři = .data$teachers,
      Workshopy = .data$workshops,
      Odpovědi = .data$responses,
      dplyr::all_of(intersect(score_columns, names(.))),
      `Hlavní hodnocení` = .data$`Hlavní hodnocení`,
      Trend
    )
}

mod_entity_ui <- function(id, entity_type) {
  ns <- shiny::NS(id)
  config <- entity_module_config(entity_type)

  bslib::nav_panel(
    config$tab_title,
    bslib::layout_columns(
      bslib::card(
        bslib::card_header(config$focus_title),
        shiny::selectizeInput(
          ns("selected_entities"),
          config$select_label,
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = config$select_placeholder)
        ),
        shiny::uiOutput(ns("selection_note"))
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Profil hodnocených oblastí"),
        shiny::uiOutput(ns("profile_chart_ui"))
      ),
      col_widths = c(4, 8)
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(config$trend_title),
      ju_with_spinner(
        plotly::plotlyOutput(ns("trend_plot"), height = "360px"),
        type = 6,
        color = ju_palette$dark_teal
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(config$summary_title),
      shiny::div(
        class = "ju-table-wrap",
        DT::DTOutput(ns("summary_table"))
      )
    )
  )
}

mod_entity_server <- function(id, entity_type, filtered_data, comparison_data, filters_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    config <- entity_module_config(entity_type)
    id_col <- config$id_col
    name_col <- config$name_col

    observeEvent(filtered_data(), {
      responses <- summarise_responses(filtered_data())
      entity_choices <- responses |>
        dplyr::distinct(.data[[id_col]], .data[[name_col]]) |>
        dplyr::arrange(.data[[name_col]])

      selected <- input$selected_entities %||% entity_choices[[id_col]]
      selected <- intersect(selected, entity_choices[[id_col]])
      if (!length(selected)) {
        selected <- entity_choices[[id_col]]
      }

      shiny::updateSelectizeInput(
        session,
        "selected_entities",
        choices = stats::setNames(entity_choices[[id_col]], entity_choices[[name_col]]),
        selected = selected,
        server = TRUE
      )
    }, ignoreInit = FALSE)

    selected_entity_ids <- shiny::reactive({
      available <- summarise_responses(filtered_data()) |>
        dplyr::distinct(.data[[id_col]]) |>
        dplyr::pull(.data[[id_col]])
      selected <- input$selected_entities %||% available
      selected <- intersect(selected, available)
      if (!length(selected)) {
        selected <- available
      }
      selected
    })

    selected_feedback <- shiny::reactive({
      filter_feedback_by_entity(filtered_data(), config$entity_type, selected_entity_ids())
    })

    output$selection_note <- shiny::renderUI({
      selected_n <- length(selected_entity_ids())
      message <- if (selected_n <= 1L) {
        config$single_note
      } else {
        paste("Porovnáváte", selected_n, config$multi_label, "napříč hodnocenými oblastmi a trendem v čase.")
      }

      shiny::tags$div(class = "ju-inline-status", message)
    })

    output$profile_chart_ui <- shiny::renderUI({
      output_id <- if (length(selected_entity_ids()) == 1L) {
        "radar_plot"
      } else {
        "grouped_bar_plot"
      }

      ju_with_spinner(
        plotly::plotlyOutput(session$ns(output_id), height = "420px"),
        type = 6,
        color = if (length(selected_entity_ids()) == 1L) {
          config$primary_color
        } else {
          ju_palette$dark_teal
        }
      )
    })

    output$radar_plot <- plotly::renderPlotly({
      dataset <- selected_feedback()
      score_data <- feedback_scores_long(dataset)
      if (!nrow(score_data) || length(selected_entity_ids()) != 1L) {
        return(ju_empty_plotly(config$radar_empty))
      }

      metric_order <- ordered_score_area_keys(score_data$metric_key)
      entity_profile <- summarise_score_areas(dataset, config$entity_type) |>
        drop_excluded_score_areas(config$entity_type) |>
        dplyr::filter(.data$entity_id == selected_entity_ids()) |>
        dplyr::mutate(metric_label_cs = factor(.data$metric_label_cs, levels = ordered_score_area_labels(metric_order))) |>
        dplyr::arrange(.data$metric_label_cs)

      benchmark <- feedback_scores_long(filtered_data()) |>
        drop_excluded_score_areas(config$entity_type) |>
        dplyr::group_by(.data$metric_key, .data$metric_label_cs) |>
        dplyr::summarise(
          average_score = mean(.data$score, na.rm = TRUE),
          response_n = dplyr::n_distinct(.data$submission_key),
          .groups = "drop"
        ) |>
        dplyr::mutate(metric_label_cs = factor(.data$metric_label_cs, levels = ordered_score_area_labels(metric_order))) |>
        dplyr::arrange(.data$metric_label_cs)

      entity_name <- unique(entity_profile$entity_name)
      fig <- plotly::plot_ly() |>
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
          line = list(color = config$primary_color, width = 3),
          marker = list(color = config$primary_color)
        ) |>
        plotly::add_trace(
          type = "scatterpolar",
          mode = "lines+markers",
          fill = "toself",
          r = benchmark$average_score,
          theta = as.character(benchmark$metric_label_cs),
          name = config$benchmark_label,
          text = paste0(
            config$benchmark_label,
            "<br>",
            benchmark$metric_label_cs,
            ": ",
            scales::number(benchmark$average_score, accuracy = 0.1),
            "<br>n=",
            scales::comma(benchmark$response_n)
          ),
          hovertemplate = "%{text}<extra></extra>",
          line = list(color = config$benchmark_color, width = 3),
          marker = list(color = config$benchmark_color)
        )

      ju_apply_plotly_theme(fig) |>
        plotly::layout(
          polar = list(
            radialaxis = list(
              visible = TRUE,
              range = c(1, detect_score_scale_max(c(entity_profile$average_score, benchmark$average_score))),
              gridcolor = ju_palette$light_gray
            )
          )
        )
    })

    output$grouped_bar_plot <- plotly::renderPlotly({
      dataset <- selected_feedback()
      score_data <- feedback_scores_long(dataset)
      if (!nrow(score_data) || length(selected_entity_ids()) < 2L) {
        return(ju_empty_plotly(config$grouped_empty))
      }

      plot_data <- summarise_score_areas(dataset, config$entity_type) |>
        drop_excluded_score_areas(config$entity_type) |>
        dplyr::mutate(
          metric_label_cs = factor(.data$metric_label_cs, levels = ordered_score_area_labels(.data$metric_key)),
          tooltip = paste0(
            .data$entity_name,
            "<br>",
            .data$metric_label_cs,
            ": ",
            scales::number(.data$average_score, accuracy = 0.1),
            "<br>n=",
            scales::comma(.data$response_n)
          )
        )

      fig <- plotly::plot_ly(
        plot_data,
        x = ~entity_name,
        y = ~average_score,
        color = ~metric_label_cs,
        colors = ju_series_colors,
        type = "bar",
        text = ~tooltip,
        hovertemplate = "%{text}<extra></extra>"
      )

      ju_apply_plotly_theme(fig) |>
        plotly::layout(
          barmode = "group",
          xaxis = ju_axis_style(config$axis_entity),
          yaxis = ju_axis_style("Průměrné skóre hodnocené oblasti")
        )
    })

    output$trend_plot <- plotly::renderPlotly({
      rolling <- compute_rolling_average(selected_feedback(), config$entity_type)
      if (!nrow(rolling)) {
        return(ju_empty_plotly(config$trend_empty))
      }

      rolling <- rolling |>
        dplyr::mutate(
          detail_label = if (identical(config$entity_type, "teacher")) .data$workshop_topic_label else .data$teacher_name,
          detail_prefix = if (identical(config$entity_type, "teacher")) "Téma" else "Lektor",
          tooltip = paste0(
            .data$entity_name,
            "<br>",
            vapply(.data$workshop_date, format_cz_date, character(1)),
            "<br>", .data$detail_prefix, ": ", .data$detail_label,
            "<br>Klouzavý průměr: ",
            scales::number(.data$rolling_average, accuracy = 0.1),
            "<br>Průměr workshopu: ",
            scales::number(.data$workshop_score, accuracy = 0.1),
            "<br>n=",
            scales::comma(.data$response_n)
          )
        )

      fig <- plotly::plot_ly(
        rolling,
        x = ~workshop_date,
        y = ~rolling_average,
        color = ~entity_name,
        colors = ju_series_colors,
        type = "scatter",
        mode = "lines+markers",
        text = ~tooltip,
        hovertemplate = "%{text}<extra></extra>",
        size = ~response_n,
        sizes = c(8, 20),
        marker = list(opacity = 0.75)
      )

      ju_apply_plotly_theme(fig) |>
        plotly::layout(
          xaxis = ju_axis_style("Datum workshopu"),
          yaxis = ju_axis_style("Klouzavý průměr hlavního hodnocení"),
          shapes = list(list(
            type = "line",
            x0 = min(rolling$workshop_date),
            x1 = max(rolling$workshop_date),
            y0 = alert_floor_score,
            y1 = alert_floor_score,
            line = list(color = ju_palette$pink, dash = "dash", width = 2)
          ))
        )
    })

    output$summary_table <- DT::renderDT({
      responses <- summarise_responses(selected_feedback())
      if (!nrow(responses)) {
        return(DT::datatable(data.frame(), options = list(dom = "t", language = ju_dt_language)))
      }

      filters <- filters_reactive()
      response_summary <- build_entity_response_summary(selected_feedback(), config$entity_type)
      score_summary <- summarise_score_areas(selected_feedback(), config$entity_type) |>
        dplyr::mutate(display = format_score_with_n(.data$average_score, .data$response_n)) |>
        dplyr::select(entity_id, metric_label_cs, display) |>
        tidyr::pivot_wider(names_from = .data$metric_label_cs, values_from = .data$display)

      trend_summary <- if (length(filters$date_range %||% c()) == 2L) {
        filter_feedback_by_entity(comparison_data(), config$entity_type, selected_entity_ids()) |>
          compute_entity_period_delta(
            entity = config$entity_type,
            start_date = filters$date_range[[1]],
            end_date = filters$date_range[[2]]
          ) |>
          dplyr::transmute(
            entity_id = .data[[id_col]],
            trend = dplyr::case_when(
              is.na(.data$delta) ~ "Bez předchozího období",
              TRUE ~ paste0(
                trend_arrow(.data$delta),
                " ",
                scales::number(.data$delta, accuracy = 0.1, style_positive = "plus")
              )
            )
          )
      } else {
        tibble::tibble(entity_id = character(), trend = character())
      }

      table_data <- response_summary |>
        dplyr::left_join(
          score_summary,
          by = stats::setNames("entity_id", id_col)
        ) |>
        dplyr::left_join(
          trend_summary,
          by = stats::setNames("entity_id", id_col)
        ) |>
        dplyr::mutate(
          `Hlavní hodnocení` = format_score_with_n(.data$overall_average, .data$responses),
          Trend = dplyr::coalesce(.data$trend, "Bez předchozího období")
        )

      DT::datatable(
        build_entity_summary_table(table_data, config$entity_type),
        rownames = FALSE,
        options = list(
          pageLength = 8,
          scrollX = TRUE,
          autoWidth = TRUE,
          language = ju_dt_language
        )
      )
    })
  })
}
