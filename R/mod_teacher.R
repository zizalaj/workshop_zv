mod_teacher_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    "Teacher",
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("Teacher focus"),
        shiny::selectizeInput(
          ns("selected_teachers"),
          "Teachers in this view",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "All teachers in the current filter")
        ),
        shiny::uiOutput(ns("selection_note"))
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Dimension profile"),
        shiny::uiOutput(ns("profile_chart_ui"))
      ),
      col_widths = c(4, 8)
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Teacher trend over time"),
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(ns("trend_plot"), height = "360px"),
        type = 6,
        color = ju_palette$dark_teal
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Teacher summary"),
      shiny::div(
        class = "ju-table-wrap",
        DT::DTOutput(ns("summary_table"))
      )
    )
  )
}

mod_teacher_server <- function(id, filtered_data, comparison_data, filters_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    observeEvent(filtered_data(), {
      dataset <- filtered_data()
      teacher_choices <- dataset |>
        dplyr::distinct(.data$teacher_id, .data$teacher_name) |>
        dplyr::arrange(.data$teacher_name)

      selected <- input$selected_teachers %||% teacher_choices$teacher_id
      selected <- intersect(selected, teacher_choices$teacher_id)
      if (!length(selected)) {
        selected <- teacher_choices$teacher_id
      }

      updateSelectizeInput(
        session,
        "selected_teachers",
        choices = stats::setNames(teacher_choices$teacher_id, teacher_choices$teacher_name),
        selected = selected,
        server = TRUE
      )
    }, ignoreInit = FALSE)

    selected_teacher_ids <- shiny::reactive({
      available <- filtered_data() |>
        dplyr::distinct(.data$teacher_id) |>
        dplyr::pull(.data$teacher_id)
      selected <- input$selected_teachers %||% available
      selected <- intersect(selected, available)
      if (!length(selected)) {
        selected <- available
      }
      selected
    })

    selected_feedback <- shiny::reactive({
      dataset <- filtered_data()
      if (!nrow(dataset)) {
        return(dataset)
      }

      dataset |>
        dplyr::filter(.data$teacher_id %in% selected_teacher_ids())
    })

    output$selection_note <- shiny::renderUI({
      selected_n <- length(selected_teacher_ids())
      if (selected_n <= 1L) {
        shiny::tags$div(class = "ju-inline-status", "Single teacher selected, showing radar comparison versus all teachers.")
      } else {
        shiny::tags$div(class = "ju-inline-status", paste("Comparing", selected_n, "teachers across dimensions and trend history."))
      }
    })

    output$profile_chart_ui <- shiny::renderUI({
      if (length(selected_teacher_ids()) == 1L) {
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(session$ns("radar_plot"), height = "420px"),
          type = 6,
          color = ju_palette$primary_mint
        )
      } else {
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(session$ns("grouped_bar_plot"), height = "420px"),
          type = 6,
          color = ju_palette$dark_teal
        )
      }
    })

    output$radar_plot <- plotly::renderPlotly({
      dataset <- selected_feedback()
      if (!nrow(dataset) || length(selected_teacher_ids()) != 1L) {
        return(ju_empty_plotly("Select a single teacher to view the radar chart."))
      }

      dimension_order <- ordered_dimensions(dataset$dimension)
      teacher_profile <- summarise_dimension_scores(dataset, "teacher") |>
        dplyr::filter(.data$entity_id == selected_teacher_ids()) |>
        dplyr::mutate(dimension = factor(.data$dimension, levels = dimension_order)) |>
        dplyr::arrange(.data$dimension)

      benchmark <- filtered_data() |>
        dplyr::group_by(.data$dimension) |>
        dplyr::summarise(
          average_score = mean(.data$dimension_score, na.rm = TRUE),
          response_n = dplyr::n_distinct(.data$response_id),
          .groups = "drop"
        ) |>
        dplyr::mutate(dimension = factor(.data$dimension, levels = dimension_order)) |>
        dplyr::arrange(.data$dimension)

      teacher_name <- unique(teacher_profile$entity_name)
      fig <- plotly::plot_ly()
      fig <- fig |>
        plotly::add_trace(
          type = "scatterpolar",
          mode = "lines+markers",
          fill = "toself",
          r = teacher_profile$average_score,
          theta = as.character(teacher_profile$dimension),
          name = teacher_name,
          text = paste0(
            teacher_name,
            "<br>",
            teacher_profile$dimension,
            ": ",
            scales::number(teacher_profile$average_score, accuracy = 0.1),
            "<br>n=",
            scales::comma(teacher_profile$response_n)
          ),
          hovertemplate = "%{text}<extra></extra>",
          line = list(color = ju_palette$primary_mint, width = 3),
          marker = list(color = ju_palette$primary_mint)
        ) |>
        plotly::add_trace(
          type = "scatterpolar",
          mode = "lines+markers",
          fill = "toself",
          r = benchmark$average_score,
          theta = as.character(benchmark$dimension),
          name = "All teachers",
          text = paste0(
            "All teachers<br>",
            benchmark$dimension,
            ": ",
            scales::number(benchmark$average_score, accuracy = 0.1),
            "<br>n=",
            scales::comma(benchmark$response_n)
          ),
          hovertemplate = "%{text}<extra></extra>",
          line = list(color = ju_palette$purple, width = 3),
          marker = list(color = ju_palette$purple)
        )

      ju_apply_plotly_theme(fig) |>
        plotly::layout(
          polar = list(
            radialaxis = list(
              visible = TRUE,
              range = c(1, detect_score_scale_max(dataset)),
              gridcolor = ju_palette$light_gray
            )
          )
        )
    })

    output$grouped_bar_plot <- plotly::renderPlotly({
      dataset <- selected_feedback()
      if (!nrow(dataset) || length(selected_teacher_ids()) < 2L) {
        return(ju_empty_plotly("Select two or more teachers to compare dimensions."))
      }

      plot_data <- summarise_dimension_scores(dataset, "teacher") |>
        dplyr::mutate(
          tooltip = paste0(
            .data$entity_name,
            "<br>", .data$dimension,
            ": ", scales::number(.data$average_score, accuracy = 0.1),
            "<br>n=", scales::comma(.data$response_n)
          )
        )

      fig <- plotly::plot_ly(
        plot_data,
        x = ~entity_name,
        y = ~average_score,
        color = ~dimension,
        colors = ju_series_colors,
        type = "bar",
        text = ~tooltip,
        hovertemplate = "%{text}<extra></extra>"
      )

      ju_apply_plotly_theme(fig) |>
        plotly::layout(
          barmode = "group",
          xaxis = ju_axis_style("Teacher"),
          yaxis = ju_axis_style("Average dimension score")
        )
    })

    output$trend_plot <- plotly::renderPlotly({
      rolling <- compute_rolling_average(selected_feedback(), "teacher")
      if (!nrow(rolling)) {
        return(ju_empty_plotly("No teacher trend data matches the current filters."))
      }

      rolling <- rolling |>
        dplyr::mutate(
          tooltip = paste0(
            .data$entity_name,
            "<br>", format(.data$workshop_date, "%d %b %Y"),
            "<br>Rolling average: ", scales::number(.data$rolling_average, accuracy = 0.1),
            "<br>Workshop average: ", scales::number(.data$workshop_score, accuracy = 0.1),
            "<br>n=", scales::comma(.data$response_n)
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
          xaxis = ju_axis_style("Workshop date"),
          yaxis = ju_axis_style("Rolling composite score"),
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
      dataset <- selected_feedback()
      if (!nrow(dataset)) {
        return(DT::datatable(data.frame(), options = list(dom = "t")))
      }

      selected_ids <- selected_teacher_ids()
      filters <- filters_reactive()

      response_summary <- summarise_responses(dataset) |>
        dplyr::group_by(.data$teacher_id, .data$teacher_name) |>
        dplyr::summarise(
          workshops = dplyr::n_distinct(.data$workshop_id),
          responses = dplyr::n(),
          composite_average = mean(.data$composite_score, na.rm = TRUE),
          .groups = "drop"
        )

      dimension_summary <- summarise_dimension_scores(dataset, "teacher") |>
        dplyr::mutate(display = format_score_with_n(.data$average_score, .data$response_n)) |>
        dplyr::select(entity_id, dimension, display) |>
        tidyr::pivot_wider(names_from = .data$dimension, values_from = .data$display)

      trend_summary <- comparison_data() |>
        dplyr::filter(.data$teacher_id %in% selected_ids) |>
        compute_entity_period_delta(
          entity = "teacher",
          start_date = filters$date_range[[1]],
          end_date = filters$date_range[[2]]
        ) |>
        dplyr::transmute(
          teacher_id,
          trend = dplyr::case_when(
            is.na(.data$delta) ~ "– No prior period",
            TRUE ~ paste0(
              trend_arrow(.data$delta),
              " ",
              scales::number(.data$delta, accuracy = 0.1, style_positive = "plus")
            )
          )
        )

      table_data <- response_summary |>
        dplyr::left_join(dimension_summary, by = c("teacher_id" = "entity_id")) |>
        dplyr::left_join(trend_summary, by = "teacher_id") |>
        dplyr::mutate(
          `Composite avg` = format_score_with_n(.data$composite_average, .data$responses),
          Trend = .data$trend %||% "– No prior period"
        ) |>
        dplyr::select(
          Teacher = .data$teacher_name,
          Workshops = .data$workshops,
          Responses = .data$responses,
          dplyr::all_of(intersect(ordered_dimensions(dataset$dimension), names(.))),
          `Composite avg`,
          Trend
        )

      DT::datatable(
        table_data,
        rownames = FALSE,
        options = list(pageLength = 8, scrollX = TRUE, autoWidth = TRUE)
      )
    })
  })
}
