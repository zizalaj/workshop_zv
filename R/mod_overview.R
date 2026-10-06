overview_kpi_value_ui <- function(value, meta = NULL) {
  shiny::tags$div(
    class = "ju-kpi-stack",
    shiny::tags$span(class = "ju-kpi-value", value),
    if (!is.null(meta)) shiny::tags$span(class = "ju-kpi-meta", meta)
  )
}

mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    "Přehled",
    bslib::layout_columns(
      bslib::value_box(
        title = "Celkové kompozitní skóre",
        value = shiny::uiOutput(ns("overall_value")),
        showcase = shiny::div(plotly::plotlyOutput(ns("overall_gauge"), height = "165px")),
        theme = bslib::value_box_theme(bg = ju_palette$white, fg = ju_palette$near_black)
      ),
      bslib::value_box(
        title = "Návratnost",
        value = shiny::uiOutput(ns("response_rate_value")),
        showcase = shiny::div(plotly::plotlyOutput(ns("response_rate_gauge"), height = "165px")),
        theme = bslib::value_box_theme(bg = ju_palette$pale_mint, fg = ju_palette$near_black)
      ),
      bslib::value_box(
        title = "Workshopy ve výběru",
        value = shiny::uiOutput(ns("workshop_count_value")),
        theme = bslib::value_box_theme(bg = ju_palette$off_white_gray, fg = ju_palette$near_black)
      ),
      bslib::value_box(
        title = "Změna oproti předchozímu období",
        value = shiny::uiOutput(ns("delta_value")),
        theme = bslib::value_box_theme(bg = ju_palette$near_black, fg = ju_palette$white)
      ),
      col_widths = c(6, 6, 6, 6)
    ),
    bslib::layout_columns(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Trend kompozitního skóre"),
        ju_with_spinner(
          plotly::plotlyOutput(ns("trend_plot"), height = "360px"),
          type = 6,
          color = ju_palette$dark_teal
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Heatmapa lektorů po měsících"),
        ju_with_spinner(
          plotly::plotlyOutput(ns("heatmap_plot"), height = "360px"),
          type = 6,
          color = ju_palette$primary_mint
        )
      ),
      col_widths = c(7, 5)
    )
  )
}

mod_overview_server <- function(id, filtered_data, comparison_data, filters_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    kpi_metrics <- shiny::reactive({
      dataset <- filtered_data()
      responses <- summarise_responses(dataset)
      filters <- filters_reactive()

      delta_info <- if (length(filters$date_range %||% c()) == 2L) {
        compute_overall_delta(
          comparison_data(),
          filters$date_range[[1]],
          filters$date_range[[2]]
        )
      } else {
        list(current_average = NA_real_, prior_average = NA_real_, current_n = 0L, prior_n = 0L, delta = NA_real_)
      }

      list(
        score_scale = detect_score_scale_max(dataset),
        overall_score = if (nrow(responses)) mean(responses$composite_score, na.rm = TRUE) else NA_real_,
        response_n = nrow(responses),
        response_rate = compute_response_rate(dataset),
        workshop_n = dplyr::n_distinct(dataset$workshop_id),
        delta_info = delta_info
      )
    })

    output$overall_value <- shiny::renderUI({
      metrics <- kpi_metrics()
      overview_kpi_value_ui(
        value = if (is.na(metrics$overall_score)) "N/A" else scales::number(metrics$overall_score, accuracy = 0.1),
        meta = paste0("n=", scales::comma(metrics$response_n))
      )
    })

    output$response_rate_value <- shiny::renderUI({
      metrics <- kpi_metrics()
      if (is.na(metrics$response_rate)) {
        return(overview_kpi_value_ui("N/A", "Chybí počty účastníků"))
      }

      overview_kpi_value_ui(
        value = scales::percent(metrics$response_rate, accuracy = 1),
        meta = paste0("n=", scales::comma(metrics$response_n))
      )
    })

    output$workshop_count_value <- shiny::renderUI({
      metrics <- kpi_metrics()
      overview_kpi_value_ui(
        value = scales::comma(metrics$workshop_n),
        meta = paste0("Odpovědi n=", scales::comma(metrics$response_n))
      )
    })

    output$delta_value <- shiny::renderUI({
      delta_info <- kpi_metrics()$delta_info
      delta_label <- if (is.na(delta_info$delta)) {
        "Bez předchozího období"
      } else {
        paste0(trend_arrow(delta_info$delta), " ", scales::number(delta_info$delta, accuracy = 0.1, style_positive = "plus"))
      }

      overview_kpi_value_ui(
        value = delta_label,
        meta = paste0("Aktuální n=", delta_info$current_n, " vs. předchozí n=", delta_info$prior_n)
      )
    })

    output$overall_gauge <- plotly::renderPlotly({
      metrics <- kpi_metrics()
      ju_donut_gauge(
        value = metrics$overall_score,
        max_value = metrics$score_scale,
        detail = if (is.na(metrics$overall_score)) "N/A" else scales::number(metrics$overall_score, accuracy = 0.1),
        label = paste0("n=", scales::comma(metrics$response_n)),
        color = ju_palette$primary_mint
      )
    })

    output$response_rate_gauge <- plotly::renderPlotly({
      metrics <- kpi_metrics()
      ju_donut_gauge(
        value = metrics$response_rate,
        max_value = 1,
        detail = if (is.na(metrics$response_rate)) "N/A" else scales::percent(metrics$response_rate, accuracy = 1),
        label = if (is.na(metrics$response_rate)) {
          "Chybí počty účastníků"
        } else {
          paste0("n=", scales::comma(metrics$response_n))
        },
        color = ju_palette$dark_teal
      )
    })

    output$trend_plot <- plotly::renderPlotly({
      responses <- summarise_responses(filtered_data())
      if (!nrow(responses)) {
        return(ju_empty_plotly("Aktuálním filtrům neodpovídá žádná zpětná vazba."))
      }

      trend_data <- responses |>
        dplyr::group_by(.data$workshop_id, .data$workshop_date) |>
        dplyr::summarise(
          composite_score = mean(.data$composite_score, na.rm = TRUE),
          response_n = dplyr::n(),
          .groups = "drop"
        ) |>
        dplyr::arrange(.data$workshop_date, .data$workshop_id) |>
        dplyr::mutate(
          rolling_average = moving_average(.data$composite_score),
          tooltip = paste0(
            vapply(.data$workshop_date, format_cz_date, character(1)),
            "<br>Klouzavý průměr: ", scales::number(.data$rolling_average, accuracy = 0.1),
            "<br>Průměr workshopu: ", scales::number(.data$composite_score, accuracy = 0.1),
            "<br>n=", scales::comma(.data$response_n)
          )
        )

      fig <- plotly::plot_ly(trend_data, x = ~workshop_date)
      fig <- fig |>
        plotly::add_lines(
          y = ~rolling_average,
          name = "Klouzavý průměr",
          line = list(color = ju_palette$dark_teal, width = 3),
          text = ~tooltip,
          hovertemplate = "%{text}<extra></extra>"
        ) |>
        plotly::add_markers(
          y = ~rolling_average,
          name = "Workshopy",
          text = ~tooltip,
          hovertemplate = "%{text}<extra></extra>",
          size = ~response_n,
          sizes = c(9, 22),
          marker = list(color = ju_palette$primary_mint, opacity = 0.85)
        )

      ju_apply_plotly_theme(fig) |>
        plotly::layout(
          xaxis = ju_axis_style("Datum workshopu"),
          yaxis = ju_axis_style("Kompozitní skóre"),
          shapes = list(list(
            type = "line",
            x0 = min(trend_data$workshop_date),
            x1 = max(trend_data$workshop_date),
            y0 = alert_floor_score,
            y1 = alert_floor_score,
            line = list(color = ju_palette$pink, dash = "dash", width = 2)
          ))
        )
    })

    output$heatmap_plot <- plotly::renderPlotly({
      dataset <- filtered_data()
      if (!nrow(dataset)) {
        return(ju_empty_plotly("Aktuálním filtrům neodpovídá žádná zpětná vazba."))
      }

      heatmap_data <- dataset |>
        dplyr::mutate(month = lubridate::floor_date(.data$workshop_date, "month")) |>
        dplyr::group_by(.data$teacher_name, .data$month) |>
        dplyr::summarise(
          average_score = mean(.data$composite_score, na.rm = TRUE),
          response_n = dplyr::n_distinct(.data$response_id),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          month_label = vapply(.data$month, format_cz_month_label, character(1)),
          tooltip = paste0(
            .data$teacher_name,
            "<br>", .data$month_label,
            "<br>Kompozitní skóre: ", scales::number(.data$average_score, accuracy = 0.1),
            "<br>n=", scales::comma(.data$response_n)
          )
        )

      fig <- plotly::plot_ly(
        heatmap_data,
        x = ~month_label,
        y = ~teacher_name,
        z = ~average_score,
        type = "heatmap",
        text = ~tooltip,
        hovertemplate = "%{text}<extra></extra>",
        colors = c(ju_palette$pale_mint, ju_palette$primary_mint, ju_palette$dark_teal)
      )

      ju_apply_plotly_theme(fig, showlegend = FALSE) |>
        plotly::layout(
          xaxis = ju_axis_style("Měsíc"),
          yaxis = ju_axis_style("Lektor")
        )
    })
  })
}

mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    "Přehled",
    bslib::layout_columns(
      bslib::value_box(
        title = "Hlavní hodnocení workshopu",
        value = shiny::uiOutput(ns("overall_value")),
        showcase = shiny::div(plotly::plotlyOutput(ns("overall_gauge"), height = "165px")),
        theme = bslib::value_box_theme(bg = ju_palette$white, fg = ju_palette$near_black)
      ),
      bslib::value_box(
        title = "Počet odpovědí",
        value = shiny::uiOutput(ns("response_count_value")),
        theme = bslib::value_box_theme(bg = ju_palette$pale_mint, fg = ju_palette$near_black)
      ),
      bslib::value_box(
        title = "Workshopy ve výběru",
        value = shiny::uiOutput(ns("workshop_count_value")),
        theme = bslib::value_box_theme(bg = ju_palette$off_white_gray, fg = ju_palette$near_black)
      ),
      bslib::value_box(
        title = "Změna oproti předchozímu období",
        value = shiny::uiOutput(ns("delta_value")),
        theme = bslib::value_box_theme(bg = ju_palette$near_black, fg = ju_palette$white)
      ),
      col_widths = c(6, 6, 6, 6)
    ),
    bslib::layout_columns(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Trend hlavního hodnocení workshopu"),
        ju_with_spinner(
          plotly::plotlyOutput(ns("trend_plot"), height = "360px"),
          type = 6,
          color = ju_palette$dark_teal
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Měsíční průměr hlavního hodnocení podle lektora"),
        ju_with_spinner(
          plotly::plotlyOutput(ns("heatmap_plot"), height = "360px"),
          type = 6,
          color = ju_palette$primary_mint
        )
      ),
      col_widths = c(7, 5)
    )
  )
}

mod_overview_server <- function(id, filtered_data, comparison_data, filters_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    kpi_metrics <- shiny::reactive({
      responses <- summarise_responses(filtered_data())
      filters <- filters_reactive()

      delta_info <- if (length(filters$date_range %||% c()) == 2L) {
        compute_overall_delta(
          comparison_data(),
          filters$date_range[[1]],
          filters$date_range[[2]]
        )
      } else {
        list(current_average = NA_real_, prior_average = NA_real_, current_n = 0L, prior_n = 0L, delta = NA_real_)
      }

      list(
        score_scale = detect_score_scale_max(responses$overall_score),
        overall_score = if (nrow(responses)) mean(responses$overall_score, na.rm = TRUE) else NA_real_,
        response_n = nrow(responses),
        workshop_n = dplyr::n_distinct(responses$workshop_instance_id),
        delta_info = delta_info
      )
    })

    output$overall_value <- shiny::renderUI({
      metrics <- kpi_metrics()
      overview_kpi_value_ui(
        value = if (is.na(metrics$overall_score)) "N/A" else scales::number(metrics$overall_score, accuracy = 0.1),
        meta = paste0("n=", scales::comma(metrics$response_n))
      )
    })

    output$response_count_value <- shiny::renderUI({
      metrics <- kpi_metrics()
      overview_kpi_value_ui(
        value = scales::comma(metrics$response_n),
        meta = paste0("Workshopy n=", scales::comma(metrics$workshop_n))
      )
    })

    output$workshop_count_value <- shiny::renderUI({
      metrics <- kpi_metrics()
      overview_kpi_value_ui(
        value = scales::comma(metrics$workshop_n),
        meta = paste0("Odpovědi n=", scales::comma(metrics$response_n))
      )
    })

    output$delta_value <- shiny::renderUI({
      delta_info <- kpi_metrics()$delta_info
      delta_label <- if (is.na(delta_info$delta)) {
        "Bez předchozího období"
      } else {
        paste0(trend_arrow(delta_info$delta), " ", scales::number(delta_info$delta, accuracy = 0.1, style_positive = "plus"))
      }

      overview_kpi_value_ui(
        value = delta_label,
        meta = paste0("Aktuální n=", delta_info$current_n, " vs. předchozí n=", delta_info$prior_n)
      )
    })

    output$overall_gauge <- plotly::renderPlotly({
      metrics <- kpi_metrics()
      ju_donut_gauge(
        value = metrics$overall_score,
        max_value = metrics$score_scale,
        detail = if (is.na(metrics$overall_score)) "N/A" else scales::number(metrics$overall_score, accuracy = 0.1),
        label = paste0("n=", scales::comma(metrics$response_n)),
        color = ju_palette$primary_mint
      )
    })

    output$trend_plot <- plotly::renderPlotly({
      responses <- summarise_responses(filtered_data())
      if (!nrow(responses)) {
        return(ju_empty_plotly("Aktuálním filtrům neodpovídá žádná zpětná vazba."))
      }

      trend_data <- responses |>
        dplyr::group_by(
          .data$workshop_instance_id,
          .data$workshop_date,
          .data$workshop_topic_label,
          .data$teacher_name
        ) |>
        dplyr::summarise(
          overall_score = mean(.data$overall_score, na.rm = TRUE),
          response_n = dplyr::n_distinct(.data$submission_key),
          .groups = "drop"
        ) |>
        dplyr::arrange(.data$workshop_date, .data$workshop_instance_id) |>
        dplyr::mutate(
          rolling_average = moving_average(.data$overall_score),
          tooltip = paste0(
            vapply(.data$workshop_date, format_cz_date, character(1)),
            "<br>Téma: ", .data$workshop_topic_label,
            "<br>Lektor: ", .data$teacher_name,
            "<br>Klouzavý průměr: ", scales::number(.data$rolling_average, accuracy = 0.1),
            "<br>Průměr workshopu: ", scales::number(.data$overall_score, accuracy = 0.1),
            "<br>n=", scales::comma(.data$response_n)
          )
        )

      fig <- plotly::plot_ly(trend_data, x = ~workshop_date)
      fig <- fig |>
        plotly::add_lines(
          y = ~rolling_average,
          name = "Klouzavý průměr",
          line = list(color = ju_palette$dark_teal, width = 3),
          text = ~tooltip,
          hovertemplate = "%{text}<extra></extra>"
        ) |>
        plotly::add_markers(
          y = ~rolling_average,
          name = "Workshopy",
          text = ~tooltip,
          hovertemplate = "%{text}<extra></extra>",
          size = ~response_n,
          sizes = c(9, 22),
          marker = list(color = ju_palette$primary_mint, opacity = 0.85)
        )

      ju_apply_plotly_theme(fig) |>
        plotly::layout(
          xaxis = ju_axis_style("Datum workshopu"),
          yaxis = ju_axis_style("Hlavní hodnocení workshopu"),
          shapes = list(list(
            type = "line",
            x0 = min(trend_data$workshop_date),
            x1 = max(trend_data$workshop_date),
            y0 = alert_floor_score,
            y1 = alert_floor_score,
            line = list(color = ju_palette$pink, dash = "dash", width = 2)
          ))
        )
    })

    output$heatmap_plot <- plotly::renderPlotly({
      responses <- summarise_responses(filtered_data())
      if (!nrow(responses)) {
        return(ju_empty_plotly("Aktuálním filtrům neodpovídá žádná zpětná vazba."))
      }

      heatmap_data <- responses |>
        dplyr::mutate(month = lubridate::floor_date(.data$workshop_date, "month")) |>
        dplyr::group_by(.data$teacher_name, .data$month) |>
        dplyr::summarise(
          average_score = mean(.data$overall_score, na.rm = TRUE),
          response_n = dplyr::n_distinct(.data$submission_key),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          month_label = vapply(.data$month, format_cz_month_label, character(1)),
          tooltip = paste0(
            .data$teacher_name,
            "<br>", .data$month_label,
            "<br>Hlavní hodnocení: ", scales::number(.data$average_score, accuracy = 0.1),
            "<br>n=", scales::comma(.data$response_n)
          )
        )

      fig <- plotly::plot_ly(
        heatmap_data,
        x = ~month_label,
        y = ~teacher_name,
        z = ~average_score,
        type = "heatmap",
        text = ~tooltip,
        hovertemplate = "%{text}<extra></extra>",
        colors = c(ju_palette$pale_mint, ju_palette$primary_mint, ju_palette$dark_teal)
      )

      ju_apply_plotly_theme(fig, showlegend = FALSE) |>
        plotly::layout(
          xaxis = ju_axis_style("Měsíc"),
          yaxis = ju_axis_style("Lektor")
        )
    })
  })
}
