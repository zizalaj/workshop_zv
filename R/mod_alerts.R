mod_alerts_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    "Upozornění",
    shiny::uiOutput(ns("empty_state")),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Sledovaní lektoři"),
      shiny::div(
        class = "ju-table-wrap",
        DT::DTOutput(ns("alerts_table"))
      )
    )
  )
}

mod_alerts_server <- function(id, filtered_data) {
  shiny::moduleServer(id, function(input, output, session) {
    alerts_data <- shiny::reactive({
      flag_teacher_alerts(filtered_data())
    })

    output$empty_state <- shiny::renderUI({
      if (nrow(alerts_data())) {
        return(NULL)
      }

      shiny::tags$div(
        class = "ju-empty-state",
        "Žádný lektor se aktuálně nenachází pod prahem."
      )
    })

    output$alerts_table <- DT::renderDT({
      alerts <- alerts_data()
      if (!nrow(alerts)) {
        return(DT::datatable(data.frame(), options = list(dom = "t", language = ju_dt_language)))
      }

      table_data <- alerts |>
        dplyr::mutate(
          `Aktuální klouzavý průměr` = format_score_with_n(.data$current_rolling_average, .data$current_response_n),
          Práh = scales::number(.data$effective_threshold, accuracy = 0.1),
          `Workshopy pod prahem` = .data$below_threshold_n,
          Trend = vapply(.data$sparkline_values, make_svg_sparkline, character(1), stroke = ju_palette$pink)
        ) |>
        dplyr::select(
          Lektor = .data$teacher_name,
          `Aktuální klouzavý průměr`,
          Práh,
          `Workshopy pod prahem`,
          Trend
        )

      DT::datatable(
        table_data,
        rownames = FALSE,
        escape = FALSE,
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
