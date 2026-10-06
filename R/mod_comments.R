ju_comment_stopwords <- c(
  "workshop",
  "workshopu",
  "juiceup",
  "juiceupu",
  "lektor",
  "lektorka",
  "bylo",
  "jsme",
  "vice",
  "mene"
)

tokenize_czech_comments <- function(comment_text) {
  if (!length(comment_text)) {
    return(character())
  }

  cleaned <- tolower(enc2utf8(comment_text))
  cleaned <- gsub("[[:digit:]]+", " ", cleaned)
  cleaned <- gsub("[^[:alnum:][:space:]]", " ", cleaned)

  tokens <- unlist(strsplit(cleaned, "\\s+"), use.names = FALSE)
  tokens[nzchar(tokens)]
}

comment_prompt_choices <- c(
  comment_positive = "Co jste na workshopu nejvíce ocenili?",
  comment_change = "Co byste na workshopu změnili nebo co chybělo?",
  comment_for_teacher = "Zpráva nebo tip pro lektora"
)

build_comments_table_data <- function(dataset) {
  responses <- summarise_responses(dataset)
  if (!nrow(responses)) {
    return(tibble::tibble())
  }

  responses |>
    dplyr::mutate(
      Datum = vapply(.data$workshop_date, format_cz_date, character(1)),
      `Hlavní hodnocení` = scales::number(.data$overall_score, accuracy = 0.1),
      `Profesní přínos` = scales::number(.data$professional_benefit_score, accuracy = 0.1),
      `Osobní přínos` = scales::number(.data$personal_benefit_score, accuracy = 0.1),
      Lektoři = scales::number(.data$lecturer_score, accuracy = 0.1),
      Organizace = scales::number(.data$organization_score, accuracy = 0.1)
    ) |>
    dplyr::select(
      Lektor = .data$teacher_name,
      `Téma workshopu` = .data$workshop_topic_label,
      Datum,
      `Hlavní hodnocení`,
      `Profesní přínos`,
      `Osobní přínos`,
      Lektoři,
      Organizace,
      `Co jste ocenili` = .data$comment_positive,
      `Co změnit` = .data$comment_change,
      `Zpráva pro lektora` = .data$comment_for_teacher
    )
}

mod_comments_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    "Komentáře",
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Mrak slov z komentářů"),
      shiny::selectInput(
        ns("comment_prompt"),
        "Komentář pro mrak slov",
        choices = stats::setNames(names(comment_prompt_choices), unname(comment_prompt_choices)),
        selected = "comment_positive"
      ),
      ju_with_spinner(
        shiny::plotOutput(ns("wordcloud_plot"), height = "420px"),
        type = 6,
        color = ju_palette$primary_mint
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Detail komentářů"),
      shiny::div(
        class = "ju-table-wrap",
        DT::DTOutput(ns("comments_table"))
      )
    )
  )
}

mod_comments_server <- function(id, filtered_data) {
  shiny::moduleServer(id, function(input, output, session) {
    selected_comment_column <- shiny::reactive({
      input$comment_prompt %||% "comment_positive"
    })

    comments_only <- shiny::reactive({
      responses <- summarise_responses(filtered_data())
      comment_column <- selected_comment_column()

      responses |>
        dplyr::filter(
          !is.na(.data[[comment_column]]),
          nzchar(trimws(.data[[comment_column]]))
        )
    })

    word_frequency <- shiny::reactive({
      dataset <- comments_only()
      comment_column <- selected_comment_column()
      if (!nrow(dataset)) {
        return(tibble::tibble(word = character(), n = integer()))
      }

      tokens <- tokenize_czech_comments(
        dataset |>
          dplyr::distinct(.data$submission_key, .data[[comment_column]]) |>
          dplyr::pull(.data[[comment_column]])
      )
      stopwords <- unique(c(stopwords::stopwords("cs", source = "stopwords-iso"), ju_comment_stopwords))
      
      tibble::tibble(word = tokens) |>
        dplyr::filter(
          !(.data$word %in% stopwords),
          nchar(.data$word) > 2
        ) |>
        dplyr::count(.data$word, sort = TRUE, name = "n") |>
        dplyr::slice_head(n = 120)
    })

    output$wordcloud_plot <- shiny::renderPlot({
      word_data <- word_frequency()
      if (!nrow(word_data)) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "Pro zvolený komentář nejsou v aktuálním filtru k dispozici žádná data.")
        return(invisible(NULL))
      }

      ggplot2::ggplot(
        word_data,
        ggplot2::aes(
          label = .data$word,
          size = .data$n,
          color = .data$n
        )
      ) +
        ggwordcloud::geom_text_wordcloud_area(
          family = ju_body_family(),
          rm_outside = TRUE
        ) +
        ggplot2::scale_size_area(max_size = 18) +
        ggplot2::scale_color_gradientn(colours = ju_series_colors) +
        ggplot2::theme_void(base_family = ju_body_family()) +
        ggplot2::theme(legend.position = "none")
    })

    output$comments_table <- DT::renderDT({
      response_details <- build_comments_table_data(filtered_data())
      if (!nrow(response_details)) {
        return(DT::datatable(data.frame(), options = list(dom = "t", language = ju_dt_language)))
      }

      DT::datatable(
        response_details,
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          autoWidth = TRUE,
          language = ju_dt_language
        )
      )
    })
  })
}
