options(sass.cache = FALSE)

source(file.path("R", "utils_scoring.R"), local = FALSE)
source(file.path("R", "utils_theme.R"), local = FALSE)
source(file.path("R", "data_fetch.R"), local = FALSE)
source(file.path("R", "mod_sidebar_filters.R"), local = FALSE)
source(file.path("R", "mod_overview.R"), local = FALSE)
source(file.path("R", "mod_entity.R"), local = FALSE)
source(file.path("R", "mod_compare.R"), local = FALSE)
source(file.path("R", "mod_comments.R"), local = FALSE)
source(file.path("R", "mod_alerts.R"), local = FALSE)

ju_register_fonts()

build_navbar_title <- function() {
  shiny::tags$div(
    class = "ju-navbar-brand",
    ju_logo_tag("ju-navbar-logo"),
    shiny::tags$div(
      class = "ju-navbar-copy",
      shiny::tags$span(class = "ju-navbar-title", "juiceUP dashboard zpětné vazby"),
      shiny::tags$span(
        class = "ju-navbar-subtitle",
        "Výsledky workshopů, lektorů a témat workshopů v čase"
      )
    )
  )
}

loading_state_ui <- function() {
  shiny::tags$div(
    class = "ju-state-shell",
    ju_logo_tag("ju-state-logo"),
    shiny::tags$div(class = "ju-spinner"),
    shiny::tags$h1(class = "ju-state-title", "Načítám zpětnou vazbu z workshopů"),
    shiny::tags$p(
      class = "ju-state-copy",
      "Pro tuto relaci právě načítám nejnovější data z API."
    ),
    shiny::tags$div(class = "ju-droplet-mark")
  )
}

error_state_ui <- function(error_state) {
  title_text <- if (identical(error_state$type, "config_error")) {
    "Konfigurace není kompletní"
  } else {
    "Nepodařilo se načíst data"
  }

  shiny::tags$div(
    class = "ju-state-shell",
    ju_logo_tag("ju-state-logo"),
    shiny::tags$h1(class = "ju-state-title", title_text),
    shiny::tags$p(class = "ju-state-copy", error_state$message %||% "Neznámá chyba."),
    shiny::actionButton(
      "app_retry",
      "Zkusit znovu",
      class = "btn ju-primary-button"
    ),
    shiny::tags$div(class = "ju-droplet-mark")
  )
}

dashboard_ui <- function() {
  shiny::tagList(
    bslib::page_navbar(
      title = build_navbar_title(),
      sidebar = bslib::sidebar(
        width = 320,
        bg = ju_palette$off_white_gray,
        open = "desktop",
        title = "Globální filtry",
        mod_sidebar_filters_ui("sidebar")
      ),
      fillable = TRUE,
      gap = "1rem",
      mod_overview_ui("overview"),
      mod_entity_ui("teacher", "teacher"),
      mod_entity_ui("topic", "topic"),
      mod_compare_ui("compare"),
      mod_comments_ui("comments"),
      mod_alerts_ui("alerts")
    ),
    shiny::tags$div(class = "ju-droplet-mark")
  )
}

ui <- shiny::fluidPage(
  theme = ju_theme(),
  shiny::tags$head(
    shiny::includeCSS(file.path("www", "app.css"))
  ),
  shiny::uiOutput("app_shell")
)

server <- function(input, output, session) {
  state <- shiny::reactiveValues(
    data = NULL,
    loading = FALSE,
    error = NULL,
    last_updated = NULL,
    initialized = FALSE
  )

  as_app_error <- function(error, has_existing_data = FALSE) {
    error_type <- if (inherits(error, "feedback_config_error")) {
      "config_error"
    } else {
      "fetch_error"
    }

    error_message <- conditionMessage(error)
    if (has_existing_data && !identical(error_type, "config_error")) {
      error_message <- paste(
        "Aktualizace se nepodařila.",
        error_message,
        "Zobrazená data zůstávají beze změny."
      )
    }

    list(
      type = error_type,
      message = error_message
    )
  }

  load_feedback_data <- function() {
    if (isTRUE(state$loading)) {
      return(invisible(NULL))
    }

    had_existing_data <- !is.null(state$data) && nrow(feedback_responses(state$data)) > 0L
    state$initialized <- TRUE
    state$loading <- TRUE
    state$error <- NULL

    fetch_result <- tryCatch(
      fetch_feedback(),
      error = function(error) error
    )

    if (inherits(fetch_result, "error")) {
      state$error <- as_app_error(fetch_result, has_existing_data = had_existing_data)
      state$loading <- FALSE
      return(invisible(NULL))
    }

    state$data <- fetch_result
    state$error <- NULL
    state$last_updated <- Sys.time()
    state$loading <- FALSE
    invisible(NULL)
  }

  sidebar_state <- mod_sidebar_filters_server(
    "sidebar",
    dataset_reactive = shiny::reactive(state$data),
    app_state_reactive = shiny::reactive(list(
      loading = state$loading,
      error = state$error,
      last_loaded_at = state$last_updated,
      has_data = !is.null(state$data) && nrow(feedback_responses(state$data)) > 0L
    ))
  )

  filtered_without_date <- shiny::reactive({
    apply_feedback_filters(
      state$data %||% empty_feedback_data(),
      sidebar_state$filters(),
      include_date = FALSE
    )
  })

  filtered_data <- shiny::reactive({
    apply_feedback_filters(
      state$data %||% empty_feedback_data(),
      sidebar_state$filters(),
      include_date = TRUE
    )
  })

  mod_overview_server(
    "overview",
    filtered_data = filtered_data,
    comparison_data = filtered_without_date,
    filters_reactive = sidebar_state$filters
  )

  mod_entity_server(
    "teacher",
    entity_type = "teacher",
    filtered_data = filtered_data,
    comparison_data = filtered_without_date,
    filters_reactive = sidebar_state$filters
  )

  mod_entity_server(
    "topic",
    entity_type = "topic",
    filtered_data = filtered_data,
    comparison_data = filtered_without_date,
    filters_reactive = sidebar_state$filters
  )

  mod_compare_server(
    "compare",
    filtered_data = filtered_data
  )

  mod_comments_server(
    "comments",
    filtered_data = filtered_data
  )

  mod_alerts_server(
    "alerts",
    filtered_data = filtered_data
  )

  observeEvent(sidebar_state$refresh_trigger(), {
    load_feedback_data()
  }, ignoreInit = TRUE)

  observeEvent(input$app_retry, {
    load_feedback_data()
  }, ignoreInit = TRUE)

  observeEvent(TRUE, {
    load_feedback_data()
  }, once = TRUE, ignoreInit = FALSE)

  output$app_shell <- shiny::renderUI({
    current_data <- state$data

    if (is.null(current_data) && (!isTRUE(state$initialized) || isTRUE(state$loading))) {
      return(loading_state_ui())
    }

    if (is.null(current_data) && !is.null(state$error)) {
      return(error_state_ui(state$error))
    }

    dashboard_ui()
  })
}

shiny::shinyApp(ui, server)
