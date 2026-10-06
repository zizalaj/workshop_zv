ju_palette <- list(
  primary_mint = "#63E8C6",
  near_black = "#1F1C24",
  light_gray = "#D2D2D2",
  dark_teal = "#00AB8B",
  pale_mint = "#B7FFE6",
  off_white_gray = "#F0F0F2",
  pink = "#FF67AA",
  purple = "#AD92FF",
  yellow = "#FFD920",
  white = "#FEFFFF"
)

cz_month_abbr <- c(
  "led", "úno", "bře", "dub", "kvě", "čer",
  "čvc", "srp", "zář", "říj", "lis", "pro"
)

ju_series_colors <- c(
  ju_palette$primary_mint,
  ju_palette$dark_teal,
  ju_palette$pink,
  ju_palette$purple,
  ju_palette$yellow
)

ju_dt_language <- list(
  decimal = ",",
  thousands = " ",
  emptyTable = "Tabulka neobsahuje žádná data",
  info = "Zobrazuji _START_ až _END_ z _TOTAL_ řádků",
  infoEmpty = "Zobrazuji 0 až 0 z 0 řádků",
  infoFiltered = "(filtrováno z _MAX_ řádků celkem)",
  lengthMenu = "Zobrazit _MENU_ řádků",
  loadingRecords = "Načítám...",
  processing = "Zpracovávám...",
  search = "Hledat:",
  zeroRecords = "Nenalezeny žádné odpovídající záznamy",
  paginate = list(
    first = "První",
    last = "Poslední",
    `next` = "Další",
    previous = "Předchozí"
  )
)

ju_register_fonts <- local({
  registered <- FALSE

  function() {
    if (registered) {
      return(invisible(TRUE))
    }

    if (!requireNamespace("sysfonts", quietly = TRUE) ||
        !requireNamespace("showtext", quietly = TRUE)) {
      return(invisible(FALSE))
    }

    europa_regular <- file.path("www", "fonts", "EuropaGroNr2JU Regular.otf")
    europa_bold <- file.path("www", "fonts", "EuropaGroNr2JU Bold.ttf")
    arial_regular <- file.path("www", "fonts", "ArialNova.ttf")
    arial_bold <- file.path("www", "fonts", "ArialNova-Bold.ttf")

    if (file.exists(europa_regular) && file.exists(europa_bold)) {
      sysfonts::font_add(
        family = "EuropaGroNr2JU",
        regular = europa_regular,
        bold = europa_bold
      )
    }

    if (file.exists(arial_regular) && file.exists(arial_bold)) {
      sysfonts::font_add(
        family = "Arial Nova",
        regular = arial_regular,
        bold = arial_bold
      )
    }

    showtext::showtext_auto()
    registered <<- TRUE
    invisible(TRUE)
  }
})

ju_heading_family <- function() {
  if (file.exists(file.path("www", "fonts", "EuropaGroNr2JU Regular.otf"))) {
    return("EuropaGroNr2JU")
  }

  "Arial"
}

ju_body_family <- function() {
  if (file.exists(file.path("www", "fonts", "ArialNova.ttf"))) {
    return("Arial Nova")
  }

  "Arial"
}

brand_logo_src <- function() {
  logo_path <- file.path("www", "logo", "ju_logo.svg")
  if (file.exists(logo_path)) {
    return("logo/ju_logo.svg")
  }

  NULL
}

ju_logo_tag <- function(class_name = "ju-logo") {
  logo_src <- brand_logo_src()

  if (!is.null(logo_src)) {
    return(shiny::tags$img(
      src = logo_src,
      class = class_name,
      alt = "Logo juiceUP"
    ))
  }

  shiny::tags$span(class = class_name, "juiceUP")
}

ju_with_spinner <- function(ui, ...) {
  ui
}

format_cz_date <- function(date_value) {
  if (length(date_value) == 0L || is.na(date_value)) {
    return(NA_character_)
  }

  date_value <- as.Date(date_value)
  paste0(
    as.integer(format(date_value, "%d")),
    ". ",
    as.integer(format(date_value, "%m")),
    ". ",
    format(date_value, "%Y")
  )
}

format_cz_datetime <- function(datetime_value) {
  if (length(datetime_value) == 0L || is.na(datetime_value)) {
    return(NA_character_)
  }

  paste(format_cz_date(as.Date(datetime_value)), format(datetime_value, "%H:%M"))
}

format_cz_month_label <- function(date_value) {
  if (length(date_value) == 0L || is.na(date_value)) {
    return(NA_character_)
  }

  date_value <- as.Date(date_value)
  month_index <- as.integer(format(date_value, "%m"))
  paste(cz_month_abbr[[month_index]], format(date_value, "%Y"))
}

ju_theme <- function() {
  ju_register_fonts()

  bslib::bs_theme(
    version = 5,
    bg = ju_palette$white,
    fg = ju_palette$near_black,
    primary = ju_palette$primary_mint,
    secondary = ju_palette$dark_teal,
    success = ju_palette$dark_teal,
    warning = ju_palette$yellow,
    danger = ju_palette$pink
  ) |>
    bslib::bs_add_rules(
      paste(
        ":root {",
        sprintf("--ju-primary-mint: %s;", ju_palette$primary_mint),
        sprintf("--ju-near-black: %s;", ju_palette$near_black),
        sprintf("--ju-light-gray: %s;", ju_palette$light_gray),
        sprintf("--ju-dark-teal: %s;", ju_palette$dark_teal),
        sprintf("--ju-pale-mint: %s;", ju_palette$pale_mint),
        sprintf("--ju-off-white-gray: %s;", ju_palette$off_white_gray),
        sprintf("--ju-pink: %s;", ju_palette$pink),
        sprintf("--ju-purple: %s;", ju_palette$purple),
        sprintf("--ju-yellow: %s;", ju_palette$yellow),
        sprintf("--ju-white: %s;", ju_palette$white),
        "}"
      )
    )
}

ju_axis_style <- function(title = NULL) {
  list(
    title = title,
    gridcolor = ju_palette$light_gray,
    linecolor = ju_palette$light_gray,
    zerolinecolor = ju_palette$light_gray,
    tickfont = list(
      family = ju_body_family(),
      color = ju_palette$near_black
    ),
    titlefont = list(
      family = ju_body_family(),
      color = ju_palette$near_black
    )
  )
}

ju_apply_plotly_theme <- function(fig, title = NULL, showlegend = TRUE) {
  plotly::layout(
    fig,
    paper_bgcolor = ju_palette$white,
    plot_bgcolor = ju_palette$white,
    font = list(
      family = ju_body_family(),
      color = ju_palette$near_black,
      size = 13
    ),
    title = if (is.null(title)) {
      NULL
    } else {
      list(
        text = title,
        font = list(
          family = ju_heading_family(),
          size = 20,
          color = ju_palette$near_black
        )
      )
    },
    legend = list(
      orientation = "h",
      x = 0,
      y = -0.15
    ),
    margin = list(l = 60, r = 30, t = if (is.null(title)) 40 else 72, b = 60),
    showlegend = showlegend
  )
}

ju_empty_plotly <- function(message) {
  fig <- plotly::plot_ly()
  ju_apply_plotly_theme(fig, showlegend = FALSE) |>
    plotly::layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(list(
        text = message,
        showarrow = FALSE,
        x = 0.5,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        font = list(
          family = ju_body_family(),
          size = 15,
          color = ju_palette$near_black
        )
      ))
    )
}

ju_donut_gauge <- function(value, max_value, detail, label, color = ju_palette$primary_mint) {
  if (is.na(value) || is.na(max_value) || max_value <= 0) {
    return(
      ju_empty_plotly("Nedostupné") |>
        plotly::layout(
          annotations = list(list(
            text = paste0(detail, "<br><span style='font-size:12px;'>", label, "</span>"),
            showarrow = FALSE,
            x = 0.5,
            y = 0.5,
            xref = "paper",
            yref = "paper"
          ))
        )
    )
  }

  remaining <- max(max_value - value, 0)
  fig <- plotly::plot_ly(
    labels = c("value", "remaining"),
    values = c(value, remaining),
    type = "pie",
    hole = 0.78,
    sort = FALSE,
    direction = "clockwise",
    textinfo = "none",
    hoverinfo = "skip",
    marker = list(colors = c(color, ju_palette$light_gray))
  )

  ju_apply_plotly_theme(fig, showlegend = FALSE) |>
    plotly::layout(
      margin = list(l = 10, r = 10, t = 10, b = 10),
      annotations = list(list(
        text = paste0(
          "<span style='font-size:26px;font-family:",
          ju_heading_family(),
          ";'>",
          detail,
          "</span><br><span style='font-size:12px;'>",
          label,
          "</span>"
        ),
        showarrow = FALSE,
        x = 0.5,
        y = 0.5,
        xref = "paper",
        yref = "paper"
      ))
    )
}

make_svg_sparkline <- function(values, stroke = ju_palette$pink) {
  if (!length(values) || all(is.na(values))) {
    return("")
  }

  values <- values[!is.na(values)]
  x_values <- seq(0, 100, length.out = length(values))

  value_range <- range(values)
  if (diff(value_range) == 0) {
    y_values <- rep(20, length(values))
  } else {
    scaled <- (values - value_range[[1]]) / diff(value_range)
    y_values <- 40 - (scaled * 32)
  }

  points <- paste(
    sprintf("%.1f,%.1f", x_values, y_values),
    collapse = " "
  )

  paste0(
    "<svg width='110' height='40' viewBox='0 0 100 40' xmlns='http://www.w3.org/2000/svg'>",
    "<polyline fill='none' stroke='", stroke, "' stroke-width='3' points='", points, "'/>",
    "</svg>"
  )
}
