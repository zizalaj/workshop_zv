generate_mock_feedback <- function(seed = 20260831) {
  set.seed(seed)

  teachers <- tibble::tribble(
    ~teacher_id, ~teacher_name, ~base_score, ~trend_slope,
    "teacher-1", "Adela Kratochvilova", 4.8, -0.22,
    "teacher-2", "Martin Havel", 4.6, -0.18,
    "teacher-3", "Petra Novakova", 4.2, 0.03,
    "teacher-4", "Jan Smetana", 4.0, 0.06
  )

  clients <- tibble::tribble(
    ~client_id, ~client_name, ~client_effect,
    "client-1", "Asteria Tech", 0.05,
    "client-2", "Brno Logistics", -0.04,
    "client-3", "Cobalt Retail", 0.02,
    "client-4", "Delta Finance", -0.02
  )

  positive_comments <- c(
    "Skvely workshop, vse bylo srozumitelne a prakticke.",
    "Lektorka vedla diskusi velmi prirozene a s dobrou energii.",
    "Oceňuji konkretni priklady i prostor pro dotazy.",
    "Tempo bylo prijemne a materialy jsme hned vyuzili v praxi.",
    "Velmi uzitecne, odnasime si jasne dalsi kroky."
  )

  mixed_comments <- c(
    "Obsah byl prinosny, ale uvital bych vic casu na cviceni.",
    "Workshop byl dobry, jen posledni cast byla trochu rychla.",
    "Fajn energie, nektere priklady by mohly byt vic z naseho prostredi.",
    "Prakticke tipy super, materialy by mohly byt prehlednejsi.",
    "Dobre vedene, ale cast ucastniku by potrebovala vice zapojit."
  )

  concern_comments <- c(
    "Tentokrat bylo tempo moc rychle a cast tymu se ztracela.",
    "Chybely konkretni priklady pro nas kazdodenni provoz.",
    "Workshop pusobil mene pripravene nez minule setkani.",
    "Materialy byly tentokrat slabsi a diskuse se tezko rozjela.",
    "Obsah dava smysl, ale doruceni bylo mene presvedcive."
  )

  dimensions <- ordered_dimensions(names(dimension_weights))
  month_starts <- seq(
    from = lubridate::`%m-%`(
      lubridate::floor_date(Sys.Date(), "month"),
      lubridate::period(months = 11)
    ),
    by = "1 month",
    length.out = 12
  )

  workshops <- tidyr::expand_grid(
    teacher_id = teachers$teacher_id,
    month_start = month_starts,
    workshop_index = 1:2
  ) |>
    dplyr::left_join(teachers, by = "teacher_id") |>
    dplyr::mutate(
      client_id = sample(clients$client_id, dplyr::n(), replace = TRUE),
      workshop_date = .data$month_start + sample(1:24, dplyr::n(), replace = TRUE),
      workshop_rank = dplyr::dense_rank(.data$month_start),
      workshop_id = sprintf("ws-%s-%03d", .data$teacher_id, dplyr::row_number())
    ) |>
    dplyr::left_join(clients, by = "client_id") |>
    dplyr::mutate(
      n_participants = sample(c(8:22, NA_integer_), dplyr::n(), replace = TRUE),
      recent_penalty = dplyr::case_when(
        .data$teacher_id == "teacher-1" & .data$workshop_rank >= 10 ~ -0.95,
        .data$teacher_id == "teacher-2" & .data$workshop_rank >= 10 ~ -0.72,
        TRUE ~ 0
      ),
      workshop_score = pmin(
        4.9,
        pmax(
          2.6,
          .data$base_score +
            (.data$workshop_rank - 1) * .data$trend_slope / 2 +
            .data$client_effect +
            .data$recent_penalty +
            stats::rnorm(dplyr::n(), mean = 0, sd = 0.16)
        )
      )
    )

  responses <- purrr::pmap_dfr(workshops, function(teacher_id,
                                                   month_start,
                                                   workshop_index,
                                                   teacher_name,
                                                   base_score,
                                                   trend_slope,
                                                   client_id,
                                                   workshop_date,
                                                   workshop_rank,
                                                   workshop_id,
                                                   client_name,
                                                   client_effect,
                                                   n_participants,
                                                   recent_penalty,
                                                   workshop_score) {
    response_count <- sample(4:9, 1)

    tibble::tibble(
      response_id = sprintf("%s-r%02d", workshop_id, seq_len(response_count)),
      workshop_id = workshop_id,
      workshop_date = as.Date(workshop_date),
      teacher_id = teacher_id,
      teacher_name = teacher_name,
      client_id = client_id,
      client_name = client_name,
      n_participants = as.integer(n_participants),
      response_score = pmin(
        5,
        pmax(
          1,
          workshop_score + stats::rnorm(response_count, 0, 0.24)
        )
      )
    ) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        comment_text = {
          score <- .data$response_score
          comment_pool <- if (score >= 4.3) {
            positive_comments
          } else if (score >= 3.6) {
            mixed_comments
          } else {
            concern_comments
          }

          if (runif(1) < 0.2) {
            NA_character_
          } else {
            enc2utf8(sample(comment_pool, 1))
          }
        }
      ) |>
      dplyr::ungroup()
  })

  feedback <- tidyr::expand_grid(
    responses,
    dimension = dimensions
  ) |>
    dplyr::mutate(
      dimension_score = pmin(
        5,
        pmax(
          1,
          .data$response_score +
            dplyr::case_when(
              .data$dimension == "content" ~ 0.10,
              .data$dimension == "delivery" ~ 0.04,
              .data$dimension == "engagement" ~ -0.03,
              .data$dimension == "materials" ~ -0.06,
              TRUE ~ 0
            ) +
            stats::rnorm(dplyr::n(), mean = 0, sd = 0.22)
        )
      )
    ) |>
    dplyr::select(
      response_id,
      workshop_id,
      workshop_date,
      teacher_id,
      teacher_name,
      client_id,
      client_name,
      n_participants,
      dimension,
      dimension_score,
      comment_text
    ) |>
    dplyr::arrange(.data$workshop_date, .data$teacher_name, .data$response_id)

  append_composite_score(feedback)
}
