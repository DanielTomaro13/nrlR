#' Return the fixture for a particular round of matches
#'
#' @description
#' `fetch_fixture` returns the Fixture for a given NRL Round. 
#' Internally, it calls a `fetch_fixture_*` function depending on the source.
#'
#' By default it uses "NRL", but can be extended later to other sources.
#'
#' @param season Numeric. Season year (e.g. 2025).
#' @param round_number Numeric. Round number (e.g. 4).
#' @param comp Competition id, default `111` for Telstra NRL Premiership.
#' @param source Source of the data ("NRL").
#' @param ... Additional arguments passed to source-specific functions.
#' 
#' @return A tibble with the fixture.
#' @export
#'
#' @examples
#' \dontrun{
#' fetch_fixture(2025, 4)
#' fetch_fixture(2025, 18, source = "NRL")
#' }
#' 
#' @family fetch fixture functions
fetch_fixture <- function(season = NULL,
                          round_number = NULL,
                          comp = 111,
                          source = "NRL",
                          ...) {
  # Dispatch by source
  dat <- switch(toupper(source),
                "NRL" = fetch_fixture_nrl(season, round_number, comp, ...),
                {
                  cli::cli_warn("Unknown source '{source}'. Currently supported: 'NRL'.")
                  return(dplyr::tibble())
                }
  )
  
  return(dat)
}


#' @rdname fetch_fixture
#' @export
fetch_fixture_nrl <- function(season = NULL, round_number = NULL, comp = 111) {
  if (is.null(season) | is.null(round_number)) {
    cli::cli_abort("Please provide both season and round_number for NRL fixtures.")
  }
  
  comp_name <- match_comp_name(comp)
  msg <- glue::glue("Returning {comp_name} fixture for round {round_number}, season {season}")
  cli::cli_progress_step(msg)
  
  # Pull the page
  url <- glue::glue("https://www.nrl.com/draw/?competition={comp}&round={round_number}&season={season}")
  page <- rvest::read_html(url)
  q_data <- rvest::html_node(page, "#vue-draw")
  q_data <- rvest::html_attr(q_data, "q-data")
  
  data_list <- jsonlite::fromJSON(q_data)
  
  # Handle empty fixture
  if (length(data_list[["fixtures"]]) == 0) {
    cli::cli_warn("No fixture data found for round {round_number} in season {season}")
    return(dplyr::tibble())
  }
  
  fixtures <- data_list[["fixtures"]]
  fixtures <- dplyr::filter(fixtures, .data$type == "Match")
  
  # Convert each row to list (fix flattened vectors for historical matches)
  home_team_list <- purrr::map(seq_len(nrow(fixtures)), ~ fixtures$homeTeam[.x, , drop=FALSE])
  away_team_list <- purrr::map(seq_len(nrow(fixtures)), ~ fixtures$awayTeam[.x, , drop=FALSE])
  
  fixtures <- dplyr::mutate(
    fixtures,
    comp = comp_name,
    home_team = purrr::map_chr(home_team_list, ~ purrr::pluck(.x, "nickName", .default = NA_character_)),
    home_score = purrr::map_int(home_team_list, ~ purrr::pluck(.x, "score", .default = NA_integer_)),
    away_team = purrr::map_chr(away_team_list, ~ purrr::pluck(.x, "nickName", .default = NA_character_)),
    away_score = purrr::map_int(away_team_list, ~ purrr::pluck(.x, "score", .default = NA_integer_)),
    kickoff_utc = lubridate::ymd_hms(.data$clock$kickOffTimeLong, tz = "UTC"),
    kickoff_local = lubridate::with_tz(.data$kickoff_utc, "Australia/Sydney"),
    match_link = paste0("https://www.nrl.com", .data$matchCentreUrl)
  )
  
  fixtures <- dplyr::transmute(
    fixtures,
    comp = .data$comp,
    round = .data$roundTitle,
    venue = .data$venue,
    city = .data$venueCity,
    home_team = .data$home_team,
    home_score = .data$home_score,
    away_team = .data$away_team,
    away_score = .data$away_score,
    kickoff_utc = .data$kickoff_utc,
    kickoff_local = .data$kickoff_local,
    match_link = .data$match_link
  )
  
  return(fixtures)
}
