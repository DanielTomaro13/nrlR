#' Return the fixture for a particular round of matches
#'
#' @description
#' fetch_fixture returns the Fixture for a given NRL Round. 
#' Internally, it calls a fetch_fixture_* function depending on the source.
#'
#' By default it uses "NRL", but can be extended later to other sources.
#'
#' @param season Numeric. Season year (e.g. 2025).
#' @param round_number Numeric. Round number (e.g. 4).
#' @param comp Competition id, default 111 for Telstra NRL Premiership.
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
  if (is.null(season) || is.null(round_number)) {
    cli::cli_abort("Please provide both season and round_number for NRL fixtures.")
  }
  
  comp_name <- match_comp_name(comp)
  msg <- glue::glue("Returning {comp_name} fixture for round {round_number}, season {season}")
  cli::cli_progress_step(msg)
  
  url <- glue::glue("https://www.nrl.com/draw/?competition={comp}&round={round_number}&season={season}")
  page <- rvest::read_html(url)
  q_data_node <- rvest::html_node(page, "#vue-draw")
  q_data_attr <- rvest::html_attr(q_data_node, "q-data")
  
  data_list <- jsonlite::fromJSON(q_data_attr)
  
  if (length(data_list[["fixtures"]]) == 0L) {
    cli::cli_warn(glue::glue("No fixture data found for round {round_number} in season {season}"))
    return(tibble::tibble())
  }
  
  fixtures <- data_list[["fixtures"]]
  fixtures <- fixtures[fixtures$type == "Match", , drop = FALSE]
  
  # Extract home and away team info as lists
  n_matches <- nrow(fixtures)
  home_team_list <- vector("list", n_matches)
  away_team_list <- vector("list", n_matches)
  for (i in seq_len(n_matches)) {
    home_team_list[[i]] <- fixtures$homeTeam[i, , drop = FALSE]
    away_team_list[[i]] <- fixtures$awayTeam[i, , drop = FALSE]
  }
  
  # Extract home/away team names and scores using base lapply
  home_team_vec <- vapply(home_team_list, function(x) {
    if (!is.null(x$nickName)) x$nickName else NA_character_
  }, character(1))
  
  home_score_vec <- vapply(home_team_list, function(x) {
    if (!is.null(x$score)) x$score else NA_integer_
  }, integer(1))
  
  away_team_vec <- vapply(away_team_list, function(x) {
    if (!is.null(x$nickName)) x$nickName else NA_character_
  }, character(1))
  
  away_score_vec <- vapply(away_team_list, function(x) {
    if (!is.null(x$score)) x$score else NA_integer_
  }, integer(1))
  
  # Extract kickoff times safely
  kickoff_times <- vapply(fixtures$clock, function(x) {
    if (!is.null(x$kickOffTimeLong)) x$kickOffTimeLong else NA_character_
  }, character(1))
  
  kickoff_utc_vec <- lubridate::ymd_hms(kickoff_times, tz = "UTC")
  kickoff_local_vec <- lubridate::with_tz(kickoff_utc_vec, "Australia/Sydney")
  
  # Construct match links
  match_links <- paste0("https://www.nrl.com", fixtures$matchCentreUrl)
  
  # Build final tibble without NSE
  result <- tibble::tibble(
    comp = comp_name,
    round = fixtures$roundTitle,
    venue = fixtures$venue,
    city = fixtures$venueCity,
    home_team = home_team_vec,
    home_score = home_score_vec,
    away_team = away_team_vec,
    away_score = away_score_vec,
    kickoff_utc = kickoff_utc_vec,
    kickoff_local = kickoff_local_vec,
    match_link = match_links
  )
  
  return(result)
}
