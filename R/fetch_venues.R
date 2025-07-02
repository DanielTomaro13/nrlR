#' Fetch Rugby League Venues (main wrapper)
#'
#' @param season Integer. Season year (from 1998).
#' @param league Character. One of: "nrl", "super_league", "championship", "league_one",
#'   "womens_super_league", "qld_cup", "nsw_cup", "state_of_origin", "challenge_cup", "1895_cup".
#' @param source Character. Only "rugbyleagueproject" is currently supported.
#'
#' @return A tibble of venue details.
#' @export
#' @examples
#' fetch_venues(2024, league = "nrl")
#'
#' @importFrom cli cli_inform cli_abort cli_warn
#' @importFrom rvest read_html html_elements html_element html_text html_attr
#' @importFrom glue glue
#' @importFrom tibble tibble as_tibble_row
#' @importFrom dplyr mutate relocate bind_rows
fetch_venues <- function(season,
                         league = c("nrl", "super_league", "championship", "league_one",
                                    "womens_super_league", "qld_cup", "nsw_cup",
                                    "state_of_origin", "challenge_cup", "1895_cup"),
                         source = c("rugbyleagueproject")) {
  source <- base::match.arg(source)
  league <- base::match.arg(league)
  
  switch(source,
         "rugbyleagueproject" = fetch_venues_rugbyleagueproject(season = season, league = league),
         cli::cli_abort("Unsupported source: {.val {source}}")
  )
}

#' Fetch Rugby League Venues from Rugby League Project
#'
#' @inheritParams fetch_venues
#' @noRd
fetch_venues_rugbyleagueproject <- function(season, league) {
  current_year <- base::as.integer(base::format(base::Sys.Date(), "%Y"))
  if (!season %in% 1998:current_year) {
    cli::cli_abort("Season must be between 1998 and {current_year}.")
  }
  
  league_slugs <- list(
    "nrl" = "nrl",
    "super_league" = "super-league",
    "championship" = "championship",
    "league_one" = "league-one",
    "womens_super_league" = "womens-super-league",
    "qld_cup" = "qld-cup",
    "nsw_cup" = "nsw-cup",
    "state_of_origin" = "state-of-origin",
    "challenge_cup" = "challenge-cup",
    "1895_cup" = "1895-cup"
  )
  slug <- league_slugs[[league]]
  if (base::is.null(slug)) {
    cli::cli_abort("Unsupported league: {.val {league}}.")
  }
  
  url <- glue::glue("https://www.rugbyleagueproject.org/seasons/{slug}-{season}/venues.html")
  
  cli::cli_inform("Fetching venues for {.val league} {.val season}...")
  
  page <- tryCatch(
    rvest::read_html(url),
    error = function(e) {
      cli::cli_warn("Failed to fetch venues for {.val season}.")
      return(NULL)
    }
  )
  if (base::is.null(page)) return(NULL)
  
  rows <- rvest::html_elements(page, "table tr")
  rows <- rows[2:base::length(rows)]  # exclude header row
  
  venue_data <- base::lapply(rows, function(row) {
    rvest::html_elements(row, "td") |>
      rvest::html_text(trim = TRUE)
  })
  
  venue_links <- base::vapply(rows, function(row) {
    link <- rvest::html_element(row, "td a") |> rvest::html_attr("href")
    if (base::is.na(link)) {
      NA_character_
    } else {
      glue::glue("https://www.rugbyleagueproject.org{link}")
    }
  }, FUN.VALUE = character(1))
  
  max_cols <- max(base::lengths(venue_data))
  padded <- base::lapply(venue_data, function(x) {
    base::c(x, base::rep(NA_character_, max_cols - base::length(x)))
  })
  
  venues_df <- tibble::tibble()
  for (row_data in padded) {
    venues_df <- dplyr::bind_rows(venues_df, tibble::as_tibble_row(row_data, .name_repair = "minimal"))
  }
  
  expected_cols <- c(
    "venue", "alt_name", "home_team", "location", "country",
    "games", "avg_attendance", "max_attendance", "total_attendance"
  )
  base::colnames(venues_df)[seq_along(expected_cols)] <- expected_cols[seq_along(expected_cols)]
  
  venues_df$season <- season
  venues_df$league <- league
  venues_df$venue_link <- venue_links
  
  
  venues_df <- venues_df[, c("season", "league", "venue", "home_team", "location", "country",
                             "games", "avg_attendance", "max_attendance", "total_attendance", "venue_link")]
  
  
  return(venues_df)
}
