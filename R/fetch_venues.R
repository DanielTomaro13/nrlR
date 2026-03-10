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
  

# NEW APPROACH (CLEAN):
# 1. Convert list of row vectors to matrix first - ensures rectangular structure
venue_matrix <- do.call(rbind, padded)[, 1:10, drop = FALSE]
  
  # 2. Explicitly name columns BEFORE tibble conversion - prevents auto-naming warnings
colnames(venue_matrix) <- c("venue", "alt_name", "home_team", "location", "country",
                           "games", "avg_attendance", "max_attendance", "total_attendance", 
                           "extra")
# 3. Convert to tibble then add metadata columns with modern tidyverse pipe
venues_df <- tibble::as_tibble(venue_matrix) |>
  dplyr::mutate(
    season = season,
    league = league,
    venue_link = venue_links,
    .before = 1
  ) |>
  dplyr::select(season, league, venue, home_team, location, country,
                games, avg_attendance, max_attendance, total_attendance, venue_link) |> 
  # convert attendance and games columns to numeric types, removing commas first
  dplyr::mutate(
    avg_attendance = as.numeric(stringr::str_remove(avg_attendance, ",")),
    max_attendance = as.numeric(stringr::str_remove(max_attendance, ",")),
    total_attendance = as.numeric(stringr::str_remove(total_attendance, ",")),
    games = as.integer(games)
  )

# Result: Clean tibble, no console warnings, proper column names from the start
 
  return(venues_df)
}
