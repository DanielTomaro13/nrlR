#' Fetch Rugby League Match Results
#'
#' Main wrapper to fetch match results for one or more seasons.
#'
#' @param seasons Integer vector. One or more seasons to fetch (1998 or later).
#' @param league Character. League name.
#' @param source Character. Data source; only "rugbyleagueproject" currently supported.
#'
#' @return A tibble of match results.
#' @export
fetch_results <- function(seasons,
                          league = c("nrl", "super_league", "championship", "league_one",
                                     "womens_super_league", "qld_cup", "nsw_cup",
                                     "state_of_origin", "challenge_cup", "1895_cup"),
                          source = c("rugbyleagueproject")) {
  source <- base::match.arg(source)
  league <- base::match.arg(league)
  
  if (source == "rugbyleagueproject") {
    return(fetch_results_rugbyproject(seasons = seasons, league = league))
  } else {
    cli::cli_abort(glue::glue("Source {.val {source}} not supported."))
  }
}

#' Fetch Rugby League Match Results from Rugby League Project
#'
#' @inheritParams fetch_results
#' @return A tibble of match results.
#' @export
fetch_results_rugbyproject <- function(seasons, league) {
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
    cli::cli_abort(glue::glue("Unsupported league: {.val {league}}."))
  }
  
  current_year <- base::as.integer(base::format(base::Sys.Date(), "%Y"))
  if (any(!seasons %in% 1998:current_year)) {
    cli::cli_abort(glue::glue("All seasons must be between 1998 and {current_year}."))
  }
  
  if (interactive()) cli::cli_progress_bar("Fetching match results", total = length(seasons))
  
  results <- purrr::map_dfr(seasons, function(season) {
    if (interactive()) cli::cli_progress_update()
    
    url <- glue::glue("https://www.rugbyleagueproject.org/seasons/{slug}-{season}/results.html")
    
    page <- tryCatch(
      rvest::read_html(url),
      error = function(e) {
        cli::cli_warn(glue::glue("Failed to fetch page: {url}"))
        return(NULL)
      }
    )
    if (base::is.null(page)) return(NULL)
    
    rows <- rvest::html_elements(page, "table tr")
    rows <- base::tail(rows, -1)  # drop header row
    
    purrr::map_dfr(rows, function(row) {
      cols <- rvest::html_elements(row, "td")
      if (base::length(cols) < 11) return(NULL)
      
      tibble::tibble(
        season = season,
        league = league,
        date = rvest::html_text(cols[[2]], trim = TRUE),
        time = rvest::html_text(cols[[3]], trim = TRUE),
        home_team = rvest::html_text(cols[[4]], trim = TRUE),
        home_score = base::as.integer(rvest::html_text(cols[[5]], trim = TRUE)),
        away_team = rvest::html_text(cols[[6]], trim = TRUE),
        away_score = base::as.integer(rvest::html_text(cols[[7]], trim = TRUE)),
        referee = rvest::html_text(cols[[8]], trim = TRUE),
        venue = rvest::html_text(cols[[9]], trim = TRUE),
        attendance = base::as.integer(stringr::str_remove_all(rvest::html_text(cols[[10]], trim = TRUE), ","))
      )
    })
  })
  
  if (interactive()) cli::cli_progress_done()
  
  return(results)
}
