#' Fetch Rugby League Coaches (main wrapper)
#'
#' @param season Integer. Season year (from 1998).
#' @param league Character. One of: "nrl", "super_league", "championship", "league_one",
#'   "womens_super_league", "qld_cup", "nsw_cup",
#'   "state_of_origin", "challenge_cup", "1895_cup".
#' @param source Character. Only "rugbyleagueproject" is currently supported.
#'
#' @return A tibble of coach details.
#' @export
#' @examples
#' fetch_coaches(2024, league = "nrl")
fetch_coaches <- function(season,
                          league = c("nrl", "super_league", "championship", "league_one",
                                     "womens_super_league", "qld_cup", "nsw_cup",
                                     "state_of_origin", "challenge_cup", "1895_cup"),
                          source = c("rugbyleagueproject")) {
  source <- base::match.arg(source)
  league <- base::match.arg(league)
  
  switch(source,
         "rugbyleagueproject" = fetch_coaches_rugbyleagueproject(season = season, league = league),
         cli::cli_abort(paste0("Unsupported source: ", source))
  )
}

#' Fetch Rugby League Coaches from Rugby League Project
#'
#' @inheritParams fetch_coaches
#' @noRd
fetch_coaches_rugbyleagueproject <- function(season, league) {
  current_year <- base::as.integer(base::format(base::Sys.Date(), "%Y"))
  if (!season %in% 1998:current_year) {
    cli::cli_abort(paste0("Season must be between 1998 and ", current_year, "."))
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
    cli::cli_abort(paste0("Unsupported league: ", league, "."))
  }
  
  url <- glue::glue("https://www.rugbyleagueproject.org/seasons/{slug}-{season}/coaches.html")
  
  cli::cli_inform(paste0("Fetching coach data for ", league, " ", season, "..."))
  
  page <- tryCatch(
    rvest::read_html(url),
    error = function(e) {
      cli::cli_warn(paste0("Failed to fetch coaches page for ", league, " ", season, "."))
      return(NULL)
    }
  )
  if (base::is.null(page)) return(NULL)
  
  rows <- rvest::html_elements(page, "table tr")
  rows <- rows[-1]  # drop header row
  
  # Helper to clean numeric columns safely
  clean_as_integer <- function(x) {
    txt <- rvest::html_text(x, trim = TRUE)
    txt <- ifelse(txt == "" | txt == "-", NA_character_, txt)
    suppressWarnings(as.integer(txt))
  }
  
  # Helper to extract one row of coach data
  extract_row <- function(row) {
    cols <- rvest::html_elements(row, "td")
    if (length(cols) < 5) return(NULL)
    
    tibble::tibble(
      season = season,
      league = league,
      coach = rvest::html_text(cols[[1]], trim = TRUE),
      team = rvest::html_text(cols[[2]], trim = TRUE),
      games = clean_as_integer(cols[[3]]),
      wins = clean_as_integer(cols[[4]]),
      losses = clean_as_integer(cols[[5]])
    )
  }
  
  list_tbls <- vector("list", length(rows))
  for (i in seq_along(rows)) {
    list_tbls[[i]] <- extract_row(rows[[i]])
  }
  list_tbls <- Filter(Negate(is.null), list_tbls)
  coaches_df <- dplyr::bind_rows(list_tbls)
  
  return(coaches_df)
}
