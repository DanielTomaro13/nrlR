#' Fetch Rugby League Ladder (main wrapper)
#'
#' Fetches ladder standings for a given season and league from a specified source.
#'
#' @param season Integer. Season year (1998 or later).
#' @param league Character. One of: "nrl", "super_league", "championship", "league_one",
#'   "womens_super_league", "qld_cup", "nsw_cup",
#'   "state_of_origin", "challenge_cup", "1895_cup".
#' @param source Character. Either "rugbyleagueproject" or "nrl".
#'
#' @return A tibble with ladder standings and statistics.
#' @export
#'
#' @examples
#' fetch_ladder(2025, league = "nrl", source = "nrl")
#' fetch_ladder(2024, league = "super_league")
fetch_ladder <- function(season,
                         league = c("nrl", "super_league", "championship", "league_one",
                                    "womens_super_league", "qld_cup", "nsw_cup",
                                    "state_of_origin", "challenge_cup", "1895_cup"),
                         source = c("rugbyleagueproject", "nrl")) {
  source <- base::match.arg(source)
  league <- base::match.arg(league)
  
  switch(source,
         "rugbyleagueproject" = fetch_ladder_rugbyleagueproject(season = season, league = league),
         "nrl" = {
           if (league != "nrl") {
             cli::cli_abort("Source 'nrl' only supports league = 'nrl'.")
           }
           fetch_ladder_nrl(season = season)
         },
         cli::cli_abort("Unsupported source: {.val {source}}")
  )
}

#' Fetch Rugby League Ladder from Rugby League Project
#'
#' @inheritParams fetch_ladder
#' @noRd
fetch_ladder_rugbyleagueproject <- function(season, league) {
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
    cli::cli_abort("Unsupported league: {.val {league}}")
  }
  
  url <- glue::glue("https://www.rugbyleagueproject.org/seasons/{slug}-{season}/summary.html")
  cli::cli_inform(glue::glue("Fetching {league} ladder for {season}..."))
  
  page <- tryCatch(
    rvest::read_html(url),
    error = function(e) {
      cli::cli_warn(glue::glue("Failed to read ladder page for {league}, {season}."))
      return(NULL)
    }
  )
  if (base::is.null(page)) return(NULL)
  
  rows <- rvest::html_elements(page, "tr.data")
  
  extract_row <- function(row) {
    cols <- rvest::html_elements(row, "td")
    if (base::length(cols) < 27) return(NULL)
    
    tibble::tibble(
      season = season,
      league = league,
      position = base::as.integer(stringr::str_remove(rvest::html_text(cols[[1]], trim = TRUE), "\\.")),
      team = rvest::html_text(cols[[2]], trim = TRUE),
      home_p = rvest::html_text(cols[[3]]),
      home_w = rvest::html_text(cols[[4]]),
      home_l = rvest::html_text(cols[[5]]),
      home_d = rvest::html_text(cols[[6]]),
      home_f = rvest::html_text(cols[[7]]),
      home_a = rvest::html_text(cols[[8]]),
      home_pd = rvest::html_text(cols[[9]]),
      away_p = rvest::html_text(cols[[10]]),
      away_w = rvest::html_text(cols[[11]]),
      away_l = rvest::html_text(cols[[12]]),
      away_d = rvest::html_text(cols[[13]]),
      away_f = rvest::html_text(cols[[14]]),
      away_a = rvest::html_text(cols[[15]]),
      away_pd = rvest::html_text(cols[[16]]),
      total_p = rvest::html_text(cols[[17]]),
      total_w = rvest::html_text(cols[[18]]),
      total_l = rvest::html_text(cols[[19]]),
      total_d = rvest::html_text(cols[[20]]),
      total_bye = rvest::html_text(cols[[21]]),
      total_f = rvest::html_text(cols[[22]]),
      total_a = rvest::html_text(cols[[23]]),
      points = rvest::html_text(cols[[24]]),
      pd = rvest::html_text(cols[[25]]),
      fpg = rvest::html_text(cols[[26]]),
      apg = rvest::html_text(cols[[27]])
    )
  }
  
  list_tbls <- vector("list", length(rows))
  for (i in seq_along(rows)) {
    list_tbls[[i]] <- extract_row(rows[[i]])
  }
  list_tbls <- Filter(Negate(is.null), list_tbls)
  raw_tbl <- dplyr::bind_rows(list_tbls)
  
  char_cols <- sapply(raw_tbl, is.character)
  for (colname in names(char_cols)[char_cols]) {
    raw_tbl[[colname]] <- stringr::str_replace_all(raw_tbl[[colname]], ",", "")
  }
  
  exclude_cols <- c("team", "league")
  for (colname in setdiff(names(raw_tbl), exclude_cols)) {
    raw_tbl[[colname]] <- suppressWarnings(as.numeric(raw_tbl[[colname]]))
  }
  
  return(raw_tbl)
}

#' Fetch NRL Ladder for a Given Season and Round
#'
#' Retrieves the NRL ladder (standings) for a specified season and optionally round.
#'
#' @param season Integer scalar. Season year (e.g. 2025).
#' @param round_number Integer scalar or NULL. Round number (e.g. 4). If NULL, fetches current ladder.
#' @param comp Integer scalar. Competition ID (default 111 for Telstra Premiership).
#'
#' @return A tibble with ladder positions and stats.
#' @export
fetch_ladder_nrl <- function(season, round_number = NULL, comp = 111) {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  if (!(season %in% 2000:current_year)) {
    cli::cli_abort(paste0("Season must be between 2000 and ", current_year, "."))
  }
  
  comp_name <- match_comp_name(comp)
  
  if (is.null(round_number)) {
    round_number <- ""
  }
  
  cli::cli_inform(glue::glue("Fetching {comp_name} ladder for season {season}..."))
  
  url <- glue::glue("https://www.nrl.com/ladder/?competition={comp}&round={round_number}&season={season}")
  
  page <- tryCatch(
    rvest::read_html(url),
    error = function(e) {
      cli::cli_warn(glue::glue("Failed to read ladder page for {comp_name}, season {season}."))
      return(NULL)
    }
  )
  if (is.null(page)) {
    return(tibble::tibble())
  }
  
  json_raw <- rvest::html_attr(rvest::html_node(page, "#vue-ladder"), "q-data")
  
  if (is.null(json_raw)) {
    cli::cli_warn(glue::glue("No ladder data found for {comp_name}, season {season}."))
    return(tibble::tibble())
  }
  
  data_list <- jsonlite::fromJSON(json_raw)
  positions <- data_list[["positions"]]
  
  ladder <- data.frame(
    comp = rep(comp_name, nrow(positions)),
    team = positions$teamNickname,
    played = positions$stats$played,
    wins = positions$stats$wins,
    draws = positions$stats$drawn,
    losses = positions$stats$lost,
    byes = positions$stats$byes,
    points_for = positions$stats[["points for"]],
    points_against = positions$stats[["points against"]],
    points_diff = positions$stats[["points difference"]],
    points = positions$stats$points,
    streak = positions$stats$streak,
    form = positions$stats$form,
    stringsAsFactors = FALSE
  )
  
  return(ladder)
}

#' Helper: Match Competition Name from ID
#'
#' @param comp Integer. Competition ID.
#' @return Character. Competition name.
#' @noRd
match_comp_name <- function(comp) {
  # Example mapping, expand as needed
  comp_map <- list(
    "111" = "Telstra Premiership",
    "123" = "NRLW",
    "124" = "State of Origin"
  )
  
  comp_chr <- as.character(comp)
  if (comp_chr %in% names(comp_map)) {
    return(comp_map[[comp_chr]])
  }
  
  # Default fallback
  paste("Competition", comp)
}
