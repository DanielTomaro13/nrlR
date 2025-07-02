#' Fetch Rugby League Match Results
#'
#' Main wrapper to fetch match results for one or more seasons.
#'
#' @param seasons Integer vector. One or more seasons to fetch (1998 or later).
#' @param league Character. League name. See options.
#' @param source Character. Data source; only "rugbyleagueproject" currently supported.
#'
#' @return A tibble of match results with parsed date and round.
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
#' @return A tibble of match results with parsed date, aligned to weekday, and round.
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
  if (is.null(slug)) {
    cli::cli_abort(glue::glue("Unsupported league: {.val {league}}."))
  }
  
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  if (any(!seasons %in% 1998:current_year)) {
    cli::cli_abort(glue::glue("All seasons must be between 1998 and {current_year}."))
  }
  
  all_results <- list()
  
  for (season in seasons) {
    message(glue::glue("Fetching results for season {season}..."))
    
    url <- glue::glue("https://www.rugbyleagueproject.org/seasons/{slug}-{season}/results.html")
    
    page <- tryCatch(
      rvest::read_html(url),
      error = function(e) {
        cli::cli_warn(glue::glue("Failed to fetch page: {url}"))
        return(NULL)
      }
    )
    if (is.null(page)) next
    
    rows <- rvest::html_elements(page, "table tr")
    rows <- utils::tail(rows, -1)
    
    raw_data <- purrr::map_dfr(rows, function(row) {
      cols <- rvest::html_elements(row, "td")
      if (length(cols) < 11) return(NULL)
      
      tibble::tibble(
        season = season,
        league = league,
        date_raw = rvest::html_text(cols[[2]], trim = TRUE),
        time = rvest::html_text(cols[[3]], trim = TRUE),
        home_team = rvest::html_text(cols[[4]], trim = TRUE),
        home_score = as.integer(rvest::html_text(cols[[5]], trim = TRUE)),
        away_team = rvest::html_text(cols[[6]], trim = TRUE),
        away_score = as.integer(rvest::html_text(cols[[7]], trim = TRUE)),
        referee = rvest::html_text(cols[[8]], trim = TRUE),
        venue = rvest::html_text(cols[[9]], trim = TRUE),
        attendance = as.integer(stringr::str_remove_all(rvest::html_text(cols[[10]], trim = TRUE), ","))
      )
    })
    
    current_month <- NA_character_
    dates_full <- purrr::map_chr(raw_data$date_raw, function(x) {
      if (stringr::str_detect(x, "[A-Za-z]")) {
        current_month <<- x
        return(glue::glue("{current_month} {season}"))
      } else if (!is.na(current_month)) {
        return(glue::glue("{current_month} {x} {season}"))
      } else {
        return(NA_character_)
      }
    })
    
    dates_full <- stringr::str_replace(dates_full, "^([A-Za-z]+) (\\d{4})$", "\\1 1 \\2")
    parsed_dates <- suppressWarnings(
      lubridate::parse_date_time(dates_full, orders = c("b d Y", "d b Y"), tz = "UTC")
    )
    raw_data$date <- as.Date(parsed_dates)
    
    idx <- which(!is.na(raw_data$date))
    for (i in seq_along(raw_data$date)) {
      if (is.na(raw_data$date[i]) && length(idx) > 0) {
        prev <- max(idx[idx < i], na.rm = TRUE)
        raw_data$date[i] <- raw_data$date[prev]
      }
    }
    
    raw_data$weekday_str <- stringr::str_sub(raw_data$time, 1, 3)
    target_days <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
    adjusted_dates <- raw_data$date
    
    for (i in seq_along(adjusted_dates)) {
      target_wday <- match(raw_data$weekday_str[i], target_days, nomatch = NA) - 1
      if (is.na(target_wday) || is.na(adjusted_dates[i])) next
      while (as.integer(format(adjusted_dates[i], "%w")) != target_wday) {
        adjusted_dates[i] <- adjusted_dates[i] + 1
      }
    }
    raw_data$date <- adjusted_dates
    
    round_vec <- integer(length = nrow(raw_data))
    current_round <- 1
    for (i in seq_along(raw_data$date_raw)) {
      if (stringr::str_detect(raw_data$date_raw[i], "[A-Za-z]")) {
        if (i > 1) current_round <- current_round + 1
      }
      round_vec[i] <- current_round
    }
    raw_data$round <- round_vec
    
    raw_data <- raw_data[, c("season", "league", "round", "date", "time",
                             "home_team", "home_score", "away_team", "away_score",
                             "referee", "venue", "attendance"), drop = FALSE]
    
    all_results[[length(all_results) + 1]] <- raw_data
  }
  
  results <- dplyr::bind_rows(all_results)
  return(results)
}
