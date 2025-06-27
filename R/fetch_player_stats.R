#' Fetch Rugby League Player Stats by Match
#'
#' Main wrapper to fetch player stats for one or more seasons and rounds.
#'
#' @param seasons Integer vector. One or more seasons to fetch (from 1998 onward).
#' @param league Character. One of: "nrl", "super_league", "championship", "league_one",
#'   "womens_super_league", "qld_cup", "nsw_cup".
#' @param rounds Integer vector. Which rounds to include (default 1:30).
#' @param save_dir Character. Directory to save .rds files (optional).
#'
#' @return Saves .rds files by season or returns a combined tibble if `save_dir` is NULL.
#' @export
fetch_player_stats <- function(seasons,
                               league = c("nrl", "super_league", "championship", "league_one",
                                          "womens_super_league", "qld_cup", "nsw_cup"),
                               rounds = 1:30,
                               save_dir = NULL) {
  league <- base::match.arg(league)
  current_year <- base::as.integer(base::format(base::Sys.Date(), "%Y"))
  if (any(!seasons %in% 1998:current_year)) {
    cli::cli_abort(paste0("All seasons must be between 1998 and ", current_year, "."))
  }
  
  if (is.null(save_dir)) {
    # Return combined data for all seasons
    return(fetch_player_stats_rugbyproject(seasons, league, rounds))
  } else {
    # Save each season's data to separate .rds files
    for (season in seasons) {
      stats <- fetch_player_stats_rugbyproject(season, league, rounds)
      dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
      file_path <- paste0(save_dir, "/player_stats_", league, "_", season, ".rds")
      readr::write_rds(stats, file_path)
      cli::cli_inform(paste0("Saved ", base::nrow(stats), " rows to ", file_path))
    }
    invisible(NULL)
  }
}

#' Internal: Fetch Rugby League Player Stats from Rugby League Project
#'
#' @param seasons Integer vector of length 1 (single season).
#' @param league Character scalar.
#' @param rounds Integer vector of rounds.
#' @return A tibble of player stats.
#' @noRd
fetch_player_stats_rugbyproject <- function(seasons, league, rounds) {
  # Define team slugs for the league
  league_team_slugs <- list(
    nrl = c(
      "brisbane-broncos", "canberra-raiders", "canterbury-bankstown-bulldogs",
      "cronulla-sutherland-sharks", "dolphins", "gold-coast-titans",
      "manly-warringah-sea-eagles", "melbourne-storm", "newcastle-knights",
      "new-zealand-warriors", "north-queensland-cowboys", "parramatta-eels",
      "penrith-panthers", "south-sydney-rabbitohs", "st-george-illa-dragons",
      "sydney-roosters", "wests-tigers"
    )
  )
  
  # Check if slug list exists for the league
  if (base::is.null(league_team_slugs[[league]])) {
    cli::cli_abort(paste0("No team slug list defined for ", league, "."))
  }
  
  teams <- league_team_slugs[[league]]
  
  # Function to scrape a single match stats page
  scrape_match <- function(url, round, season) {
    # Extract team slugs from the url for naming
    slug <- stringr::str_extract(url, "(?<=round-\\d{1,2}/)[^/]+")
    teams_in_url <- base::strsplit(slug, "-vs-")[[1]]
    team_names <- stringr::str_replace_all(teams_in_url, "-", " ")
    team_names <- stringr::str_to_title(team_names)
    
    # Read the page; return NULL if failed
    page <- tryCatch(xml2::read_html(url), error = function(e) return(NULL))
    if (base::is.null(page)) return(NULL)
    
    tables <- rvest::html_elements(page, "table.list")
    if (base::length(tables) < 2) return(NULL)
    
    # Extract stats table for a team
    extract_table <- function(tbl, team_name) {
      rows <- rvest::html_elements(tbl, "tr")
      if (base::length(rows) < 4) return(NULL)
      rows <- rows[3:base::length(rows)]
      
      # Extract cell text for each row
      data <- lapply(rows, function(row) {
        cells <- rvest::html_elements(row, "td")
        sapply(cells, function(cell) rvest::html_text(cell, trim = TRUE))
      })
      
      # Keep rows with 6 or more columns
      data <- Filter(function(x) length(x) >= 6, data)
      
      # Convert each row to tibble
      tbl_rows <- lapply(data, function(x) {
        tibble::tibble(
          player = x[1],
          age = suppressWarnings(base::as.numeric(x[2])),
          tries = suppressWarnings(base::as.numeric(x[3])),
          goals = suppressWarnings(base::as.numeric(x[4])),
          field_goals = suppressWarnings(base::as.numeric(x[5])),
          points = suppressWarnings(base::as.numeric(x[6])),
          team = team_name,
          round = round,
          match_url = url,
          season = season,
          league = league
        )
      })
      
      combined <- dplyr::bind_rows(tbl_rows)
      # Remove total rows
      combined <- dplyr::filter(combined, player != "total" & player != "Total")
      return(combined)
    }
    
    # Extract tables for both teams and combine
    tbl1 <- extract_table(tables[[1]], team_names[1])
    tbl2 <- extract_table(tables[[2]], team_names[2])
    
    dplyr::bind_rows(tbl1, tbl2)
  }
  
  # Check if URL exists and reachable
  check_url_exists <- function(url) {
    res <- tryCatch(httr::HEAD(url, httr::timeout(5)), error = function(e) NULL)
    if (base::is.null(res)) return(FALSE)
    res$status_code == 200
  }
  
  # For each season, build all match URLs, filter existing ones, scrape stats
  all_data_list <- lapply(seasons, function(season) {
    # Create all possible team matchups and rounds
    matchups <- expand.grid(
      round = rounds,
      team_a = teams,
      team_b = teams,
      stringsAsFactors = FALSE
    )
    
    # Remove self-matches
    matchups <- matchups[matchups$team_a != matchups$team_b, ]
    
    # Add slug and url columns
    matchups$slug <- mapply(function(a, b) paste0(a, "-vs-", b), matchups$team_a, matchups$team_b, USE.NAMES = FALSE)
    matchups$url <- mapply(function(rnd, slg) {
      glue::glue("https://www.rugbyleagueproject.org/seasons/{league}-{season}/round-{rnd}/{slg}/stats.html")
    }, matchups$round, matchups$slug, USE.NAMES = FALSE)
    
    # Filter URLs that exist
    exists_vec <- sapply(matchups$url, check_url_exists)
    matchups <- matchups[exists_vec, ]
    
    cli::cli_inform(paste0("Found ", nrow(matchups), " valid matches for ", league, " ", season))
    
    # Scrape all matches for the season
    match_data <- Map(scrape_match, matchups$url, matchups$round, MoreArgs = list(season = season))
    
    dplyr::bind_rows(match_data)
  })
  
  # Combine all seasons data into one tibble
  all_data <- dplyr::bind_rows(all_data_list)
  
  return(all_data)
}

