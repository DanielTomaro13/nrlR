#' Fetch Player Stats from Multiple Rugby League Sources
#'
#' Unified wrapper to fetch player stats from either Champion Data MC API (NRL/NRLW/State of Origin)
#' or Rugby League Project web scraping.
#'
#' For `source = "championdata"`, provide `comp` (competition ID). `round` is optional.
#' For `source = "rugbyproject"`, provide `season`, `league`, and optionally `round`.
#'
#' @param season Integer scalar. Season year (rugbyproject only).
#' @param league Character scalar. League name (rugbyproject only).
#' @param round Integer scalar or NULL. Round number filter (both sources).
#' @param comp Integer. Competition ID (championdata only).
#' @param source Character scalar. One of "championdata" or "rugbyproject". Default is "championdata".
#'
#' @return A tibble of player stats joined with fixture info.
#' @export
fetch_player_stats <- function(season = NULL,
                               league = c("nrl", "super_league", "championship", "league_one",
                                          "womens_super_league", "qld_cup", "nsw_cup"),
                               round = NULL,
                               comp = NULL,
                               source = c("championdata", "rugbyproject")) {
  
  source <- match.arg(source)
  
  if (source == "rugbyproject") {
    league <- match.arg(league)
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    if (is.null(season)) {
      stop("Please provide a 'season' for rugbyproject source.", call. = FALSE)
    }
    if (!season %in% 1998:current_year) {
      stop(paste0("Season must be between 1998 and ", current_year, "."), call. = FALSE)
    }
    seasons <- c(season)
    rounds <- if (is.null(round)) 1:30 else c(round)
    return(fetch_player_stats_rugbyproject(seasons, league, rounds))
  }
  
  if (source == "championdata") {
    if (is.null(comp)) {
      stop("Parameter 'comp' (competition ID) is required for Champion Data source.", call. = FALSE)
    }
    return(fetch_player_stats_championdata(comp = comp, round = round))
  }
  
  stop("Unsupported source: ", source, call. = FALSE)
}

#' Fetch Champion Data Player Stats for Competition
#'
#' Fetches player stats for the given Champion Data competition ID. Use fetch_cd_competitions for all ids.
#' If round is provided, filters fixtures to that round.
#' Otherwise fetches all completed matches available.
#'
#' @param comp Integer. Competition ID (required).
#' @param round Integer or NULL. Round filter (optional).
#'
#' @return Tibble of player stats joined with fixture info.
#' @export
fetch_player_stats_championdata <- function(comp, round = NULL) {
  if (missing(comp)) {
    stop("Parameter 'comp' (competition ID) is required.", call. = FALSE)
  }
  
  competitions_df <- get_competitions()
  
  if (!(comp %in% competitions_df$id)) {
    stop("Competition ID ", comp, " not found in available competitions.", call. = FALSE)
  }
  
  fixtures <- get_fixtures(comp)
  
  if (!is.null(round)) {
    fixtures <- fixtures[fixtures$roundNumber == round, , drop = FALSE]
  }
  
  fixtures <- fixtures[fixtures$matchStatus == "complete", , drop = FALSE]
  
  if (nrow(fixtures) == 0L) {
    stop("No completed matches found for competition ", comp,
         if (!is.null(round)) paste0(", round ", round), ".", call. = FALSE)
  }
  
  player_stats <- get_player_stats_for_fixtures(fixtures, comp)
  
  if (nrow(player_stats) == 0L) {
    message("No player stats found for competition ", comp, ".")
    return(tibble::tibble())
  }
  
  joined <- dplyr::left_join(
    player_stats,
    fixtures[, c("matchId", "competition_id", "roundNumber", "homeSquadName", 
                 "awaySquadName", "matchStatus", "utcStartTime"), drop = FALSE],
    by = c("match_id" = "matchId", "competition_id")
  )
  
  names(joined)[names(joined) == "roundNumber"] <- "round"
  names(joined)[names(joined) == "homeSquadName"] <- "home_team"
  names(joined)[names(joined) == "awaySquadName"] <- "away_team"
  names(joined)[names(joined) == "matchStatus"] <- "match_status"
  names(joined)[names(joined) == "utcStartTime"] <- "utc_start"
  
  return(joined)
}

#' Internal: Get Competitions List from Champion Data API
#'
#' @return Tibble of competitions data.
#' @noRd
get_competitions <- function() {
  comps_url <- "https://mc.championdata.com/data/competitions.json"
  competitions_raw <- jsonlite::fromJSON(comps_url)
  tibble::as_tibble(competitions_raw[["competitionDetails"]]$competition)
}

#' Internal: Get Fixtures for a Competition
#'
#' @param comp Integer. Competition ID.
#'
#' @return Tibble of fixture data.
#' @noRd
get_fixtures <- function(comp) {
  url_fixtures <- glue::glue("https://mc.championdata.com/data/{comp}/fixture.json")
  fixture_json <- tryCatch(jsonlite::fromJSON(url_fixtures), error = function(e) NULL)
  
  if (is.null(fixture_json$fixture$match)) {
    stop("No fixtures found for competition ", comp, ".", call. = FALSE)
  }
  
  fixtures <- tibble::as_tibble(fixture_json$fixture$match)
  fixtures$competition_id <- comp
  return(fixtures)
}

#' Internal: Fetch Player Stats for Given Fixtures
#'
#' @param fixtures Tibble of fixture data.
#' @param comp Integer. Competition ID.
#'
#' @return Tibble of player stats.
#' @noRd
get_player_stats_for_fixtures <- function(fixtures, comp) {
  player_stats_list <- vector("list", length = nrow(fixtures))
  
  for (i in seq_len(nrow(fixtures))) {
    match_id <- fixtures$matchId[i]
    url_match <- glue::glue("https://mc.championdata.com/data/{comp}/{match_id}.json")
    
    match_data <- tryCatch(jsonlite::fromJSON(url_match), error = function(e) NULL)
    if (is.null(match_data) || is.null(match_data$matchStats)) next
    
    ps <- NULL
    if ("playerStats" %in% names(match_data$matchStats) &&
        "player" %in% names(match_data$matchStats$playerStats) &&
        length(match_data$matchStats$playerStats$player) > 0L) {
      ps <- tibble::as_tibble(match_data$matchStats$playerStats$player)
      
      if ("playerInfo" %in% names(match_data$matchStats) &&
          "player" %in% names(match_data$matchStats$playerInfo) &&
          length(match_data$matchStats$playerInfo$player) > 0L) {
        player_info <- tibble::as_tibble(match_data$matchStats$playerInfo$player)
        player_info <- player_info[, c("playerId", "firstname", "surname", 
                                       "shortDisplayName", "displayName"), drop = FALSE]
        ps <- dplyr::left_join(ps, player_info, by = "playerId")
      }
      ps$match_id <- match_id
      ps$competition_id <- comp
    }
    
    player_stats_list[[i]] <- ps
  }
  
  dplyr::bind_rows(player_stats_list)
}

#' Internal: Fetch Rugby League Project Player Stats
#'
#' Fetch player stats from Rugby League Project website by scraping match pages.
#'
#' @param seasons Integer vector. Seasons to fetch.
#' @param league Character scalar. League name.
#' @param rounds Integer vector. Rounds to fetch.
#'
#' @return Tibble of player stats.
#' @noRd
fetch_player_stats_rugbyproject <- function(seasons, league, rounds = 1:30) {
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
  
  if (is.null(league_team_slugs[[league]])) {
    cli::cli_abort(paste0("No team slug list defined for ", league, "."))
  }
  
  teams <- league_team_slugs[[league]]
  
  scrape_match <- function(url, round, season) {
    slug <- stringr::str_extract(url, "(?<=round-\\d{1,2}/)[^/]+")
    teams_in_url <- strsplit(slug, "-vs-")[[1]]
    team_names <- stringr::str_to_title(gsub("-", " ", teams_in_url))
    
    page <- tryCatch(xml2::read_html(url), error = function(e) NULL)
    if (is.null(page)) return(NULL)
    
    tables <- rvest::html_elements(page, "table.list")
    if (length(tables) < 2) return(NULL)
    
    extract_table <- function(tbl, team_name) {
      rows <- rvest::html_elements(tbl, "tr")
      if (length(rows) < 4) return(NULL)
      rows <- rows[3:length(rows)]
      
      data <- lapply(rows, function(row) {
        cells <- rvest::html_elements(row, "td")
        vapply(cells, rvest::html_text, character(1), trim = TRUE)
      })
      
      data <- Filter(function(x) length(x) >= 6, data)
      
      tbl_rows <- lapply(data, function(x) {
        tibble::tibble(
          player = x[1],
          age = suppressWarnings(as.numeric(x[2])),
          tries = suppressWarnings(as.numeric(x[3])),
          goals = suppressWarnings(as.numeric(x[4])),
          field_goals = suppressWarnings(as.numeric(x[5])),
          points = suppressWarnings(as.numeric(x[6])),
          team = team_name,
          round = round,
          match_url = url,
          season = season,
          league = league
        )
      })
      
      combined <- dplyr::bind_rows(tbl_rows)
      combined[!(combined$player %in% c("total", "Total")), , drop = FALSE]
    }
    
    tbl1 <- extract_table(tables[[1]], team_names[1])
    tbl2 <- extract_table(tables[[2]], team_names[2])
    
    dplyr::bind_rows(tbl1, tbl2)
  }
  
  check_url_exists <- function(url) {
    res <- tryCatch(httr::HEAD(url, httr::timeout(5)), error = function(e) NULL)
    if (is.null(res)) return(FALSE)
    res$status_code == 200L
  }
  
  all_data_list <- list()
  for (season in seasons) {
    matchups <- expand.grid(
      round = rounds,
      team_a = teams,
      team_b = teams,
      stringsAsFactors = FALSE
    )
    matchups <- matchups[matchups$team_a != matchups$team_b, , drop = FALSE]
    matchups$slug <- mapply(function(a, b) paste0(a, "-vs-", b),
                            matchups$team_a, matchups$team_b,
                            USE.NAMES = FALSE)
    matchups$url <- mapply(function(rnd, slg) {
      glue::glue("https://www.rugbyleagueproject.org/seasons/{league}-{season}/round-{rnd}/{slg}/stats.html")
    }, matchups$round, matchups$slug, USE.NAMES = FALSE)
    
    exists_vec <- vapply(matchups$url, check_url_exists, logical(1))
    matchups <- matchups[exists_vec, , drop = FALSE]
    
    cli::cli_inform(paste0("Found ", nrow(matchups), " valid matches for ", league, " ", season))
    
    match_data <- mapply(scrape_match, matchups$url, matchups$round, 
                         MoreArgs = list(season = season), SIMPLIFY = FALSE)
    all_data_list[[as.character(season)]] <- dplyr::bind_rows(match_data)
  }
  
  dplyr::bind_rows(all_data_list)
}
