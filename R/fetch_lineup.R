#' Fetch NRL Team Lineups
#'
#' Fetches NRL team lineups from nrl.com for a given round using a direct URL.
#' Prints a simple CLI message about the URL being fetched.
#'
#' @param url Character. The full URL to the team list page on nrl.com.
#' @param source Character. Currently only "nrl.com" is supported.
#' @param type Character. Currently only "team_list".
#'
#' @return A tibble with game, first_name, last_name, team, role.
#' @export
#'
#' @examples
#' fetch_lineups(url = "https://www.nrl.com/news/2024/05/07/nrl-team-lists-round-10/")
fetch_lineups <- function(url, source = "nrl.com", type = "team_list") {
  if (source != "nrl.com") {
    cli::cli_abort("Only source = 'nrl.com' is currently supported.")
  }
  if (type != "team_list") {
    cli::cli_abort("Only type = 'team_list' is currently implemented.")
  }
  if (is.null(url)) {
    cli::cli_abort("`url` must be provided. This function only supports fetching by explicit URL.")
  }
  
  cli::cli_inform(glue::glue("Fetching team lineups from {url}"))
  
  page <- xml2::read_html(url)
  
  # Walk DOM
  parent_nodes <- rvest::html_elements(page, xpath = "//*[(self::div or self::ul)]")
  lineups_list <- list()
  game_index <- 0
  games_info <- tibble::tibble()
  
  for (node in parent_nodes) {
    class_attr <- rvest::html_attr(node, "class")
    if (!is.na(class_attr) && stringr::str_detect(class_attr, "match-header")) {
      home_team <- rvest::html_text(rvest::html_element(node, ".match-team__name--home"), trim = TRUE)
      away_team <- rvest::html_text(rvest::html_element(node, ".match-team__name--away"), trim = TRUE)
      games_info <- dplyr::bind_rows(games_info, tibble::tibble(
        game = glue::glue("{home_team} vs {away_team}"),
        home_team = home_team,
        away_team = away_team
      ))
      game_index <- game_index + 1
    }
    
    if (rvest::html_name(node) == "ul") {
      li_nodes <- rvest::html_elements(node, "li.team-list")
      if (length(li_nodes) > 0 && game_index > 0) {
        this_game <- games_info[game_index,]
        
        for (li in li_nodes) {
          # HOME
          home_node <- rvest::html_element(li, ".team-list-profile--home .team-list-profile__name")
          home_role_full <- rvest::html_text(rvest::html_element(home_node, "span.u-visually-hidden"), trim = TRUE)
          home_role <- stringr::str_extract(home_role_full, "^[^ ]+")
          home_last <- rvest::html_text(rvest::html_element(home_node, "span.u-font-weight-700"), trim = TRUE)
          home_first <- rvest::html_text2(home_node) %>%
            stringr::str_replace(home_role_full, "") %>%
            stringr::str_replace(home_last, "") %>%
            stringr::str_squish()
          home_role <- ifelse(is.na(home_role), "Unknown", home_role)
          
          # AWAY
          away_node <- rvest::html_element(li, ".team-list-profile--away .team-list-profile__name")
          away_role_full <- rvest::html_text(rvest::html_element(away_node, "span.u-visually-hidden"), trim = TRUE)
          away_role <- stringr::str_extract(away_role_full, "^[^ ]+")
          away_last <- rvest::html_text(rvest::html_element(away_node, "span.u-font-weight-700"), trim = TRUE)
          away_first <- rvest::html_text2(away_node) %>%
            stringr::str_replace(away_role_full, "") %>%
            stringr::str_replace(away_last, "") %>%
            stringr::str_squish()
          away_role <- ifelse(is.na(away_role), "Unknown", away_role)
          
          lineups_list[[length(lineups_list)+1]] <- tibble::tibble(
            game = this_game$game,
            first_name = home_first,
            last_name = home_last,
            team = this_game$home_team,
            role = home_role
          )
          lineups_list[[length(lineups_list)+1]] <- tibble::tibble(
            game = this_game$game,
            first_name = away_first,
            last_name = away_last,
            team = this_game$away_team,
            role = away_role
          )
        }
      }
    }
  }
  
  dplyr::bind_rows(lineups_list) %>%
    dplyr::filter(!is.na(.data$first_name), !is.na(.data$last_name))
}
