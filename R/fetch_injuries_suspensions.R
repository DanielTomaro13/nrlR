#' Fetch NRL Injuries and Suspensions
#'
#' @description
#' `fetch_injuries_suspensions` retrieves injury and suspension data for the NRL.
#' It supports multiple data sources via the `source` parameter.
#'
#' @param source Character. Source of the data. Currently only `"zerotackle"` is supported.
#' @param ... Additional arguments passed to source-specific functions.
#'
#' @return A tibble with columns: team, player, reason, expected_return.
#' @export
#'
#' @examples
#' \dontrun{
#'   fetch_injuries_suspensions()
#' }
fetch_injuries_suspensions <- function(source = "zerotackle", ...) {
  source <- tolower(source)

  if (source == "zerotackle") {
    return(fetch_injuries_suspensions_zerotackle(...))
  } else {
    cli::cli_abort("Invalid source `{source}`. Currently only {.val zerotackle} is supported.")
  }
}


#' Fetch NRL Injuries and Suspensions from Zero Tackle
#'
#' @description
#' Scrapes Zero Tackle's NRL Injuries & Suspensions page and returns a tidy tibble.
#'
#' @return A tibble with columns: team, player, reason, expected_return.
#' @export
fetch_injuries_suspensions_zerotackle <- function() {
  url <- "https://www.zerotackle.com/nrl/injuries-suspensions/"
  cli::cli_inform("Fetching data from {.url {url}}...")

  page <- xml2::read_html(url)
  shadow_boxes <- rvest::html_elements(page, ".shadowBox")

  all_data <- purrr::map_dfr(shadow_boxes, function(box) {
    team_node <- rvest::html_element(box, "h4")
    team_name <- rvest::html_text(team_node, trim = TRUE)

    table_node <- rvest::html_element(box, "table")
    if (!is.na(table_node)) {
      df <- rvest::html_table(table_node, fill = TRUE)

      if (ncol(df) >= 4) {
        df <- df[, 2:4]
        colnames(df) <- c("player", "reason", "expected_return")

        df$team <- team_name
        df <- df[, c("team", "player", "reason", "expected_return"), drop = FALSE]
      } else {
        df <- tibble::tibble(
          team = team_name,
          player = paste(df[[1]], collapse = ", "),
          reason = NA_character_,
          expected_return = NA_character_
        )
      }
      return(df)
    }

    tibble::tibble(
      team = character(),
      player = character(),
      reason = character(),
      expected_return = character()
    )
  })

  return(all_data)
}
