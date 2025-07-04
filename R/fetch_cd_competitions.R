#' Fetch Champion Data Competitions List
#'
#' Retrieves the list of competitions from Champion Data MC API.
#'
#' @return A tibble containing competition details.
#' @export
fetch_cd_comps <- function() {
  url <- "https://mc.championdata.com/data/competitions.json"
  
  comps_raw <- tryCatch(
    jsonlite::fromJSON(url),
    error = function(e) {
      cli::cli_warn("Failed to fetch Champion Data competitions: {e$message}")
      return(NULL)
    }
  )
  
  if (is.null(comps_raw)) {
    return(tibble::tibble())
  }
  
  comps_tbl <- tibble::as_tibble(comps_raw[["competitionDetails"]]$competition)
  
  return(comps_tbl)
}

