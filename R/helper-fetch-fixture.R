#' Map NRL competition ID to short name
#'
#' @param comp The competition ID as a number (e.g., 111)
#' @return A string like "NRL", "SOO", etc.
#' @keywords internal
#' @noRd

match_comp_name <- function(comp_id) {
  comp_map <- c(
    "111" = "NRL",
    "116" = "SOO",
    "156" = "WSOO",
    "161" = "NRLW",
    "145" = "All Stars",
    "114" = "QRL",
    "113" = "NSWC",
    "167" = "WNC",
    "195" = "PC",
    "196" = "WPC",
    "134" = "WCC",
    "117" = "Nines",
    "178" = "WNines",
    "162" = "HNSW",
    "179" = "BMD",
    "133" = "Intl",
    "135" = "WIntl",
    "157" = "WRWC",
    "130" = "WRLWC",
    "115" = "NSChamp",
    "131" = "RLWC",
    "169" = "WAS",
    "184" = "WSOU19",
    "183" = "SOU19",
    "119" = "PSC",
    "999" = "Unknown"
  )
  out <- comp_map[as.character(comp_id)]
  if (is.na(out)) out <- paste0("Comp ", comp_id)
  return(out)
}
