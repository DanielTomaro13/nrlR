% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_ladder.R
\name{fetch_ladder}
\alias{fetch_ladder}
\title{Fetch Rugby League Ladder (main wrapper)}
\usage{
fetch_ladder(
  season,
  league = c("nrl", "super_league", "championship", "league_one", "womens_super_league",
    "qld_cup", "nsw_cup", "state_of_origin", "challenge_cup", "1895_cup"),
  source = c("rugbyleagueproject", "nrl")
)
}
\arguments{
\item{season}{Integer. Season year (1998 or later).}

\item{league}{Character. One of: "nrl", "super_league", "championship", "league_one",
"womens_super_league", "qld_cup", "nsw_cup",
"state_of_origin", "challenge_cup", "1895_cup".}

\item{source}{Character. Either "rugbyleagueproject" or "nrl".}
}
\value{
A tibble with ladder standings and statistics.
}
\description{
Fetches ladder standings for a given season and league from a specified source.
}
\examples{
fetch_ladder(2025, league = "nrl", source = "nrl")
fetch_ladder(2024, league = "super_league")
}
