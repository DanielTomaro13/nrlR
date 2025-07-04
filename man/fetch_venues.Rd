% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_venues.R
\name{fetch_venues}
\alias{fetch_venues}
\title{Fetch Rugby League Venues (main wrapper)}
\usage{
fetch_venues(
  season,
  league = c("nrl", "super_league", "championship", "league_one", "womens_super_league",
    "qld_cup", "nsw_cup", "state_of_origin", "challenge_cup", "1895_cup"),
  source = c("rugbyleagueproject")
)
}
\arguments{
\item{season}{Integer. Season year (from 1998).}

\item{league}{Character. One of: "nrl", "super_league", "championship", "league_one",
"womens_super_league", "qld_cup", "nsw_cup", "state_of_origin", "challenge_cup", "1895_cup".}

\item{source}{Character. Only "rugbyleagueproject" is currently supported.}
}
\value{
A tibble of venue details.
}
\description{
Fetch Rugby League Venues (main wrapper)
}
\examples{
fetch_venues(2024, league = "nrl")

}
