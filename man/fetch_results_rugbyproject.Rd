% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_results.R
\name{fetch_results_rugbyproject}
\alias{fetch_results_rugbyproject}
\title{Fetch Rugby League Match Results from Rugby League Project}
\usage{
fetch_results_rugbyproject(seasons, league)
}
\arguments{
\item{seasons}{Integer vector. One or more seasons to fetch (1998 or later).}

\item{league}{Character. League name. See options.}
}
\value{
A tibble of match results with parsed date, aligned to weekday, and round.
}
\description{
Fetch Rugby League Match Results from Rugby League Project
}
