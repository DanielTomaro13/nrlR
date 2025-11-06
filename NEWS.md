# nrlR 0.1.2

## Bug Fixes

* Fixed NA handling in `fetch_lineups()` that caused CRAN check failures on multiple platforms
* Added proper NA validation before `stringr::str_replace()` calls to prevent pattern errors when HTML elements are missing

# nrlR 0.1.1

## New Features

* Added `fetch_injuries()` function to scrape injury and suspension data from ZeroTackle (08/07/2025)
* Added advanced statistics functions for player and team metrics supplied by Champion Data via NRL.com
* Added `fetch_fixtures()` function to scrape NRL.com draw for future fixtures and past results (02/07/2025)
* Added `fetch_lineups()` function to scrape team lists released by NRL.com on Tuesdays (30/06/2025)

## Improvements

* Expanded `fetch_player_stats()` with additional data sources

# nrlR 0.1.0

Initial release with support for scraping historical Rugby League data from rugbyleagueproject.org.

## Core Functionality

* `fetch_results()`: Match-level results across NRL, Super League, QLD Cup, NSW Cup, etc.
* `fetch_ladder()`: Team ladders with detailed home/away splits and points differentials
* `fetch_venues()`: Venue usage and attendance by team and competition
* `fetch_coaches()`: Coaching records across leagues and seasons
* `fetch_player_stats()`: Player match stats (tries, goals, field goals, points) for all valid games between 1998 and present

## General Enhancements

* All functions include `league`, `season`, and `source` arguments
* Slug mapping and URL building are automated internally
* `cli` progress bars and error handling for a clean user experience
* `readr::write_rds()` integration for bulk .rds saving by season
* Fully vectorized `fetch_player_stats()` for batch scraping across years

## Development Practices

* Compliant with CRAN DESCRIPTION and Roxygen2 docs
* Modular design allows easy future expansion
