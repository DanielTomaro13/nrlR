# nrlR News

## Improvements 2/07/2025

Added a fetch_fixtures function which scrapes NRL.com draw for future fixtures and past results

## Improvements 30/06/2025
Added a fetch-lineup function which scrapes team lists released by NRL.com on a Tuesday 


## What's Coming - 28/06/2025

Future versions of `nrlR` will introduce:
- Integration with advanced statistics from additional sources (e.g., NRL.com)
- More granular player-level metrics (e.g., tackles, run meters, errors, line breaks)
- Improved team and player ID resolution across seasons and competitions

---

## nrlR 0.1.0

Initial release with support for scraping historical Rugby League data from [rugbyleagueproject.org].

### 🔍 Core Functionality
- `fetch_results()`: Match-level results across NRL, Super League, QLD Cup, NSW Cup, etc.
- `fetch_ladder()`: Team ladders with detailed home/away splits and points differentials
- `fetch_venues()`: Venue usage and attendance by team and competition
- `fetch_coaches()`: Coaching records across leagues and seasons
- `fetch_player_stats()`: Player match stats (tries, goals, field goals, points) for all valid games between 1998 and present

### ⚙️ General Enhancements
- All functions include `league`, `season`, and `source` arguments
- Slug mapping and URL building are automated internally
- `cli` progress bars and error handling for a clean user experience
- `readr::write_rds()` integration for bulk .rds saving by season
- Fully vectorized `fetch_player_stats()` for batch scraping across years

### 🛠 Development Practices
- Compliant with CRAN DESCRIPTION and Roxygen2 docs
- Modular design allows easy future expansion

---

Stay tuned as `nrlR` becomes a one-stop shop for Rugby League data pipelines.
