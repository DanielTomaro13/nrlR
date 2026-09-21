# nrlR 📊🏉

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/nrlR)](https://CRAN.R-project.org/package=nrlR)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/nrlR)](https://CRAN.R-project.org/package=nrlR)
[![Docs](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://danieltomaro13.github.io/nrlR/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE.md)
<!-- badges: end -->

*An R package to scrape, clean and analyse publicly available rugby league data. Available on CRAN.*

📖 **Full function reference: [danieltomaro13.github.io/nrlR](https://danieltomaro13.github.io/nrlR/)**

---

## 🚀 Overview

`nrlR` gives R users one consistent set of functions for rugby league data from public sources. It covers:

- 🏆 **NRL and NRLW**
- 🏟️ **State of Origin**
- 🌏 **Super League, Championship, League One, Challenge Cup**
- 🏉 **NSW Cup, QLD Cup, Women's Super League**
- 🔥 **Historical seasons back to 1998**

With `nrlR` you can pull:

✅ Match fixtures and results, including referees, venues and attendance  
✅ Ladders with home, away and total splits  
✅ Player and team statistics from Champion Data, with 60+ metrics per player per match  
✅ Team lists, injuries and suspensions  
✅ Venues and coaching records

Every function returns a tidy tibble, ready for `dplyr`, `ggplot2` or a model.

---

## 💿 Installation

From CRAN:

```r
install.packages("nrlR")
```

Or the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("DanielTomaro13/nrlR")
```

---

## 🔥 Quick examples

All output below is real, captured from the package.

```r
library(nrlR)
library(dplyr)
```

### 📊 The 2025 NRL ladder

```r
fetch_ladder(season = 2025, league = "nrl") |>
  select(position, team, total_p, total_w, total_l, total_f, total_a, pd, points)
#> # A tibble: 17 x 9
#>   position team       total_p total_w total_l total_f total_a    pd points
#>      <dbl> <chr>        <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <dbl>  <dbl>
#> 1        1 Canberra        24      19       5     654     506   148     44
#> 2        2 Melbourne       24      17       7     671     459   212     40
#> 3        3 Canterbury      24      16       8     534     414   120     38
#> 4        4 Brisbane        24      15       9     680     508   172     36
#> 5        5 Cronulla        24      15       9     599     490   109     36
#> # i 12 more rows
```

The full ladder has 29 columns, including separate home and away records and points per game.

### 🏟️ Fixtures and scores for a round

```r
fetch_fixture(season = 2025, round_number = 5)
#> # A tibble: 8 x 11
#>   comp  round   venue            city  home_team home_score away_team away_score kickoff_utc
#>   <chr> <chr>   <chr>            <chr> <chr>          <int> <chr>          <int> <dttm>
#> 1 NRL   Round 5 GIO Stadium      Canb~ Raiders           24 Sharks            20 2025-04-03 09:00:00
#> 2 NRL   Round 5 CommBank Stadium Sydn~ Panthers          18 Cowboys           22 2025-04-04 07:00:00
#> 3 NRL   Round 5 Accor Stadium    Sydn~ Rabbitohs         20 Roosters          14 2025-04-04 09:00:00
#> 4 NRL   Round 5 CommBank Stadium Sydn~ Eels              23 Dragons           22 2025-04-05 04:00:00
#> 5 NRL   Round 5 Cbus Super Stad~ Gold~ Titans            10 Dolphins          36 2025-04-05 06:30:00
#> # i 3 more rows
#> # i 2 more variables: kickoff_local <dttm>, match_link <chr>
```

### 📅 A full season of results

```r
fetch_results(seasons = 2025, league = "nrl")
#> # A tibble: 213 x 12
#>   season league round date       time       home_team  home_score away_team away_score referee venue
#>    <dbl> <chr>  <dbl> <date>     <chr>      <chr>           <int> <chr>          <int> <chr>   <chr>
#> 1   2025 nrl        1 2025-03-01 Sat 4:00pm Canberra           30 Warriors           8 G. Atk~ Alle~
#> 2   2025 nrl        1 2025-03-01 Sat 8:30pm Penrith            28 Cronulla          22 A. Kle~ Alle~
#> 3   2025 nrl        1 2025-03-06 Thu 8:00pm Sydney             14 Brisbane          50 G. Sut~ Alli~
#> # i 210 more rows
#> # i 1 more variable: attendance <int>
```

Pass a vector such as `seasons = 1998:2025` to build a historical dataset in one call.

### 📈 Champion Data player and team stats

```r
# 1. Find the competition ID
fetch_cd_comps() |>
  filter(season == 2025, grepl("NRL|Origin", name)) |>
  select(id, name, rounds, season)
#> # A tibble: 6 x 4
#>      id name                         rounds season
#>   <int> <chr>                         <int>  <int>
#> 1 12755 2025 Telstra NRL Premiership     27   2025
#> 2 12757 2025 NRLW                        11   2025
#> 3 12735 2025 State of Origin              1   2025
#> 4 12736 2025 State of Origin Womens       1   2025
#> 5 12756 2025 Telstra NRL Finals           4   2025
#> # i 1 more row

# 2. Pull every player's stats for a round
stats <- fetch_player_stats_championdata(comp = 12755, round = 4)
dim(stats)
#> [1] 288  63
```

The 63 columns include tries, try assists, line breaks, tackle breaks, metres gained, post-contact metres, tackles, missed tackles, offloads, kick metres, errors and penalties conceded. `fetch_team_stats_championdata()` returns the same metrics at team level.

---

## 🛠 Function reference

| Function | What it returns |
|---|---|
| `fetch_fixture()` | Fixtures and scores for a round from NRL.com |
| `fetch_results()` | Match results for one or more seasons, across 10 competitions |
| `fetch_ladder()` | Ladder with home, away and total splits |
| `fetch_ladder_nrl()` | Official NRL.com ladder for a season and round |
| `fetch_player_stats()` | Player stats, with a choice of source |
| `fetch_player_stats_championdata()` | Champion Data player stats for a competition and round |
| `fetch_team_stats_championdata()` | Champion Data team stats for a competition and round |
| `fetch_cd_comps()` | Every Champion Data competition ID, 2009 onward |
| `fetch_lineups()` | Team lists from an NRL.com team-list article |
| `fetch_injuries_suspensions()` | Current injuries and suspensions |
| `fetch_venues()` | Venue usage and attendance |
| `fetch_coaches()` | Coaching records by league and season |

Full argument documentation is on the [pkgdown site](https://danieltomaro13.github.io/nrlR/reference/) or via `?fetch_ladder` in R.

---

## 📊 Data sources and coverage

- **NRL.com** for fixtures, ladders and team lists
- **Rugby League Project** for historical results, ladders, venues and coaches
- **Champion Data** match-centre feeds for advanced player and team statistics
- **Zero Tackle** for injuries and suspensions

| `league =` | Competition |
|---|---|
| `"nrl"` | NRL Premiership |
| `"state_of_origin"` | State of Origin |
| `"nsw_cup"`, `"qld_cup"` | NSW Cup, QLD Cup |
| `"super_league"`, `"championship"`, `"league_one"` | UK competitions |
| `"womens_super_league"` | Women's Super League |
| `"challenge_cup"`, `"1895_cup"` | UK knockout cups |

---

## 🚨 Ethical usage

Please use `nrlR` responsibly:

- ✅ Respect each site's terms of service
- ✅ Cache what you download rather than re-scraping it
- ✅ Attribute data sources in your analysis
- ❌ Don't hammer servers with rapid repeated requests
- ❌ Don't redistribute scraped data commercially without permission

---

## 🆘 Getting help

- 📖 **Documentation**: [danieltomaro13.github.io/nrlR](https://danieltomaro13.github.io/nrlR/) or `help(package = "nrlR")`
- 💬 **Bugs and requests**: [GitHub Issues](https://github.com/DanielTomaro13/nrlR/issues)

---

## 📄 Citation

```r
citation("nrlR")
```

```bibtex
@Manual{nrlR,
  title = {nrlR: Functions to Scrape Rugby Data},
  author = {Daniel Tomaro},
  year = {2025},
  note = {R package version 0.1.2},
  url = {https://CRAN.R-project.org/package=nrlR},
}
```

---

## 📝 License

MIT © [Daniel Tomaro](https://github.com/DanielTomaro13)

This package is not affiliated with the NRL, Rugby League Project, Champion Data, or any official rugby league organisation. All data is sourced from publicly available information.
