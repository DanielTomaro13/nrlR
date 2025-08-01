# nrlR 📊🏉

*A powerful R package to scrape, clean, and analyze publicly available Rugby data*

---

## 🚀 Overview

`nrlR` provides a streamlined toolkit for R users to scrape, clean, and analyze rugby data from public sources specifically rugby league. It covers:

- 🏆 **NRL & NRLW**
- 🏟️ **State of Origin**
- 🌏 **Super League, Championship, League One**
- 🏉 **NSW Cup, QLD Cup, Women's Super League**
- 🔥 **Historical seasons back to 1998**

With `nrlR`, you can easily pull:

✅ Match fixtures and results  
✅ Ladder standings  
✅ Player statistics (runs, tries, tackles, points)  
✅ Team statistics (totals, differentials)  
✅ Venues, crowds & more

Use it to build dashboards, run predictive models, or simply explore your favourite teams.

---

## 💿 Installation

Install the development version directly from GitHub using:

```r
# install.packages("devtools")
devtools::install_github("DanielTomaro13/nrlR")
```

---

## 🔥 Quick Examples

```r
library(nrlR)
```

### 📊 Get the NRL ladder for 2025

```r
ladder <- fetch_ladder(season = 2025, league = "nrl", source = "nrl")
head(ladder)
#> # A tibble: 6 × 12
#>   position team        played  wins draws losses points_for points_against
#>      <int> <chr>        <int> <int> <int>  <int>      <int>          <int>
#> 1        1 Storm           8     7     0      1        168             98
#> 2        2 Panthers        8     6     1      1        152            106
#> 3        3 Roosters        8     6     0      2        144            112
#> 4        4 Sharks          8     5     1      2        138            118
#> 5        5 Cowboys         8     5     0      3        142            124
#> 6        6 Eels            8     5     0      3        126            128
```

### 🏟️ Fetch fixtures for NRL Round 5, 2025

```r
fixtures <- fetch_fixture(season = 2025, round_number = 5)
print(fixtures)
#> # A tibble: 8 × 10
#>   match_id date       round home_team    away_team    venue      crowd result
#>      <int> <date>     <int> <chr>        <chr>        <chr>      <int> <chr> 
#> 1   401234 2025-04-03     5 Storm        Panthers     AAMI Park  18500 19-16
#> 2   401235 2025-04-04     5 Roosters     Sharks       Allianz    35241 24-10
#> 3   401236 2025-04-05     5 Cowboys      Eels         QCB        22000 28-12
#>   # ... with 5 more rows
```

### 📈 Fetch Champion Data team stats

```r
# Get the available competitions first
comps <- fetch_cd_competitions()
print(comps)
#> # A tibble: 15 × 4
#>   comp_id competition_name             season year 
#>     <int> <chr>                         <int> <int>
#> 1   12755 NRL Telstra Premiership       2025  2025
#> 2   12754 NRLW Premiership              2025  2025
#> 3   12753 State of Origin               2025  2025
#>   # ... with 12 more rows

# Then fetch team stats, e.g. comp ID 12755
cd_team_stats <- fetch_team_stats_championdata(comp = 12755, round = 4)
head(cd_team_stats)
#> # A tibble: 6 × 20
#>   team          round tries line_breaks tackle_breaks metres_gained
#>   <chr>         <int> <int>       <int>         <int>         <int>
#> 1 Melbourne Storm   4     4          12            18          1450
#> 2 Penrith Panthers  4     3          10            15          1380
#> 3 Sydney Roosters   4     5          14            20          1520
#>   # ... with 3 more rows and 15 more variables
```

### 👟 Fetch Champion Data player stats

```r
# For competition ID 12755 (2025 NRL Telstra Premiership)
cd_player_stats <- fetch_player_stats(
  comp = 111, 
  round = 4, 
  source = "championdata"
)
head(cd_player_stats)
#> # A tibble: 6 × 18
#>   player_name    team     position round tries assists line_breaks
#>   <chr>          <chr>    <chr>    <int> <int>   <int>       <int>
#> 1 Nathan Cleary  Panthers Halfback     4     0       3           2
#> 2 Jahrome Hughes Storm    Halfback     4     1       2           4
#> 3 James Tedesco  Roosters Fullback     4     2       1           6
#>   # ... with 3 more rows and 11 more variables
```

---

## 📊 Data Sources

`nrlR` aggregates data from multiple reliable sources:

### Primary Sources
- **NRL.com** - Official fixtures, results, ladders
- **Rugby League Project** - Comprehensive historical statistics
- **Champion Data** - Advanced statistics and analytics
- **Super League** - UK competitions data

### Supported Competitions

| Competition | Years Available | Source Options |
|-------------|----------------|----------------|
| **NRL** | 1998-2025 | nrl, rugbyproject, championdata |
| **NRLW** | 2018-2025 | nrl, rugbyproject |
| **State of Origin** | 1998-2025 | nrl, rugbyproject |
| **NSW Cup** | 2008-2025 | nrl |
| **QLD Cup** | 2008-2025 | nrl |
| **Super League** | 2002-2025 | superleague |
| **Championship** | 2009-2025 | superleague |
| **League One** | 2009-2025 | superleague |

---

## 🛠 Core Functions

### Fixtures & Results
```r
# Get fixtures for a specific round
fetch_fixture(season = 2025, round_number = 5, league = "nrl")

# Get all results for a season
fetch_results(season = 2024, league = "nrl", source = "nrl")

# Get specific match details
fetch_match_details(match_id = 401234, source = "nrl")
```

### Ladder & Standings
```r
# Current season ladder
fetch_ladder(season = 2025, league = "nrl")

# Historical ladder positions
fetch_historical_ladder(season = 2020, round = 10, league = "nrl")
```

### Player Statistics
```r
# Individual player stats
fetch_player_stats(
  season = 2024, 
  league = "nrl", 
  round = 1:27,
  player_name = "Nathan Cleary"
)

# Top try scorers
fetch_player_leaders(
  season = 2024, 
  stat_type = "tries", 
  league = "nrl"
)
```

### Team Statistics
```r
# Team performance metrics
fetch_team_stats(season = 2024, league = "nrl", source = "rugbyproject")

# Head-to-head records
fetch_h2h_record(team1 = "Storm", team2 = "Panthers", years = 2020:2024)
```

### Advanced Analytics
```r
# Champion Data advanced stats
fetch_cd_player_advanced(comp = 12755, round = 4)

# Team efficiency metrics
fetch_team_efficiency(season = 2024, league = "nrl")
```

---

## ⚙️ Configuration

### Rate Limiting
```r
# Set request delays to be respectful to data sources
set_nrl_config(delay_seconds = 1, max_retries = 3)
```

### Caching
```r
# Enable caching to speed up repeated requests
enable_caching(cache_dir = "~/.nrlR_cache", expire_hours = 24)
```

### Data Quality
```r
# Check data completeness
check_data_completeness(season = 2024, league = "nrl")

# Validate scraped data
validate_nrl_data(data = my_nrl_data)
```

---

## 🧪 Testing & Reliability

`nrlR` includes comprehensive testing to ensure data quality:

```r
# Run package tests
devtools::test()

# Check specific scraper functionality
test_scraper_health("nrl")
test_scraper_health("rugbyproject")
test_scraper_health("championdata")
```

### Error Handling
The package gracefully handles common issues:
- Missing data (returns NA with warnings)
- Network timeouts (automatic retries)
- Structural changes to source websites (informative error messages)
- Invalid inputs (parameter validation)

---

## 🤝 Contributing

We welcome contributions! Here's how you can help:

### 🐛 Bug Reports
Found an issue? Please [create an issue](https://github.com/DanielTomaro13/nrlR/issues) with:
- Reproducible example
- Expected vs actual behavior
- Your R session info (`sessionInfo()`)

### 💡 Feature Requests
Want a new feature? Open an issue describing:
- The use case
- Expected functionality
- Proposed implementation (if any)

### 🔧 Development
1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes with tests
4. Run `devtools::check()` to ensure quality
5. Submit a pull request

### 📊 New Data Sources
Adding a new competition or data source:
1. Create scraper functions following existing patterns
2. Add comprehensive tests
3. Update documentation
4. Ensure data format consistency

---

## 🚨 Ethical Usage

Please use `nrlR` responsibly:

- ✅ **Respect robots.txt** and website terms of service
- ✅ **Implement delays** between requests (default: 1 second)
- ✅ **Cache data** to minimize repeated requests
- ✅ **Attribute data sources** in your analysis
- ❌ **Don't overwhelm** servers with rapid requests
- ❌ **Don't redistribute** scraped data commercially without permission

---

## 🆘 Getting Help

- 📖 **Documentation**: `?function_name` or `help(package = "nrlR")`
- 💬 **Issues**: [GitHub Issues](https://github.com/DanielTomaro13/nrlR/issues)
- 📧 **Email**: daniel.tomaro@icloud.com

---

## 📄 Citation

If you use `nrlR` in academic research, please cite:

```
Tomaro, D. (2025). nrlR: An R package for rugby league data analysis. 
R package version 0.1.1. https://github.com/DanielTomaro13/nrlR
```

BibTeX:
```bibtex
@Manual{nrlR,
  title = {nrlR: An R package for rugby league data analysis},
  author = {Daniel Tomaro},
  year = {2025},
  note = {R package version 0.1.1},
  url = {https://github.com/DanielTomaro13/nrlR},
}
```

---

## 📝 License

MIT © [Daniel Tomaro](https://github.com/DanielTomaro13)

This package is not affiliated with the NRL, Rugby League Project, Champion Data, or any official rugby league organization. All data is sourced from publicly available information.

---

---

**📢 Build your next footy model, dashboard, or data viz with `nrlR`.**  

**Happy coding! 🏉📊**

---

*Last updated: August 2025*
