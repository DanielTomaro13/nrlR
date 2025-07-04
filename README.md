# nrlR 📊🏉
_A powerful R package to scrape, clean, and analyze publicly available NRL and Rugby League data_

---

## 🚀 Overview

`nrlR` provides a streamlined toolkit for R users to scrape, clean, and analyze rugby league data from public sources. It covers:

- 🏆 **NRL & NRLW**
- 🏟️ **State of Origin**
- 🌏 **Super League, Championship, League One**
- 🏉 **NSW Cup, QLD Cup, Women’s Super League**
- 🔥 Historical seasons back to 1998

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

## 🔥 Quick examples

```r
library(nrlR)
```

---

### 📊 Get the NRL ladder for 2025

```r
ladder <- fetch_ladder(season = 2025, league = "nrl", source = "nrl")
head(ladder)
```

---

### 🏟️ Fetch fixtures for NRL Round 5, 2025

```r
fixtures <- fetch_fixture(season = 2025, round_number = 5)
print(fixtures)
```

---

### 🏉 Fetch player stats from Rugby League Project

```r
# Fetch player stats for NRL season 2024, rounds 1-3
player_stats <- fetch_player_stats(season = 2024, league = "nrl", round = 1:3, source = "rugbyproject")
head(player_stats)
```

---

### 📈 Fetch Champion Data team stats

```r
# Get the available competitions first
comps <- fetch_cd_competitions()
print(comps)

# Then fetch team stats, e.g. comp ID 12755
cd_team_stats <- fetch_team_stats_championdata(comp = 12755, round = 4)
head(cd_team_stats)
```

---

### 👟 Fetch Champion Data player stats

```r
# For competition ID 12755 (2025 NRL Telstra Premiership)
cd_player_stats <- fetch_player_stats(comp = 111, round = 4, source = "championdata")
head(cd_player_stats)
```

---

## 🛠 Features

✅ Clean, tidy tibbles ready for `dplyr` or `data.table` workflows  
✅ Robust scraping with smart handling of missing or partial data  
✅ Historical & current season support (1998+)  
✅ Flexible — works with multiple data sources 

---

## 🤝 Contributing

PRs are welcome!  
If you want to improve scrapers, add new competitions, or help write tests — open an issue or a pull request.

---

## 📝 License

MIT © Daniel Tomaro

---

📢 **Build your next footy model, dashboard, or data viz with `nrlR`.**  
Happy coding!
