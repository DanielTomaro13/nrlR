test_that("fetch_player_stats returns player stats for Rugby League Project NRL 2024 rounds 1 and 2", {
  stats_round_1 <- fetch_player_stats(season = 2024, league = "nrl", round = 1, source = "rugbyproject")
  stats_round_2 <- fetch_player_stats(season = 2024, league = "nrl", round = 2, source = "rugbyproject")
  stats <- dplyr::bind_rows(stats_round_1, stats_round_2)

  skip_if(nrow(stats) == 0, "No data returned for rugbyproject NRL 2024 rounds 1 and 2")

  expect_true(is.data.frame(stats))
  expect_true(all(c("player", "team", "points", "round", "season") %in% names(stats)))
  expect_true(all(stats$season == 2024))
  expect_true(all(stats$round %in% c(1, 2)))
})

test_that("fetch_player_stats returns player stats for NRL 2025 rounds 1 and 2", {
  stats_round_1 <- fetch_player_stats(comp = 111, round = 1, source = "nrl")
  stats_round_2 <- fetch_player_stats(comp = 111, round = 2, source = "nrl")
  stats <- dplyr::bind_rows(stats_round_1, stats_round_2)

  skip_if(nrow(stats) == 0, "No data returned for NRL 2025 rounds 1 and 2")

  expect_true(is.data.frame(stats))
  expect_true(all(c("match_id", "competition_id", "round", "home_team", "away_team") %in% names(stats)))
  expect_true(all(stats$round %in% c(1, 2)))
})
