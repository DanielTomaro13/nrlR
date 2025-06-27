test_that("fetch_player_stats returns player stats for NRL 2024", {
  stats <- fetch_player_stats(seasons = 2024, league = "nrl", rounds = 1:2)

  expect_true(is.data.frame(stats))
  expect_true(all(c("player", "team", "points", "round", "season") %in% names(stats)))
  expect_true(all(stats$season == 2024))
})
