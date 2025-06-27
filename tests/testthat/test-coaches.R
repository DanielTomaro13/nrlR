test_that("fetch_coaches returns coach data for NRL 2022", {
  coaches <- fetch_coaches(season = 2022, league = "nrl", source = "rugbyleagueproject")

  expect_s3_class(coaches, "data.frame")
  expect_true(all(c("coach", "team", "games", "wins", "losses", "season", "league") %in% names(coaches)))
  expect_gt(nrow(coaches), 0)
  expect_true(all(coaches$season == 2022))
})
