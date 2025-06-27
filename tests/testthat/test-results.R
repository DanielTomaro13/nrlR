test_that("fetch_results returns valid data for NRL 2024", {
  res <- fetch_results(season = 2024, league = "nrl")
  expect_s3_class(res, "data.frame")
  expect_true(all(c("home_team", "away_team", "home_score", "away_score") %in% names(res)))
  expect_gt(nrow(res), 0)
  expect_true(all(res$season == 2024))
})
