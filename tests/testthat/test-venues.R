test_that("fetch_venues returns venue data for NRL 2023", {
  venues <- fetch_venues(season = 2023, league = "nrl")

  expect_s3_class(venues, "data.frame")
  expect_true(all(c("venue", "games", "avg_attendance", "season", "venue_link") %in% names(venues)))
  expect_gt(nrow(venues), 0)
  expect_true(all(venues$season == 2023))
})
