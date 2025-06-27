test_that("fetch_ladder returns valid ladder for Super League 2023", {
  ladder <- fetch_ladder(season = 2023, league = "super_league")

  expect_s3_class(ladder, "data.frame")
  expect_true(all(c("team", "points", "position") %in% names(ladder)))
  expect_gt(nrow(ladder), 0)
  expect_true(all(ladder$season == 2023))
  expect_true(all(ladder$league == "super_league"))
})
