test_that("fetch_ladder returns valid ladder for Super League 2023 (rugbyleagueproject)", {
  ladder <- fetch_ladder(season = 2023, league = "super_league", source = "rugbyleagueproject")

  expect_s3_class(ladder, "data.frame")
  expect_true(all(c("team", "points", "position") %in% names(ladder)))
  expect_gt(nrow(ladder), 0)
  expect_true(all(ladder$season == 2023))
  expect_true(all(ladder$league == "super_league"))
})

test_that("fetch_ladder returns valid ladder for NRL 2025 (nrl.com)", {
  ladder <- fetch_ladder(season = 2025, league = "nrl", source = "nrl")

  expect_s3_class(ladder, "data.frame")
  expect_true(all(c("team", "points", "wins", "losses") %in% names(ladder)))
  expect_gt(nrow(ladder), 0)
  expect_true(all(ladder$comp == "NRL"))
})
