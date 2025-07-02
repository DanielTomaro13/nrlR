test_that("fetch_fixture_nrl returns a valid tibble", {
  fix <- fetch_fixture_nrl(2025, 18)
  
  expect_s3_class(fix, "tbl_df")
  expect_true(all(c("comp", "round", "venue", "city", "home_team", "home_score", 
                    "away_team", "away_score", "kickoff_utc", "kickoff_local", 
                    "match_link") %in% names(fix)))
  
  expect_type(fix$home_score, "double")
  expect_type(fix$kickoff_utc, "double") # POSIXct is stored as double
})

test_that("fetch_fixture works through generic router", {
  fix <- fetch_fixture(2025, 18, source = "NRL")
  
  expect_s3_class(fix, "tbl_df")
  expect_equal(unique(fix$comp), "NRL")
})

test_that("fetch_fixture_nrl handles non-existent data gracefully", {
  fix <- fetch_fixture_nrl(2025, 99) # unlikely round
  expect_s3_class(fix, "tbl_df")
  expect_equal(nrow(fix), 0)
})
