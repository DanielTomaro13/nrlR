test_that("fetch_results returns correct structure and types", {
  skip_on_cran()
  skip_if_offline()

  results <- fetch_results(2025, "nrl")

  # Should be tibble / data.frame
  expect_s3_class(results, "tbl_df")

  # Should have expected columns
  expect_true(all(c("season", "league", "round", "date", "time", "home_team",
                    "home_score", "away_team", "away_score", "referee", "venue", "attendance")
                  %in% colnames(results)))

  # Types
  expect_s3_class(results$date, "Date")
  expect_type(results$round, "double")

  # No NA dates
  expect_false(any(is.na(results$date)))

  # Rounds are positive
  expect_true(all(results$round > 0))
})
