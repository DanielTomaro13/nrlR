test_that("fetch_lineups works for explicit URL", {
  skip_on_cran()
  skip_if_offline()

  url <- "https://www.nrl.com/news/2024/05/07/nrl-team-lists-round-10/"
  lineups <- fetch_lineups(url = url)

  expect_s3_class(lineups, "tbl_df")
  expect_true(all(c("game", "first_name", "last_name", "team", "role") %in% names(lineups)))
  expect_true(nrow(lineups) > 0)
})
