test_that("fetch_injuries_suspensions returns a valid tibble from zerotackle", {
  result <- fetch_injuries_suspensions(source = "zerotackle")

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("team", "player", "reason", "expected_return") %in% colnames(result)))
})

test_that("fetch_injuries_suspensions fails with an invalid source", {
  expect_error(
    fetch_injuries_suspensions(source = "invalidsource"),
    regexp = "Invalid source"
  )
})
