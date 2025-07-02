test_that("fetch_cd_comps returns a non-empty tibble with expected columns", {
  comps <- fetch_cd_comps()

  expect_true(tibble::is_tibble(comps))
  expect_gt(nrow(comps), 0)

  # Check some expected columns exist
  expected_cols <- c("id", "name", "season", "rounds")
  expect_true(all(expected_cols %in% colnames(comps)))
})
