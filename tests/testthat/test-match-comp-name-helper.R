test_that("match_comp_name returns correct mappings", {
  expect_equal(match_comp_name(111), "NRL")
  expect_equal(match_comp_name(116), "SOO")
  expect_equal(match_comp_name(999), "999")
})
