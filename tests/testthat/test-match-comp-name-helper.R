 # modified because match_comp_name() returns a named vector. added unname()
test_that("match_comp_name returns correct mappings", {
  expect_equal(unname(match_comp_name(111)), "NRL")
  expect_equal(unname(match_comp_name(116)), "SOO")
  expect_equal(unname(match_comp_name(999)), "Unknown")
})

match_comp_name(999)
