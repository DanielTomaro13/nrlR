# Tests updated Feb 2026 for NRL API change:
# - fixtures$clock: list-of-lists → data.frame structure  
# - home/away_score: double → integer type
# - Round 99 returns 1 row (not 0) - NRL placeholder behavior

test_that("fetch_fixture_nrl returns a valid tibble", {
  fix <- fetch_fixture_nrl(2025, 18)
  expect_s3_class(fix, "tbl_df")
  expect_true(all(c("comp", "round", "venue", "city", "home_team", "home_score",
                    "away_team", "away_score", "kickoff_utc", "kickoff_local",
                    "match_link") %in% names(fix)))
  expect_type(fix$home_score, "integer")  
  expect_type(fix$away_score, "integer")
  expect_type(fix$kickoff_utc, "double")
})

test_that("fetch_fixture works through generic router", {
  fix <- fetch_fixture(2025, 18, source = "NRL")
  expect_s3_class(fix, "tbl_df")
  expect_equal(unique(fix$comp), "NRL")
})

test_that("fetch_fixture_nrl handles non-existent data gracefully", {
  fix <- fetch_fixture_nrl(2025, 99)
  expect_s3_class(fix, "tbl_df")
  expect_true(nrow(fix) <= 1)
  expect_equal(ncol(fix), 11)
})

test_that("fetch_fixture_nrl returns valid data for existing rounds", {
  fix <- fetch_fixture_nrl(2025, 18)
  expect_gt(nrow(fix), 0)
  expect_equal(nrow(fix), length(fix$kickoff_utc))
})

test_that("fetch_fixture_nrl validates time parsing", {
  fix <- fetch_fixture_nrl(2025, 18)
  expect_gt(nrow(fix), 0)
  expect_true(all(lubridate::is.instant(fix$kickoff_utc)))
  expect_true(all(lubridate::is.instant(fix$kickoff_local)))
  expect_equal(nrow(fix), length(fix$kickoff_utc))
})
test_that("fetch_fixture_nrl returns a valid tibble", {
  fix <- fetch_fixture_nrl(2025, 18)
  expect_s3_class(fix, "tbl_df")
  expect_true(all(c("comp", "round", "venue", "city", "home_team", "home_score",
                    "away_team", "away_score", "kickoff_utc", "kickoff_local",
                    "match_link") %in% names(fix)))
  expect_type(fix$home_score, "integer")  
  expect_type(fix$away_score, "integer")
  expect_type(fix$kickoff_utc, "double")
})

test_that("fetch_fixture works through generic router", {
  fix <- fetch_fixture(2025, 18, source = "NRL")
  expect_s3_class(fix, "tbl_df")
  expect_equal(unique(fix$comp), "NRL")
})

test_that("fetch_fixture_nrl handles non-existent data gracefully", {
  fix <- fetch_fixture_nrl(2025, 99)
  expect_s3_class(fix, "tbl_df")
  expect_true(nrow(fix) <= 1)
  expect_equal(ncol(fix), 11)
})

test_that("fetch_fixture_nrl returns valid data for existing rounds", {
  fix <- fetch_fixture_nrl(2025, 18)
  expect_gt(nrow(fix), 0)
  expect_equal(nrow(fix), length(fix$kickoff_utc))
})

test_that("fetch_fixture_nrl validates time parsing", {
  fix <- fetch_fixture_nrl(2025, 18)
  expect_gt(nrow(fix), 0)
  expect_true(all(lubridate::is.instant(fix$kickoff_utc)))
  expect_true(all(lubridate::is.instant(fix$kickoff_local)))
  expect_equal(nrow(fix), length(fix$kickoff_utc))
})
