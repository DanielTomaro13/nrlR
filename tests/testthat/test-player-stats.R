test_that("fetch_team_stats_championdata returns team stats for NRL 2025 rounds 1 and 2", {
  stats_round_1 <- fetch_team_stats_championdata(comp = 12445, round = 1)  # Example comp ID
  stats_round_2 <- fetch_team_stats_championdata(comp = 12445, round = 2)
  stats <- dplyr::bind_rows(stats_round_1, stats_round_2)

  skip_if(nrow(stats) == 0, "No data returned for Champion Data NRL 2025 rounds 1 and 2")

  expect_true(is.data.frame(stats))

  # Check for essential columns
  expect_true(all(c("match_id", "competition_id", "squadId",
                    "team_name", "team_location", "total_score", "round") %in% names(stats)))

  # Validate round values
  expect_true(all(stats$round %in% c(1, 2)))

  # Ensure scores are numeric (when present)
  expect_true(is.numeric(stats$total_score) || all(is.na(stats$total_score)))
})
