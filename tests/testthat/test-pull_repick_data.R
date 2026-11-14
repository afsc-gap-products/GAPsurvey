# Tests for pull_repick_data(); the database is mocked.

skip_if_not_installed("gapindex")

make_repick_df <- function() {
  data.frame(
    HAUL = 1:3, CRUISE = 202601, VESSEL_ID = 176,
    CRUISE_ID = 999, HAUL_ID = 101:103,
    HAUL_TYPE = 3, STATION = c("A-1", "A-2", "A-3"), STRATUM = 10,
    PERFORMANCE = 0, EDIT_GEAR_DEPTH = 100, EDIT_BOTTOM_DEPTH = 120,
    EDIT_DISTANCE_FISHED = 1.5, HAUL_DATE = "06/22/2026",
    EDIT_LATITUDE = 5230.1, EDIT_LONGITUDE = -17530.2,
    ONBOTTOM = "12:00:00", OFFBOTTOM = "12:15:00"
  )
}

test_that("pull_repick_data() writes a CSV without HAUL_ID / CRUISE_ID", {
  testthat::local_mocked_bindings(
    sql_query = function(channel, query) make_repick_df(),
    .package = "gapindex"
  )
  out <- withr::local_tempfile(fileext = ".csv")
  suppressMessages(pull_repick_data(202601, 176, channel = "fake", output_path = out))

  expect_true(file.exists(out))
  written <- utils::read.csv(out)
  expect_equal(nrow(written), 3)
  expect_false(any(c("HAUL_ID", "CRUISE_ID") %in% names(written)))
  expect_true(all(c("HAUL", "STATION", "ONBOTTOM", "OFFBOTTOM") %in% names(written)))
})

test_that("pull_repick_data() returns the data frame invisibly", {
  testthat::local_mocked_bindings(
    sql_query = function(channel, query) make_repick_df(),
    .package = "gapindex"
  )
  out <- withr::local_tempfile(fileext = ".csv")
  expect_invisible(
    suppressMessages(pull_repick_data(202601, 176, channel = "fake", output_path = out))
  )
  res <- suppressMessages(
    pull_repick_data(202601, 176, channel = "fake", output_path = out)
  )
  expect_s3_class(res, "data.frame")
  expect_false("HAUL_ID" %in% names(res))
})

test_that("pull_repick_data() reports where it wrote the file", {
  testthat::local_mocked_bindings(
    sql_query = function(channel, query) make_repick_df(),
    .package = "gapindex"
  )
  out <- withr::local_tempfile(fileext = ".csv")
  expect_message(
    pull_repick_data(202601, 176, channel = "fake", output_path = out),
    "Successfully wrote haul file"
  )
})

test_that("pull_repick_data() puts vessel and cruise in the right SQL clauses", {
  captured <- NULL
  testthat::local_mocked_bindings(
    sql_query = function(channel, query) { captured <<- query; make_repick_df() },
    .package = "gapindex"
  )
  out <- withr::local_tempfile(fileext = ".csv")
  suppressMessages(pull_repick_data(cruise = "202601", vessel = "176",
                                    channel = "fake", output_path = out))
  expect_match(captured, "c.vessel_id = 176", fixed = TRUE)
  expect_match(captured, "c.cruise = 202601", fixed = TRUE)
})

test_that("pull_repick_data() errors and writes nothing when no hauls match", {
  testthat::local_mocked_bindings(
    sql_query = function(channel, query) make_repick_df()[0, ],
    .package = "gapindex"
  )
  out <- withr::local_tempfile(fileext = ".csv")
  expect_error(
    pull_repick_data(202601, 176, channel = "fake", output_path = out),
    "No matching haul found for Cruise: 202601, Vessel: 176"
  )
  expect_false(file.exists(out))
})

test_that("pull_repick_data() only connects when channel is NULL", {
  n_connect <- 0
  testthat::local_mocked_bindings(
    get_connected = function(...) { n_connect <<- n_connect + 1; "fake" },
    sql_query     = function(channel, query) make_repick_df(),
    .package = "gapindex"
  )
  out <- withr::local_tempfile(fileext = ".csv")
  suppressMessages(pull_repick_data(202601, 176, channel = "given", output_path = out))
  expect_equal(n_connect, 0)
  suppressMessages(pull_repick_data(202601, 176, output_path = out))
  expect_equal(n_connect, 1)
})
