# Tests for check_haul_depths(), check_haul_abundance(), find_duplicate_timestamps()
# No Oracle connection is needed: gapindex::sql_query() / get_connected() are mocked.

skip_if_not_installed("gapindex")

# ---- helpers ----------------------------------------------------------------

# Real Oracle results come back with UPPERCASE names, so fixtures do too.
make_depth_df <- function() {
  data.frame(
    HAUL_ID      = 1:5,
    CRUISE       = 202601,
    REGION       = "AI",
    VESSEL_ID    = c(176, 176, 176, 148, 148),
    HAUL         = c(1, 2, 3, 5, 4),
    STATION      = paste0("S", 1:5),
    PERFORMANCE  = 0,
    GEAR_DEPTH   = c(100, 0, 50, 80, 200),
    BOTTOM_DEPTH = c(150, 100, 0, 80, 150)
  )
}

# NOTE: column order matters for check_haul_abundance(); the function selects
# `haul:duration`, so the haul columns must stay in this order.
make_haul_df <- function() {
  data.frame(
    HAUL_ID     = 1:10,
    CRUISE      = 202601,
    REGION      = "AI",
    VESSEL_ID   = 176,
    HAUL        = 1:10,
    STATION     = c("A-1", "A-2", "A-3", "A-4", "A-5", "A-6", "B-2", "B-2", "A-9", "A-10"),
    STRATUM     = 10,
    HAUL_TYPE   = c(3, 3, 3, 4, 3, 3, 3, 3, 3, 3),
    PERFORMANCE = c(0, 0, 0, 0, -1, 0, 0, 0, 0, 0),
    ACCESSORIES = c(15, 99, 15, 15, 15, 15, 15, 15, 130, 15),
    GEAR        = c(44, 44, 99, 44, 44, 44, 44, 172, 44, 44),
    DURATION    = c(15, 15, 15, 15, 15, 5, 15, 15, 5, 10)
  )
}

make_catch_df <- function() {
  data.frame(
    HAUL_ID      = c(1, 2, 3, 4, 5, 7, 8, 9, 10),   # haul 6 deliberately has no catch
    SPECIES_CODE = 21740,
    TOTAL_WEIGHT = c(50000, 50000, 50000, 50000, 50000, 50000, 50000, 10000, 0)
  )
}

# ---- check_haul_depths() ----------------------------------------------------

test_that("check_haul_depths() rejects an invalid region before touching the DB", {
  expect_error(check_haul_depths(202601, "XYZ"), "Invalid region 'XYZ'")
})

test_that("check_haul_depths() flags zero, equal, and inverted depths only", {
  testthat::local_mocked_bindings(
    sql_query = function(channel, query) make_depth_df(),
    .package = "gapindex"
  )
  res <- check_haul_depths(202601, "AI", channel = "fake")

  expect_s3_class(res, "data.frame")
  # haul 1 (gear 100 < bottom 150) is fine; the other four are flagged
  expect_setequal(res$haul_id, c(2, 3, 4, 5))
  expect_named(
    res,
    c("haul_id", "cruise", "region", "vessel_id", "haul", "station",
      "performance", "gear_depth", "bottom_depth")
  )
})

test_that("check_haul_depths() sorts by region, cruise, vessel, haul", {
  testthat::local_mocked_bindings(
    sql_query = function(channel, query) make_depth_df(),
    .package = "gapindex"
  )
  res <- check_haul_depths(202601, "AI", channel = "fake")
  expect_equal(res$vessel_id, c(148, 148, 176, 176))
  expect_equal(res$haul,      c(4, 5, 2, 3))
})

test_that("check_haul_depths() flags missing (NA) depths, as documented", {
  # The roxygen says missing depths are flagged, but dplyr::filter() silently
  # drops NA comparisons. .
  df <- make_depth_df()
  df$GEAR_DEPTH[1] <- NA
  testthat::local_mocked_bindings(
    sql_query = function(channel, query) df,
    .package = "gapindex"
  )
  res <- check_haul_depths(202601, "AI", channel = "fake")
  expect_true(1 %in% res$haul_id)
})

test_that("check_haul_depths() is case-insensitive on region and builds the right SQL", {
  captured <- NULL
  testthat::local_mocked_bindings(
    sql_query = function(channel, query) { captured <<- query; make_depth_df() },
    .package = "gapindex"
  )
  check_haul_depths(202601, "goa", channel = "fake")
  expect_match(captured, "CRUISE = 202601", fixed = TRUE)
  expect_match(captured, "IN (39,47)", fixed = TRUE)

  check_haul_depths(202601, "ebs", channel = "fake")
  expect_match(captured, "IN (98)", fixed = TRUE)
})

test_that("check_haul_depths() errors when the query returns nothing", {
  testthat::local_mocked_bindings(
    sql_query = function(channel, query) make_depth_df()[0, ],
    .package = "gapindex"
  )
  expect_error(check_haul_depths(202601, "AI", channel = "fake"),
               "No records found for Cruise: 202601")
})

test_that("check_haul_depths() only connects when channel is NULL", {
  n_connect <- 0
  testthat::local_mocked_bindings(
    get_connected = function(...) { n_connect <<- n_connect + 1; "fake" },
    sql_query     = function(channel, query) make_depth_df(),
    .package = "gapindex"
  )
  check_haul_depths(202601, "AI", channel = "supplied")
  expect_equal(n_connect, 0)
  check_haul_depths(202601, "AI")
  expect_equal(n_connect, 1)
})

# ---- check_haul_abundance() -------------------------------------------------

mock_abundance_db <- function(hauls = make_haul_df(), catch = make_catch_df(),
                              env = parent.frame()) {
  testthat::local_mocked_bindings(
    sql_query = function(channel, query) {
      if (grepl("EDIT_CATCH_SPECIES", query, fixed = TRUE)) catch else hauls
    },
    .package = "gapindex",
    .env = env
  )
}

issue_for <- function(res, haul) res$issue[res$haul == haul]

test_that("check_haul_abundance() rejects an invalid region", {
  expect_error(check_haul_abundance(202601, "NOPE"), "Invalid region 'NOPE'")
})

test_that("check_haul_abundance() assigns each issue label", {
  mock_abundance_db()
  res <- check_haul_abundance(202601, "AI", channel = "fake")

  expect_equal(issue_for(res, 2), "improper accessories")
  expect_equal(issue_for(res, 3), "improper gear")
  expect_equal(issue_for(res, 4), "haul type not 3")
  expect_equal(issue_for(res, 5), "performance inadequate")
  expect_equal(issue_for(res, 6), "catch too small for tow duration")
  expect_equal(issue_for(res, 7), "station duplicated")
})

test_that("check_haul_abundance() does not flag clean hauls or boundary cases", {
  mock_abundance_db()
  res <- check_haul_abundance(202601, "AI", channel = "fake")

  expect_false(1  %in% res$haul)  # all good
  expect_false(9  %in% res$haul)  # 5 min tow but 10 t catch >= duration; accessories 130 allowed
  expect_false(10 %in% res$haul)  # duration == 10 is not "< 10", even with 0 catch
})

test_that("check_haul_abundance() treats hauls with no catch rows as 0 t", {
  mock_abundance_db()
  res <- check_haul_abundance(202601, "AI", channel = "fake")
  expect_equal(res$haul_weight_t[res$haul == 6], 0)
})

test_that("check_haul_abundance() sums catch and converts to tonnes", {
  hauls <- make_haul_df()[6, ]
  hauls$DURATION <- 5
  catch <- data.frame(HAUL_ID = 6, SPECIES_CODE = c(1, 2, 3),
                      TOTAL_WEIGHT = c(1000, 2000, 500))   # 3.5 t total
  mock_abundance_db(hauls, catch)
  res <- check_haul_abundance(202601, "AI", channel = "fake")
  expect_equal(res$haul_weight_t, 3.5)
  expect_equal(res$issue, "catch too small for tow duration")
})

test_that("check_haul_abundance() works when the catch query is empty", {
  hauls <- make_haul_df()
  mock_abundance_db(hauls, catch = make_catch_df()[0, ])
  res <- check_haul_abundance(202601, "AI", channel = "fake")
  expect_true(all(res$haul_weight_t == 0))
})

test_that("check_haul_abundance() returns zero rows when everything is fine", {
  hauls <- make_haul_df()[1, ]
  mock_abundance_db(hauls, make_catch_df())
  res <- check_haul_abundance(202601, "AI", channel = "fake")
  expect_equal(nrow(res), 0)
})

test_that("check_haul_abundance() output is sorted and has the expected columns", {
  mock_abundance_db()
  res <- check_haul_abundance(202601, "AI", channel = "fake")
  expect_false(is.unsorted(res$haul))
  expect_true(all(c("cruise", "region", "vessel_id", "haul", "station", "stratum",
                    "haul_weight_t", "issue") %in% names(res)))
  expect_false("n_station" %in% names(res))
  expect_false(any(res$issue == "none"))
})

test_that("check_haul_abundance() errors when no hauls are found", {
  mock_abundance_db(hauls = make_haul_df()[0, ])
  expect_error(check_haul_abundance(202601, "AI", channel = "fake"),
               "No haul records found")
})

# ---- find_duplicate_timestamps() -------------------------------------------

test_that("find_duplicate_timestamps() builds the query from its arguments", {
  captured <- NULL
  testthat::local_mocked_bindings(
    sql_query = function(channel, query) { captured <<- query; data.frame(X = 1) },
    .package = "gapindex"
  )
  find_duplicate_timestamps(cruise = 202601, vessel_id = 176, haul = 121, channel = "fake")
  expect_match(captured, "vessel_id = 176", fixed = TRUE)
  expect_match(captured, "cruise = 202601", fixed = TRUE)
  expect_match(captured, "a.haul = 121", fixed = TRUE)
})

test_that("find_duplicate_timestamps() returns the query result", {
  dupes <- data.frame(POSITION_ID = 1:2, RECORD_COUNT = 2)
  testthat::local_mocked_bindings(
    sql_query = function(channel, query) dupes,
    .package = "gapindex"
  )
  expect_equal(find_duplicate_timestamps(202601, 176, 121, channel = "fake"), dupes)
})

test_that("find_duplicate_timestamps() messages (not errors) when there are none", {
  testthat::local_mocked_bindings(
    sql_query = function(channel, query) data.frame(),
    .package = "gapindex"
  )
  expect_message(
    res <- find_duplicate_timestamps(202601, 176, 121, channel = "fake"),
    "No duplicate timestamps found"
  )
  expect_equal(nrow(res), 0)
})
