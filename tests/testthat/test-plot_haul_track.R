# Tests for plot_haul_track(). The DB and akgfmaps are mocked; the grid is a
# tiny synthetic sf object in lon/lat so no projection data is needed.

skip_if_not_installed("gapindex")
skip_if_not_installed("akgfmaps")
skip_if_not_installed("sf")

# ---- helpers ----------------------------------------------------------------

cell <- function(xmin, ymin = 52) {
  sf::st_polygon(list(rbind(
    c(xmin, ymin), c(xmin + 1, ymin), c(xmin + 1, ymin + 1),
    c(xmin, ymin + 1), c(xmin, ymin)
  )))
}

make_grid <- function() {
  sf::st_sf(
    STATION = c("A", "B", "C", "Z"),
    STRATUM = c(10, 20, 30, 40),
    geometry = sf::st_sfc(cell(-176), cell(-175), cell(-174), cell(-170), crs = 4326)
  )
}

# Track runs west->east across cells A and B only.
good_track <- data.frame(LON = c(-175.8, -174.5), LAT = c(52.5, 52.5))

mock_plot_deps <- function(track = good_track,
                           haul_info = data.frame(HAUL_ID = 999, STATION = "B"),
                           env = parent.frame()) {
  testthat::local_mocked_bindings(
    sql_query = function(channel, query) {
      if (grepl("EDIT_EVENTS", query, fixed = TRUE)) track else haul_info
    },
    .package = "gapindex", .env = env
  )
  testthat::local_mocked_bindings(
    get_base_layers = function(...) list(survey.grid = make_grid()),
    .package = "akgfmaps", .env = env
  )
}

open_null_device <- function(env = parent.frame()) {
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off(), envir = env)
}

# ---- tests ------------------------------------------------------------------

test_that("plot_haul_track() returns the documented list invisibly", {
  open_null_device()
  mock_plot_deps()

  expect_invisible(plot_haul_track(202601, 176, 5, channel = "fake"))
  res <- plot_haul_track(202601, 176, 5, channel = "fake")

  expect_named(res, c("haul_id", "station", "track", "midpoint", "grid"))
  expect_equal(res$haul_id, 999)
  expect_equal(res$station, "B")
  expect_s3_class(res$track, "sf")
  expect_s3_class(res$midpoint, "sf")
  expect_true(all(sf::st_geometry_type(res$track) == "LINESTRING"))
  expect_true(all(sf::st_geometry_type(res$midpoint) == "POINT"))
})

test_that("only grid stations intersected by the track are returned", {
  open_null_device()
  mock_plot_deps()
  res <- plot_haul_track(202601, 176, 5, channel = "fake")
  expect_setequal(unique(res$grid$STATION), c("A", "B"))
})

test_that("grid colours are assigned by stratum rank", {
  open_null_device()
  mock_plot_deps()
  res <- plot_haul_track(202601, 176, 5, channel = "fake")
  expect_true(all(c("STRATUM_NUM", "STRATUM_RANK", "fill_color") %in% names(res$grid)))
  expect_equal(sort(unique(res$grid$STRATUM_RANK)), c(1, 2))
  expect_false(anyNA(res$grid$fill_color))
})

test_that("plot_haul_track() errors when the haul does not exist", {
  open_null_device()
  mock_plot_deps(haul_info = data.frame(HAUL_ID = numeric(0), STATION = character(0)))
  expect_error(plot_haul_track(202601, 176, 5, channel = "fake"),
               "No matching haul found for Cruise: 202601, Vessel: 176, Haul: 5")
})

test_that("plot_haul_track() warns and returns NULL with fewer than 2 positions", {
  open_null_device()
  mock_plot_deps(track = good_track[1, ])
  expect_warning(
    res <- plot_haul_track(202601, 176, 5, channel = "fake"),
    "Fewer than 2 event positions"
  )
  expect_null(res)
})

test_that("plot_haul_track() warns and returns NULL when no grid cell intersects", {
  open_null_device()
  far_track <- data.frame(LON = c(-150, -149), LAT = c(60, 60))
  mock_plot_deps(track = far_track)
  expect_warning(
    res <- plot_haul_track(202601, 176, 5, channel = "fake"),
    "No grid cells intersect"
  )
  expect_null(res)
})

test_that("plot_haul_track() restores graphics parameters on exit", {
  open_null_device()
  mock_plot_deps()
  before <- graphics::par("mar")
  plot_haul_track(202601, 176, 5, channel = "fake")
  expect_equal(graphics::par("mar"), before)
})

test_that("plot_haul_track() restores graphics parameters even after an error", {
  open_null_device()
  mock_plot_deps(haul_info = data.frame(HAUL_ID = numeric(0), STATION = character(0)))
  before <- graphics::par("mar")
  try(plot_haul_track(202601, 176, 5, channel = "fake"), silent = TRUE)
  expect_equal(graphics::par("mar"), before)
})

test_that("plot_haul_track() only connects when channel is NULL", {
  open_null_device()
  n_connect <- 0
  mock_plot_deps()
  testthat::local_mocked_bindings(
    get_connected = function(...) { n_connect <<- n_connect + 1; "fake" },
    .package = "gapindex"
  )
  plot_haul_track(202601, 176, 5, channel = "given")
  expect_equal(n_connect, 0)
  plot_haul_track(202601, 176, 5)
  expect_equal(n_connect, 1)
})
