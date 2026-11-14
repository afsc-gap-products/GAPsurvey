# Tests for merge_scs_files(); uses small synthetic SCS XML files.

skip_if_not_installed("xml2")

# ---- helpers ----------------------------------------------------------------

ts <- function(hhmmss) paste0("2026-06-22T", hhmmss, ".000Z")   # UTC ISO stamps

# Build an SCS-style XML file. Each of triggers/diag/outputs is a vector of
# UTC "HH:MM:SS" strings; meta is a character vector of MetaItem names.
write_scs <- function(path, start, end,
                      triggers = character(), diag = character(),
                      outputs = character(), meta = character()) {
  stamped <- function(tag, times, prefix) {
    sprintf('<%s timestamp="%s">%s@%s</%s>', tag, ts(times), prefix, times, tag)
  }
  xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    sprintf('<EventResult event-start-time="%s" event-end-time="%s">', ts(start), ts(end)),
    '<EventData>',
    '<Triggers>',          paste(stamped("Trigger", triggers, "trig"), collapse = ""), '</Triggers>',
    '<DiagnosticsLog>',    paste(stamped("DiagnosticsLogEntry", diag, "diag"), collapse = ""), '</DiagnosticsLog>',
    '<MetaItems>',         paste(sprintf('<MetaItem name="%s">v</MetaItem>', meta), collapse = ""), '</MetaItems>',
    '<Outputs>',           paste(stamped("Output", outputs, "out"), collapse = ""), '</Outputs>',
    '</EventData></EventResult>'
  )
  writeLines(xml, path)
  path
}

count_nodes <- function(doc, xpath) length(xml2::xml_find_all(doc, xpath))
trigger_stamps <- function(doc) {
  xml2::xml_attr(xml2::xml_find_all(doc, "//Trigger"), "timestamp")
}

merge_quiet <- function(...) suppressMessages(merge_scs_files(...))

# ---- input validation / basic behaviour ------------------------------------

test_that("merge_scs_files() errors when no files are supplied", {
  expect_error(merge_scs_files(character(0)), "No file paths provided")
})

test_that("merge_scs_files() writes the output, returns an xml_document invisibly, and messages", {
  a <- write_scs(withr::local_tempfile(fileext = ".xml"), "20:00:00", "20:10:00", triggers = "20:01:00")
  b <- write_scs(withr::local_tempfile(fileext = ".xml"), "20:10:00", "20:20:00", triggers = "20:11:00")
  out <- withr::local_tempfile(fileext = ".xml")

  expect_message(merge_scs_files(c(a, b), output_path = out), "Successfully merged 2 files")
  expect_true(file.exists(out))
  expect_invisible(merge_quiet(c(a, b), output_path = out))
  expect_s3_class(merge_quiet(c(a, b), output_path = out), "xml_document")
})

test_that("merge_scs_files() accepts a single file", {
  a <- write_scs(withr::local_tempfile(fileext = ".xml"), "20:00:00", "20:10:00", triggers = "20:01:00")
  out <- withr::local_tempfile(fileext = ".xml")
  merge_quiet(a, output_path = out)
  merged <- xml2::read_xml(out)
  expect_equal(count_nodes(merged, "//Trigger"), 1)
})

# ---- merging & ordering -----------------------------------------------------

test_that("files are merged chronologically regardless of input order", {
  a <- write_scs(withr::local_tempfile(fileext = ".xml"), "20:00:00", "20:10:00",
                 triggers = "20:01:00", diag = "20:02:00", outputs = "20:03:00", meta = "alpha")
  b <- write_scs(withr::local_tempfile(fileext = ".xml"), "20:10:00", "20:20:00",
                 triggers = "20:11:00", diag = "20:12:00", outputs = "20:13:00", meta = "beta")
  out <- withr::local_tempfile(fileext = ".xml")

  merge_quiet(c(b, a), output_path = out)   # deliberately reversed
  merged <- xml2::read_xml(out)

  expect_equal(trigger_stamps(merged), c(ts("20:01:00"), ts("20:11:00")))
  expect_equal(count_nodes(merged, "//DiagnosticsLogEntry"), 2)
  expect_equal(count_nodes(merged, "//Output"), 2)
  expect_equal(count_nodes(merged, "//MetaItem"), 2)
  # Root times span first start to last end
  expect_equal(xml2::xml_attr(merged, "event-start-time"), ts("20:00:00"))
  expect_equal(xml2::xml_attr(merged, "event-end-time"),   ts("20:20:00"))
})

test_that("three files merge in order", {
  a <- write_scs(withr::local_tempfile(fileext = ".xml"), "20:00:00", "20:10:00", triggers = "20:01:00")
  b <- write_scs(withr::local_tempfile(fileext = ".xml"), "20:10:00", "20:20:00", triggers = "20:11:00")
  third <- write_scs(withr::local_tempfile(fileext = ".xml"), "20:20:00", "20:30:00", triggers = "20:21:00")
  out <- withr::local_tempfile(fileext = ".xml")
  merge_quiet(c(third, a, b), output_path = out)
  merged <- xml2::read_xml(out)
  expect_equal(trigger_stamps(merged), ts(c("20:01:00", "20:11:00", "20:21:00")))
  expect_equal(xml2::xml_attr(merged, "event-end-time"), ts("20:30:00"))
})

# ---- deduplication ----------------------------------------------------------

test_that("identical records in overlapping files are de-duplicated", {
  a <- write_scs(withr::local_tempfile(fileext = ".xml"), "20:00:00", "20:10:00",
                 triggers = c("20:01:00", "20:09:00"), diag = "20:09:00",
                 outputs = "20:09:00", meta = "vessel")
  b <- write_scs(withr::local_tempfile(fileext = ".xml"), "20:09:00", "20:20:00",
                 triggers = c("20:09:00", "20:11:00"), diag = "20:09:00",
                 outputs = "20:09:00", meta = "vessel")
  out <- withr::local_tempfile(fileext = ".xml")

  merge_quiet(c(a, b), output_path = out)
  merged <- xml2::read_xml(out)

  expect_equal(trigger_stamps(merged), ts(c("20:01:00", "20:09:00", "20:11:00")))
  expect_equal(count_nodes(merged, "//DiagnosticsLogEntry"), 1)
  expect_equal(count_nodes(merged, "//Output"), 1)
  expect_equal(count_nodes(merged, "//MetaItem"), 1)
})

test_that("records that differ only in text are both kept", {
  a <- write_scs(withr::local_tempfile(fileext = ".xml"), "20:00:00", "20:10:00", meta = "alpha")
  b <- write_scs(withr::local_tempfile(fileext = ".xml"), "20:10:00", "20:20:00", meta = "beta")
  out <- withr::local_tempfile(fileext = ".xml")
  merge_quiet(c(a, b), output_path = out)
  names <- xml2::xml_attr(xml2::xml_find_all(xml2::read_xml(out), "//MetaItem"), "name")
  expect_setequal(names, c("alpha", "beta"))
})

# ---- time-window cropping ---------------------------------------------------

test_that("start_time / end_time crop records outside the window (tz = UTC)", {
  a <- write_scs(withr::local_tempfile(fileext = ".xml"), "20:00:00", "20:10:00",
                 triggers = "20:01:00", outputs = "20:02:00")
  b <- write_scs(withr::local_tempfile(fileext = ".xml"), "20:10:00", "20:20:00",
                 triggers = c("20:11:00", "20:19:00"), outputs = "20:12:00")
  out <- withr::local_tempfile(fileext = ".xml")

  merge_quiet(c(a, b), output_path = out,
              start_time = "2026-06-22 20:05:00",
              end_time   = "2026-06-22 20:15:00", tz = "UTC")
  merged <- xml2::read_xml(out)

  expect_equal(trigger_stamps(merged), ts("20:11:00"))
  expect_equal(count_nodes(merged, "//Output"), 1)
  expect_equal(xml2::xml_attr(merged, "event-start-time"), ts("20:05:00"))
  expect_equal(xml2::xml_attr(merged, "event-end-time"),   ts("20:15:00"))
})

test_that("records exactly at the window edges are kept", {
  a <- write_scs(withr::local_tempfile(fileext = ".xml"), "20:00:00", "20:10:00",
                 triggers = c("20:05:00", "20:15:00"))
  out <- withr::local_tempfile(fileext = ".xml")
  merge_quiet(a, output_path = out,
              start_time = "2026-06-22 20:05:00",
              end_time   = "2026-06-22 20:15:00", tz = "UTC")
  expect_equal(count_nodes(xml2::read_xml(out), "//Trigger"), 2)
})

test_that("only start_time (or only end_time) can be supplied", {
  a <- write_scs(withr::local_tempfile(fileext = ".xml"), "20:00:00", "20:10:00",
                 triggers = c("20:01:00", "20:09:00"))
  out <- withr::local_tempfile(fileext = ".xml")

  merge_quiet(a, output_path = out, start_time = "2026-06-22 20:05:00", tz = "UTC")
  merged <- xml2::read_xml(out)
  expect_equal(trigger_stamps(merged), ts("20:09:00"))
  expect_equal(xml2::xml_attr(merged, "event-start-time"), ts("20:05:00"))
  expect_equal(xml2::xml_attr(merged, "event-end-time"),   ts("20:10:00"))  # untouched

  a2 <- write_scs(withr::local_tempfile(fileext = ".xml"), "20:00:00", "20:10:00",
                  triggers = c("20:01:00", "20:09:00"))
  merge_quiet(a2, output_path = out, end_time = "2026-06-22 20:05:00", tz = "UTC")
  merged <- xml2::read_xml(out)
  expect_equal(trigger_stamps(merged), ts("20:01:00"))
  expect_equal(xml2::xml_attr(merged, "event-start-time"), ts("20:00:00"))   # untouched
  expect_equal(xml2::xml_attr(merged, "event-end-time"),   ts("20:05:00"))
})

test_that("default tz handles Alaska daylight and standard time", {
  a <- write_scs(withr::local_tempfile(fileext = ".xml"), "20:00:00", "23:00:00",
                 triggers = "20:40:00")
  out <- withr::local_tempfile(fileext = ".xml")

  # Summer: AKDT = UTC-8, so 12:34 local = 20:34Z
  merge_quiet(a, output_path = out, start_time = "2026-06-22 12:34:00")
  expect_equal(xml2::xml_attr(xml2::read_xml(out), "event-start-time"),
               "2026-06-22T20:34:00.000Z")

  # Winter: AKST = UTC-9, so 12:00 local = 21:00Z
  merge_quiet(a, output_path = out, start_time = "2026-01-15 12:00:00")
  expect_equal(xml2::xml_attr(xml2::read_xml(out), "event-start-time"),
               "2026-01-15T21:00:00.000Z")
})

test_that("the crop window is interpreted in the supplied tz", {
  a <- write_scs(withr::local_tempfile(fileext = ".xml"), "20:00:00", "21:00:00",
                 triggers = c("20:10:00", "20:50:00"))
  out <- withr::local_tempfile(fileext = ".xml")
  # 12:30 AKDT == 20:30Z, so the 20:10Z trigger is cropped and 20:50Z is kept
  merge_quiet(a, output_path = out, start_time = "2026-06-22 12:30:00")
  expect_equal(trigger_stamps(xml2::read_xml(out)), ts("20:50:00"))
})
