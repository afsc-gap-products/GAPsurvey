library(GAPsurvey)

testthat::test_that(
  "Test convert_ctd_btd()",
  {convert_ctd_btd(
    filepath_hex = system.file(paste0("exdata/convert_ctd_btd/",
                                      "2021_06_13_0003.hex"), package = "GAPsurvey"),
    filepath_xmlcon = system.file(paste0("exdata/convert_ctd_btd/",
                                         "19-8102_Deploy2021.xmlcon"), package = "GAPsurvey"),
    latitude = 55,
    VESSEL = 94,
    CRUISE = 202101,
    HAUL = 107,
    SERIAL_NUMBER = 8105,
    MODEL_NUMBER = 1,
    VERSION_NUMBER = 1);
    # Check files exist
    fpath_btd <- paste0(getwd(), "/", "HAUL0107_new.BTD");
    fpath_bth <- paste0(getwd(), "/", "HAUL0107_new.BTH");
    testthat::expect_true(file.exists(fpath_btd));
    testthat::expect_true(file.exists(fpath_bth));
    # Check that dimensions match expected output
    btd_out <- utils::read.csv(fpath_btd);
    bth_out <- utils::read.csv(fpath_bth);
    testthat::expect_true(all(dim(btd_out) == c(2602, 7)));
    testthat::expect_true(all(dim(bth_out) == c(1, 14)));
    suppressWarnings(file.remove(c(fpath_btd, fpath_bth)));
    suppressWarnings(
      file.remove(c(list.files(path = getwd(), pattern = ".cnv", full.names = TRUE)))
    );
  }
)

testthat::test_that(
  "Test convert_bvdr_marp()",
  {
    syspath_bvdr <- system.file("exdata/convert_bvdr_marp/20220811-00Za.bvdr", package = "GAPsurvey");
    fpath_bvdr <- paste0(getwd(), "/", basename(syspath_bvdr));
    file.copy(syspath_bvdr, fpath_bvdr);
    convert_bvdr_marp(path_bvdr = fpath_bvdr);
    # Check files exist
    fpath_marp <- paste0(getwd(), "/20220811-00Za.marp");
    testthat::expect_true(file.exists(fpath_marp));
    # Check that dimensions match expected output
    marp_out <- readLines(fpath_marp)
    testthat::expect_true(length(marp_out) == 22249);
    suppressWarnings(file.remove(c(fpath_bvdr, fpath_marp)));
  }
)

testthat::test_that(
  "Test convert_log_gps()",
  {convert_log_gps(
    VESSEL = 94,
    CRUISE = 201901,
    HAUL = 3,
    DATE = "06/06/2017",
    path_in = system.file("exdata/convert_log_gps/06062017.log",
                          package = "GAPsurvey"),
    path_out = getwd(),
    filename_add = "newlog"
  );
    # Check files exist
    fpath_gps <- paste0(getwd(), "/", "HAUL0003_newlog.gps");
    testthat::expect_true(file.exists(fpath_gps));
    # Check that dimensions match expected output
    gps_out <- utils::read.csv(fpath_gps)
    testthat::expect_true(all(dim(gps_out) == c(1091, 6)));
    suppressWarnings(file.remove(fpath_gps));
    }
)

testthat::test_that(
  "Test convert_ted_btd()",
  {
    convert_ted_btd(
      VESSEL = 94,
      CRUISE = 201901,
      HAUL = 3,
      MODEL_NUMBER = 123,
      VERSION_NUMBER = 456,
      SERIAL_NUMBER = 789,
      path_in = system.file("exdata/convert_bvdr_btd/", package = "GAPsurvey"),
      path_out = getwd(),
      filename_add = "newted");
    # Check files exist
    fpath_btd <- paste0(getwd(), "/", "HAUL0003_newted.BTD");
    fpath_bth <- paste0(getwd(), "/", "HAUL0003_newted.BTH");
    testthat::expect_true(file.exists(fpath_btd));
    testthat::expect_true(file.exists(fpath_bth));
    # Check that dimensions match expected output
    btd_out <- utils::read.csv(fpath_btd)
    bth_out <- utils::read.csv(fpath_bth);
    testthat::expect_true(all(dim(btd_out) == c(1583, 8)));
    testthat::expect_true(all(dim(bth_out) == c(1, 14)));
    suppressWarnings(
      file.remove(c(fpath_btd, fpath_bth), showWarnings = FALSE)
    );
  }
)
