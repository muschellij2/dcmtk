testthat::context("Running dcmtk commands")

have_dcmtk = function() {
  have_dcmtk_cmd("dcmdump")
}

skip_if_no_dcmtk = function() {
  if (!have_dcmtk()) {
    skip("DCMTK not installed")
  }
}

dcm_dir = system.file("extdata", package = "dcmtk")
ofiles = list.files(pattern = ".dcm$",
                    path = dcm_dir,
                    full.names = TRUE)

# copy files for modification
files = file.path(tempdir(), basename(ofiles))
file.copy(ofiles, files)
glob = file.path(tempdir(), "*.dcm")


test_that("dcmodify with files", {
  skip_if_no_dcmtk()
  expect_message(dcmodify(files[1], frontopts = "--erase-private"))
  expect_silent(dcmodify(files[1], frontopts = "--erase-private", verbose = FALSE))
  expect_equal(dcmodify(files[1], frontopts = "--erase-private", verbose = FALSE),
               0)
})


test_that("dcmodify with glob", {
  skip_if_no_dcmtk()
  expect_message(dcmodify(glob, frontopts = "--erase-private"))
  expect_silent(dcmodify(glob, frontopts = "--erase-private", verbose = FALSE))
  expect_equal(dcmodify(glob, frontopts = "--erase-private", verbose = FALSE),
               0)
})

