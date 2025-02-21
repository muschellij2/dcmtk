context("Running non-modifying dcmtk commands")

have_dcmtk = function() {
  have_dcmtk_cmd("dcmdump")
}

skip_if_no_dcmtk = function() {
  if (!have_dcmtk()) {
    skip("DCMTK not installed")
  }
}

dcm_dir = system.file("extdata", package = "dcmtk")
files = list.files(pattern = ".dcm$",
                   path = dcm_dir,
                   full.names = TRUE)

# copy files for modification
tfiles = file.path(tempdir(), basename(files))
file.copy(files, tfiles)
glob = file.path(dcm_dir, "*.dcm")

test_that("Data directory exists", {
  skip_if_no_dcmtk()
  expect_true(dcm_dir != "")
})


test_that("dcmdump with files", {
  skip_if_no_dcmtk()
  expect_message(dcmdump(files[1]))
  expect_silent(dcmdump(files[1], verbose = FALSE))
})

test_that("dcmdump with glob", {
  skip_if_no_dcmtk()
  expect_message(dcmdump(glob))
  expect_silent(dcmdump(glob, verbose = FALSE))
})
