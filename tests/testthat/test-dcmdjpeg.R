context("Running DECOMPRESS commands")

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

test_that("dcmdjpeg with files", {
  skip_if_no_dcmtk()
  expect_message({res = dcmdjpeg(file = ofiles)})
  expect_silent({res = dcmdjpeg(file = ofiles, verbose = FALSE)})
  expect_true(length(res) == length(ofiles))

})
