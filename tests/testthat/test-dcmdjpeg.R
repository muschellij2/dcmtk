context("Running DECOMPRESS commands")

if (!install_dcmtk()) {
  install_dcmtk()
}
dcm_dir = system.file("extdata", package = "dcmtk")
ofiles = list.files(pattern = ".dcm$",
                    path = dcm_dir,
                    full.names = TRUE)

test_that("dcmdjpeg with files", {

  expect_message({res = dcmdjpeg(file = ofiles)})
  expect_silent({res = dcmdjpeg(file = ofiles, verbose = TRUE)})
  expect_true(length(res) == length(ofiles))

})
