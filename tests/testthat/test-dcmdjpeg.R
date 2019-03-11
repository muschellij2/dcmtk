context("Running DECOMPRESS commands")

if (!install_dcmtk(install_dir = install_dir)) {
  install_dcmtk(install_dir = install_dir)
}
dcm_dir = system.file("extdata", package = "dcmtk")
ofiles = list.files(pattern = ".dcm$",
                    path = dcm_dir,
                    full.names = TRUE)

test_that("dcmdjpeg with files", {

  expect_message({res = dcmdjpeg(file = ofiles)})
  expect_silent({res = dcmdjpeg(file = ofiles, verbose = FALSE)})
  expect_true(length(res) == length(ofiles))

})
