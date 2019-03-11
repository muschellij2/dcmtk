context("Running non-modifying dcmtk commands")


if (!have_dcmtk) {
  install_dcmtk(install_dir = install_dir)
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
  expect_true(dcm_dir != "")
})

if (!have_dcmtk) {
  test_that("dcmtk is installed", {
    expect_true(install_dcmtk(install_dir = install_dir))
  })
}

test_that("dcmdump with files", {
  expect_message(dcmdump(files[1]))
  expect_silent(dcmdump(files[1], verbose = FALSE))
})

test_that("dcmdump with glob", {
  expect_message(dcmdump(glob))
  expect_silent(dcmdump(glob, verbose = FALSE))
})
