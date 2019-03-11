library(testthat)
library(dcmtk)

have_dcmtk = have_dcmtk_cmd("dcmdump")
if (!have_dcmtk) {
  install_dir = tempdir()
  options(dcmtk.path = install_dir)
}
in_ci <- function() {
  nzchar(Sys.getenv("CI"))
}
if (!have_dcmtk) {
  res = try({
    install_dcmtk(install_dir = install_dir)
  })
  if (inherits(res, "try-error")) {
    res = FALSE
  }
  if (!res) {
    source_install_dcmtk(install_dir = install_dir)
  }
}


test_check("dcmtk")
