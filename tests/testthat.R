library(testthat)
library(dcmtk)

install_dir = tempdir()
options(dcmtk.path = install_dir)
in_ci <- function() {
  nzchar(Sys.getenv("CI"))
}
if (in_ci()) {
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
