library(testthat)
library(dcmtk)


in_ci <- function() {
  nzchar(Sys.getenv("CI"))
}
if (in_ci()) {
  source_install_dcmtk()
}
test_check("dcmtk")
