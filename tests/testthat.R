library(testthat)
library(dcmtk)

have_dcmtk = function() {
  have_dcmtk_cmd("dcmdump")
}

skip_if_no_dcmtk = function() {
  if (!have_dcmtk()) {
    skip("DCMTK not installed")
  }
}

test_check("dcmtk")
