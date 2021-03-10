testthat::context("Test Downloads")

testthat::test_that("Download files exists", {

  type =   type = c("osx", "linux_static", "linux_dynamic", "windows")
  arch = c("x86_64", "i686")
  version = c("3.6.3", "3.6.0")
  eg = expand.grid(type = type, arch = arch, version = version,
                   stringsAsFactors = FALSE)
  lists = apply(eg, 1, as.list)
  x = lists[[1]]

  sapply(lists, function(x) {
    # install_dir =  tempfile()
    # dir.create(install_dir, showWarnings = FALSE, recursive = TRUE)
    base_url = dcmtk_ftp_url(x$version)
    filename = do.call(dcmtk_filename, args = x)
    url = paste0(base_url, filename)
    res = httr::HEAD(url)
    testthat::expect_true(httr::status_code(res) < 400)
  })

})