#' @title DICOM Modify
#' @description Wrapper of \code{dcmodify} from DCMTK
#'
#' @param file DICOM file name
#' @param ... options passed to \code{\link{dcmtk_cmd}}, other than
#' \code{intern}
#'
#' @return Character vector of information
#' @export
dcmodify = function(
  file,
  ...) {

  suppressWarnings({
    file = normalizePath(file, winslash = "/")
  })
  hdr = dcmtk_cmd(cmd = "dcmodify",
                  file = file,
                  ...)

  return(hdr)
}