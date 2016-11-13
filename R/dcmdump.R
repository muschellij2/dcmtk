#' @title DICOM Dump
#' @description Wrapper of \code{dcmdump} from DCMTK
#'
#' @param file DICOM file name
#' @param ... options passed to \code{\link{dcmtk_cmd}}, other than
#' \code{intern}
#'
#' @return Character vector of information
#' @export
dcmdump = function(file,
                   ...) {

  hdr = dcmtk_cmd(cmd = "dcmdump",
                  file = file,
                  intern = TRUE,
                  ...)

  return(hdr)
}
