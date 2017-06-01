#' @rdname dcmdump
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
  status = attr(hdr, "status")
  if (!is.null(status)) {
    status = as.numeric(status)
    if (is.na(status) || status > 0) {
      warning("dcmdump gave back non-zero status")
    }
  }
  return(hdr)
}

#' @export
#' @note \code{dcmdump_full} uses the
#' \code{frontopts = "-q --print-all --load-short --print-filename"} by
#' default
#' @rdname dcmdump
dcmdump_full = function(file,
                        ...) {
  args = list(...)
  frontopts = args$frontopts
  if (is.null(frontopts)) {
    frontopts = ""
  } else {
    frontopts = paste(frontopts, collapse = " ")
  }
  frontopts = paste("-q --print-all --load-short --print-filename",
               frontopts)
  args$frontopts = frontopts
  do.call("dcmdump", args)
}
