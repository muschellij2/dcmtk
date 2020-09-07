#' @rdname dcmdjpeg
#' @title Decode JPEG-compressed DICOM file
#' @description Wrapper of \code{dcmdjpeg} from DCMTK
#'
#' @param file DICOM file name
#' @param outfile output DICOM file name
#' @param ... options passed to \code{\link{dcmtk_cmd}}
#'
#' @return Character output file name
#' @export
dcmdjpeg = function(
  file,
  outfile = NULL,
  ...) {

  if (length(file) > 1) {
    if (!is.null(outfile)) {
      stopifnot(length(file) == length(outfile))
    } else {
      outfile = lapply(file, function(x) NULL)
    }
    res = mapply(function(ifile, ofile) {
      dcmdjpeg(ifile, outfile = ofile, ...)
    }, file, outfile)
    res = unname(res)
    return(res)
  }
  if (is.null(outfile)) {
    outfile = tempfile(fileext = ".dcm")
  }
  outfile = path.expand(outfile)
  xoutfile = shQuote(outfile)

  args = list(...)
  args$opts = c(xoutfile, args$opts)

  file = shQuote(normalizePath(file, winslash = "/"))
  args$file = file
  args$cmd = "dcmdjpeg"

  hdr = do.call("dcmtk_cmd", args = args)
  if (hdr != 0) {
    warning("Result was not 0 from command, may not execute properly")
  }
  return(outfile)
}
