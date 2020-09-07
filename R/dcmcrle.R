#' @title Encode DICOM with RLE
#' @description Wrapper of \code{dcmcrle} from DCMTK to
#' encode DICOM with run-length encoding
#'
#' @param file DICOM file name
#' @param opts options to pass to \code{dcmcrle}
#' @param outfile output DICOM filename
#'
#' @return Character vector of output filename
#' @export
dcmcrle = function(
  file,
  opts = "",
  outfile = NULL
) {

  if (is.null(outfile)) {
    outfile = tempfile(fileext = ".dcm")
  }
  outfile = path.expand(outfile)
  file = shQuote(normalizePath(file, winslash = "/"))
  dcmtk_cmd(
    cmd = "dcmcrle",
    frontopts = opts,
    opts = outfile,
    file = file)

  return(outfile)
}
