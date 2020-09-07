#' @title Decode DICOM with RLE
#' @description Wrapper of \code{dcmdrle} from DCMTK to
#' decode DICOM with run-length encoding
#'
#' @param file DICOM file name
#' @param opts options to pass to \code{dcmdrle}
#' @param outfile output DICOM filename
#'
#' @return Character vector of output filename
#' @export
dcmdrle = function(
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
    cmd = "dcmdrle",
    frontopts = opts,
    opts = outfile,
    file = file)

  return(outfile)
}
