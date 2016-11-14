#' @title DICOM to XML
#' @description Wrapper of \code{dcm2xml} from DCMTK
#'
#' @param file DICOM file name
#' @param opts options to pass to \code{dcm2xml}
#' @param outfile output XML filename
#'
#' @return Character vector of output filename
#' @export
dcm2xml = function(
  file,
  opts = "",
  outfile = NULL
) {

  if (is.null(outfile)) {
    outfile = tempfile(fileext = ".xml")
  }
  dcmtk_cmd(
    cmd = "dcm2xml",
    frontopts = opts,
    opts = outfile,
    file = file)

  return(outfile)
}
