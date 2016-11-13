#' @title DICOM to PDF
#' @description Wrapper of \code{dcm2pdf} from DCMTK
#'
#' @param file DICOM file name
#' @param opts options to pass to \code{dcm2pdf}
#' @param outfile output PDF filename
#'
#' @return Character vector of output filename
#' @export
dcm2pdf = function(
  file,
  opts = "",
  outfile = NULL
  ) {

  if (is.null(outfile)) {
    outfile = tempfile(fileext = ".pdf")
  }
  dcmtk_cmd(
    cmd = "dcm2pdf",
    frontopts = opts,
    opts = outfile,
    file = file)

  return(outfile)
}