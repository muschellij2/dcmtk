#' Convert DICOM images to PGM/PPM, PNG, JPEG or BMP using
#' \code{dcmdump} from DCMTK
#'
#' @param file DICOM file name
#' @param outfile output file name passed to \code{\link{dcmtk_cmd}}
#' @param opts opts passed to \code{\link{dcmtk_cmd}}
#' @param ... options passed to \code{\link{dcmtk_cmd}}
#' \code{intern}
#'
#' @return Character vector of information
#' @export
#' @examples
#' if (!have_dcmtk_cmd("dcmj2pnm")) {
#'   install_dir = tempdir()
#'   res = try({
#'     install_dcmtk(install_dir = install_dir)
#'   })
#'   if (inherits(res, "try-error")) {
#'     res = FALSE
#'   }
#'   if (!res) {
#'     source_install_dcmtk(install_dir = install_dir)
#'   }
#'   options(dcmtk.path = install_dir)
#' }
#' file = system.file("extdata", "example.dcm", package = "dcmtk")
#' png_file = dcmj2pnm(file)
#' print(png_file)
#' img = png::readPNG(png_file)
#' plot(1:2, type='n')
#' image(img)
#'
dcmj2pnm = function(file,
                    outfile = tempfile(fileext = ".png"),
                    opts = "--write-png",
                    ...) {

  names(outfile) = NULL
  res = dcmtk_cmd(
    cmd = "dcmj2pnm",
    file = file,
    opts = opts,
    outfile = outfile,
    ...)
  status = attr(res, "status")
  if (!is.null(status)) {
    status = as.numeric(status)
    if (is.na(status) || status > 0) {
      warning("dcmj2pnm gave back non-zero status")
    }
  }
  attr(outfile, "input_file") = file
  return(outfile)
}