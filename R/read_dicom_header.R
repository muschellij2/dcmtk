#' @title Read DICOM File
#' @description Reads in the DICOM header from a file
#'
#' @param file Name of DICOM file
#'
#' @return Character vector of header information
#' @export
read_dicom_header = function(file) {

  hdr = dcmdump(file = file,
                opts = "-q --print-all --load-short")
  hdr = parse_hdr(hdr)
  return(hdr)
}

#' @rdname read_dicom_header
#' @export
dcmhd = function(file) {
  read_dicom_header(file = file)
}