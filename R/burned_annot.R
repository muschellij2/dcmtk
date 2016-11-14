#' @title Extract Burned-in Annotation field
#' @description Reads the DICOM header and extracts the burned in tag:
#' \code{(0028,0301)}.
#' @param file DICOM file name
#'
#' @return Character vector from header
#' @export
burned_annot = function(file) {
  hdr = read_dicom_header(file)
  val = hdr[ hdr$tag %in% "(0028,0301)", "value"]
  if (length(val) == 0) {
    val = NA
  }
  return(val)
}