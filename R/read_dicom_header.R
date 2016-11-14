#' @title Read DICOM File
#' @description Reads in the DICOM header from a file
#'
#' @param file Name of DICOM file
#'
#' @return Character vector of header information
#' @export
read_dicom_header = function(file, replace_names = FALSE) {

  hdr = dcmdump(file = file,
                opts = "-q --print-all --load-short --print-filename")
  hdr = parse_hdr(hdr)
  if (replace_names) {
    hdr$ind = seq(nrow(hdr))
    tag =  dcmtk::dicom_tags[, c("tag", "name")]
    colnames(tag)[2] = "dname"
    hdr = merge(hdr, tag, all.x = TRUE, sort = FALSE)
    ind = hdr$name == "UnknownTagAndData" & !is.na(hdr$dname)
    hdr$name[ ind ] = hdr$dname[ind]
    hdr$dname = NULL
    hdr = hdr[ order(hdr$ind),]
    hdr$ind = NULL
  }
  return(hdr)
}

#' @rdname read_dicom_header
#' @export
dcmhd = function(file) {
  read_dicom_header(file = file)
}