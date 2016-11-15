#' @title Read DICOM File
#' @description Reads in the DICOM header from a file
#'
#' @param file Name of DICOM file
#' @param replace_names logical indicating if unknown tag names should be
#' inferred from \code{dicom_tags}
#' @param add_opts additional options to pass to \code{\link{dcmdump}}.
#' The flags already added are
#' \code{-q --print-all --load-short --print-filename}
#'
#' @return Character vector of header information
#' @export
read_dicom_header = function(file,
                             replace_names = FALSE,
                             add_opts = "") {
  add_opts = paste(add_opts, collapse = " ")
  opts = paste("-q --print-all --load-short --print-filename",
                    add_opts)
  hdr = dcmdump(file = file,
                frontopts = opts)
  hdr = parse_hdr(hdr)
  if (replace_names) {
    hdr$ind = seq(nrow(hdr))
    tag =  dcmtk::dicom_tags[, c("tag", "keyword")]
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