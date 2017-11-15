#' @title Read DICOM File
#' @description Reads in the DICOM header from a file
#'
#' @param file DICOM input file or directory.  If \code{recursivee = TRUE},
#' then this will be the pattern to match within \code{path}
#' @param replace_names logical indicating if unknown tag names should be
#' inferred from \code{dicom_tags}
#' @param add_opts additional options to pass to \code{\link{dcmdump}}.
#' The flags already added are
#' \code{-q --print-all --load-short --print-filename}
#' @param recursive logical indicating if the \code{--recurse} flag be passed to
#' \code{\link{dcmdump}}
#' @param path if \code{recursive = TRUE}, then this will the path scanned.
#'
#' @return Character vector of header information
#' @export
read_dicom_header = function(
  file = "*.dcm",
  replace_names = FALSE,
  add_opts = "",
  recursive = FALSE,
  path = "."
) {
  if (recursive) {
    add_opts = c(add_opts,
                 "--recurse",
                 paste0("--scan-directories ",
                        normalizePath(path)),
                 "--scan-pattern")
  }
  add_opts = paste(add_opts, collapse = " ")
  opts = paste("-q --print-all --load-short --print-filename",
               add_opts)
  hdr = dcmdump(file = file,
                frontopts = opts)
  hdr = enc2utf8(hdr)
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