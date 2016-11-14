#' @title DICOM Tags
#'
#' @description A \code{data.frame} of tags to look up in DICOM
#' header.  Not an exhaustive list, but extensive.
#'
#' @note Adapted from
#' \url{http://dicom.nema.org/dicom/2013/output/chtml/part06/chapter_6.html}
#'
#' @format A \code{data.frame} with 6 columns
#' \describe{
#' \item{tag}{the tag identifier for the field }
#' \item{name}{the name of the field }
#' \item{keyword}{more readable name}
#' \item{vr}{value representation}
#' \item{vm}{value multiplier}
#' \item{retired}{indication of retired tag}
#' }
"dicom_tags"
