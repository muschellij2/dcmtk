#' @title DICOM Anonymization
#' @description Anonymizes DICOM images
#'
#' @param file DICOM Image
#' @param base_date baseline date to subtract dates from.
#' The new date is
#' \code{date - base_date + origin}.
#' @param origin origin to add back in.  The new date is
#' \code{date - base_date + origin}.
#' @param date_tags tags to modify
#'
#' @note If no tags are changed, then a warning will be printed.
#' @return Output filename
#' @export
#' @examples \dontrun{
#' file = "~/Desktop/000000.dcm"
#' base_date = lubridate::ymd("2004-06-10")
#' origin = as.Date("19000101", format = "%Y%m%d")
#' tfile = tempfile()
#' file.copy(file, tfile)
#' anon = dcm_subtract_date(tfile, base_date = base_date)
#' hdr = read_dicom_header(anon)
#' }
#' @importFrom lubridate ymd
dcm_subtract_date = function(
  file,
  base_date,
  origin = as.Date("19000101", format = "%Y%m%d"),
  date_tags = dcmtk::date_tags$tag
) {

  args = list()
  args$file = file

  hdr = read_dicom_header(args$file)
  hdr = hdr[ !is.na(hdr$value), ]

  date_tags = date_tags[ date_tags %in% hdr$tag ]

  #######################################
  # Do Date Manipulation
  #######################################
  date_hdr = hdr[ hdr$tag %in% date_tags, ]
  if (nrow(date_hdr) > 0) {

    date_hdr$value = gsub("(\\[|\\])", "", date_hdr$value)
    date_hdr$value = lubridate::ymd(date_hdr$value)
    origin = as.Date(origin)
    date_hdr$value = date_hdr$value - base_date + origin
    date_hdr$value = as.character(date_hdr$value)
    date_hdr$value = gsub("-", "", date_hdr$value)

    # Tags to Modify
    mod_tags = date_hdr$value
    names(mod_tags) = date_hdr$tag
    mod_tags = paste0('"', date_hdr$tag,
                      "=", date_hdr$value, '"')

    mod_tags = paste("--modify", mod_tags)
    mod_tags = paste(mod_tags, collapse = " ")


    args$frontopts = mod_tags
    res = do.call("dcmodify", args = args)
    rm(res)
  } else {
    warning("No modification - no date headers available!")
  }

  return(args$file)
}
