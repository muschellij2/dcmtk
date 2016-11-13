#' @title DICOM Anonymization
#' @description Anonymizes DICOM images
#'
#' @param file DICOM Image
#' @param rem_tags tags to remove
#' @param mod_tags tags to modify.  Must be a character vector, with the
#' names of that vector as the tags.
#' @param insert_tags tags to insert.  Must be a character vector, with the
#' names of that vector as the tags.
#' @param new_uids Should new unique identifiers (IDs) be generated
#' @param new_date date to be change date fields to.
#' @param new_time time to be change time fields to.
#' @param verbose print diagnostic messages
#' @param ... Additional arguments to add to \code{\link{dcmodify}}
#'
#' @return Result of \code{dcmodify} command
#' @export
#' @examples \dontrun{
#' file = "~/Desktop/000000.dcm"
#' anon = dcm_anon(file, new_date = "20060101")
#' new_hd = read_dicom_header(anon)
#' }
dcm_anon = function(
  file,
  rem_tags = dcmtk::removal_tags$tag,
  mod_tags = NULL,
  insert_tags = NULL,
  new_uids = TRUE,
  new_date = NULL,
  new_time = NULL,
  verbose = FALSE,
  ...
) {

  # args = as.list(...)
  args = list()

  tfile = tempfile()
  file.copy(file, tfile)
  args$file = tfile
  hdr = read_dicom_header(tfile)

  hdr = hdr[ !is.na(hdr$value), ]

  ####################################
  # Changing out Date
  ####################################
  date_tags = hdr[ grepl("date", tolower(hdr$name)) ,]
  date_tags = date_tags$tag
  if (!is.null(new_date)) {
    n = date_tags
    date_tags = rep_len(new_date, length.out = length(n))
    names(date_tags) = n
    mod_tags = c(mod_tags, date_tags)
  }

  ####################################
  # Changing out Time
  ####################################
  time_tags = hdr[ grepl("time", tolower(hdr$name)) ,]
  time_tags = time_tags[
    !time_tags$name %in% c("RepetitionTime", "EchoTime"),
    ]
  time_tags = time_tags$tag
  if (!is.null(new_time)) {
    n = time_tags
    time_tags = rep_len(new_time, length.out = length(n))
    names(time_tags) = n
    mod_tags = c(mod_tags, time_tags)
  }


  all_tags = hdr$tag

  rem_tags = tolower(rem_tags)
  rem_tags = rem_tags[ rem_tags %in% all_tags ]

  add_opts = "--ignore-errors --erase-private"

  ###################################
  # Removal tags
  ###################################
  if (!is.null(rem_tags)) {
    rem_tags = paste0('--erase-all "', rem_tags, '"')
    rem_tags = paste(rem_tags, collapse = " ")
  }
  add_opts = paste(add_opts, rem_tags)


  ###################################
  # Helper fuctions
  ###################################
  no_na_names = function(x) {
    xnames <- names(x)
    if (is.null(xnames)) {
      xnames = rep("", length(x))
    } else {
      xnames[is.na(xnames)] = ""
    }
    return(xnames)
  }
  has_name = function(x) {
    all(no_na_names(x) != "")
  }


  ###################################
  # Running Tags
  ###################################
  run_tags = function(tags,
                      head_opt = "--modify",
                      type = "Modifier") {
    if (!is.null(tags)) {
      if (!has_name(tags)) {
        stop(paste0(type, " tags must be named with the ",
                    "tag and value in the string"
        ))
      }
      these_tags = names(tags)
      these_tags = tolower(these_tags)
      keep = these_tags %in% all_tags
      if (any(keep)) {
        tags = paste0('"', these_tags[keep],
                      "=", tags[keep], '"')
        tags = paste(head_opt, tags)
        tags = paste(tags, collapse = " ")
      } else {
        return(NULL)
      }
    }
    return(tags)
  }

  # Modifier Tags
  insert_tags = run_tags(insert_tags,
                         head_opt = "--insert",
                         type = "Insert")
  mod_tags = run_tags(mod_tags,
                      head_opt = "--modify",
                      type = "Modifier")

  add_opts = paste(add_opts, insert_tags)
  add_opts = paste(add_opts, mod_tags)
  if (new_uids) {
    add_opts = paste(
      add_opts,
      "--gen-ser-uid --gen-stud-uid --gen-inst-uid")
  }

  if (verbose) {
    add_opts = paste(add_opts, "--verbose")
  }
  args$frontopts = paste(add_opts, args$frontopts)
  res = do.call("dcmodify", args = args)
  rm(res)
  # new_hdr = read_dicom_header(args$file)

  return(args$file)
}
