#' @title Parse DICOM header
#' @description Parses a DICOM header to a \code{data.frame}
#'
#' @param hdr Character vector from \code{\link{dcmdump}}
#'
#' @return \code{data.frame} of tags and values
#' @export
parse_hdr = function(hdr){
  # xhdr = hdr
  #
  # hdr = xhdr

  hdr = trimws(hdr)
  hdr = hdr[ grepl("^\\(", hdr)]
  hdr = gsub("Unknown Tag & Data",
             "UnknownTagAndData",
             hdr, fixed = TRUE)

  ss = strsplit(hdr, "# ")
  ss = lapply(ss, trimws)

  ##################################
  # Check if all the lengths are
  # separated correctly
  ##################################
  first = function(x) x[1]
  second = function(x) x[2]

  ###################
  # Grabbing the information
  ###################
  info = lapply(ss, first)
  info = lapply(info, function(x) {
    x = strsplit(x, " ")[[1]]
    if (length(x) > 2) {
      ind = seq(3, length(x))
      x[3] = paste(x[ind], collapse = " ")
      x = x[1:3]
    }
    return(x)
  })
  ilen = sapply(info, length)
  icheck = all(ilen == 3)
  if (!icheck) {
    stop("Header not likely parsed correctly")
  }
  info = do.call("rbind", info)
  colnames(info) = c("tag", "val_rep", "value")

  ######################################
  # Grabbing the information
  ######################################
  lengths = sapply(ss, length)
  check = all(lengths == 2)
  if (!check) {
    warning("Header not likely parsed correctly")
  } else {
    ss = lapply(ss, second)
    ss = lapply(ss, function(x) {
      x = strsplit(x, " ")[[1]]
      x[1] = gsub(",$", "", x[1])
      return(x)
    })
    lengths = sapply(ss, length)
    check = all(lengths == 3)
    if (!check) {
      warning("Header not likely parsed correctly")
    }
    extra = do.call("rbind", ss)
    colnames(extra) = c("length", "val_mult", "name")
    info = cbind(info, extra)
  }


  df = data.frame(info,
                  stringsAsFactors = FALSE)
  df$value[
    df$value %in% c("(no value available)",
                    "(not loaded)") ] = NA

  return(df)
}