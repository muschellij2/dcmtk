#' @title Parse DICOM header
#' @description Parses a DICOM header to a \code{data.frame}
#'
#' @param hdr Character vector from \code{\link{dcmdump}}
#' @param convert_non_ascii Should [iconv] be run before parsing?  This
#' can help failures in UTF-8 issues, but may alter the output from
#' [dcmdump()]
#'
#' @return \code{data.frame} of tags and values
#' @export
#' @examples
#' file = system.file("extdata", "example.dcm", package = "dcmtk")
#' hdr = dcmdump(file)
#' parsed = parse_hdr(hdr)
parse_hdr = function(hdr,
                     convert_non_ascii = TRUE){
  # xhdr = hdr
  #
  # hdr = xhdr

  if (convert_non_ascii) {
    hdr <- iconv(hdr, from = "latin1", to = "UTF-8", sub = "byte")
  }

  hdr = trimws(hdr)

  na_locf = function(x) {
    ind = !is.na(x)
    cs = cumsum(ind)
    ss = split(x, cs)
    ss = lapply(ss, function(r){
      r = rep(r[1], length = length(r))
    })
    ss = unlist(ss)
    names(ss) = NULL
    ss
  }
  ############################
  # Need this for filenames
  ############################
  fname = rep(NA, length = length(hdr))
  fname_ind = grep("^#\\s*dcmdump.*:(.*)", tolower(hdr))
  fname[fname_ind] = gsub("^.*:(.*)", "\\1", hdr[fname_ind])
  fname[fname_ind] = trimws(fname[fname_ind])
  fname = na_locf(fname)

  df = data.frame(hdr = hdr,
                  file = fname,
                  stringsAsFactors = FALSE)
  ############################
  # Multi-line stuff
  ############################
  tags = !grepl("^#", df$hdr) & df$hdr != ""
  df = df[tags,]
  if (nrow(df) == 0) {
    return(NULL)
  }
  # # tags = grepl("^\\(", hdr)
  # hdr = hdr[ tags ]
  # fname = fname[ tags ]
  df$hdr = gsub("Unknown Tag & Data",
                "UnknownTagAndData",
                df$hdr, fixed = TRUE)


  ############################
  # Each Tag should be one line
  ############################
  df$tag_num = grepl("^\\(", df$hdr)
  df$tag_num = cumsum(df$tag_num)

  ss = split(df, df$tag_num)
  nrows = sapply(ss, nrow)
  nrows = nrows > 1
  if (any(nrows)) {
    ss[nrows] = lapply(ss[nrows], function(x){
      x$hdr[1] = paste0(x$hdr, collapse = "\n")
      x[1, , drop = FALSE]
    })
    df = do.call("rbind", ss)
  }
  rm(list = "ss")


  df$hdr = sub("length #=", "length=", df$hdr)
  count_pound = stringr::str_count(df$hdr, pattern = "#")
  subber = count_pound > 1
  df$hdr[ subber ] = sub("#", "", df$hdr[ subber ])
  ss = strsplit(df$hdr, "# ")
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
  if (NROW(info) > 0) {
    colnames(info) = c("tag", "val_rep", "value")
  }


  ######################################
  # Grabbing the information
  ######################################
  lengths = sapply(ss, length)
  check = all(lengths == 2)
  if (!check) {
    warning(paste0("Splitting each value wrong, ", "
            hdr not likely parsed correctly"))
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
    extra = do.call("rbind",ss)
    colnames(extra) = c("length", "val_mult", "name")
    info = cbind(info, extra)
  }
  info = data.frame(info,
                    stringsAsFactors = FALSE)
  df = cbind(df, info)
  df$hdr = df$tag_num = NULL

  df$value[
    df$value %in% c("(no value available)",
                    "(not loaded)") ] = NA

  return(df)
}