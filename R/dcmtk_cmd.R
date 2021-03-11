#' @title DCMTK Command Wrapper
#' @description This function calls DCMTK command passed to \code{cmd}.
#'
#' @param cmd (character) DCMTK command
#' @param file (character) files to be manipulated
#' @param outfile (character) output filenames
#' @param opts (character) operations to be passed to \code{cmd}
#' @param frontopts (character) options/character to put in before filename
#' @param verbose (logical) print out command before running
#' @param intern (logical) to be passed to \code{\link{system}}
#' @param run (logical) should the command be executed?
#' @param ... additional arguments passed to \code{\link{system}}.
#'
#' @return Result from system command, depends if intern is
#' \code{TRUE} or \code{FALSE}.
#'
#' @export
dcmtk_cmd = function(
  cmd,
  file = NULL,
  opts = "",
  outfile = NULL,
  frontopts = "",
  verbose = TRUE,
  intern = FALSE,
  run = TRUE,
  ...){

  check_dcmtk_cmd(cmd)
  cmd = dcmtk_cmd_path(cmd = cmd)
  sysname = tolower(Sys.info()["sysname"])
  DCMDICTPATH = ""
  if (length(cmd) > 0 && nchar(cmd) > 0 && !sysname %in% "windows") {
    DCMDICTPATH = dirname(dirname(cmd))
    DCMDICTPATH = file.path(DCMDICTPATH, "share", "dcmtk", "dicom.dic")
    if (file.exists(DCMDICTPATH)) {
      DCMDICTPATH = shQuote(normalizePath(DCMDICTPATH, winslash = "/"))
      DCMDICTPATH = paste0("export DCMDICTPATH=", DCMDICTPATH, "; ")
    } else {
      DCMDICTPATH = ""
    }
  }
  ##########################
  # Add frontopts
  ##########################
  s = sprintf('%s %s %s ', DCMDICTPATH, shQuote(cmd), frontopts)
  s = gsub("\\s\\s+", " ", s)
  # if statement for non-unicode things
  if (grepl("\t", s) |
      grepl("\r", s) |
      grepl("\n", s)) {
    s = sub("[ \t\r\n]+$", "", s, perl = TRUE)
  }
  if (!is.null(file)) {
    file = path.expand(file)
  }
  # file = shQuote(file)
  s = paste(s, file)
  opts = paste(opts, collapse = " ")
  cmd = paste(s, opts)
  if (!is.null(outfile)) {
    outfile = paste(names(outfile), shQuote(outfile))
    outfile = paste(outfile, collapse = " ")
    cmd = paste(cmd, outfile)
  }

  if (verbose) {
    message(cmd, "\n")
  }
  if (run) {
    res = system(cmd, intern = intern, ...)
    return(res)
  } else {
    return(invisible(NULL))
  }
}
