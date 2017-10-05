#' @title DCMTK Command Wrapper
#' @description This function calls DCMTK command passed to \code{cmd}.
#'
#' @param cmd (character) DCMTK command
#' @param file (character) files to be manipulated
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
  frontopts = "",
  verbose = TRUE,
  intern = FALSE,
  run = TRUE,
  ...){

  check_dcmtk_cmd(cmd)
  cmd = dcmtk_cmd_path(cmd = cmd)

  ##########################
  # Add frontopts
  ##########################
  s = sprintf('%s %s ', cmd, frontopts)
  s = gsub("\\s\\s+", " ", s)
  s = sub("[ \t\r\n]+$", "", s, perl = TRUE)
  if (!is.null(file)) {
    file = path.expand(file)
  }
  # file = shQuote(file)
  s = paste(s, file)
  cmd = paste(s, opts)

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
