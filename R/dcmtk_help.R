#' @title Wrapper for getting DCMTK help
#' @description This function takes in the function and returns the
#' help from DCMTK for that function
#' @param cmd DCMTK command
#' @param help.arg Argument to print help, usually "--help"
#' @return Prints help output and returns output as character vector
#' @export
dcmtk_help = function(cmd, help.arg = "--help"){
  suppressWarnings({
    res = dcmtk_cmd(cmd = "dcmodify",
              run = TRUE,
              opts = help.arg,
              intern = TRUE)
  })
  message(res, sep = "\n")
  return(invisible(res))
}
