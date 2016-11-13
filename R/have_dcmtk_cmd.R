#' @title Logical check if a DCMTK function is available
#' @description Uses \code{Sys.which} to check if command is in the PATH.
#' @param cmd DCMTK command name
#' @return Logical \code{TRUE} if command is available.
#' @export
#' @examples
#' have_dcmtk_cmd("dcmodify")
have_dcmtk_cmd = function(cmd){
  res = dcmtk_cmd_path(cmd)
  ret = res != ""
  return(ret)
}


#' @rdname have_dcmtk_cmd
#' @export
check_dcmtk_cmd = function(cmd){
  ret = have_dcmtk_cmd(cmd)
  run_str = paste0("DCMTK command ", cmd, " not found! ",
                   "If DCMTK not installed system wide, ",
                   "run install_dcmtk()!")
  if (!ret) {
    stop(run_str)
  } else {
    res = dcmtk_cmd_path(cmd)
    if (!file.exists(res)) {
      stop(run_str)
    }
  }
  return(invisible(NULL))
}
