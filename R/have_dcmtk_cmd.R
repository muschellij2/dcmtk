#' @title Logical check if a DCMTK function is available
#' @description Uses \code{Sys.which} to check if command is in the PATH.
#' @param cmd DCMTK command name
#' @return Logical \code{TRUE} if command is available.
#' @export
#' @examples
#' have_dcmtk_cmd("dcmodify")
have_dcmtk_cmd = function(cmd){
  res = dcmtk_cmd(cmd)
  ret = res != ""
  return(ret)
}


#' @rdname have_dcmtk_cmd
check_dcmtk_cmd = function(cmd){
  ret = have_dcmtk_cmd(cmd)
  run_str = paste0("DCMTK command ", cmd, " not found!")
  if (!ret) {
    stop(run_str)
  } else {
    res = dcmtk_cmd(cmd)
    if (!file.exists(res)) {
      stop(run_str)
    }
  }
  return(invisible(NULL))
}
