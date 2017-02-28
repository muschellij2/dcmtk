#' @title Get path for DCMTK command
#' @description Uses \code{Sys.which} to check if command is in the PATH.
#' @param cmd DCMTK command name
#' @return Logical \code{TRUE} if command is available.
#' @export
#' @examples
#' if (!have_dcmtk_cmd("dcmodify")) {
#' install_dcmtk()
#' }
#' dcmtk_cmd("dcmodify")
dcmtk_cmd_path = function(cmd){
  check_cmd = Sys.which(cmd)
  if (check_cmd == "") {
    dcmtk_dir = getOption("dcmtk.path")
    if (!is.null(dcmtk_dir)) {
      check_cmd = file.path(dcmtk_dir, "bin", cmd)
      if (!file.exists(check_cmd)) {
        check_cmd = ""
      }
    }
    if (check_cmd == "") {
      check_cmd = system.file("bin", cmd, package = "dcmtk")
    }
  }
  if (is.null(check_cmd)) {
    check_cmd = ""
  }
  return(check_cmd)
}
