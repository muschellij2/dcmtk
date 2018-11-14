#' Install DCMTK
#' @description Installs the DICOM ToolKit
#'
#' @param type Which format of dcmtk should be downloaded.  If not specified,
#' it will be chosen from the platform
#' @param force Should installing be forced even if folders exist?
#' @param install_dir Installation Directory
#'
#' @return Logical
#' @export
#'
#' @importFrom utils download.file unzip untar
#' @importFrom tools file_ext
#' @examples
#' install_dir =  tempfile()
#' dir.create(install_dir, showWarnings = FALSE, recursive = TRUE)
#' install_dcmtk(install_dir = install_dir)
install_dcmtk = function(
  type = c("osx",
           "linux_static",
           "linux_dynamic",
           "windows"),
  force = FALSE,
  install_dir = system.file(package = "dcmtk")
) {
  # type = "osx"

  if (missing(type)) {
    sysname = tolower(Sys.info()["sysname"])
    type = switch(sysname,
                  "linux" = "linux_static",
                  "darwin" = "osx",
                  "windows" = "windows"
    )
  }
  type = match.arg(
    type,
    choices = c(
      "osx",
      "linux_static",
      "linux_dynamic",
      "windows"))
  base_url = paste0("ftp://dicom.offis.de/pub/dicom/offis/software",
                    "/dcmtk/dcmtk360/bin/")

  filename = switch(
    type,
    osx = "dcmtk-3.6.0-mac-i686-static.tar.bz2",
    linux_dynamic = "dcmtk-3.6.0-linux-i686-dynamic.tar.bz2",
    linux_static = "dcmtk-3.6.0-linux-i686-static.tar.bz2",
    windows = "dcmtk-3.6.0-win32-i386.zip"
  )
  url = paste0(base_url, filename)


  install_dir = system.file(package = "dcmtk")
  fols = c("bin", "share",
           # "lib", "include",
           "etc")
  out_fols = file.path(install_dir, fols)

  if (!all(file.exists(out_fols)) || force) {

    destfile = file.path(
      install_dir,
      filename)
    dl = utils::download.file(url, destfile)
    if (dl != 0) {
      warning(paste0("Download indicated not successful - ",
                     "please rerun with force = TRUE if errors"))
    }
    ext = tools::file_ext(filename)
    if (ext == "zip") {
      files = utils::unzip(
        destfile,
        exdir = install_dir,
        list = TRUE)
      files = files$Name
    }
    if (ext == "bz2") {
      files = utils::untar(
        destfile,
        compressed = "bzip2",
        list = TRUE,
        exdir = install_dir)
    }
    fnames = strsplit(files, .Platform$file.sep)
    fol = sapply(fnames, `[`, 1)
    fol = unique(fol)
    stopifnot(length(fol) == 1)
    if (ext == "zip") {
      res = utils::unzip(
        destfile,
        exdir = install_dir)
    }
    if (ext == "bz2") {
      res = utils::untar(
        destfile,
        compressed = "bzip2",
        exdir = install_dir)
      if (res != 0) {
        warning("Untarring the download did not succeed correctly!")
      }
    }

    files = file.path(install_dir, fol, fols)
    out_files = file.path(install_dir, fols)

    file.rename(files, out_files)
    file.remove(destfile)
  }
  return(all(file.exists(out_fols)))
}