#' @title Install DCMTK
#' @description Installs the DICOM ToolKit
#'
#' @param type Which format of dcmtk should be downloaded.  If not specified,
#' it will be chosen from the platform
#' @param force Should installing be forced even if folders exist?
#' @param install_dir Installation Directory
#' @param version version of DCMTK to install
#' @param arch architecture to install, either 64 or 32
#'
#' @return Logical if installed
#' @export
#'
#' @importFrom utils download.file unzip untar
#' @importFrom tools file_ext
#' @rdname aaa_install_dcmtk
#' @examples
#' in_ci <- function() {
#'   nzchar(Sys.getenv("CI"))
#' }
#'
#' install_dir =  tempfile()
#' dir.create(install_dir, showWarnings = FALSE, recursive = TRUE)
#' res = try({
#' install_dcmtk(install_dir = install_dir)
#' })
#' if (inherits(res, "try-error")) {
#' res = FALSE
#' }
#' if (in_ci() & !res) {
#'   # source_install_dcmtk(install_dir = install_dir)
#' }
#'
#' type =   type = c("osx", "linux_static", "linux_dynamic", "windows")
#' arch = c("x86_64", "i686")
#' version = c( "3.6.3", "3.6.0")
#' eg = expand.grid(type = type, arch = arch, version = version,
#' stringsAsFactors = FALSE)
#' lists = apply(eg, 1, as.list)
#' eg$filename = sapply(lists, function(x) {
#' do.call(dcmtk_filename, args = x)
#' })
install_dcmtk = function(
  type = c("osx",
           "linux_static",
           "linux_dynamic",
           "windows"),
  force = FALSE,
  version = c("3.6.3", "3.6.0"),
  arch = c("x86_64", "i686"),
  install_dir = system.file(package = "dcmtk")
) {
  # type = "osx"

  sysname = tolower(Sys.info()["sysname"])
  if (missing(type)) {
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
  version = match.arg(version)

  base_url = dcmtk_ftp_url(version)
  arch = match.arg(arch)

  filename = dcmtk_filename(type, arch, version)
  url = paste0(base_url, filename)


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
    if (sysname %in% c("linux", "darwin")) {
      for (ifile in out_files) {
        system(sprintf("chmod +x %s", ifile))
      }
    }
  }
  return(all(file.exists(out_fols)))
}


#' @export
#' @rdname aaa_install_dcmtk
dcmtk_filename = function(type, arch, version) {
  ending = ""
  if (version == "3.6.0") {
    # windows was 386
    if (type == "windows") {
      arch = "i386"
      arch = paste0("win32-", arch)
    } else {
      # only 686 versions out
      arch = "i686"
      ending = ""
      # had endings
      if (grepl("static", type) ||
          grepl("osx", type)) {
        ending = "-static"
      }
      if (grepl("dynamic", type)) {
        ending = "-dynamic"
      }
    }
    if (type == "osx") {
      arch = paste0("mac-", arch)
    }
  } else {
    if (type == "windows") {
      if (arch == "i686") {
        arch = paste0("win32-", "dynamic")
      } else {
        arch = paste0("win64-", "dynamic")
      }
    } else {
      arch = "x86_64"
      if (grepl("dynamic", type) ||
          grepl("osx", type)) {
        ending = ""
      } else if (grepl("static", type)) {
        ending = "-static"
      }
    }
    if (type == "osx") {
      arch = paste0("macosx-", arch)
    }
  }
  start = paste0("dcmtk-", version, "-")
  filename = switch(
    type,
    osx = paste0(arch, ending, ".tar.bz2"),
    linux_dynamic = paste0("linux-", arch, ending, ".tar.bz2"),
    linux_static = paste0("linux-", arch, ending, ".tar.bz2"),
    windows = paste0(arch, ".zip")
  )
  filename = paste0(start, filename)
  return(filename)
}



#' @export
#' @rdname aaa_install_dcmtk
dcmtk_ftp_url = function(version) {

  fol = gsub("[.]", "", version)
  if (nchar(fol) != 3) {
    warning("If giving version, should be 3 numbers: major.minor.fix")
  }
  base_url = paste0("ftp://dicom.offis.de/pub/dicom/offis/software",
                    "/dcmtk/", "dcmtk", fol, "/bin/")
  return(base_url)
}

#' @export
#' @param cmake_opts options for CMAKE \code{cmake} command.
#' Can be a character vector or character scalar.
#' For example, if want to use \code{ICU} versus \code{libiconv},
#' use \code{cmake_opts = "-DDCMTK_ENABLE_CHARSET_CONVERSION=ICU"}
#' @rdname aaa_install_dcmtk
#' @importFrom git2r clone checkout
source_install_dcmtk = function(
  type = c("osx",
           "linux",
           "windows"),
  version = c( "3.6.3", "3.6.0"),
  install_dir = system.file(package = "dcmtk"),
  cmake_opts = NULL
) {
  sysname = tolower(Sys.info()["sysname"])
  if (missing(type)) {
    type = switch(sysname,
                  "linux" = "linux",
                  "darwin" = "osx",
                  "windows" = "windows"
    )
  }
  type[grepl("linux", type) & type != "linux"] = "linux"
  type[type == "darwin"] = "osx"
  type = match.arg(
    type,
    choices = c(
      "osx",
      "linux",
      "windows"))
  version = match.arg(version)

#   install_dir = tempfile()
#   cmake_opts = "-DDCMTK_ENABLE_CHARSET_CONVERSION=ICU"
  dir.create(install_dir, showWarnings = FALSE, recursive = TRUE)
  tdir = tempfile()
  dir.create(tdir)
  repo = git2r::clone("https://github.com/DCMTK/dcmtk", local_path = tdir)

  gh_version = c(
    "3.6.4" = "1967b13134308f311e6a827e616958c6a4da5bc9",
    "3.6.3" = "1f20e10ca48ebcbf4cb3e9fd1aae4ce6eaee0609",
    "3.6.0" = "79892c9c325be0fe1060858b0c5622989ce0aa26")
  commit_id = match(version[1], names(gh_version))
  if (!is.na(commit_id)) {
    commit_id = gh_version[commit_id]
    git2r::checkout(repo, branch = commit_id)
  }
  build_dir = tempfile()
  dir.create(build_dir)
  owd = getwd()
  on.exit({
    setwd(owd)
  })
  setwd(build_dir)
  cmake_opts = c(paste0("-DCMAKE_INSTALL_PREFIX=", install_dir),
                 cmake_opts)
  cmake_opts = paste(cmake_opts, collapse = " ")
  cmd = paste("cmake", cmake_opts, tdir)
  res = system(cmd)
  if (res != 0) {
    warning("CMake install non-zero exit status")
  }
  make_cmd = "make"
  res = system(make_cmd)
  if (res != 0) {
    warning("Make returned non-zero status")
  }

  make_install_cmd = "make install"
  res = system(make_install_cmd)
  if (res != 0) {
    warning("Make install non-zero exit status")
  }
  fols = c("bin", "share",
           "etc")
  out_fols = file.path(install_dir, fols)
  return(all(file.exists(out_fols)))

}