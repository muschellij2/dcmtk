% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dcmdump.R
\name{dcmdump}
\alias{dcmdump}
\alias{dcmdump_full}
\title{DICOM Dump}
\usage{
dcmdump(file, ...)

dcmdump_full(file, ...)
}
\arguments{
\item{file}{DICOM file name}

\item{...}{options passed to \code{\link{dcmtk_cmd}}, other than
\code{intern}}
}
\value{
Character vector of information
}
\description{
Wrapper of \code{dcmdump} from DCMTK
}
\note{
\code{dcmdump_full} uses the
\code{frontopts = "-q --print-all --load-short --print-filename"} by
default
}
\examples{
if (have_dcmtk_cmd("dcmdump")) {
  file = system.file("extdata", "example.dcm", package = "dcmtk")
  dcmdump(file)
}
}
