\name{read82summary}
\alias{read82summary}
\title{read82summary}

\usage{read82summary(fName, chamber_for_port = seq(1L, 
    12L), ..., tz = "UTC", na = c("", "NA", 
    "NAN", "\\"NAN\\"", "-9999"))}
\arguments{
  \item{fName}{scalar string: file name}
  \item{chamber_for_port}{chamber ids indexed by port number.
defaults to port number itself}
  \item{\dots}{further arguments to \code{\link{read_csv}}}
  \item{tz}{specify a time zone when converting to POSIXct,
default: UTC}
  \item{na}{see \code{\link{read.table}}}
}



\author{Thomas Wutzler, Oscar Perez Priego}




\examples{
fName <- system.file("genData/82m-0147-20220125000000_dense_summary.csv", package = "RespChamberProc")
ds = ds0 = read82summary(fName)
str(ds)
}
