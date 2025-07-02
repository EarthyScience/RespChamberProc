\name{read82z_single}
\alias{read82z_single}
\title{read82z single}
\description{Read a csv file inside a Licor generated .82z file into a data-frame with guessing initial rows}
\usage{read82z_single(fName, ..., tz = "UTC", na = c("", 
    "NA", "NAN", "\\"NAN\\"", "-9999"), iChunk = 1L, 
    filter_states = c(5L))}
\arguments{
  \item{fName}{scalar string: file name}
  \item{\dots}{further arguments to \code{\link{read_csv}}}
  \item{tz}{specify a time zone when converting to POSIXct,
default: UTC}
  \item{na}{see \code{\link{read.table}}}
  \item{iChunk}{
}
  \item{filter_states}{set to NA to return all the data}
}
\details{CAUTION: This parses a proprietary format as it was reasonably well working
at the time of development of the function. The function may skip important
meta-information. Further, it may use data-rows that are not meant to be
part of the concentration fitting.
If you are more familiar with the format, please suggest improvements
as an issue at the RespChamberProc repository.

Filters for row with STATE \%in\% filter_states (defaults to 5L)
set to NA to return all rows}


\author{Thomas Wutzler, Oscar Perez Priego}



\seealso{\code{\link{readDat}}}
\examples{
fName = "develop/x82_cases/82m-0147-20220125000045.82z"
if (file.exists(fName)) {
  ds = read82z_single(fName, iChunk=2L)
  str(ds)
}
}
