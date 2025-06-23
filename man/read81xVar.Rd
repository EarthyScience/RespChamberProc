\name{read81xVar}
\alias{read81xVar}
\title{read81xVar}
\description{Read a Licor generated .x81 file into a data-frame with guessing initial rows}
\usage{read81xVar(fName, sep = "\\t", ..., colsTimeStamp = 3, 
    formatTS = NULL, tz = "UTC", na.strings = c("", 
        "NA", "NAN", "\\"NAN\\""), labelID = "Label:", 
    iChunkBase = 0L)}
\arguments{
  \item{fName}{scalar string: file name
file information}
  \item{sep}{column separator}
  \item{\dots}{further arguments to \code{\link{read.table}}}
  \item{colsTimeStamp}{integer vector: colums with time stamp column
(will be set to POSIXct)}
  \item{formatTS}{format of the timestamp columns,
see \code{\link{strptime}}}
  \item{tz}{specify a time zone when converting to POSIXct,
default: UTC}
  \item{na.strings}{see \code{\link{read.table}}}
  \item{labelID}{string at the start of lines indicating the label}
  \item{iChunkBase}{added to generatged iChunk_raw =  1:n_chunk
Make sure to pass integer (literal L) for integer iChunk column}
}
\details{CAUTION: This parses a proprietary format as it was reasonably well working
at the time of development of the function. The function may skip important
meta-information. Further, it may use data-rows that are not meant to be
part of the concentration fitting.
If you are more familiar with the format, please suggest improvements
as an issue at the RespChamberProc repository.}


\author{Thomas Wutzler, Oscar Perez Priego}



\seealso{\code{\link{readDat}}}
\examples{
#fName <- "inst/genData/Flux2_140929_1700.81x"
#fName <- "inst/genData/Flux2_140929_1700.81x"
fName <- system.file(
  "genData/Flux2_140929_1700.81x", package = "RespChamberProc")
if (nzchar(fName)) {
  ds <- read81xVar(fName)
  #plot( CO2 ~ Date, ds )
  #plot( CO2 ~ Date, ds[ds$iChunk == 9,] )
}
}
