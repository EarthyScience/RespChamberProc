\name{read82z}
\alias{read82z}
\title{read82z}
\description{Read and combine all csv file insides a Licor generated .zip file into a tibble}
\usage{read82z(fName, temp_base_dir = tempdir(), 
    ..., iChunkBase = 0L, catch_error = TRUE)}
\arguments{
  \item{fName}{scalar string: file name}
  \item{temp_base_dir}{temporary directory where the zip file is
extracted to a subdirectory, recommended to specify
a location on an in-memory file system, such as '/dev/shm'.}
  \item{\dots}{further arguments to \code{\link{read82z_single}}}
  \item{iChunkBase}{offset for chunk identifies}
  \item{catch_error}{set to FALSE to quit on error instead of warning}
}
\details{Inside the zip file is a directory structure of further zip files of
Chunks of concentration measurements.
All these files are unzipped to a temporary directory (best specify an
in memory temp_base_dir), and read and combined with assigning a
consecutive integer iChunk identifier.}


\author{Thomas Wutzler, Oscar Perez Priego}




\examples{
fName = "develop/x82_cases/licor82.zip"
ds = read82z(fName, temp_base_dir = "/dev/shm")
str(ds)
}
