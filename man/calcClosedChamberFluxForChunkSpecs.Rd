\name{calcClosedChamberFluxForChunkSpecs}
\alias{calcClosedChamberFluxForChunkSpecs}
\title{calcClosedChamberFluxForChunkSpecs}
\description{apply \code{\link{calcClosedChamberFlux}} for each chunk in data}
\usage{calcClosedChamberFluxForChunkSpecs(dsChunk, 
    collar_spec, ..., isVerbose = TRUE)}
\arguments{
  \item{dsChunk}{tibble or data.frame with columns iChunk and collar}
  \item{collar_spec}{data.frame with columns
\code{collar}, \code{area}, \code{volume}, \code{tlag}
with rows for each collar.
To supply parameters to \code{\link{calcClosedChamberFlux}}.}
  \item{\dots}{further arguments to
\code{\link{calcClosedChamberFlux}}}
  \item{isVerbose}{set to FALSE to avoid messages}
}

\value{a tibble with a row for each measurement cycle and additional
column <colChunk> identifying the measurement cycle}

\author{Thomas Wutzler, Oscar Perez Priego}





