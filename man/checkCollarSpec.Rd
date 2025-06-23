\name{checkCollarSpec}
\alias{checkCollarSpec}
\title{checkCollarSpec}
\description{create a data.frame that specifies collar, area, volume and tlag for each iChunk}
\usage{checkCollarSpec(dsChunk, collar_spec)}
\arguments{
  \item{dsChunk}{tibble or data.frame with column iChunk that identifies
a measurement cycle}
  \item{collar_spec}{data.frame with a row for each unique `collar`,
that specifies the colums `area` (m2) and `volume` (m3)
as well a column `tlag` (s).}
}



\author{Thomas Wutzler, Oscar Perez Priego}





