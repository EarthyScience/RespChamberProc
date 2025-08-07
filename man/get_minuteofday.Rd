\name{get_minuteofday}
\alias{get_minuteofday}
\title{get minuteofday}
\description{compute the accumulated minute since midnight}
\usage{get_minuteofday(t)}
\arguments{
  \item{t}{An time that can be interpreted by as.POSIXlt}
}

\value{minutes after midnight in [0,24*60-1]}

\author{Thomas Wutzler, Oscar Perez Priego}




\examples{
t <- as.POSIXctUTC(c("2014-06-23 00:00:01","2014-06-23 05:12:01","2014-06-23 23:59:01"))
get_minuteofday(t)
}
