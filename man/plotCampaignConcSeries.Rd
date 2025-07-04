\name{plotCampaignConcSeries}
\alias{plotCampaignConcSeries}
\title{plotCampaignConcSeries}
\description{get a series of ggplots of the time series and its fits}
\usage{plotCampaignConcSeries(ds, dsFits = NULL, 
    varName = "CO2_dry", colChunk = "iChunk", 
    timeCol = "TIMESTAMP", qualityFlag = 0, 
    fTextBR = NULL, fTextTL = NULL, fTextTR = NULL, 
    fTextHead = NULL, plotsPerPage = 64, 
    fileName = "", ggplotList = c(), ylabel = paste0(varName, 
        " (", yunit, ")"), yunit = "", isVerbose = TRUE, 
    prepent_doi = TRUE)}
\arguments{
  \item{ds}{data frame to plot, with columns \code{colChunk}
, \code{collar}, \code{timeCol} and \code{varName}}
  \item{dsFits}{tibble of results of
\code{\link{calcClosedChamberFlux}}
with columns <colChunk>, flux, sdFlux, model,}
  \item{varName}{variable to plot}
  \item{colChunk}{column name of identifier of one time series}
  \item{timeCol}{column name of the time collumn}
  \item{qualityFlag}{vector of length unique(ds[[colChunk]]):
quality flag. For chunks where
this flag is not 0, subplots are dimmed.}
  \item{fTextBR}{function(resFit) to add some text to the
bottom right of the plot, by default the rSquared from fitting object}
  \item{fTextTL}{function(resFit) to add some text to the
top left of the plot, by default the autocorrelation from fitting object}
  \item{fTextTR}{function(resFit) to add a second text to the
top right of the plot, by default the provided quality flag,
where it is not zero}
  \item{fTextHead}{function(resFit) to specify the facet heading text
defaults to \code{paste0(resFit[[colChunk]],"/", resFit$collar)}}
  \item{plotsPerPage}{number of plots per page}
  \item{fileName}{if non-zero length string, the fileName where
all plots are printed to  #paste0(varName,".pdf")}
  \item{ggplotList}{list added to each ggplot.}
  \item{ylabel}{y-label,
may provide a string of the unit}
  \item{yunit}{scalar string, part of default y label}
  \item{isVerbose}{set to FALSE to avoid messages}
  \item{prepent_doi}{set to FALSE to not prepend the yday to the ActiveCycle}
}

\value{a tibble with columns page and plot, the second column
holding plotting objects
\code{dsPlots \%>\% rowwise() \%>\% do({print(.$plot)})}}

\author{Thomas Wutzler, Oscar Perez Priego}





