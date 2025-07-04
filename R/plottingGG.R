plotCampaignConcSeries <- function(
  ### get a series of ggplots of the time series and its fits
  ds                     ##<< data frame to plot, with columns \code{colChunk}
    ## , \code{collar}, \code{timeCol} and \code{varName}
  , dsFits = NULL        ##<< tibble of results of
    ## \code{\link{calcClosedChamberFlux}}
    ## with columns <colChunk>, flux, sdFlux, model,
  , varName = "CO2_dry"  ##<< variable to plot
  , colChunk = "iChunk"  ##<< column name of identifier of one time series
  , timeCol = "TIMESTAMP"##<< column name of the time collumn
  , qualityFlag = 0      ##<< vector of length unique(ds[[colChunk]]):
    ## quality flag. For chunks where
    ## this flag is not 0, subplots are dimmed.
  , fTextBR = NULL       ##<< function(resFit) to add some text to the
    ## bottom right of the plot, by default the rSquared from fitting object
  , fTextTL = NULL       ##<< function(resFit) to add some text to the
    ## top left of the plot, by default the autocorrelation from fitting object
  , fTextTR = NULL       ##<< function(resFit) to add a second text to the
    ## top right of the plot, by default the provided quality flag,
    ## where it is not zero
  , fTextHead = NULL     ##<< function(resFit) to specify the facet heading text
    ## defaults to \code{paste0(resFit[[colChunk]],"/", resFit$collar)}
  , plotsPerPage = 64    ##<< number of plots per page
  , fileName = ""        ##<< if non-zero length string, the fileName where
    ## all plots are printed to  #paste0(varName,".pdf")
  , ggplotList = c()     ##<< list added to each ggplot.
  , ylabel = paste0(varName," (",yunit,")")    ##<< y-label,
    ## may provide a string of the unit
  , yunit = ""           ##<< scalar string, part of default y label
  , isVerbose = TRUE     ##<< set to FALSE to avoid messages
  , prepent_doi = TRUE   ##<< set to FALSE to not prepend the yday to the ActiveCycle
){
  if (!requireNamespace("ggplot2")) stop(
    "package ggplot2 must be installed for this function to work.")
  # do not clutter the function declaration with these long defaults,
  # better assign defaults in body:
  if (!length(fTextBR) ) fTextBR <- function(resFit){
    ifelse( is.finite(resFit$r2), format(resFit$r2,digits = 2), "")}
  if (!length(fTextTL) ) fTextTL <- function(resFit){
    ifelse( is.finite(resFit$autoCorr), format(resFit$autoCorr,digits = 2), "")}
  if (!length(fTextTR) ) fTextTR <- function(resFit){
    ifelse( length(resFit$qf) & resFit$qf != 0, as.character(resFit$qf), "")}
  #iCamp <- 1
  #dss <- subset(ds, campaign==1 & Chamber==1)
  uniqueId <- unique(ds[[colChunk]])
  if (is.null(fTextHead)) {
    fTextHead <- if (prepent_doi) function(resFit){
      doi <- as.POSIXlt(resFit$timestamp)$yday
      paste0(doi,":",resFit[[colChunk]], "/", resFit$collar)
    } else function(resFit) {
      paste0(resFit[[colChunk]], "/", resFit$collar)
    }
  }
  id_labels <- structure(fTextHead(dsFits), names = paste(dsFits[[colChunk]]))
  N <- length(uniqueId)
  ds$id <- factor(ds[[colChunk]])  # drop unused factor levels
  idu <- unique(ds$id)
  #if (length(resL) && is.null(names(resL)) ) names(resL) <- levels(ds$id)
  if (length(qualityFlag) == 1L) qualityFlag <- rep(qualityFlag, N)
  if (length(qualityFlag) != N) warning(
    "provided quality flag with different length than than number of "
    , "measurement cycles in data.")
  # join quality flag to ds and to dsFits
  # factor to get discrete color scale
  dsQf <- data.frame(id = uniqueId, qf = factor(qualityFlag))
  ds <- suppressWarnings(left_join(ds, dsQf, by = "id"))
  dsQf[[colChunk]] <- dsQf$id; dsQf$id <- NULL
  dsFits <- suppressWarnings(left_join(dsFits, dsQf, by = colChunk))
  dsFin <- filter(ds, is.finite(!!sym(varName)))
  colCodes <- structure(
    rep("lightgray", length(levels(dsFin$qf)) ), names = levels(dsFin$qf))
  colCodes["0"] <- "black"
  #colCodes[uniqueQf==10] <- "darkgrey"
  #(as.numeric(unique(ds$id))-1)%/%plotsPerPage+1
  #dsp <- as_tibble(cbind(iPage =
  #factor((as.numeric(dsFin$id) - 1) %/% plotsPerPage + 1), dsFin))
  dsp <- as_tibble(dsFin)
  dsp$iPage = as.integer((as.numeric(dsp$id) - 1L) %/% plotsPerPage + 1L)
  if (isVerbose ) message(paste(
    "Number of pages (each ",plotsPerPage," plots): "
    , length(unique(dsp$iPage))), sep = "" )
  #dss <- filter(dsp, iPage == 1)
  plotPage <-  function(dss){
    idsPage <- unique(dss$id)
    if (isVerbose) message(paste(idsPage, collapse = ","))
    # calculate times0
    dss <- dss %>%
      group_by(!!sym("id")) %>%
      arrange(!!sym(timeCol))  %>% # sort ascending time within each
      mutate(times0 =
                as.numeric(UQ(sym(timeCol))) - as.numeric(UQ(sym(timeCol))[1]))
    #dss <- dsc
    p1 <- ggplot2::ggplot( dss, ggplot2::aes(x = times0, y = .data[[varName]]) ) +
      ggplot2::geom_point(
        shape = 1, ggplot2::aes(col = qf), na.rm = TRUE) +
      #ggplot2::facet_wrap( ~id, scales = "free") +
      ggplot2::facet_wrap(
        ~id, scales = "free", labeller = ggplot2::labeller(id = id_labels)) +
      ggplot2::scale_color_manual(values = colCodes, guide  =  "none") +
      ggplot2::xlab("time (s)") + ggplot2::ylab(ylabel) +
      ggplot2::theme_bw(base_size = 9) +
      #ggplot2::theme(panel.grid.minor = element_blank())
      ggplot2::theme(panel.grid = ggplot2::element_blank())
    if (length(dsFits)) {
      dsFitsPage <- filter(dsFits, UQ(sym(colChunk)) %in% !!idsPage)
      dsFitsPage$id <- dsFitsPage[[colChunk]]
      #. <- unlist(filter_(dsFitsPage, ~iChunk==4 ), recursive  = FALSE)
      # rowwise coerces a one-row tibble to a list, with list columns expanded
      # so that no [[1]] is needed
      fExtract <- function(.){
        if (!length(.$model)) return(data.frame())
        data.frame(
          id = .[[colChunk]]
          , fitted = fitted(.$model)
          , times0 = .$tLag + .$times
        )}
      dfFitted <- dsFitsPage %>% rowwise() %>% do( fExtract(.))
      dfTextBR <- data.frame(id = dsFitsPage[[colChunk]], text = fTextBR(dsFitsPage))
      dfTextTL <- data.frame(id = dsFitsPage[[colChunk]], text = fTextTL(dsFitsPage))
      dfTextTR <- data.frame(id = dsFitsPage[[colChunk]], text = fTextTR(dsFitsPage))
      p1b <-
        p1 +
        ggplot2::geom_vline(
          data = select(ungroup(dsFitsPage), !!sym("tLag"), !!sym("tmax"), !!sym("id"))
          , ggplot2::aes(xintercept = tLag)
          , color = "darkgrey", linetype = "dashed", na.rm = TRUE) +
        ggplot2::geom_vline(
          data = select(ungroup(dsFitsPage), !!sym("tLag"), !!sym("tmax"), !!sym("id"))
          , ggplot2::aes(xintercept = tmax)
          , color = "darkgrey", linetype = "dashed", na.rm = TRUE) +
        {
            if (length(dfFitted)) {
              ggplot2::geom_line(
                data = dfFitted, ggplot2::aes(y = fitted)
                , col = "red", na.rm = TRUE  )
              } else c()
            } +
        ggplot2::geom_text(
          data = dfTextBR, ggplot2::aes(label = text)
          , x = +Inf, y = -Inf, hjust = 1.05, vjust = 0, na.rm = TRUE) +
        ggplot2::geom_text(
          data = dfTextTL, ggplot2::aes(label = text)
          , x = -Inf, y = +Inf, hjust = 0, vjust = 1, na.rm = TRUE) +
        ggplot2::geom_text(
          data = dfTextTR, ggplot2::aes(label = text)
          , x = +Inf, y = +Inf, hjust = 1, vjust = 1, na.rm = TRUE) +
        ggplot2::theme()
    }
    p1b
  }
  # warning on unequal factor levels
  # dsPlots_depr <- suppressWarnings(
  #   dsp %>%
  #     group_by(!!sym("iPage")) %>%
  #     do( tibble::tibble(plot = list(plotPage(.))) )
  #   )
  dsPlots <- dsp %>%
    group_by(!!sym("iPage")) %>%
    summarize(nrow = n(), plot = list(plotPage(pick(everything()))))
  if (nzchar(fileName)) {
    f_row <- function(iPage, plot){
      if (isVerbose ) message(",",iPage, appendLF = FALSE)
      print(plot)
      invisible(iPage)
    }
    pdf(width = 11.7,height = 8.3,file = fileName)
    on.exit(dev.off())
    dsPlots %>% rowwise() %>% summarize(tmp = f_row(iPage, plot))
    #dsPlots %>% rowwise() %>% summarize(tmp = f_row2(pick(everything()))) # error on list columns
    if (isVerbose ) message("\nprinted plots to file ",fileName)
  }
  ##value<< a tibble with columns page and plot, the second column
  ##holding plotting objects
  ## \code{dsPlots %>% rowwise() %>% do({print(.$plot)})}
  dsPlots
}


