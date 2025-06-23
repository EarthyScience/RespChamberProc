#' @export
calcClosedChamberFluxForChunks <- function(
  ### apply \code{\link{calcClosedChamberFlux}} for each chunk in data
  dsChunk                    ##<< tibble or data.frame
  , colChunk = "iChunk"  ##<< column name (scalar string) holding a factor
    ## unique to each chunk
  , ...                 ##<< further arguments to
    ## \code{\link{calcClosedChamberFlux}}
  , volume              ##<< volume inside the chamber im [m3]
  , volumesByChunk      ##<< tibble or data.frame with column <colChunk> and
    ## column  \code{volume} giving a volume for each chunk identifier
    ## By default, value of argument volume is used for eah chunkg
    ## Allows for adjusting the chamber volume across different chunks
    ## (argument \code{volumne} in \code{\link{calcClosedChamberFlux}})
  , isVerbose = TRUE    ##<< set to FALSE to avoid messages
){
  warning("calcClosedChamberFluxForChunks is superseded by ",
          "calcClosedChamberFluxForChunkSpecs. Consider it instead.")
  if (missing(volumesByChunk)) {
    dsVol <- dsChunk
    dsVol$volume <- volume
  } else {
    if (!is.data.frame(volumesByChunk)) stop(
      "argument volumesByChunk must be a data.frame")
    requiredColumnNames <- c(colChunk,"volume")
    iMissingCol <- which(!(requiredColumnNames %in% names(volumesByChunk)))
    if (length(iMissingCol) ) stop(
      "Missing columns in dataframe of argument volumesByChunk: "
      , paste(requiredColumnNames[iMissingCol], collapse = ","))
    if (max(table(volumesByChunk[[colChunk]])) > 1L) stop(
      "values in column ", colChunk, " in volumesByChunk must be unique.")
    dsVol <- suppressWarnings(
      left_join(
        dsChunk
        , select(volumesByChunk, !!!syms(c(colChunk, "volume")))
        , colChunk))
    if (nrow(dsVol) != nrow(dsChunk)) stop(
      "could not assign unique volumes to chunks.")
    if (!all(is.finite(dsVol$volume)) ) {
      chunksMissing <- unique(dsVol[[colChunk]][ !is.finite(dsVol$volume)])
      stop("need to provide a finite volume for each chunk. Check chunks "
           , paste(chunksMissing, collapse = ","))
    }
  }
  #. <- filter_(dsVol, paste0(colChunk,"==",colChunk,"[1]"))
  ans <- dsVol %>% group_by( !!sym(colChunk) ) %>%
      do({
            iChunk <- as.character(.[[colChunk]][1])
            vol <- .$volume[1]
            .collar <- .$collar[1]
            mutate(calcClosedChamberFlux(.,...,volume = vol), collar = .collar)
          })
  names(ans)[names(ans) == "iChunk"] <- colChunk
  ##value<< a tibble with a row for each measurement cycle and additional
  ## column <colChunk> identifying the measurement cycle
  ans
}

#' @export
checkCollarSpec <- function(
  ### create a data.frame that specifies collar, area, volume and tlag for each iChunk
  dsChunk        ##<< tibble or data.frame with column iChunk that identifies
  ## a measurement cycle
  , collar_spec  ##<< data.frame with a row for each unique `collar`,
  ## that specifies the colums `area` (m2) and `volume` (m3)
  ## as well a column `tlag` (s).
){
  # check that there is a unique collar for each iChunk
  collar_map <- dsChunk %>% distinct(iChunk, collar)
  dsFail <- collar_map %>% group_by(iChunk) %>%
    summarize(n_collar = length(unique(collar))) %>%
    ungroup() %>% filter(n_collar != 1)
  #dsFail <- collar_map %>% group_by(iChunk) %>% summarize(n_collar = length(unique(collar))) %>% head(3)
  if (nrow(dsFail) > 0) return(structure(FALSE, msg = paste(
    "Expected collar_map to associate unique collars to each iChunk. ",
    "But got not unique collars for iChunks ",
    paste(dsFail$iChunk, collapse=","))))
  # check if all necessary columns are present
  required_columns <- c("collar","area","volume","tlag")
  missing_cols = setdiff(required_columns, names(collar_spec))
  #missing_cols = setdiff(c("collar","area","volume","lag","non1","non2"), names(collar_spec))
  if (length(missing_cols)) return(structure(FALSE, msg = paste(
    "Expected collar_spec to provide following missing columns ",
    paste(missing_cols, collapse=","))))
  # check if collar_spec has a unique row for each chamber
  collar_spec_sel <- select(collar_spec, all_of(required_columns))
  ds_join <- collar_map %>% left_join(
    collar_spec, by = join_by(collar), relationship = "many-to-many")
  dsFail <- ds_join %>% group_by(iChunk,collar) %>%
    summarize(n_collar = n()) %>% filter(n_collar != 1)
  #dsFail <- ds_join %>% group_by(collar) %>% summarize(n_collar = length(unique(collar))) %>% ungroup() %>% head(3)
  if (nrow(dsFail) > 0) return(structure(FALSE, msg = paste(
    "Expected collar_spec to associate unique specification for each collar ",
    "But got not unique specification for collars ",
    paste(unique(dsFail$collar), collapse=","))))
  # check if volume is present for each iChunk
  ds_noV <- filter(ds_join,is.na(volume))
  if (nrow(ds_noV)) return(structure(FALSE, msg = paste(
    "Expected finite volume for each iChunk, but did not specify volume for iChunks", paste(ds_noV$iChunk, collapse=","), "of collars ", paste(unique(ds_noV$collar), collapse=","))))
  # check row numbers
  if (nrow(ds_join) != nrow(collar_map)) return(structure(FALSE, msg = paste(
    "Expected joining collar_spec to keep number of records, but changed.")))
  if (exists("msg")) stop("Should have returned earlier")
  structure(TRUE, msg="ok")
}


#' @export
calcClosedChamberFluxForChunkSpecs <- function(
  ### apply \code{\link{calcClosedChamberFlux}} for each chunk in data
  dsChunk           ##<< tibble or data.frame with columns iChunk and collar
  ,collar_spec      ##<< data.frame with area, volume, tlag for each collar
  ## as created by \code{\link{setupCollarSpec}}
  , ...               ##<< further arguments to
  ## \code{\link{calcClosedChamberFlux}}
  , isVerbose = TRUE  ##<< set to FALSE to avoid messages
){
  dsChunkNested <- dsChunk %>% nest(.by=c(iChunk, collar))
  dsChunkSpec <- dsChunkNested %>% left_join(collar_spec, by=join_by(collar))
  if (nrow(dsChunkSpec) != nrow(dsChunkSpec) || any(is.na(dsChunkSpec$volume)))  {
    isValid <- checkCollarSpec(dsChunk, collar_spec)
    if (!isValid) stop(attr(isValid, "msg")) else stop("Problem joining ")
  }
  f_chunk <- function(dss, area, volume, tlag, ...){
    res <- calcClosedChamberFlux(
      dss, area=area, volume=volume, useFixedTLag = tlag, ...)
  }
  ans <- dsChunkSpec  %>%
    mutate(res = future_pmap(
      list(data, area, volume, tlag), f_chunk, ..., .options=furrr_options(seed = NULL))) %>%
    select(iChunk, collar, res) %>% unnest(cols = c(res))
  ##value<< a tibble with a row for each measurement cycle and additional
  ## column <colChunk> identifying the measurement cycle
  ans
}
