read82z <- function(
  ### Read and combine all csv file insides a Licor generated .zip file into a tibble
  fName					          ##<< scalar string: file name
  , temp_base_dir = tempdir()   ##<< temporary directory where the zip file is
  ## extracted to a subdirectory, recommended to specify
  ## a location on an in-memory file system, such as '/dev/shm'.
  , ...					          ##<< further arguments to \code{\link{read82z_single}}
  , iChunkBase = 0L       ##<< offset for chunk identifies
  , catch_error = TRUE    ##<< set to FALSE to quit on error instead of warning
){
  ##details<<
  ## Inside the zip file is a directory structure of further zip files of
  ## Chunks of concentration measurements.
  ## All these files are unzipped to a temporary directory (best specify an
  ## in memory temp_base_dir), and read and combined with assigning a
  ## consecutive integer iChunk identifier.
  temp_dir = tempfile("read82z_",temp_base_dir)
  on.exit(unlink(temp_dir, recursive=TRUE))
  zFiles = unzip(fName, exdir = temp_dir)
  iChunks = iChunkBase + seq_along(zFiles)
  #zFile = zFiles[1]
  #iChunk = iChunks[1]
  #tmp = read82z_single(zFile, ..., iChunk = iChunk)
  read82z_single_call = if (catch_error) {
      function(zFile, iChunk) {
        tryCatch(
          read82z_single(zFile, ..., iChunk = iChunk),
          #read82z_single(zFile, iChunk = iChunk),
          error = function(e) {
            warning(e$message)
            tibble()
            })
          }
  } else {
    function(zFile, iChunk) read82z_single(zFile, ..., iChunk = iChunk)
  }
  dsl = map2(zFiles, iChunks, read82z_single_call)
             #function(zFile, iChunk) read82z_single(zFile, ..., iChunk = iChunk) )
  ds <- list_rbind(dsl)
}
attr(read82z,"ex") <- function(){
  fName = "develop/x82_cases/licor82.zip"
  ds = read82z(fName, temp_base_dir = "/dev/shm")
  str(ds)
}

read82z_single <- function(
  ### Read a csv file inside a Licor generated .82z file into a data-frame with guessing initial rows
  fName					          ##<< scalar string: file name
  , ...					          ##<< further arguments to \code{\link{read_csv}}
  , tz = "UTC"				    ##<< specify a time zone when converting to POSIXct,
  ## default: UTC
  , na = c('','NA','NAN','"NAN"','-9999') ##<< see \code{\link{read.table}}
  , iChunk = 1L
){
  ##seealso<< \code{\link{readDat}}
  ##details<<
  ## CAUTION: This parses a proprietary format as it was reasonably well working
  ## at the time of development of the function. The function may skip important
  ## meta-information. Further, it may use data-rows that are not meant to be
  ## part of the concentration fitting.
  ## If you are more familiar with the format, please suggest improvements
  ## as an issue at the RespChamberProc repository.
  lines_all <- readLines(unz(fName, "data.csv"))
  lines = lines_all[c(2,4:length(lines_all))] # skip 1st and 3rd row
  col_types = cols(
    DATE = col_character(), TIME = col_character(),
    CO2 = col_double(),
    CO2_DRY = col_double(),
    H2O = col_double(),
    # PA_CELL = col_double(),
    # T_CELL = col_double(),
    PA = col_double(),
    TA = col_double(),
    STATE = col_integer()
  )
  ds0 = read_csv(
    I(lines), col_types=col_types, col_select = all_of(names(col_types$cols)),
    name_repair = "minimal", progress=FALSE, na = na)
  attr(ds0,"spec") = NULL
  ds0$Date = lubridate::as_datetime(paste(ds0$DATE, ds0$TIME), tz = tz)
  # parsing the label from metadata.json: "LI-8250"/"PORT_LABEL"
  metadata = fromJSON(unz(fName, "metadata.json"))
  label = metadata$`LI-8250`$PORT_LABEL
  ##details<< Filters for row with STATE==5
  ds = as_tibble(ds0) %>%
    filter(STATE == 5) %>%
    mutate(Pa=PA*1000, iChunk=iChunk, label=label) %>% # convert from kPa to Pa
    select(iChunk, label, Date, CO2, CO2_dry=CO2_DRY, TA_Avg=TA, H2O, Pa)
}
attr(read82z_single,"ex") <- function(){
  fName = "develop/x82_cases/82m-0147-20220125000045.82z"
  ds = read82z_single(fName, iChunk=2L)
  str(ds)
}

