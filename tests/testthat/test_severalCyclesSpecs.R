#require(testthat)
#library(dplyr)
context("severalCyclesSpecs")

# fit chambers in parallel inside calcClosedChamberFluxForChunkSpecs
library(furrr)
plan(multisession, workers = 2) 

fName <- system.file(
  "genData/SMANIE_Chamber1_26032015.zip", package = "RespChamberProc")
if (nzchar(fName)) { 
  ds0 <- readDat(
    unz(fName, filename = unzip(fName, list = TRUE)[1,"Name"] ),tz = "UTC")
  ds <- ds0 %>% 
    #filter(TIMESTAMP < as.POSIXct("2015-03-26 06:19:01", tz = "UTC"))
    filter(Collar <= 2)
  ds$Pa <- ds$AirPres * 100  # convert hPa to Pa
  ds$CO2_dry <- corrConcDilution(ds, colConc = "CO2_LI840", colVapour = "H2O_LI840")
  ds$H2O_dry <- corrConcDilution(ds, colConc = "H2O_LI840", colVapour = "H2O_LI840")
  ds$VPD <- calcVPD( ds$SurTemp, ds$Pa, ds$H2O_LI840)
  #
  dsChunk <- subsetContiguous(ds, colTime = "TIMESTAMP", colIndex = "Collar") 
  test_that("subsetContiguous",{
        expect_true( nrow(dsChunk) > 1 )    
        expect_true( nrow(dsChunk) < nrow(ds) )
        expect_true( all( names(ds) %in% names(dsChunk)) )
        expect_true( all( "iChunk" %in% names(dsChunk)) )
        expect_true( length(unique(dsChunk$iChunk)) > 1 )
        expect_true( all( is.finite(dsChunk$CO2_dry) ))
        expect_true( "collar" %in% names(dsChunk))
      })
  #
  chamberVol = 0.6*0.6*0.6    # chamber was a cube of 0.6m length
  surfaceArea = 0.6*0.6
  #
  collar_spec <- tibble(
    collar = unique(dsChunk$collar), 
    depth = pmax(0,seq(0,0.03,length.out=length(collar))),
    area = surfaceArea,
    volume = chamberVol + surfaceArea * depth,
    tlag = NA)
  #
  resChunks <- calcClosedChamberFluxForChunkSpecs(
    dsChunk, collar_spec, colTemp = "T_LI840"
    # linear and saturating shape
    ,fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh)  
    ,debugInfo = list(omitEstimateLeverage = TRUE))  # faster

  test_that("calcClosedChamberFluxForChunks", {
        expect_true( nrow(resChunks) > 1 )    
        expect_true( all( c("flux","sdFlux") %in% names(resChunks)) )
        expect_true( all( c("iChunk","collar") %in% names(resChunks)) )
        expect_true( all( table(resChunks$iChunk) == 1) )
      })
  
  test_that("non-unique collars per iChunk",{
        dsChunk2 <- dsChunk; dsChunk2$collar[1:8] <- 3
        expect_error(
          resChunks2 <- calcClosedChamberFluxForChunkSpecs(
            dsChunk2, collar_spec, colTemp = "T_LI840"
            , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh)  
            , debugInfo = list(omitEstimateLeverage = TRUE))
          ,"not unique collars.*3")
      })
  
  test_that("missing collars per iChunk",{
    dsChunk2 <- mutate(dsChunk, collar = ifelse(collar == 1, NA, collar))
    expect_error(
      resChunks2 <- calcClosedChamberFluxForChunkSpecs(
        dsChunk2, collar_spec, colTemp = "T_LI840"
        , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh)  
        , debugInfo = list(omitEstimateLeverage = TRUE))
      ,"not specify volume.*iChunks.*3,4")
    collar_spec2 <- collar_spec %>% filter(collar != 2)
    expect_error(
      resChunks2 <- calcClosedChamberFluxForChunkSpecs(
        dsChunk, collar_spec2, colTemp = "T_LI840"
        , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh)  
        , debugInfo = list(omitEstimateLeverage = TRUE))
      ,"not specify volume.*collars.*2")
  })
  
  test_that("missing collumns in collar_spec",{
    collar_spec2 <- collar_spec %>% select(!tlag)
    is_valid <- checkCollarSpec(dsChunk, collar_spec2)
    expect_false(is_valid)
    expect_match(attr(is_valid,"msg"),"missing columns.*tlag")
  })

  test_that("several rows per chamber in collar_spec",{
    collar_spec2 <- bind_rows(collar_spec, collar_spec)
    is_valid <- checkCollarSpec(dsChunk, collar_spec2)
    expect_false(is_valid)
    expect_match(attr(is_valid,"msg"),"not unique specification.*1,2")
  })

  #
  suppressMessages(
    dsPlots <- plotCampaignConcSeries( dsChunk, resChunks, isVerbose = FALSE)
    )
  #dsPlots <- plotCampaignConcSeries(dsChunk, resChunks1, isVerbose = FALSE)
  #dsPlots$plot[[1]]
  test_that("plotCampaignConcSeries",{
        expect_true( nrow(dsPlots) > 0 )    
        expect_true( all( c("iPage","plot") %in% names(dsPlots)) )
        expect_true( inherits( dsPlots$plot[[1]],"ggplot") )
      })
  #print(dsPlots$plot[[1]])
  #dsPlots %>% rowwise() %>% do({print(.$plot); data.frame()})

} # if nzchar(fname)
