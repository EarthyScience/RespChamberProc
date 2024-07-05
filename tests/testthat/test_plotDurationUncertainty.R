#require(testthat)
context("plotDurationUncertainty")

test_that("run plotDurationUncertainty",{
  # very strong (and therefore precise) uptake
  ds <- subset(chamberLoggerEx2, iChunk == 99)	
  #plot( CO2_dry ~ TIMESTAMP, ds )
  resDur <- plotDurationUncertainty( 
    ds, colTemp = "AirTemp", volume = 0.6*0.6*0.6
    , maxSdFlux = 0.8
    , nDur = 10
    , durations = c(100,120,150)
  )
  expect_that(resDur$duration, equals(120))
})

test_that("error message when specifying too short duration",{
  #https://github.com/bgctw/RespChamberProc/issues/8
  df <- readRDS("inst/genData/issue8_duration.rds")
  chamberVol = 0.4*0.4*0.4		# define here chamber volume and surface area
  surfaceArea = 0.4*0.4
  # reproduces the error
  expect_error(  
    resDur_H2O <- plotDurationUncertainty( df
                                         ,colTime = "TIMESTAMP"
                                         ,colConc = "H2Oppt"
                                         ,colTemp="AirTemp"
                                         ,volume = chamberVol
                                         , durations = seq(124,404,by=5))
    ,"requires at least 16 records")
})


			