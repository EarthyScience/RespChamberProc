#require(testthat)
context("readDat")

test_that("x81 example",{
			fName <- system.file("genData/Flux2_140929_1700.81x", package = "RespChamberProc")
			if (nzchar(fName)) {
				#ds <- read81x(fName)
				ds <- read81xVar(fName, iChunkBase = 1000)
				expect_true( is.factor(ds$iChunk))
        expect_true( ds$iChunk[1] == "271:1001")
				expect_true( table(ds$iChunk)[1] > 1 )
				expect_true( ds$label[1] == "Flux2_140929_1600" )
				#plot( CO2 ~ Date, ds )
				#plot( CO2 ~ Date, ds[ds$iChunk==2,] )
			}
		})

test_that("x81 annotation",{
			fName <- system.file("genData/testVaryingColNumber.81x", package = "RespChamberProc")
			if (nzchar(fName)) {
				#ds <- read81x(fName)
				ds <- read81xVar(fName)
				expect_true( table(ds$iChunk)[1] > 1 )
				expect_true( "some Comment" %in% ds$Annotation )
				#table( ds$label )
				expect_true( all( filter(ds, iChunk==1)$label == "ChunkLabel1" ))
				expect_true( all( filter(ds, iChunk==2)$label == "ChunkLabel2" ))
				#plot( CO2 ~ Date, ds )
				#plot( CO2 ~ Date, ds[ds$iChunk==9,] )
			}
		})

test_that("x81 bad cases",{
  # due to instrument errors, x81 files have sometimes been screwed up
  # collect those cases in develop directory and read them
  # to be executed from package directory
  testDir <- "develop/x81Cases"
  if (dir.exists(testDir)) {
    fNames <- file.path(testDir, dir(testDir))
    fName <- fNames[1]
    for (fName in fNames) {
      message(fName)
      ds <- read81xVar(fName)
    }
  }
})


test_that("zipped file",{
  fName <- system.file(
  "genData/SMANIE_Chamber1_26032015.zip", package = "RespChamberProc")
  skip_if_not(nzchar(fName))
  nConn <- nrow(showConnections())
  con <- unz(fName, filename = unzip(fName, list = TRUE)[1,"Name"] )
  ds0 <- readDat(con, tz = "UTC")
  expect_true( nrow(ds0) > 0 )
  expect_error( isOpen(con) ) # invalid connection, because closed
  # number of open connections did not change
  #expect_equal( nConn, nrow(showConnections())) # can fail because parallel
})

test_that("zipped file with explicit opening connection",{
  fName <- system.file(
    "genData/SMANIE_Chamber1_26032015.zip", package = "RespChamberProc")
  skip_if_not(nzchar(fName))
  con <- unz(fName, filename = unzip(fName, list = TRUE)[1,"Name"] )
  open(con) # when explicitly opening, then also need to close connection
  ds0 <- readDat(con, tz = "UTC")
  expect_true( nrow(ds0) > 0 )
  expect_true( isOpen(con) )
  close(con)
})

test_that("reading dat file",{
  fName <- system.file(
    "genData/chamberLoggerEx1_short.dat", package = "RespChamberProc")
  skip_if_not(nzchar(fName))
  nConn <- nrow(showConnections())
  ds0 <- readDat(fName, tz = "UTC")
  expect_true( nrow(ds0) > 0 )
  # number of open connections did not change
  expect_equal( nConn, nrow(showConnections()))
})

test_that("issue #5",{
  #fName <- file.path("develop","x81Cases","2312_issue005_small.81x")
  fName <- system.file(
    "genData/2312_issue005_small.81x", package = "RespChamberProc")
  skip_if_not(nzchar(fName))
  ds <- read81xVar(fName)
  expect_true( ds$iChunk[1] == "274:1" )
  expect_true( table(ds$iChunk)[1] > 1 )
})

tmpf = function() {
  # testing reading Manip logger data for setup of North and sourth tower
  fName <- "develop/x81Cases/licor_north1.81x"
  fName <- "develop/x81Cases/licor_north3.81x"
  if (nzchar(fName)) {
    #ds <- read81x(fName)
    #trace(read81xVar, recover) #untrace(read81xVar)
    ds <- RespChamberProc::read81xVar(fName)
    expect_true( max(ds$iChunk) > 1 )
    expect_true( table(ds$iChunk)[1] > 1 )
    expect_true( ds$label[1] == "CH1" )
    #plot( CO2 ~ Date, ds )
    #plot( CO2 ~ Date, ds[ds$iChunk==2,] )
  }#
}


test_that("x82 single",{
  # testing reading Manip logger data for setup of North and sourth tower
  #fName = "develop/x82_cases/2022/01/25/82m-0147-20220125000045.82z"
  fName = "develop/x82_cases/82m-0147-20220125000045.82z"
  if (file.exists(fName)) {
    ds <- RespChamberProc:::read82z_single(fName)
    expect_true( all(ds$iChunk == 1 ))
    #plot( CO2 ~ TIMESTAMP, ds )
  }
})

test_that("x82 day",{
  fName <- system.file("genData/licor82.zip", package = "RespChamberProc")
  if (nzchar(fName)) {
    #ds = read82z(fName, temp_base_dir = "/dev/shm")
    ds = read82z(fName, iChunkBase = 1000L)
    expect_true( inherits(ds$iChunk, "integer"))
    expect_true( all(ds$iChunk > 1000 ))
    expect_true( table(ds$iChunk)[1] > 1 )
    #plot( CO2 ~ Date, ds )
    #plot( CO2 ~ Date, subset(ds, iChunk==1001))
  }
})

test_that("x82 summary",{
  fName <- system.file("genData/82m-0147-20220125000000_dense_summary.csv", package = "RespChamberProc")
  chamber_for_port = LETTERS[seq( from = 1, to = 12 )]
  ds = ds0 = read82summary(fName, chamber_for_port)
  expect_equal(ds$collar[1:2], c("A","B"))
})




