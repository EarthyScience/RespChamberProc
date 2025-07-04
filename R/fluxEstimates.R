
calcClosedChamberFlux <- function(
	### Calculate gas flux and its uncertainties for a non steady-state canopy chamber.
	ds						             ##<< data.frame with concentration and time column
	  ## of a chamber measurement of one replicate
	, colConc = "CO2_dry"		   ##<< column name of CO2 concentration [ppm]
	, colTime = "TIMESTAMP"	   ##<< column name of time [s], must be of class POSIXct
	  ## or numeric or integer
	, colTemp = "TA_Avg"       ##<< column name of air temperature inside chamber [degC]
  , colPressure = "Pa"       ##<< column name of air pressure inside chamber [Pa]
	, volume = 1               ##<< volume inside the chamber im [m3]
	, area = 1					       ##<< area of the exchange surface [m2]
	, fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh )	##<< list of
	  ## functions to yield a single flux estimate, see details
	, fRegressSelect = regressSelectPref1	 ##<< function to select the regression
	  ## function based on fitting results. Signature and return must correspond
	  ## to \code{\link{regressSelectPref1}}
	, concSensitivity = 1		   ##<< measurement sensitivity of concentration.
	  ## With concentration change below this sensitivity, only a linear model is fit
	, maxLag = 50			         ##<< number of initial records to be screened for a
	  ## breakpoint, i.e. the lag (higher for water vapour than for CO2)
	, minTLag = 0				       ##<< possibility to specify a minimum lag-time in seconds
	, useFixedTLag = NA		     ##<< scalar numeric value in seconds, if not-NA,
	  ## use the specified lag-time instead of estimating the lag-time from
	  ## the concentration data
	, tmax = NA                ##<< maximum time in seconds after closing that
	  ## is taken into account for the fitting, if NA (default) all the time series
	  ## is taken into account
  , debugInfo = list(		     ##<< rarely used controls, mostly for debugging
			##describe<<
    useOscarsLagDectect = FALSE	##<< using the changepoint method for lag detection
	  , omitAutoCorrFit = FALSE ##<< set to TRUE to omit trying to fit
      ## autocorrelation (faster but maybe wrong (low) uncertainty estimate)
		, omitEstimateLeverage = FALSE	##<< set to TRUE to omit the time consuming
		  ## bootstrap for uncertainty due to leverage
		, tLagFixed = NA				  ##<< deprecated in favour of argument
		  ## \code{useFixedTLag}: possibility to specify the lagTime
		  ## instead of estimating them
		, isStopOnError = FALSE   ##<< set to TRUE to stop execution
		  ## when no model could be fitted, instead of a warning
			##end<<
	)
	, ...					##<< further arguments to \code{\link{sigmaBootLeverage}}
){
	##seealso<< \code{\link{RespChamberProc}}
	#
	isMissingCols <- !(c(colConc, colTime, colTemp, colPressure) %in% colnames(ds))
	if (any(isMissingCols) ) stop("calcClosedChamberFlux:  missing collumns "
	   , paste(c(colConc, colTime, colTemp, colPressure)[isMissingCols],collapse = ",") )
	#
	if (!(is.numeric(ds[[colTime]]) ||  inherits(ds[[colTime]], "POSIXct")) ) stop(
				"Timestamp column must be given in seconds, i.e. must be of class"
				, " POSIXct, integer, or numeric. But it was of class "
				, paste(class(ds[[colTime]]),collapse = ",") )
	#
	# formerly tLagFixed was specified with debugInfo, make this still work
	if ((length(debugInfo$tLagFixed) == 1L) && !is.na(debugInfo$tLagFixed) ) {
		warning("Using debugInfo$tLagFixed is deprecated. Please, "
		        , "use instead argument useFixedTLag.")
		useFixedTLag <- debugInfo$tLagFixed
	}
	##details<<
	## The function \code{fRegress} must conform to \code{\link{regressFluxSquare}}
	## , i.e.
	## return a vector of length 2: the flux estimate and its standard deviation.
	## Optionally, it may return the model fit object in attribute "model"
	## If several functions are given, then the best fit is selected according
	## to function with argument \code{fRegressSelect}, by default to the AIC criterion.
	## Fit an expoenential curve by using function \code{\link{regressFluxExp}}.
	#
	#plot( ds[[colConc] ~ ds[[colTime] )
	retEntries <- c("flux", "fluxMedian", "sdFlux", "tLag", "lagIndex", "autoCorr"
	                , "AIC", "sdFluxRegression", "sdFluxLeverage", "iFRegress", "sdResid"
	                , "iqrResid", "r2")
	retEmpty <- as_tibble( t(structure(rep(NA_real_, length(retEntries))
	                                   , names = retEntries )))
	retEmpty$model <- list(NULL)  # empty list column
	retEmpty$times <- list(NULL)  # empty list column
	#
	if (!sum(is.finite(ds[[colConc]]))){
	  warning("Encountered period with no finite concentrations: probably broken chamber.")
	  return(retEmpty)
	}
	if (diff(range(ds[[colConc]], na.rm = TRUE)) < 1e-8){
	  warning("Encountered all numerically equal concentrations: probably broken chamber.")
	  return(retEmpty)
	}
	#
	if (!length(names(fRegress)) ) names(fRegress) <- 1:length(fRegress)
	dslRes <- if (isTRUE(debugInfo$useOscarsLagDectect) ) {
		dslRes <- selectDataAfterLagOscar(ds, colConc = colConc, colTime = colTime
		          , tLagFixed = useFixedTLag)
	} else {
    	dslRes <- selectDataAfterLag(ds, colConc = colConc, colTime = colTime
    	         , tLagFixed = useFixedTLag, maxLag = maxLag, minTLag = minTLag)
	}
	dsl <- dslRes$ds  # only data after lag
	# constrain to finite records for fitting
	dsl <- dsl[ is.finite(dsl[[colConc]]) & is.finite(dsl[[colTime]]),,drop = FALSE]
	if (nrow(dsl) < 8L ) return(retEmpty)
	concRange <- diff( quantile(dsl[[colConc]], c(0.05,0.95), na.rm = TRUE ))
	if (concRange <= concSensitivity) {
		fRegress = c(lin = regressFluxLinear)
	}
	timesOrig <- ds[[colTime]]
	#times <- dsl[[colTime]]
	if (any(table(dsl[[colTime]]) != 1)) {
		#recover()
		stop("calcClosedChamberFlux: must provide data series with unique times "
		     , "(encountered multiple times)\n"
				 , paste(capture.output(ds[1,1:5]), collapse = "\n") )
	}
	dsl$times0 <- as.numeric(dsl[[colTime]]) - as.numeric(dsl[[colTime]][1])
	tLag <- as.numeric(timesOrig[ dslRes$lagIndex ]) - as.numeric(timesOrig[1])
	# constrain dataset to maximum time  tLag + times0 <= tmax
	if (is.finite(tmax)) dsl <- subset(dsl, times0 <= tmax - tLag)
	#abline(v = tLag+ds[1,colTime] )
	#
	# removed check for linear fit
	# plot( dsl[[colConc]] ~ dsl[[colTime]] )
	mod <- NULL
	#fReg <- fRegress[[1]]
	#trace(fReg, recover)
	fluxEstL <- lapply( fRegress, function(fReg){
		fluxEst <- fReg( dsl[[colConc]] ,  dsl$times0, tryAutoCorr = !isTRUE(debugInfo$omitAutoCorrFit) )
	})
	#plot(dsl[[colConc]] ~ dsl[[colTime]]); lines(fitted(fluxEstL[[1]]$model) ~ dsl[[colTime]], col = "blue");
	#lines(fitted(fluxEstL[[2]]$model) ~ dsl[[colTime]], col = "red");
	iBest <- fRegressSelect(fluxEstL)
	if (!length(iBest)) {
		msg <- paste0("calcClosedChamberFlux: could not fit any of the specified functions "
		        , "to the concentration dataset")
		if (isTRUE(debugInfo$isStopOnError)) stop(msg) else warning(msg)
		res <- retEmpty
		res["tLag"] <- tLag		              ##<< time of lag phase in seconds
		res["lagIndex"] <- dslRes$lagIndex
		return(res)
	}
	fReg <- fRegress[[iBest]]
	fluxEst <- fluxEstL[[iBest]]$stat
	#lines( fitted(attr(fluxEst,"model")) ~  times0 , col = "maroon")
	#abline( coefficients(attr(fluxEst,"model"))[1], fluxEst[1], col = "blue" )
	mod <- fluxEstL[[iBest]]$model
	coefStart <- coefficients(mod)
	tryAutoCorr <- is.finite( fluxEst["autoCorr"] )
	#
	leverageEst <- if (!isTRUE(debugInfo$omitEstimateLeverage) )
	  sigmaBootLeverage(dsl[[colConc]], dsl[[colTime]], fRegress = fReg
		, coefStart = coefStart, tryAutoCorr = tryAutoCorr, ... ) else NA
	##details<<
	## There are two kinds of uncertainty associated with the flux.
	## The first comes from the uncertainty of the slope of concentration increase.
	## The second comes from the leverage of starting and end points of the regression
	## (estimated by a bootstrap)
	## return value sdFlux is the maximum of those two components
	#
	# correct fluxes for density and express per chamber instead of per mol air,
	# express per area
	# chamberTemp <- ds[ dslRes$lagIndex,colTemp ]
	# better take the median temperature, as sensor might be off at the exact timelag
	chamberTemp <- median(ds[[colTemp]][dslRes$lagIndex], na.rm = TRUE)
	fluxEstTotal  =  corrFluxDensity(fluxEst, volume = volume
	                , temp = chamberTemp
	                , pressure = ds[[colPressure]][dslRes$lagIndex]) / area
	leverageEstTotal = corrFluxDensity(leverageEst, volume = volume
	                     , temp = chamberTemp
	                     , pressure = ds[[colPressure]][dslRes$lagIndex]) / area
	#
	#corrFluxDensity( dsl, vol = 2)
	resid <- residuals(mod)
  # avoid -Inf returned by max in case of only NAs
	sdFlux <- if (all(is.na(fluxEstTotal[c("sdFlux","sd")])) ) NA_real_ else
	              max(fluxEstTotal["sdFlux"],leverageEstTotal["sd"], na.rm = TRUE)
	##value<< data.frame (tibble) of one row with entries
	res <- tibble::tibble(
		flux = as.numeric(fluxEstTotal[1])			      ##<< the estimate of the CO2
		  ## flux into the chamber [mumol / m2 / s]
		,fluxMedian = as.numeric(leverageEstTotal[3])	##<< the median of the flux
		  ## bootsrap estimates [mumol / m2 / s]
		,sdFlux = sdFlux
		  ### the standard deviation of the CO2 flux
		,tLag = tLag		                             ##<< time of lag phase in seconds
		,tmax = tmax                                 ##<< maximum time fitted in seconds
		,lagIndex = dslRes$lagIndex 					       ##<< index of the row at the
		  ## end of lag-time
		,timestamp = ds[[colTime]][dslRes$lagIndex]
		,autoCorr = as.numeric(fluxEst["autoCorr"])	 ##<< autocorrelation coefficient,
		  ## NA if model with autocorrelation could not be fitted or had higher
		  ## AIC than a model without autocorrelation
		,AIC =  AIC(mod)									##<< AIC goodness of fit for the model
		,sdFluxRegression = as.numeric(fluxEstTotal["sdFlux"]) ##<< the standard
		  ## deviation of the flux by a single regression of CO2 flux
		,sdFluxLeverage = as.numeric(leverageEstTotal["sd"])   ##<< the standard
		  ## deviation of the flux by leverage of starting or end values
		  ## of the time series
		,iFRegress = as.numeric(iBest)					      ##<< index of the best
		  ## (lowest AIC) regression function
		,sdResid = sd(resid)
		,iqrResid = IQR(resid)
		,r2 = 1 - sum(resid^2) / sum((dsl[[colConc]] - mean(dsl[[colConc]], na.rm = TRUE))^2)
		  ### coefficient of determination
		,times = list(fluxEstL[[iBest]]$times)			##<< integer vector of predictor
		  ## times for the model fit (excluding and possibly first record)
		,model = list(mod)
	)
	return(res)
}
attr(calcClosedChamberFlux,"ex") <- function(){
	#data(chamberLoggerEx1s)
	library(dplyr)
	ds <- chamberLoggerEx1s
    ds$Pa <- chamberLoggerEx1s$Pa * 1000  # convert kPa to Pa
	conc <- ds$CO2_dry <- corrConcDilution(ds)
	#trace(calcClosedChamberFlux, recover)	#untrace(calcClosedChamberFlux)
	resFit <- calcClosedChamberFlux(ds, tmax = 80)
	select(resFit, flux, sdFlux) %>% unlist()
	#plotResp(ds, resFit)
	.tmp.compareFittingFunctions <- function(){
		resLin <- calcClosedChamberFlux(ds, fRegress = list(regressFluxLinear))
		resPoly <- calcClosedChamberFlux(ds, fRegress = list(regressFluxSquare))
		#trace(regressFluxExp, recover)	#untrace(regressFluxExp)
		resExp <- calcClosedChamberFlux(ds, fRegress = list(regressFluxExp) )
		#trace(regressFluxTanh, recover)	#untrace(regressFluxTanh)
		resTanh <- calcClosedChamberFlux(ds, fRegress = list(regressFluxTanh ))

		times <- ds$TIMESTAMP
		times0 <- as.numeric(times) - as.numeric(times[1])
		times0Fit <- times0[times0 >= resLin$stat["tLag"] ]
		plot( resid(resTanh$model, type = "normalized") ~  times0Fit )	# residual plots
		qqnorm(resid(resTanh$model, type = "normalized")); abline(0,1)

		#length(times0Fit)
		plot( ds$CO2_dry ~ times0, xlab = "time (s)", ylab = "" )
		mtext("CO2_dry (ppm)",2,2,las = 0)
		abline(v = resLin$stat["tLag"], col = "grey", lty = "dotted")
		lines( fitted(resExp$model) ~ times0Fit , col = "red" )
		lines( fitted(resLin$model) ~ times0Fit , col = "grey" )
		lines( fitted(resTanh$model) ~ times0Fit , col = "purple" )
		lines( fitted(resPoly$model) ~ times0Fit , col = "blue" )
		legend("topright", inset = c(0.02,0.02), legend = c("exp","lin","tanh","poly")
			, col = c("red","grey","purple","blue"), lty = "solid")
	}
}

regressSelectAIC <- function(
	### select regression function based on minimum AIC of the fits
	fluxEstL 	##<< list of return values of different regression functions
	  ## such as \code{\link{regressFluxLinear}}
){
	##value<< index of the best regression function
	AICs <- sapply(fluxEstL,function(entry){ as.numeric(entry$stat["AIC"]) })
	iBest <- which.min( AICs )	# the AIC returned
}

regressSelectPref1 <- function(
		### prefer first regf, if its AIC is not significantly different from next best
		fluxEstL 	##<< list of return values of different regression functions
		  ## such as \code{\link{regressFluxLinear}}
){
	##value<< index of the best regression function
	retFirst <- if (is.finite(fluxEstL[[1]]$stat["AIC"])) 1L else integer(0)
	if (length(fluxEstL) == 1 ) return(retFirst)
	AICs <- sapply(fluxEstL,function(entry){ as.numeric(entry$stat["AIC"]) })
	AICOthers <- AICs[-1]
	iFiniteOthers <- which(is.finite(AICOthers))
	if (!length(iFiniteOthers) ) return(retFirst)
	AICFiniteOthers <- AICOthers[iFiniteOthers]
	iBestFiniteOthers <- which.min( AICFiniteOthers )
	iBest <- if (is.finite(AICs[1]) && (AICs[1] <= AICFiniteOthers[iBestFiniteOthers] + 1.92) )
	  1L else 1L + iFiniteOthers[iBestFiniteOthers]
}

selectDataAfterLag <- function(
		### Omit the data within lag-time and normalize times to start after lag
		ds   ##<< a tibble or data.frame with time and concentration columns
		, colConc = "CO2_dry"		##<< column name of CO2 concentration per dry air [ppm]
		, colTime = "TIMESTAMP"  	##<< column name of time column [s]
		, tLagFixed = NA			##<< possibility to specify the lagTime (in seconds)
		  ## instead of estimating them
		, maxLag  =  50			##<< number of initial records to be screened for
		  ## a breakpoint, i.e. the lag
		, tLagInitial = 10			##<< the initial estimate of the length of the lag-phase
		, minTimeDataAfterBreak = 30	##<< number of minimum time (in seconds)
		  ## left after breakpoint
		, minTLag = 0				##<< possibility to specify a minimum lag-time in seconds
){
	ds <- as_tibble(ds)
	##seealso<< \code{\link{RespChamberProc}}
  maxLagConstrained <- min(maxLag, nrow(ds))
	times <- as.numeric(ds[[colTime]])[1:maxLagConstrained]
	times0 <- times - times[1]
	if (length(tLagFixed) && is.finite(tLagFixed)) {
		if (tLagFixed > tail(times0,1) ) return(list(
				lagIndex = NA_integer_
				,ds = ds[ FALSE, ,drop = FALSE]
		))
		iBreak <- min(which( times0 >= tLagFixed ))
	}else {
		if (minTLag > tail(times0,1) ) return(list(
				lagIndex = NA_integer_
				,ds = ds[ FALSE, ,drop = FALSE]
		))
		iBreak <- iBreakMin <- min(which( times0 >= minTLag ))
		obs <- ds[[colConc]][1:maxLagConstrained]
		lm0 <- lm(obs ~ times0)
    o <- try( segmented(lm0, seg.Z =  ~times0
        # initial estimate has to be inside the interval
				, psi = list(times0 = min(tLagInitial, times0[length(times0) - 1L]))
				, control = seg.control(display = FALSE)
		), silent = TRUE)
		#plot( obs ~ times0 );  lines(fitted(o) ~ times0, col = "red", lines(fitted(lm0)~times0))
    if (inherits(o,"try-error")) {
      # if there is no breakpoint, an error with this the
      # following message is thrown
      # in this case keep the iBreak = 1
      if (!length(grep("at the boundary",o)) ) stop(o)
    }
		if (!inherits(o,"try-error") && AIC(o) < AIC(lm0) ) {
			iBreak <- min(which(times0 >= o$psi[1,2] ))
		}
		iBreak <- max(iBreakMin, iBreak)
		# screen for minimum lag
		isEnoughTimeInSecondsInRemainingData <-
		  (times[length(times)] - times[iBreak]) >= minTimeDataAfterBreak
		if (!isEnoughTimeInSecondsInRemainingData ) iBreak <- iBreakMin
	}
	##value<< A list with entries
	list(
			lagIndex = iBreak    				##<< the index of the end of the lag period
			, ds = ds[ (iBreak):nrow(ds), ,drop = FALSE]    	##<< the dataset ds
			  ## without the lag-period (lagIndex included)
	)
}
attr(selectDataAfterLag,"ex") <- function(){
	#data(chamberLoggerEx1s)
	ds <- chamberLoggerEx1s
	ds$CO2_dry <- corrConcDilution(ds)
	#trace(selectDataAfterLag,recover)	#untrace(selectDataAfterLag)
	ret <- selectDataAfterLag(ds)
	plot( ds$CO2_dry)
	abline(v = ret$lagIndex, col = "red")
}


selectDataAfterLagOscar <- function(
  ### Omit the data within lag-time and normalize times to start after lag
  ds   ##<< data.frame with time and concentration columns
  , colConc = "CO2_dry"		##<< column name of CO2 concentration per dry air [ppm]
  , colTime = "TIMESTAMP"  	##<< column name of time column [s]
  , tLagFixed = NA				##<< possibility to specify the lagTime (in seconds)
    ## instead of estimating them
  , minTimeDataAfterBreak = 30	##<< number of minimum time (in seconds)
    ## left after breakpoint
){
	##seealso<< \code{\link{RespChamberProc}}
  # TODO: account for different times
  if (length(tLagFixed) && is.finite(tLagFixed)) {
  	times <- as.numeric(ds[[colTime]])
  	times0 <- times - times[1]
  	iBreak <- min(which( times0 >= tLagFixed ))
  }else {
  	iBreak <- min(cpt.mean(ds[[colConc]], penalty = "SIC"
  	                       , method = "PELT", class = FALSE))[1]
    #plot( ds[[colConc] )
    #abline(v = iBreak)
  }
  ##value<< A list with entries
  isEnoughTimeInSecondsInRemainingData <-
    (times[length(times)] - times[iBreak]) >= minTimeDataAfterBreak
  if ((iBreak < nrow(ds)) && isEnoughTimeInSecondsInRemainingData) {
	  list(
	    lagIndex = iBreak    				##<< the index of the end of the lag period
	    ,ds = ds[ (iBreak):nrow(ds), ]    ##<< the dataset ds without
	      ## the lag-period (lagIndex included)
	  )
  }else{
	  list( lagIndex = 1, ds = ds )
  }
}

regressFluxLinear <- function(
		### Estimate the initial flux by linear regression
		conc	  ##<< numeric vector of CO2 concentrations []
		, times		##<< times of conc measurements	[seconds]
		, start = c()	##<< numeric vector of starting parameters. May provide
		  ## from last bootstrap to speed up fitting
		, tryAutoCorr = TRUE	##<< set to FALSE to not try to fit
		  ## model with autocorrelation
){
	##seealso<< \code{\link{regressFluxExp}}
	##seealso<< \code{\link{RespChamberProc}}
	timesSec <- as.numeric(times) - as.numeric(times[1])
	#plot(conc ~ timesSec)
	lm1 <- try( gls(conc ~ timesSec), silent = TRUE)
	lm1Auto <- if (!isTRUE(tryAutoCorr) || inherits(lm1,"try-error")) lm1 else
				try(gls( conc ~ timesSec
						,correlation = corAR1( 0.3, form = ~ timesSec)
				),silent = TRUE)
	nlmBest <- if (!inherits(lm1Auto,"try-error") && (AIC(lm1Auto) < AIC(lm1)) )
	  lm1Auto else lm1
	if (inherits(nlmBest,"try-error")) {
		nlmBest <- lm(conc ~ timesSec)
		corStruct <- NULL
		coefSummary <- coef(summary(nlmBest))	# may have only one row if slope is NA
		sdFlux <- if (nrow(coefSummary) >= 2) coefSummary[2, 2] else NA
	} else {
		corStruct <- if (inherits(nlmBest,"gls")) nlmBest$modelStruct$corStruct else NULL
		sdFlux = as.vector(sqrt(diag(vcov(nlmBest))[2]))	##<< standard deviation of flux
	}
	##value<< list with entries
	res <- list( stat = c(	##<< numeric vector (2) with entries:
	    ## flux, sdFlux, AIC, and autoCorr:
			flux = as.vector(coefficients(nlmBest)[2]) ##<< flux estimate at starting time
			,sdFlux = sdFlux									         ##<< standard deviation of flux
			,AIC = AIC(nlmBest)									       ##<< model fit diagnostics
			,autoCorr = ##<< coefficient of autocorrelation
					## or NA if model with autocorrelation could not be fitted or had
					## higher AIC than model without autocorrelation
					# see ?nlme:::coef.corAR1
					if (length(corStruct) )
					  as.numeric(coef(corStruct, unconstrained = FALSE)) else NA
		)
		, times = timesSec		##<< used predictor vector, can be used for return for plotting
		, model = nlmBest)	  ##<< the model-fit object (here of class gls)
	res
}
attr(regressFluxLinear,"ex") <- function(){
	#data(chamberLoggerEx1s)
	ds <- chamberLoggerEx1s
	conc <- ds$CO2_Avg
	times <- ds$TIMESTAMP
	plot( conc ~ times )
	regressFluxLinear( conc, times  )
}


regressFluxSquare <- function(
		### Estimate the initial flux and its std-dev by polynomial regression
		conc	  ##<< numeric vector of CO2 concentrations [ppm]
		, times	##<< times of conc measurements	[seconds]
		, start = c()	##<< numeric vector of starting parameters. Not used here.
		, tryAutoCorr = TRUE	##<< set to FALSE to not try to fit model with autocorrelation
){
	##seealso<< \code{\link{regressFluxExp}}
	##seealso<< \code{\link{RespChamberProc}}
	  timesSec <- as.numeric(times) - as.numeric(times[1])
	  lm1 <- gls( conc ~ poly(timesSec,2,raw = TRUE) )
	  lm1Auto <- if (!isTRUE(tryAutoCorr) || inherits(lm1,"try-error")) lm1 else
				  gls( conc ~ poly(timesSec,2,raw = TRUE)
						  ,correlation = corAR1( 0.3, form = ~ timesSec)
				  )
	  nlmBest <- if (!inherits(lm1Auto,"try-error") && (AIC(lm1Auto) < AIC(lm1)) )
	    lm1Auto else lm1
	  corStruct <- nlmBest$modelStruct$corStruct
	  res <- list( stat = c(	##<< numeric vector (2) with entries:
	    ## flux, sdFlux, AIC, and autoCorr:
					  flux = as.vector(coefficients(nlmBest)[2])			   ##<< flux estimate at starting time
					  , sdFlux = as.vector(sqrt(diag(vcov(nlmBest))[2])) ##<< standard deviation of flux
					  , AIC = AIC(nlmBest)									             ##<< model fit diagnostics
					  , autoCorr = ##<< coefficient of autocorrelation
							  ## or NA if model with autocorrelation could not be fitted
							  ## or had higher AIC than model without autocorrelation
							  if (length(corStruct) )
							    as.numeric(coef(corStruct, unconstrained = FALSE)) else NA
			  )
			  , times = timesSec		##<< used predictor vector, can be used
	        ## for return for plotting
			  , model = nlmBest)	##<< the model-fit object (here of class gls)
	  res
}
attr(regressFluxSquare,"ex") <- function(){
  #data(chamberLoggerEx1s)
  ds <- chamberLoggerEx1s
  conc <- ds$CO2_dry <- corrConcDilution(ds)
  times <- ds$TIMESTAMP
  res <- regressFluxSquare( conc, times  )
  plot( conc ~ times)
  m <- attr(res,"model")
  #lines( fitted(m) ~ times )
}

regressFluxExp <- function(
		### Estimate the initial flux by fitting an exponentially saturating function
		conc	  			    ##<< numeric vector of CO2 concentrations []
		, times				    ##<< times of conc measurements	[seconds]
		, start = c()			##<< numeric vector of starting parameters.
		  ## May provide from last bootstrap to speed up fitting
		, tryAutoCorr = TRUE	##<< set to FALSE to not try to fit
		  ## model with autocorrelation
		, cSatFac = 1.5  		##<< Position of the initial saturation
		  ## (0 start, 1 end, >1 outside measured range)
){
	##seealso<< \code{\link{RespChamberProc}}
  #
	##details<<
	## The flux is calculated as the slope of the concentration change. By
	## changing the concentration gradient, however, the flux is disturbed.
	## In effect the
	## flux will decline over time and concentrations go towards a saturation.
	##
	## This method fits a polynomial regression to the concentrations and infers
	## the slope at reports
	## the slope at the initial time. Make sure to remove lag time period before.
	##
	## Other functional forms can be fitted to estimate the initial slope:
	## \itemize{
	##  \item{ Linear: \code{\link{regressFluxLinear}}  }
	##  \item{ Hyperbolic tangent saturating function: \code{\link{regressFluxTanh}}  }
	##  \item{ Exponential: \code{\link{regressFluxExp}}  }
	##  \item{ Quadratic polinomial: \code{\link{regressFluxSquare}}  }
	## }
	##
	## The hyperbolic tangent form (\code{\link{regressFluxTanh}})
	## has the advantage that
	## initially the flux is changing only very slowly. In contrast, whith the
	## exponential form the
	## slope changes much at the beginning.
	##
	## The exponential form, is more consistent with a theoretical model of
	## saturating flux (Kutzbach 2006).
	#
  functionName <- "regressFluxExp" # debugging
  timesSec <- as.numeric(times) - as.numeric(times[1])
	#plot( conc ~ timesSec )
	fluxLin <- coefficients(lm(conc ~ timesSec ))[2]
	if (!length(start)) {
	  # invert to decreasing concentrations
		concP <- if (fluxLin < 0 ) conc else -conc
		#plot( concP ~ timesSec )
		# increasing concentration
		# set the saturation twice above the range
		cRange <- quantile( concP , probs = c(0.05,0.95))
		cSat0 <- cRange[1] - cSatFac*diff(cRange)
		#abline(h = cSat0)
		#plot( I(concP-cSat0) ~ timesSec )
		cDiff <- concP - cSat0; cDiff[cDiff <= 0] <- NA
		lm1 <- suppressWarnings( lm( log(cDiff) ~ timesSec ) )
		#plot( log(concP-cSat0) ~ timesSec )
		a0 <- exp(coefficients(lm1)[1])
		b0 <- -coefficients(lm1)[2]
		r0 <- a0*-b0
		#plot( conc ~ timesSec )
		#lines( I(-r0/b0 * exp(-b0*timesSec) + cSat0) ~ timesSec )
	}
	nlm1 <- try(
			tmp <- gnls( conc ~ -r/b * exp(-b*timesSec) + c
					,params = r+b+c~1
					,start = if (length(start) ) start else {
						if (fluxLin < 0 )  list(r = r0, b  = b0, c = cSat0) else
						  list(r = -r0, b  = b0, c = -cSat0)
					}
					#,control = glsControl(msVerbose = TRUE)
					,correlation = NULL
			)
	, silent = TRUE)
	if (!length(nlm1)) stop("encountered null nlm1")
	nlm1Auto <- if (!isTRUE(tryAutoCorr) || inherits(nlm1,"try-error")) nlm1 else
	    try(
						#tmp <- update(nlm1, correlation = corAR1( 0.3, form = ~ timesSec) )
						tmp <- update(nlm1, correlation = corAR1( 0.3, form = ~ timesSec)
						  # sometimes not fitting, here already good starting values
							,control = gnlsControl(nlsTol = 0.01)
						)
			, silent = TRUE)
	if (!length(nlm1Auto)) stop("encountered null nlm1Auto")
	#plot( conc ~ timesSec )
	#lines( I(a0*exp(-b0*timesSec) + cSat0) ~ timesSec, col = "maroon"  )
	#lines( fitted(nlm1) ~ timesSec, col = "blue"  )
	#lines( fitted(nlm1) ~ timesSec, col = "orange"  )
	#plot(resid(nlm1) ~ timesSec )
	#qqnorm( resid(nlm1) ); abline(0,1)
	nlmBest <- if (!inherits(nlm1Auto,"try-error") && (AIC(nlm1Auto) < AIC(nlm1)) )
	  nlm1Auto else nlm1
	##value<<
	res <- list( stat = 	##<< numeric vector with 4 entries:
	               ## \code{flux}, \code{sdFlux}, \code{AIC}, and \code{autoCorr}:
					if (!inherits(nlm1,"try-error")) {
						corStruct <- nlmBest$modelStruct$corStruct
						c(
							flux = as.vector(coefficients(nlmBest)[1])			  ##<< flux estimate
							  ## at starting time
							,sdFlux = as.vector(sqrt(diag(vcov(nlmBest))[1]))	##<< standard
							  ## deviation of flux
							,AIC = AIC(nlmBest)									              ##<< model
							  ## fit diagnostics
							,autoCorr = ##<< coefficient of autocorrelation
							  ## or NA if model with autocorrelation could not be
							  ## fitted or had higher AIC than model without autocorrelation
							if (length(corStruct) )
							  as.numeric(coef(corStruct, unconstrained = FALSE)) else NA
						)
					}else c(
										flux = NA
										,sdFlux = NA
										, AIC = NA
										,autoCorr =  NA
								)
			, times = timesSec	##<< used predictor vector, can be used for return for plotting
			, model = nlmBest)	##<< the model-fit object (here of class gnls)
}
attr(regressFluxExp,"ex") <- function(){
	#data(chamberLoggerEx1s)
	ds <- chamberLoggerEx1s
	conc <- ds$CO2_dry <- corrConcDilution(ds)
	times <- ds$TIMESTAMP
	#trace(regressFluxExp, recover)	#untrace(regressFluxExp)
	(res <- regressFluxExp( conc, times  ))
	plot( conc ~ times)
	lines( fitted(res$model) ~ times )
}



regressFluxTanh <- function(
		### Estimate the initial flux by fitting a hyperbolic tangent saturating function
		conc	  ##<< numeric vector of CO2 concentrations []
		, times	  ##<< times of conc measurements	[seconds]
		, start = c()	##<< numeric vector of starting parameters. May provide
		  ## from last bootstrap to speed up fitting
		, cSatFac = 2  ##<< Position of the initial estimate of
		  ## saturation (0 start, 1 end, >1 outside measured range)
		, tryAutoCorr = TRUE	##<< set to FALSE to not try to fit model
		  ## with autocorrelation
){
	##seealso<< \code{\link{regressFluxExp}}
	##seealso<< \code{\link{RespChamberProc}}
	##details<<
	## For efficiency reasons, does not check for missing values (NA).
	## The caller must provide conc and times all finite.
	#functionName <- "regressFluxTanh" # debugging
	timesSec <- as.numeric(times) - as.numeric(times[1])
	fluxLin <- coefficients(lm(conc ~ timesSec ))[2]
	if (!length(start)) {
	  # invert, do not forget to invert again when flux calculated
		concP <- if (fluxLin < 0 ) -conc else conc
		# increasing concentration
		# set the saturation twice above the range
		cRange <- quantile( concP , probs = c(0.05,0.95))
		cSat0 <- cRange[1] + cSatFac*diff(cRange)
		#abline(h = cSat0)
		#plot( concP-cSat0 ~ timesSec )
		lm1 <- lm(head(concP,30)-cSat0 ~ head(timesSec,30) )
		#abline(coefficients(lm1))
		# c0 is the range to cover (= -intercept from the linear model)
		s0 <- coefficients(lm1)[2]	     # initial slope
		c00 <- -coefficients(lm1)[1]     # range to cover
	}
	#plot( (concP - cSat0)/c00 +1  ~ timesSec )		# that matches tanh def between 0 and 1
	#lines( tanh(timesSec*s0/c00) ~ timesSec)	 	# tanh function  - set equal to above and solve for concP
	#plot( concP ~ timesSec )
	#lines( (tanh(timesSec*s0/c00)-1)*c00 + cSat0)  # initial in concP range
	#lines( (tanh(timesSec*coefficients(nlm1)[1]/coefficients(nlm1)[3])-1)*coefficients(nlm1)[3] + coefficients(nlm1)[2])  # initial in concP range
	#plot( conc ~ timesSec )
	#lines( (tanh(timesSec*-s0/c00)+1)*c00 -cSat0)  # initial in concP range
	#lines( (tanh(timesSec*-coefficients(nlm1)[1]/coefficients(nlm1)[3])+1)*coefficients(nlm1)[3] - coefficients(nlm1)[2])  # initial in concP range
	# deprecated: use nlsLM from minpack.lm to switch between Newten and Levenberg, avoid singular gradient in near-linear cases
	# http://www.r-bloggers.com/a-better-nls/
	concOut <- conc[-1]
	timesSecOut <- timesSec[-1]
	# sometimes return NULL instead of an error, need to transform to error
	if (fluxLin > 0) {
	  nlm1 <- nlmNoOut <- try({
	    tmp <- suppressWarnings(gnls(
	      conc ~   (tanh(timesSec*s/c0)-1)*c0 + cSat
	      ,start = if (length(start)) start else list(s = s0, cSat = cSat0, c0 = c00)
	      ,params =  c(s+cSat+c0~1)
	      ,correlation = NULL
	    ))
	    if (is.null(tmp)) stop("could not fit tanh model")
	    tmp
	  }, silent = TRUE)
	  # fit without the first obs
	  nlmOut <- try({
	    tmp <- suppressWarnings(gnls(
	      concOut ~ (tanh(timesSecOut*s/c0)-1)*c0 + cSat
	      ,start = if (length(start)) start else list(s = s0, cSat = cSat0, c0 = c00)
	      ,params =  c(s+cSat+c0~1)
	      ,correlation = NULL
	    ))
	    if (is.null(tmp)) stop("could not fit tanh model")
	    tmp
	  }, silent = TRUE)
	} else {
	  nlm1 <- nlmNoOut <- try({
	    tmp <- suppressWarnings(gnls(
	      conc ~  (tanh(timesSec*s/c0)+1)*c0 - cSat
	      ,start = if (length(start)) start else list(s = -s0, cSat  =  cSat0, c0 = c00)
	      ,params =  c(s+cSat+c0~1)
	      ,correlation = NULL
	    ))
	    if (is.null(tmp)) stop("could not fit tanh model")
	    tmp
	  }, silent = TRUE)
	  # fit without the first obs
	  nlmOut <- try({
	    tmp <- suppressWarnings(gnls(
	      concOut ~  (tanh(timesSecOut*s/c0)+1)*c0 - cSat
	      ,start = if (length(start)) start else list(s = -s0, cSat = cSat0, c0 = c00)
	      ,params =  c(s+cSat+c0~1)
	      ,correlation = NULL
	    ))
	    if (is.null(tmp)) stop("could not fit tanh model")
	    tmp
	  }, silent = TRUE)
	}
	##detail<<
	## The tanh fit is very prone to a very low first records. Hence also try
	## to fit without the first record
	## and if this fit is significantly better, use it instead
	outNrecRatio <- (length(conc) - 1)/length(conc)
	isOmittingFirstRec <- (!inherits(nlm1,"try-error") &&
	                         !inherits(nlmOut,"try-error") &&
			(sum(resid(nlmOut)^2) < sum(resid(nlm1)^2)*outNrecRatio - 2 ))
	if (isOmittingFirstRec) {
		#message("omitting first record")
		nlm1 <- nlmOut
	}
	nlm1Auto <- if (!isTRUE(tryAutoCorr) || inherits(nlm1,"try-error") ) nlm1 else
	        try(
						suppressWarnings(update(nlm1, correlation = corAR1( 0.3, form = ~ timesSec)
					      # sometimes not fitting, here already good starting values
								,control = gnlsControl(nlsTol = 0.01)
						))
						, silent = TRUE)
	# lines(fitted(nlm1) ~ timesSec, col = "orange" )
	#
	#lines( I(cSat0 + c00*(1-tanh(timesSec*(-s0)))) ~ timesSec, col = "maroon"  )
	#lines( fitted(nlm1) ~ timesSec, col = "purple"  )
	#plot(resid(nlm1) ~ timesSec )
	#qqnorm( resid(nlm1) ); abline(0,1)
	nlmBest <- if (!inherits(nlm1Auto,"try-error") && (AIC(nlm1Auto) < AIC(nlm1)) )
	  nlm1Auto else nlm1
	res <- list( stat = 	##<< numeric vector with 4 entries:
	               ## \code{flux}, \code{sdFlux}, \code{AIC}, and \code{autoCorr}:
					if (!inherits(nlm1,"try-error")) {
						corStruct <- nlmBest$modelStruct$corStruct
						c(
								flux = as.vector(coefficients(nlmBest)[1])			  ##<<
								  ## flux estimate at starting time
								,sdFlux = as.vector(sqrt(diag(vcov(nlmBest))[1]))	##<<
								  ## standard deviation of flux
								,AIC = if (!isOmittingFirstRec) ##<< model fit diagnostics
								   AIC(nlmBest) else AIC(nlmBest)/(outNrecRatio)
								,autoCorr = ##<< coefficient of autocorrelation
										## or NA if model with autocorrelation could not be
										## fitted or had higher AIC than model without autocorrelation
										if (length(corStruct) )
										  as.numeric(coef(corStruct, unconstrained = FALSE)) else NA
						)
					}else c(
								flux = NA
								,sdFlux = NA
								, AIC = NA
								,autoCorr =  NA
						)
			, times0 = if (isOmittingFirstRec) timesSec[-1] else timesSec	##<<
			  ## used predictor vector: seconds starting from 0, can be used
			  ## for return for plotting
			, model = nlmBest)	##<< the model-fit object (here of class gnls)
}
attr(regressFluxTanh,"ex") <- function(){
	#data(chamberLoggerEx1s)
	ds <- chamberLoggerEx1s[-(1:16),]
	conc <- ds$CO2_dry <- corrConcDilution(ds)
	times <- ds$TIMESTAMP
	times0 <- as.numeric(times) - as.numeric(times[1])
	#trace(regressFluxTanh, recover)	#untrace(regressFluxTanh)
	(res <- regressFluxTanh( conc, times  ))
	plot( conc ~ times)
	lines( fitted(res$model) ~ times )
}







sigmaBootLeverage <- function(
		### Estimate uncertainty of regression due to leverage of starting and end points.
		conc	  ##<< numeric vector of CO2 concentrations
		,times	##<< times of conc measurements
		,fRegress = regressFluxExp		##<< function to yield a single flux estimate
		, probs = c(0.05,0.5,0.95)		#<< percentiles for which the flux
		  ## quantiles should be returned
		, nSample = 80
		, coefStart = c()	##<< numeric vector of starting parameters.
		  ## May provide from last bootstrap to speed up fitting
		, tryAutoCorr = TRUE	##<< set to FALSE to not try to fit
		  ## model with autocorrelation
		, nRecCentral = 20 ##<< the number of records kept in
		  ## the central part of the time series.
){
	##seealso<< \code{\link{RespChamberProc}}
  periodLength <- diff( as.numeric(times[c(1,length(times))]) )
    if (periodLength < 60) {
      warning(paste("Time series of only",periodLength
                    ," seconds is too short. Recommended are at least 60 seconds. "
                    , "Returning NA uncertainty."))
      return( c( sd = NA_real_, "5%" = NA_real_, "50%" = NA_real_, "95%" = NA_real_ ) )
    }
  start <- seq(0, 10)   # indices of starting the time series
  central_begin <- 12
  # indices of the end (deployment) of the duration
  close <- seq(max(15, length(conc) - 40), length(conc), 1)
  central_end <- close[1] -2
  ##defining the function to be bootstrapped based on starting and deployment time
  starts <-  sample(start, nSample, replace = TRUE)
  closes <-  sample(close, nSample, replace = TRUE)
  ##details<< For determining the uncertainty due to leverage,
  ## the inner part of the concentration time series has low influence.
  ## Hence, fitting is accelerated by thinning the inner part of long series to
  ## only \code{nRecCentral} records.
  central <- central_begin:central_end
  central_thin <- if (length(central) > (nRecCentral-2)) {
    i <- round(seq(1,length(central),length.out = nRecCentral))
    central[i[2:(nRecCentral-1)]]
  } else if(length(central) > 2){
    central[2:(length(central)-1)] # omit central_begin and central_end
  } else {
    # special case where central is not longer than c(central_begin, central_end)
    central[FALSE]
  }
  zz <- sapply(1:nSample, function(i){
    subIndices <- c(starts[i]:central_begin, central_thin, central_end:closes[i])
    fRegress( conc[subIndices], times[subIndices], start = coefStart
              , tryAutoCorr = tryAutoCorr  )$stat[1]
  })
  #plot(density(zz, na.rm = TRUE))
  ## a vector with first entry the standard deviation of the flux,
  ## and following entries quantiles for given argument \code{probs}
  c( sd = sd(zz, na.rm = TRUE), quantile(zz, probs, na.rm = TRUE) )
}
attr(sigmaBootLeverage,"ex") <- function(){
	#data(chamberLoggerEx1s)
	ds <- chamberLoggerEx1s
  sigmaBootLeverage( ds$CO2_Avg, ds$TIMESTAMP )
}

