plotResp <- function(
	### plot a single time series, and the fit
	dsi				##<< data.frame of a time series of a single concentration gradient measurement
	,resFlux=NULL 	##<< result of \code{\link{calcClosedChamberFlux}}: a one-row tibble with columns tLag, flux, sdFlux, and model
	,colConc="CO2_dry"		##<< column name of measurment variable
	,colTime="TIMESTAMP"	##<< column name of time [s], or POSIXct
	,ylab="CO2_dry (ppm)"	##<< label of y axis
	,xlab="Time (s)"		##<< label of x axis
	,label=""				##<< label of the time series
){
	times <- dsi[[colTime]]
	times0 <- as.numeric(times) - as.numeric(times[1])
	plot( dsi[[colConc]] ~ times0, xlab=xlab, ylab="", col = "gray" )
	mtext(ylab, 2, las=0, 2.3)
	fluxText <- ""
	if( length(resFlux) ){
		tLag <- resFlux$tLag
		tmax <- resFlux$tmax
		if (!is.finite(tmax)) tmax = max(times0)
		abline( v=tLag, lty="dotted", col="grey" )
		abline( v=tmax, lty="dotted", col="grey" )
		#lines( fitted(resFlux$model[[resFlux$iFRegress]]) ~ I(times0[times0 >= tLag]) )
		# only one model provided
		lines( fitted(resFlux$model[[1L]]) ~ I(times0[between(times0,tLag,tmax)]), col = "blue" )
		prec=	ceiling(max(0, -log10(resFlux$sdFlux) ))
		fluxText <- paste( round(resFlux$flux, prec), " \u00B1", round(resFlux$sdFlux, prec),sep="")
	}
	legend( {if(!length(resFlux) || resFlux$flux < 0) "topright" else "bottomright"}
			,legend=c(label, fluxText )
			,inset=0.01
	)
}
attr(plotResp,"ex") <- function(){
	#data(chamberLoggerEx1s)
	dsi <- chamberLoggerEx1s
	dsi$Pa <- chamberLoggerEx1s$Pa * 1000  # convert kPa to Pa
	conc <- dsi$CO2_dry <- corrConcDilution(dsi)
	#trace(regressFluxExp, recover)		#untrace(regressFluxExp)
	#trace(calcClosedChamberFlux, recover)		#untrace(calcClosedChamberFlux)
	resFlux <- calcClosedChamberFlux(dsi)
	plotResp( dsi, label="Example 1" )			# without flux regression
	plotResp( dsi, resFlux, label="Example 1" )
	#plotResp( as_tibble(dsi), resFlux, label="Example 1" )
}
