ONYXForestTrend <- function(ntrends, trends, IsDiamond, estim, stderr) {
	
	TrendLabs<-NULL
	TrendAfter<-numeric(0)
	
	for (itrend in seq(1, ntrends))
	{
		#        Display after last test element or, if the next point is a diamond, after the diamond
		After<-max(trends[[itrend]])
		if (npoints < After) stop("Trend test for non-existent point")
		if (npoints > After && IsDiamond[After+1]) After<-After+1
		TrendAfter[itrend]<-After

		TrendRaw <- MakeTrend(estim, stderr, trends[[itrend]])
		TrendLabs <- c(TrendLabs, ChiOut(TrendRaw[1], TrendRaw[2], TrendRaw[3], IsHet=FALSE))
	}
		
	return(list(TrendLabs, TrendAfter))
}