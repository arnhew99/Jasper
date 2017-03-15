# Function that spaces the ONYXForest elements in the y-direction

ONYXForestYSpacing <- function(npoints, nhets, ntrends, nblanks, Ymax=100, Ymin=0, IsDiamond, StrayTextAfter, HetAfter, TrendAfter, AdAM.mode=FALSE, pointGroups, pointGroupsFactor=0.8) {
	
	# Constructing y-locations...
	# First compute how many blanks there are at each possible location.
	# And which blanks are where	
	BlanksBefore <- rep(0, npoints+1)
	if (nblanks > 0)
	{
		BlankNums<-vector("list", length=npoints+1)
		for (iblank in seq(1, nblanks))
		{
			ipoint <- StrayTextAfter[iblank]+1
			BlanksBefore[ipoint] <- BlanksBefore[ipoint] + 1
			BlankNums[[ipoint]][BlanksBefore[ipoint]] <- iblank
		}
	}
	
	
	# if we're in AdAM.mode then we want things slightly differently
	
	
	#  Now to compute y-locations
	#  We assume the total y-distance runs 0-100 for the graph itself
	#  However, we don't have an even distribution of points, because of extra space after 
	#  diamonds and leaving room for statistical tests

	#  want: 1 'gap' between squares
	#  an extra half after diamonds
	#  an extra gap for each test
	#  an extra gap for 'blanks' and stray text (handled here as a single entity)
	#  Assume no tests after point 1, and at least 2 points

	YTemp <- numeric(npoints)
	HetLocsTemp <- numeric(nhets)
	TrendLocsTemp <- numeric(ntrends)
	BlankLocsTemp <- numeric(nblanks)

	#  First point (may be blanks previous to first point)
	YNext <- 1
	for (ipoint in seq(1, npoints))
	{
		#     First deal with any blanks before the point
		if (BlanksBefore[ipoint] > 0)
		{
			for (iblank in seq(1, BlanksBefore[ipoint]))
			{
				ThisBlank<-BlankNums[[ipoint]][iblank]
				BlankLocsTemp[ThisBlank]<-YNext
				YNext<-YNext + 1
				#            print("working")
				#            print(ThisBlank)
				#            print(YNext-1)
				#            print(BlankLocsTemp[ThisBlank])
			}
		}
		YTemp[ipoint] <- YNext
		YNext <- YNext + 1
		#     Statistical test, heterogeneity and trend
		if (sum(HetAfter==ipoint)>1) stop('Two or more heterogeneity tests after the same point not catered for.  Sorry!')
		if (sum(HetAfter==ipoint)==1)
		{
			HetLocsTemp[HetAfter==ipoint] <- YNext
			YNext <- YNext + 1
		}
		if (sum(TrendAfter==ipoint)>1) stop('Two or more trend tests after the same point not catered for.  Sorry!')
		if (sum(TrendAfter==ipoint)==1)
		{
			TrendLocsTemp[TrendAfter==ipoint] <- YNext
			YNext <- YNext + 1
		}
		#     Diamondness?
		if (IsDiamond[ipoint]) YNext<-YNext + ifelse(AdAM.mode, 0, 0.5)
	}
	
	#  May also be blanks after the final point
	if (BlanksBefore[npoints+1] > 0)
	{
		for (iblank in seq(1, BlanksBefore[npoints+1]))
		{
			ThisBlank <- BlankNums[[npoints+1]][iblank]
			BlankLocsTemp[ThisBlank] <- YNext
			YNext <- YNext + 1
		}
	}

	#  YLast is the position of the hypothetical point below the last real point
	#  (i.e. a position that can safely be put on the x-axis)
	YLast <- YNext

	#  Now have 'dummy' point locations, working upwards in (mostly) ones.  Want a range of Ymax-0 (early points have high values).
	#  Treat the current start as 0, not 1, because we want a gap at the top of the axis
	#  So wish to map 0-YLast on to Ymax-0
	yscaling_factor <- (Ymax-Ymin) / YLast
	YLocs <- Ymax - (YTemp * yscaling_factor)
	HetLocs <- Ymax - (HetLocsTemp * yscaling_factor)
	TrendLocs <- Ymax - (TrendLocsTemp * yscaling_factor)
	BlankLocs <- Ymax - (BlankLocsTemp * yscaling_factor)
	
	
	# if pointGroups has been given then move points in a point group together slightly
	if (!(is.null(pointGroups))) {
		if (!is.list(pointGroups)) stop("pointGroups should be a list")
		if (anyDuplicated(unlist(pointGroups)) > 0) stop("A point should only be in one pointGroup")
		
		for (i in 1:length(pointGroups)) {
			# get the current point group
			cpts <- pointGroups[[i]]
			cpts.y <- YLocs[cpts]
			# shrink the cpts.y by some factor
			cpts.range <- range(cpts.y)
			
			# put on [0,1] interval
			cpts.unit <- (cpts.y - min(cpts.y))/diff(cpts.range)
			
			# rescale to be in the shrunken range
			additive <- 1-pointGroupsFactor
			multiplicative <- 1-(2*additive)
			cpts.unit <- (cpts.unit*multiplicative) + additive
			
			# invert the scaling
			cpts.new <- cpts.unit*diff(cpts.range) + min(cpts.y)
			
			# reinsert
			YLocs[cpts] <- cpts.new
		}
		
	}
	
	return(list(YLocs=YLocs, HetLocs=HetLocs, TrendLocs=TrendLocs, BlankLocs=BlankLocs, YLast=YLast))

}