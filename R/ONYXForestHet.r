ONYXForestHet <- function(nhets, hets, estim, stderr) {

	HetLabs<-NULL
	HetAfter<-numeric(0)
	for (ihet in seq(1, nhets))
	{
		#        Display after the absolute value of the last element
		#        Display after last test element or, if the next point is a diamond, after the diamond
		After <- abs((hets[[ihet]])[length(hets[[ihet]])])
		if (npoints<After) stop("Heterogeneity test for non-existent point")
		HetAfter[ihet] <- After

		HetRaw<-MakeHet(estim, stderr, hets[[ihet]])
		HetLabs<-c(HetLabs, ChiOut(HetRaw[1], HetRaw[2], HetRaw[3], IsHet=TRUE))
	}
	return(list(HetLabs, HetAfter))
}