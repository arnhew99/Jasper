ONYXForestUserLabels <- function(npoints, pointlabs, YLocs, layout=NULL, lablocs=NULL, Boldness, mainfont) {
	#  x-locations are 0 + early elements of layout (the last 2 being the x-axis ends)
	#  values are supplied separately, and there may be more or less than there are x-locations
	#  For each point, I will plot the smaller number of labels by locations or values
	Font <- mainfont
	for (ipoint in seq(1, npoints))
	{
		userlabs <- unlist(pointlabs[[ipoint]])
		if (!is.null(layout)) {
			nlabels <- min(length(userlabs), length(layout)-1)
			lablocs <- c(0, layout)
		} else nlabels <- length(lablocs)
		
		if (nlabels>0)
		{
			text(lablocs[seq(1, nlabels)], YLocs[ipoint], labels=userlabs[seq(1, nlabels)], font=Boldness[ipoint], adj=c(0, 0.5), cex=1.2*Font)
			#        Save midpoints of the labels to use to centre the column labels (eg. "Age group") there
			#        (currently left-justifying at lablocs, however)
			LabelMids <- lablocs[seq(1, nlabels)] + (0.5*strwidth(userlabs[seq(1, nlabels)], cex=1.2*Font))
		}
	}
	return(list(lablocs=lablocs, LabelMids=LabelMids))
}