### set up a blank plot

blankPlot <- function(xlim, ylim, mainfont=1, XLogScale=FALSE, YLogScale=FALSE) {

	par(cex=mainfont)
	logtext <- ""
	if (XLogScale) logtext <- paste0(logtext, "x")
	if (YLogScale) logtext <- paste0(logtext, "y")
	
	plot(0,0, xlim=xlim, ylim=ylim, axes=FALSE, type="n", xlab="", ylab="", xaxs="i", yaxs="i", log=logtext)
	


}