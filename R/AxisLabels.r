# Adds the axis labels with the font customisation required.

AxisLabels <- function(xlab, ylab, mainfont=NULL, xline=3.5, yline=4, cex=NULL, adj=0.5) {

	if (is.null(mainfont) & is.null(cex)) stop("ERROR: One of mainfont or cex needs to be supplied")
	
	if (is.null(mainfont) & !is.null(cex)) mainfont <- cex
	
   title(line=yline, ylab=ylab, font.lab=2, cex.lab=mainfont, adj=adj)
   title(xlab=xlab, font.lab=2, line=xline, cex.lab=mainfont, adj=adj)

}