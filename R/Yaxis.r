## plot the y-axis, putting it on a log scale if necessary

Yaxis <- function(yticks, LinScale=TRUE, ticklabs=NULL, mainfont=1, side=2, lwd=1, col=1, line=1, allY=TRUE) {

	# LinScale 			linear scale for the y-axis?
	# yticks			Where are we putting the ticks
	# mainfont			the main font scaling parameter
	if (is.null(ticklabs)) ticklabs=Tick_Pretty(yticks)
	
	if (allY) {
		# plot an X axis that covers the whole range of X
		cur.xpd <- par("xpd")
		par(xpd = FALSE)
		# get the current xlimits
		ylim <- par("usr")[3:4]
		outerticks <- c(ylim[1] - diff(ylim)/2, ylim[2] + diff(ylim)/2)
		axis(at=outerticks, side=side, line=line, font.axis=2, cex.axis=mainfont, las=1, lend=1, lwd=lwd, col=col)
		par(xpd = cur.xpd)
	}

   if (LinScale == FALSE) axis(at=log(yticks), labels=ticklabs, side=side, line=line, font.axis=2, cex.axis=mainfont, las=1, lend=1, lwd=lwd, col=col, col.axis=col)
	  
   if (LinScale == TRUE) axis(at=yticks, labels=ticklabs, side=side, line=line, font.axis=2, cex.axis=mainfont, las=1, lend=1, lwd=lwd, col=col, col.axis=col)

}