# draw the x-axis, which Paul needed a lot of customisation for. We include those settings here, 
# although we will probably want to set some defaults at some point. 
# There's a lot of checking whether things are NULL, so we'll set those to NULL by default so 
# that the function still works.

Xaxis <- function(xticks, ticklabs=NULL, mainfont=1, VerticalTickLabs=FALSE, BootLeft=FALSE, BootRight=FALSE, lwd=1, allX=TRUE, line=1, labels.line=1, tcl=-0.5) {

	if (allX) {
		# plot an X axis that covers the whole range of X
		cur.xpd <- par("xpd")
		par(xpd = FALSE)
		# get the current xlimits
		xlim <- par("usr")[1:2]
		outerticks <- c(xlim[1] - diff(xlim)/2, xlim[2] + diff(xlim)/2)
		axis(at=outerticks, side=1, font.axis=2, las=3, cex.axis=mainfont, las=1, line=line, lend=1, ljoin=0, lwd=lwd,tcl=tcl)
		par(xpd = cur.xpd)
	}
	if (is.null(ticklabs)) axis(at=xticks, labels=Tick_Pretty(xticks), side=1, font.axis=2, las=3, cex.axis=mainfont, las=1, line=line, lend=1, ljoin=0, lwd=lwd, mgp=c(3,labels.line,0))
	if (!is.null(ticklabs)) {

	  LasVal <- 1
	  
	  if (VerticalTickLabs==TRUE) LasVal <- 3
	  
	  if (length(xticks)!=length(ticklabs)) stop("XTICKS and TICKLABS are not the same length.")
	  
		#     For the moment, always boot user-supplied axis labels, but I must do this some other way if booting becomes popular
		#     The noboot setting below also generates warnings
	  NoBoot<-TRUE
	  
	  if (any(BootLeft) || any(BootRight)) NoBoot <- (!BootLeft) & (!BootRight)
	  
	  if (any(NoBoot)) axis(at=xticks[NoBoot], labels=ticklabs[NoBoot], side=1, font.axis=2, las=3, cex.axis=1.0*mainfont, las=LasVal, line=line, lend=1, ljoin=0, lwd=lwd,tcl=tcl, mgp=c(3,labels.line,0))
	  
	  if (any(BootLeft)) axis(at=xticks[BootLeft], labels=ticklabs[BootLeft], side=1, font.axis=2, las=3, cex.axis=1.0*mainfont, las=LasVal, line=line, padj=0, lend=1, ljoin=0, lwd=lwd, mgp=c(3,labels.line,0))
	  
	  if (any(BootRight)) axis(at=xticks[BootRight], labels=ticklabs[BootRight], side=1, font.axis=2, las=3, cex.axis=1.0*mainfont, las=LasVal, line=line, padj=1, lend=1, ljoin=0, lwd=lwd, mgp=c(3,labels.line,0))

	}
  
}