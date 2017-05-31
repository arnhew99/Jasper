## draw the X-axis on an ONYX forest plot 
## MA: the parameters are a little obscure, need some examples to work out what they all mean.

ONYXForestXAxis <- function(Range, 
							LogScale=TRUE, 
							xNoEffect=NULL, 
							NoEffect.lty="solid",
							blanks=NULL, 
							yspacing=NULL, 
							Ymax=100, 
							Ymin=0, 
							xaxmin, 
							xaxmax, 
							xticks, 
							TickLabs=NULL, 
							mainfont, 
							label=TRUE, 
							xlab, 
							xlab.adjust=0,
							XLabelFont, 
							lwd=1,
							tcl=0.25) 
{

	id <- function(x) x
	
	if (LogScale) logfn <- log else logfn <- id

	#  Parameter to convert betas to paper...
	xf <- (xaxmax-xaxmin)/(logfn(Range[2])-logfn(Range[1]))
	#  Paper location of line of no effect; note that logfn(Range[1]) is probably 0 or negative
	xax <- xaxmin - (xf*logfn(Range[1]))

	Font <- mainfont
	if (is.null(xNoEffect)) xNoEffect <- 0
	# print(Range)
	# print(xNoEffect)
	# print(logfn)
	
	if (!is.na(xNoEffect)) {
		if (any(logfn(min(Range)) > xNoEffect, logfn(max(Range)) < xNoEffect)) xNoEffect <- NULL
		# print(paste("no effect line should be at", xNoEffect))
		#  Draw axes
		if (!(is.null(xNoEffect))) {
			# if there aren't any blanks, or if $hideNoEffect hasn't been provided, then do the default
			if (!is.null(blanks) & is.null(blanks$hideNoEffect)) {
				# need to compute underlying coordinates
				#underlying.coord <- xaxmin + ((xNoEffect - logfn(Range[1])) / (logfn(Range[2])-logfn(Range[1])))*(xaxmax - xaxmin)
				underlying.coord <- xaxmin + (xNoEffect - logfn(Range[1]))*xf
				segments(underlying.coord, Ymin, underlying.coord, 0.98*Ymax, lend=2, lwd=lwd, lty=NoEffect.lty)
			} else if (!(is.null(blanks$hideNoEffect))) {
				
				# if all are FALSE, then do the default
				if (all(!blanks$hideNoEffect)) {
				
					underlying.coord <- xaxmin + (xNoEffect - logfn(Range[1]))*xf
					segments(underlying.coord, Ymin, underlying.coord, 0.98*Ymax, lend=2, lwd=lwd, lty=NoEffect.lty)
				
				} else {
				
					# now we have the problem...
					# want to divide up the xNoEffect line in the y-direction
					underlying.coord <- xaxmin + (xNoEffect - logfn(Range[1]))*xf
					
					# assemble a series of break points
					y.breaks <- c(0, rev(yspacing$BlankLocs[blanks$hideNoEffect]), 100)
					
					# attempt to work out what the gap between lines is
					line.gap <- min(c(abs(diff(yspacing$YLocs)), abs(diff(yspacing$BlankLocs))))
					
					for (i in 1:(length(y.breaks)-1)) {
					
						low.y <- ifelse(y.breaks[i] == 0, 0, y.breaks[i] + (line.gap/2))
						high.y <- ifelse(y.breaks[i] == 100, 100, y.breaks[i+1] - (line.gap/2))
						# print(c(low.y, high.y))
						# the function here tests equality within the machine epsilon
						if (isTRUE(all.equal(low.y, high.y))) next else segments(underlying.coord, low.y, underlying.coord, high.y, lend=2, lwd=lwd)
					
					}
				
				
				
				}
				
			} else if (is.null(blanks)) {
			
				underlying.coord <- xaxmin + (xNoEffect - logfn(Range[1]))*xf
				segments(underlying.coord, Ymin, underlying.coord, 0.98*Ymax, lend=2, lwd=lwd)
			
			}
		
		}
	} else {
		xNoEffect <- NULL 
	}
	# else if (xax >= xaxmin) segments(xax, 0, xax, Ymax)
	segments(xaxmin, Ymin, xaxmax, Ymin, lend=2, lwd=lwd)

	#  Axis ticks and tick labels
	if (is.null(TickLabs)) TickLabs <- Tick_Pretty(xticks)
	#print(TickLabs)
	axis(side=1, at=xax+(logfn(xticks)*xf), labels=FALSE, pos=Ymin, tcl=tcl, font=2, padj=-1, lend=2, lwd=lwd, lwd.ticks=lwd)

	#  Now draw the tick labels manually so that I have full control and know exactly where everything is 
	#  going
	text(xax+(logfn(xticks)*xf), Ymin-strheight("2", cex=Font), labels=TickLabs, cex=Font, adj=c(0.5, 0.5), font=2)

	#  Record the bottom of the tick labels
	TickLabBottom <-  Ymin -1.5*strheight("2", cex=1.1*mainfont)
	
	if (label) {
	
		#  x-axis label
		xlabel<-"Hazard ratio"
		if (length(xlab)>0) xlabel<-xlab
		nlines <- length(xlabel)

		XLabelFont <- Font
		XLabelY <- TickLabBottom - (seq(1, nlines)*strheight("A", cex=XLabelFont))
	
		# by default xlab.adjust = 0
		XLabelY <- XLabelY - xlab.adjust*strheight("A")
	
		text(0.5*(xaxmin+xaxmax), XLabelY, xlabel, adj=c(0.5, 1), cex=XLabelFont, font=2)
	
	}
	
	return(TickLabBottom)
	
}