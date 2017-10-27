Histogram <- function(values, 
						breaks=NULL, 
						xlim=NULL, 
						ylim=NULL, 
						xregion=par("usr")[1:2], 
						yregion=par("usr")[3:4], 
						addNorm=TRUE, 
						addLogNorm=TRUE, 
						addQAxis=TRUE, 
						addXAxis=TRUE, 
						addYAxis=TRUE, 
						addLegend=TRUE, 
						addQuantiles=TRUE,
						xticks=NULL,
						ypercent=FALSE,
						yticks=NULL,
						yticklabs=NULL,
						xlab="", 
						ylab="", 
						LogData=FALSE,
						col=NA,
						border="black") 
{

	clend <- par("lend")
	par(lend=2)

	if (is.null(breaks)) {
		qs <- quantile(values, c(0.002, 0.998), na.rm=TRUE)
		breaks <- seq(qs[1], qs[2], length=30)
	}
	if (is.null(xlim)) {
		xlim <- c(min(breaks), max(breaks))
	}
	breaks <- c(-Inf, breaks, Inf)
	histfit <- hist(values, breaks=breaks, plot=FALSE)
	if (is.null(ylim)) {
		ylim <- c(min(histfit$density), max(histfit$density))
	}

	for (i in 2:length(histfit$breaks)) {

		cbreak <- histfit$breaks[i]
		cdens <- histfit$density[i]
		cbreak1 <- histfit$breaks[i+1]
		cdens1 <- histfit$density[i+1]
		
		if (cbreak > xlim[2]) next
		if (cbreak < xlim[1]) next
		
		
		p1 <- QMap(cbreak, 0, xlim, ylim, xregion, yregion)
		p2 <- QMap(cbreak, cdens, xlim, ylim, xregion, yregion)
		p3 <- QMap(cbreak1, cdens, xlim, ylim, xregion, yregion)
		p4 <- QMap(cbreak1, cdens1, xlim, ylim, xregion, yregion)
		
		rect(xleft=p1[1], xright=p3[1], ybottom=p1[2], ytop=p3[2], col=col)
		
		lines(c(p1[1],p2[1]), c(p1[2], p2[2]), col=border)
		lines(c(p1[1],p3[1]), c(p2[2], p2[2]), col=border)
		lines(c(p4[1],p4[1]), c(p2[2], p4[2]), col=border)
		
	}

	
	if (addQAxis) {
	
		# add quantiles using a special axis
		Qaxis.at <- quantile(values, c(0.01, 0.05, 0.5, 0.95, 0.99), na.rm=TRUE)
		newxat <- rep(0,length(Qaxis.at))
		for (i in 1:length(newxat)) {
			newpt <- QMap(Qaxis.at[i], 0,xlim, ylim, xregion, yregion)
			newxat[i] <- newpt[1]			
		}
		# restrict to drawing points that are inside the plotting region
		keepx <- newxat >= xregion[1] & newxat <= xregion[2]
		axis(side=1, at=newxat[keepx], labels=c("1%", "5%", "Median", "95%", "99%")[keepx], pos=yregion[1], font.axis=1, cex.axis=0.8, las=1, lend=1, tcl=-0.25, mgp=c(3,0.1,0))
		quantilesx <- newxat[keepx]
		
		
	}
	
	if (addXAxis) {
	
		if (is.null(xticks)) {		
			Xaxis.at <- pretty(xlim)
			Xaxis.at <- Xaxis.at[Xaxis.at > min(xlim) & Xaxis.at < max(xlim)]
		} else {
			Xaxis.at <- xticks
		}
		newxat <- rep(0,length(Xaxis.at))
		for (i in 1:length(newxat)) {
			newpt <- QMap(Xaxis.at[i], 0,xlim, ylim, xregion, yregion)
			newxat[i] <- newpt[1]			
		}
		
		# add a base line over the entire length of the xregion
		lines(x=xregion, y=rep(yregion[1],2), lend=2)
		
		# add the axis with ticks
		axis(side=1, at=newxat, labels=Xaxis.at, pos=yregion[1], font.axis=2, las=1, lend=1, tcl=ifelse(addQAxis,-0.9,-0.5), mgp=c(3,ifelse(addQAxis,1,0.6),0))
		
		# add the axis labels
		text(x=mean(xregion), y=yregion[1]-ifelse(addQAxis,4,3.5)*strheight("A"), labels=xlab, font=2, adj=c(0.5,1))
	}
		
		
	if (addYAxis) {	
	
		if (ypercent) {
			# need to work out the scaling factor from density to counts
			nonzero_counts <- histfit$counts > 0
			count_scaling <- (histfit$counts[nonzero_counts] / histfit$density[nonzero_counts])[1]
			percent_scaling <- count_scaling / length(values) * 100

			if (is.null(yticks)) {
				yticks <- c(0,20,40,60,80)
			}
			yticklabs <- yticks
			
			Yaxis.at <- yticks / percent_scaling
		
		} else {
			if (is.null(yticks)) {
				Yaxis.at <- pretty(ylim)
			} else {
				Yaxis.at <- yticks
			}
			if (any(is.null(yticklabs), !is.null(yticks))) {
				yticklabs <- Yaxis.at
			}
		}
		
		
		newyat <- rep(0,length(Yaxis.at))
		for (i in 1:length(newyat)) {
			newpt <- QMap(0, Yaxis.at[i], xlim, ylim, xregion, yregion)
			newyat[i] <- newpt[2]			
		}
		axis(side=2, at=newyat, labels=as.character(yticklabs), pos=xregion[1]-strwidth("A"), font.axis=2, las=1, lend=1)
		text(y=mean(newyat), x=xregion[1]-strheight("A")-strwidth("0.00\t\t\t  ", font=2), labels=ylab, font=2, adj=c(0.5,1), srt=90)
			
	}
	
	if (addQuantiles) {
		# add a dashed line for default quantiles
		sapply(quantilesx, function(z) lines(x=rep(z,2), y=c(min(newyat), max(newyat)), col="green3", lty="dashed", lend=2))
	}
	
	if (all(addNorm, addLogNorm)) {
		legx <- diff(xregion)*0.6+xregion[1]
		# attempt to work out if the legend will be over the histogram
		# if it might be then anchor it to the left of the plot
		if (histfit$density[21] > 0.5*max(histfit$density)) {
			legy <- yregion[2] + 2*strheight("A") 
			legx <- xregion[1] - strwidth("A")
		} else legy <- yregion[2]
		if (addLegend) legend(x=legx, y=legy, legend=c("Fitted normal distribution", "Fitted log-normal distribution"), lty=1, col=c(2,4), bty="n", adj=c(0,0.5))
	} else if (addNorm) {
		legx <- diff(xregion)*0.6+xregion[1]
		# attempt to work out if the legend will be over the histogram
		# if it might be then anchor it to the left of the plot
		if (histfit$density[21] > 0.5*max(histfit$density)) {
			legy <- yregion[2] + 2*strheight("A") 
			legx <- xregion[1] - strwidth("A")
		} else legy <- yregion[2]
		if (addLegend) legend(x=legx, y=legy, legend=c("Fitted normal distribution"), lty=1, col=2, bty="n", adj=c(0,0.5))
	} else if (addLogNorm) {
		legx <- diff(xregion)*0.6+xregion[1]
		# attempt to work out if the legend will be over the histogram
		# if it might be then anchor it to the left of the plot
		if (histfit$density[21] > 0.5*max(histfit$density)) {
			legy <- yregion[2] + 2*strheight("A") 
			legx <- xregion[1] - strwidth("A")
		} else legy <- yregion[2]
		if (addLegend) legend(x=legx, y=legy, legend=c("Fitted log-normal distribution"), lty=1, col=4, bty="n", adj=c(0,0.5))
	}
	if (addNorm) {
		xs <- seq(xlim[1], xlim[2], length=100)
		if (LogData) {
			normfit <- fitdistr(exp(na.omit(values)), "normal")[[1]]
			int <- integrate(function(x) dnorm(exp(x), mean=normfit[1], sd=normfit[2]), lower=xlim[1], upper=xlim[2])$value
			ys <- dnorm(exp(xs), mean=normfit[1], sd=normfit[2])/int
		} else {
			normfit <- fitdistr(na.omit(values), "normal")[[1]]
			ys <- dnorm(xs, mean=normfit[1], sd=normfit[2])
		}
		newxs <- rep(0,100)
		newys <- rep(0,100)
		for (i in 1:100) {
			newpt <- QMap(xs[i], ys[i],xlim, ylim, xregion, yregion)
			newxs[i] <- newpt[1]
			newys[i] <- newpt[2]
		}
		newys <- ifelse(newys > yregion[1] + 1.5*diff(yregion), NA, newys)
		lines(newxs, newys, col=2)
	}
	if (addLogNorm) {
		xs <- seq(xlim[1], xlim[2], length=100)
		if (LogData) {
			lognormfit <- fitdistr(na.omit(values), "normal")[[1]]
			ys <- dnorm(xs, mean=lognormfit[1], sd=lognormfit[2])
		} else {
			lognormfit <- fitdistr(na.omit(values[values > 0]), "log-normal")[[1]]
			ys <- dlnorm(xs, mean=lognormfit[1], sd=lognormfit[2])
		}
		newxs <- rep(0,100)
		newys <- rep(0,100)
		for (i in 1:100) {
			newpt <- QMap(xs[i], ys[i],xlim, ylim, xregion, yregion)
			newxs[i] <- newpt[1]
			newys[i] <- newpt[2]
		}
		newys <- ifelse(newys > yregion[1] + 1.5*diff(yregion), NA, newys)
		lines(newxs, newys, col=4)
	}
	# add the axis extensions
	lines(xregion, c(yregion[1],yregion[1]))
	
	
	par(lend = clend)
	
}