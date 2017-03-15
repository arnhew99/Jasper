Histogram <- function(values, breaks=NULL, xlim=NULL, ylim=NULL, xregion, yregion, addNorm=TRUE, addLogNorm=TRUE, addQAxis=TRUE, addXAxis=TRUE, addYAxis=TRUE, addLegend=TRUE, xlab="", ylab="", LogData=FALSE) {

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
		
		
		lines(c(p1[1],p2[1]), c(p1[2], p2[2]))
		lines(c(p1[1],p3[1]), c(p2[2], p2[2]))
		lines(c(p4[1],p4[1]), c(p2[2], p4[2]))
		
	}

	
	if (addQAxis) {
	
	
		# add quantiles using a special axis
		Qaxis.at <- quantile(values, c(0.01, 0.05, 0.5, 0.95, 0.99), na.rm=TRUE)
		newxat <- rep(0,length(Qaxis.at))
		for (i in 1:length(newxat)) {
			newpt <- QMap(Qaxis.at[i], 0,xlim, ylim, xregion, yregion)
			newxat[i] <- newpt[1]			
		}
		axis(side=1, at=newxat, labels=c("1%", "5%", "Median", "95%", "99%"), pos=yregion[1], font.axis=1, cex.axis=0.8, las=1, lend=1, tcl=-0.25, mgp=c(3,0.1,0))
		quantilesx <- newxat
	}
	
	if (addXAxis) {
		
		Xaxis.at <- pretty(xlim)
		Xaxis.at <- Xaxis.at[Xaxis.at > min(xlim) & Xaxis.at < max(xlim)]
		newxat <- rep(0,length(Xaxis.at))
		for (i in 1:length(newxat)) {
			newpt <- QMap(Xaxis.at[i], 0,xlim, ylim, xregion, yregion)
			newxat[i] <- newpt[1]			
		}
		axis(side=1, at=newxat, labels=Xaxis.at, pos=yregion[1], font.axis=2, las=1, lend=1, tcl=-0.9)
		# add the axis extensions
		text(x=mean(xregion), y=yregion[1]-4*strheight("A"), labels=xlab, font=2, adj=c(0.5,1))
	}
		
		
	if (addYAxis) {	
		Yaxis.at <- pretty(ylim)
		newyat <- rep(0,length(Yaxis.at))
		for (i in 1:length(newyat)) {
			newpt <- QMap(0, Yaxis.at[i], xlim, ylim, xregion, yregion)
			newyat[i] <- newpt[2]			
		}
		axis(side=2, at=newyat, labels=Yaxis.at, pos=xregion[1]-strheight("A"), font.axis=2, las=1, lend=1)
		text(y=mean(newyat), x=xregion[1]-strheight("A")-strwidth("0.00\t\t\t  ", font=2), labels=ylab, font=2, adj=c(0.5,1), srt=90)
		
		# add a dashed line for the median
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