bubblePlot <- function( Z,
						xlim=NULL, 
						ylim=NULL, 
						xn=20, 
						yn=20, 
						new.plot=TRUE, 
						drawGrid=FALSE,
						addText=FALSE, 
						offsetTextMin=200, 
						cexText=1, 
						colText=1, 
						cex.unit=1,
						pch=21,
						colBubble=2,
						colBubbleBorder=1,
						mar=c(4,4,4,4), 
						scalefn=function(x) sqrt(x), 
						axes=TRUE, 
						xaxis.line=1,
						yaxis.line=1,
						mainfont=1,
						useHardcore=FALSE,
						HCintensity=100,
						HCinhibition=0.01,
						useParallel=TRUE) {

	# Z is the two dimensional data matrix
						
	curxpd <- par("xpd")

	# if xlim and ylim are missing, guess them
	if (is.null(xlim)) {
		xticks <- pretty(range(Z[,1]))
		xlim <- c(xticks[1], xticks[length(xticks)])
	}else{
		xticks <- pretty(xlim)
	}
	if (is.null(ylim)) {
		yticks <- pretty(range(Z[,2]))
		ylim <- c(yticks[1], yticks[length(yticks)])
	}else{
		yticks <- pretty(ylim)
	}

	
	if (new.plot) {
		par(mar=mar)
		blankPlot(xlim=xlim, ylim=ylim, mainfont=mainfont)
	}
	
	# plot in order of size, so biggest first
	if (!useHardcore) {
	
		mygrid <- makeGrid(xlim=xlim, ylim=ylim, xn=xn, yn=yn)

		# use parallel here to speed things up if a large grid is requested...
		# (speeding up my rubbish code using multiple cores seems like a bit of a cheat
		#  and I should probably work out how to do this faster...)
		if (all(useParallel & any(xn > 20, yn > 20))) {
			if (library(parallel, quietly=TRUE, logical.return=TRUE)) {
				nc <- detectCores()
				cl <- makeCluster(nc, methods=FALSE)
				cexs <- parApply(cl, mygrid, 1, gridCount, dat=Z)
				stopCluster(cl)
				rm(cl)
			} else {
				stop("Error loading parallel package. Set useParallel=FALSE.")
			}
		} else {
			cexs <- apply(mygrid, 1, gridCount, dat=Z)
		}
		
		neworder <- order(cexs, decreasing=TRUE)
		mygrid <- mygrid[neworder,]
		cexs <- cexs[neworder]
		
		# draw the grid underneath if requested
		par(xpd=FALSE)
		if (drawGrid) {
			abline(h=unique(mygrid[,2]), col="grey", lty="dashed", lend=2)
			abline(v=unique(mygrid[,1]), col="grey", lty="dashed", lend=2)
		}
		par(xpd=NA)

		bg <- colBubble
		
		points(apply(mygrid[,c(1,3)], 1, mean), apply(mygrid[,c(2,4)],1,mean), cex=scalefn(cexs*cex.unit), bg=bg, col=colBubbleBorder, pch=pch)
		# points(apply(mygrid[,c(1,3)], 1, mean), apply(mygrid[,c(2,4)],1,mean), cex=scalefn(cexs*cex.unit), bg=rgb(0,0,0,0,maxColorValue=255), pch=21)
		
	} else if (useHardcore) {
	
		require(fields)
		require(spatstat)
	
		pp <- rHardcore(HCintensity,HCinhibition)
		pp <- cbind(pp$x, pp$y)
		
		# map the point process onto the current plot area
		pp[,1] <- pp[,1]*(diff(xlim)) + xlim[1]
		pp[,2] <- pp[,2]*(diff(ylim)) + ylim[1]

		dmat <- rdist(Z,pp)
		alloc <- apply(dmat, 1, which.min)
		cexs <- sapply(1:(dim(pp)[1]), function(z) sum(alloc==z))
		
		bg <- 2
			
		
		par(xpd=NA)
		points(pp[,1], pp[,2], cex=scalefn(cexs*cex.unit), bg=bg, col=colBubbleBorder, pch=21)
		points(pp[,1], pp[,2], cex=scalefn(cexs*cex.unit), bg=rgb(0,0,0,0,maxColorValue=255), pch=21)
	
	
	
	}
	
	if (addText) {
		midpointsx <- apply(mygrid[,c(1,3)], 1, mean)
		midpointsy <- apply(mygrid[,c(2,4)], 1, mean)
		for (i in (1:length(cexs))) {
			if (cexs[i] >= offsetTextMin) {
				text(midpointsx[i], midpointsy[i], labels=ifelse(cexs[i] > 0, cexs[i], ""), cex=cexText, col=colText)
			} else {
				# text(midpointsx[i]+(diff(sort(unique(midpointsx)))[1]/4), midpointsy[i]+(diff(sort(unique(midpointsy)))[1]/4), labels=ifelse(cexs[i] > 0, cexs[i], ""), cex=cexText)
			}
		}
	}
	
	if (axes) {
		Xaxis(xticks,line=xaxis.line)
		Yaxis(yticks,line=yaxis.line)
	}
	par(xpd=curxpd)
}

makeGrid <- function(xlim, ylim, xn, yn) {

	xs <- rep(seq(xlim[1], xlim[2], length=xn+1)[-1], yn)
	ys <- rep(seq(ylim[1], ylim[2], length=yn+1)[-1], each=xn)
	xgap <- diff(seq(xlim[1], xlim[2], length=xn+1)[1:2])
	ygap <- diff(seq(ylim[1], ylim[2], length=yn+1)[1:2])
	
	return(cbind(xs, ys, xs - xgap, ys - ygap))

}


gridCount <- function(z, dat) {

	datx <- dat[,1]
	daty <- dat[,2]
	
	return(sum(datx <= z[1] & daty <= z[2] & datx > z[3] & daty > z[4]))

}