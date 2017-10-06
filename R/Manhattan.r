Manhattan <- function(pvals, 
						pt_cols="black",
						yaxis.max=30, 
						yaxis.ticks.by=5,
						xlab="",
						ylab="Negative log10 P value",
						add.bonferroni=TRUE,
						font.bonferroni=1,
						new.plot=TRUE,
						blanks.after=NULL,
						cex=1, 
						xaxis.label.line=0.5, 
						margins=c(3,6,0.1,2), 
						pch=1, 
						pt.lwd=1,
						draw.base.line=TRUE) 
{
						

	npts <- length(pvals)
	npts.original <- npts
	cat(paste0("Manhattan plot with ", npts, " points\n"))
	flush.console()
	log10pvals <- (-1)*log10(pvals)
	bonferroni_level <- (-1)*log10(0.05/npts)
	
	ymax <- max(ceiling(bonferroni_level), ceiling(max(log10pvals)))+1
	
	# if the length of pt_cols is 1 expand it to the right length
	# if its something else not equal to the length of pvals then halt
	if (length(pt_cols)==1) {
		pt_cols <- rep(pt_cols,npts)
	} else if (length(pt_cols) != length(pvals)) {
		stop("Length of pt_cols doesn't seem to match length of pvals\n")
	}
	
	# do the same for pch
	if (length(pch) == 1) {
		pch <- rep(pch,npts)
	} else if (length(pch) != length(pvals)) {
		stop("Length of pch doesn't seem to match length of pvals\n")
	}
	
	# if blanks.after is not null, inject invisble points into the calculation
	if (!is.null(blanks.after)) {
	
		# create new vectors
		npts.new <- npts + length(blanks.after)
		log10pvals.new <- rep(0, npts.new)
		pt_cols.new <- rep("black", npts.new)
		pch.new <- rep(NA, npts.new)
		
		# recalculate where the new blank positions must be
		blanks.new <- blanks.after + 1:(length(blanks.after))
		
		# put the old values in the new places
		log10pvals.new[!(1:npts.new %in% blanks.new)] <- log10pvals
		pt_cols.new[!(1:npts.new %in% blanks.new)] <- pt_cols
		pch.new[!(1:npts.new %in% blanks.new)] <- pch
		
		# replace the old versions
		log10pvals <- log10pvals.new
		pt_cols <- pt_cols.new
		pch <- pch.new
		
		npts <- npts.new
	
		
	}
	
	
	par(xpd=NA)
	par(lend=2)
	if (new.plot) {
		par(mar=margins)
		blankPlot(xlim=c(0,npts+1), ylim=c(0,yaxis.max), mainfont=cex)
	}
	
	if (draw.base.line) {
		lines(x=c(0,npts+1), y=c(0,0))
	}
	

	points(1:npts, log10pvals, col=pt_cols, lwd=pt.lwd, pch=pch)
	
	if (add.bonferroni) {
		lines(x=c(0,npts+1), y=rep(bonferroni_level,2), lty="longdash", col="grey60", lwd=1)
		text(x=0+0.1*strwidth("A"), y=bonferroni_level+1*strheight("A"), labels=paste0("Bonferroni correction threshold (", npts.original, " tests, p=           )"), adj=0, col="grey60", font=font.bonferroni)
		writePvalue(pvalues=0.05/npts, x=0.1*strwidth("A")+strwidth(paste0("Bonferroni correction threshold (", npts.original, " tests, p="), font=font.bonferroni), y=bonferroni_level+1.1*strheight("A"), adj=0, col="grey60", font=font.bonferroni)
	}
	
	
	Yaxis(yticks=seq(0,yaxis.max,by=yaxis.ticks.by))
	
	AxisLabels(xlab=xlab, ylab=ylab, cex=1, xline=xaxis.label.line)
	
	# return a list of the details about what was calculated
	return.value <- list(pts.x = (1:npts)[!(1:npts %in% blanks.new)], pts.y = log10pvals, bonferroni.level=bonferroni_level)
	return(return.value)
	

}