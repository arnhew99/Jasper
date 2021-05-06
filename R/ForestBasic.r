ForestBasic <- function(rawdata, 
						LogScale=TRUE, 
						ExponentiateDataOnPlot=FALSE, 
						blanks=NULL, 
						Hets=NULL, 
						Trends=NULL, 
						xNoEffect=NULL, 
						NoEffect.lty="solid",
						xaxmin, 
						xaxmax, 
						xaxis=TRUE,
						ymin = 0,
						ymax = par("usr")[4],
						boxparmx=NULL, 
						boxparmy=NULL, 
						boxparm.stderr = NULL, 
						Range=NULL, 
						xlim=NULL, 
						xticks=NULL, 
						ticklabs=NULL, 
						xticks.ticklength=0.25,
						mainfont=1, 
						xlab="", 
						xlab.adjust=0,
						ValueLabels=TRUE, 
						ValueLabelsHeader="Hazard ratio", 
						ValueLabelsEffectSE=FALSE,
						ValueDigits=NULL,
						ValueDigitsSE=NULL,
						separator=NULL, 
						NLabel=FALSE, 
						NLabelHeader="N",
						boxsizeoverride=FALSE, 
						roundedSquares=FALSE, 
						spacing=0, 
						verbose=TRUE,	
						drawPoints=TRUE,
						AdAM.mode=FALSE,
						pointGroups=NULL,
						pointGroupsFactor=0.9,
						lwd=1,
						CISecond=NULL) 
{
	
	# rawdata			data.frame with columns SUBGROUP, EVENTS, HR, LCI, UCI, stderr, IsDiamond, Fill, Show, Labels, Colours
	# LogScale			is the x-axis on the log scale?
	# blanks			data.frame needing column "after"
	# Hets				Heterogeneity tests
	# Trends			Trend tests
	# xNoEffect			where to put the line of no effect, in user coordinates
	# xaxmin			where to put the left of the x-axis, in the 0-100 underlying coordinate system
	# xaxmax			as above, but the right hand side of the x-axis
	# boxparmx			parameter that defines the size of the boxes (which can be determined automatically if left NULL)
	# boxparmy 			as above, y direction
	# Range				"true" range of the x-axis (user coordinates)
	# xticks			where to add ticks on the x-axis
	# ticklabs			custom tick labels at the locations given by xticks
	# mainfont			the overall font size from which all other font sizes are determined
	# xlab				x-axis label
	# ValueLabels		Logical, whether to show the labels to the right of the forest
	# ValueLabelsHeader	Title to put above the labels
	# separator			What separator to use between the LCI and UCI in the label
	# NLabel			Logical, whether to show the labels to the left of the forest
	# NLabelHeader		Title to put above the left labels
	# boxsizeoverride	Use boxparmx for every box (i.e. no adjustment in box size for stderr)
	# roundedSquares	Logical, use squares with rounded corners (MA thinks this looks good, you may disagree. One reason is that R draws lines with rounded ends, which is unpleasant, so we may as well be consistent)
	# spacing			Gap between the forest and the right hand labels (in 0-100 coordinates)
	

	
	
	# MA: do some checks about what rawdata contains, if it has missing (but necessary) columns, then add the defaults here 
	
	
	# check to see if rawdata is actually a list (and if it is, reassign its contents)
	if (all(is.list(rawdata), !is.data.frame(rawdata))) {
	
		force(rawdata.orig <- rawdata)
		force(rawdata <- rawdata.orig$forest)
		
		if (!is.null(rawdata.orig$blanks)) blanks <- rawdata.orig$blanks
		if (!is.null(rawdata.orig$Hets)) Hets <- rawdata.orig$Hets
		if (!is.null(rawdata.orig$Trends)) Trends <- rawdata.orig$Trends
		
		
	
	} else {
		force(rawdata.orig <- rawdata)
	}
	if (all(is.null(Range), !is.null(xlim))) Range <- xlim
	
	
	npoints <- dim(rawdata)[1]
	
	# for reasons, we may want to be given $RR, $Estimate or $LogRR instead of HR
	# so I'll just relabel them internally
	if (all(is.null(rawdata$HR), !is.null(rawdata$Estimate))) {
		rawdata$HR <- rawdata$Estimate		
	}
	if (all(is.null(rawdata$HR), !is.null(rawdata$LogRR))) {
		rawdata$HR <- rawdata$LogRR
	}
	if (all(is.null(rawdata$HR), !is.null(rawdata$RR))) {
		rawdata$HR <- rawdata$RR
	}
	
	
	# We compute LCI and UCI from the stderr if only that has been provided, but it's not that ideal, 
	# because there might be some weird reason for wanting something else
	if (is.null(rawdata$SUBGROUP)) {
		rawdata$SUBGROUP <- rep("",npoints)
		if (verbose) cat("$SUBGROUPs not specified (defaulting to 1 SUBGROUP).\n")
	}
	if (is.null(rawdata$LCI)) {
		rawdata$LCI <- rawdata$HR - 2*rawdata$stderr
		if (verbose) cat("$LCI not specified (computing from provided $stderr, care needed).\n")
	}
	if (is.null(rawdata$UCI)) {
		rawdata$UCI <- rawdata$HR + 2*rawdata$stderr
		if (verbose) cat("$UCI not specified (computing from provided $stderr, care needed).\n")
	}
	if (is.null(rawdata$boxsize)) {
		rawdata$boxsize <- rep(1, npoints)
		if (verbose) cat("$boxsize not specified, setting to 1.\n")
	}
	if (is.null(rawdata$EVENTS)) {
		rawdata$EVENTS <- rep(100,npoints)
		if (verbose) cat("$EVENTS not specified (defaulting to N=100).\n")
	}
	if (is.null(rawdata$IsDiamond)) {
		rawdata$IsDiamond <- rep(FALSE, npoints)
		if (verbose) cat("$IsDiamond not specified (defaulting to no diamonds).\n")
	}
	if (is.null(rawdata$DiamondGuidelines)) {
		rawdata$DiamondGuidelines <- rawdata$IsDiamond
		if (verbose) cat("$DiamondGuidelines not specified (any diamonds get a guideline).\n")
	}
	if (is.null(rawdata$Show)) {
		rawdata$Show <- rep(TRUE, npoints)
		if (verbose) cat("$Show not specified (defaulting to showing all).\n")
	}
	if (is.null(rawdata$ShowCI)) {
		rawdata$ShowCI <- rep(TRUE, npoints)
		if (verbose) cat("$ShowCI not specified (defaulting to showing all).\n")
	}
	if (is.null(rawdata$FillColour)) {
		rawdata$FillColour <- rep(1, npoints)
		if (verbose) cat("$FillColour not specified (defaulting to black).\n")
	}	
	# if (is.null(rawdata$Colours)) {
		# rawdata$Colours <- rep(1, npoints)
		# if (verbose) cat("$Colours not specified (defaulting to black).\n")
	# }
	if (!("Fill" %in% names(rawdata))) {
		rawdata$Fill <- rep(TRUE,npoints)
		if (verbose) cat("$Fill not specified (defaulting to solid colour).\n")
	}
	
	if (is.null(Range)) {
		if (LogScale) Range <- c(exp(min(rawdata$LCI)), exp(max(rawdata$UCI))) else Range <- c(min(rawdata$LCI), max(rawdata$UCI))
		if (verbose) {
			cat("Range not specified (making a guess).\n")
			print(Range)
		}
	}
	
	if (is.null(xticks)) {
		xticks <- pretty(Range, n=5)
		# use .Machine$double.eps here because the logic here behaves weirdly if max(Range) == max(xticks)
		if (length(which(xticks - .Machine$double.eps > max(Range))) != 0) xticks <- xticks[-which(xticks > max(Range))]
		if (length(which(xticks + .Machine$double.eps < min(Range))) != 0) xticks <- xticks[-which(xticks < min(Range))]
		if (verbose) { 
			cat("xticks not specified (making a guess).\n")
			print(xticks)
		}
	}
	
	if (is.null(ticklabs)) {
	
		ticklabs <- Tick_Pretty(xticks)
		if (verbose) cat("ticklabs not specified (setting to xticks)\n")
	
	}
	
	if (is.null(separator)) {
		separator <- ", "
		if (verbose) cat("CI separator not specified (defaulting to ,).\n")
	}
	
	if (all(is.null(pointGroups), !is.null(rawdata.orig$pointGroups))) {
		pointGroups <- rawdata.orig$pointGroups
	}

	# evaluate the yspacings
	if (!is.null(blanks)) blanksafter = blanks$after else blanksafter = NULL
	if (!is.null(Hets)) hetsafter = Hets$after else hetsafter=NULL
	if (!is.null(Trends)) trendsafter = Trends$after else trendsafter = NULL

	yspacing <- ONYXForestYSpacing(npoints=npoints, nhets=ifelse(is.null(Hets),0,dim(Hets)[1]), ntrends=ifelse(is.null(Trends),0,dim(Trends)[1]), nblanks=ifelse(is.null(blanks),0,dim(blanks)[1]), Ymax=ymax, Ymin=ymin, IsDiamond=rawdata$IsDiamond, StrayTextAfter=blanksafter, HetAfter=hetsafter, TrendAfter=trendsafter, AdAM.mode=AdAM.mode, pointGroups=pointGroups, pointGroupsFactor=pointGroupsFactor)
	
	
	# draw the X axis
	if (xaxis) ONYXForestXAxis(Range=Range, LogScale=LogScale, xNoEffect=xNoEffect, blanks=blanks, yspacing=yspacing, xaxmin=xaxmin, xaxmax=xaxmax, xticks=xticks, TickLabs=ticklabs, mainfont=mainfont, xlab=xlab, xlab.adjust=xlab.adjust, XLabelFont=1, Ymin=ymin, Ymax=ymax, lwd=lwd, tcl=xticks.ticklength, NoEffect.lty=NoEffect.lty)
	
	npoints <- dim(rawdata)[1]
	
	# calculate a boxparm if needed
	if (is.null(boxparmx)) {
		if (is.null(boxparm.stderr)) {
			boxparms <- ONYXForestBoxparm(boxparm=NULL, stderr=rawdata$stderr[rawdata$Show & rawdata$ShowCI], Ymax=ymax, YLast = yspacing$YLast)
			boxparmx <- boxparms$boxparmx
			boxparmy <- boxparms$boxparmy
		}
		else {
			boxparms <- CalcBoxParm(boxparm.stderr, xlim=c(xaxmin, xaxmax))
			boxparmx <- boxparms[1]
			boxparmy <- boxparms[2]
		}
	}
	
	if (is.null(CISecond)) CISecond <- TRUE
	

	# add the points
	if (drawPoints) {
		XLocs <- ONYXForestDrawPoints(estim=rawdata$HR, 
		lower=rawdata$LCI, 
		upper=rawdata$UCI, 
		stderr=rawdata$stderr, 
		boxsize=rawdata$boxsize,
		LogScale = LogScale, 
		ExponentiateDataOnPlot=ExponentiateDataOnPlot, 
		npoints=npoints, 
		Range=Range, 
		xaxmin=xaxmin, 
		xaxmax=xaxmax, 
		boxparmx=boxparmx, 
		boxparmy=boxparmy, 
		YLocs=yspacing$YLocs, 
		YLast=yspacing$YLast, 
		IsDiamond=rawdata$IsDiamond, 
		DiamondGuidelines=rawdata$DiamondGuidelines, 
		Fill=rawdata$Fill, 
		Show=rawdata$Show, 
		ShowCI=rawdata$ShowCI, 
		PColours=rawdata$FillColour, 
		boxsizeoverride=boxsizeoverride, 
		roundedSquares=roundedSquares, 
		lwd=lwd,
		CISecond=CISecond)
	
	
		# add guidelines and N, hazard ratio infos
		ONYXForestGuidelines(npoints=npoints, IsDiamond=force(rawdata$IsDiamond & rawdata$DiamondGuidelines & rawdata$Show), atvals=XLocs, YLocs=yspacing$YLocs, YLast=yspacing$YLast, blanks=blanksafter, OvlineType="dashed")
	}
	
	if (ValueLabels) {
		middlevaluelabel <- ONYXForestValues(estim=rawdata$HR, upper=rawdata$UCI, lower=rawdata$LCI, stderr=rawdata$stderr, isLogged=ExponentiateDataOnPlot, xaxmax=xaxmax, YLocs=yspacing$YLocs, Boldness=ifelse(rawdata$IsDiamond,2,1), mainfont=mainfont, separator=separator, Show=rawdata$Show, ShowCI=rawdata$ShowCI, spacing=spacing, digits=ValueDigits, digits_SE=ValueDigitsSE, ValueLabelsEffectSE=ValueLabelsEffectSE)
		ONYXForestValueLabels(ThisCol=ValueLabelsHeader, MidValLab=middlevaluelabel, mainfont=mainfont, Ymax=ymax)
	}
	if (NLabel) {
		middlenevents <- ONYXForestNEvents(xaxmin=xaxmin, YLocs=yspacing$YLocs, nevents=rawdata$EVENTS, Boldness=ifelse(rawdata$IsDiamond,2,1), mainfont=mainfont)
		ONYXForestNEventsLabel(EventsTit=NLabelHeader, MidNEvents=middlenevents, mainfont=mainfont)
	}
	
	
	return(yspacing)
}
