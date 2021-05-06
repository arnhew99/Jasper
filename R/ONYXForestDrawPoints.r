##
ONYXForestDrawPoints <- function(estim, lower, upper, boxsize, LogScale=FALSE, ExponentiateDataOnPlot=FALSE, stderr, npoints, Range, xaxmin, xaxmax, boxparmx, boxparmy, YLocs, YLast, IsDiamond, DiamondGuidelines, Fill, Show, ShowCI, PColours, Guidelines=FALSE, boxsizeoverride=FALSE, roundedSquares=FALSE, lwd=1, CISecond=TRUE) {

	if (LogScale) {
		
		# should really be able to work out xaxmin and xaxmax from the current plotting window
		xf <- (xaxmax-xaxmin)/(log(Range[2])-log(Range[1]))
		#  Paper location of line of no effect; note that log(Range[1]) is probably 0 or negative
		xax <- xaxmin - (xf*log(Range[1]))
		
	}
	else {
	
		xf <- (xaxmax-xaxmin)/(Range[2]-Range[1])
		#  Paper location of line of no effect; note that log(Range[1]) is probably 0 or negative
		xax <- xaxmin - (xf*Range[1])
	
	}

	if (all(LogScale, !ExponentiateDataOnPlot)) {

		estim <- log(estim)
		lower <- log(lower)
		upper <- log(upper)
	
	}
	if (all(!LogScale, ExponentiateDataOnPlot)) {
	
		estim <- exp(estim)
		lower <- exp(lower)
		upper <- exp(upper)
	
	}
	
	# some page values
	lowvals <- xax + (xf * lower)
	highvals <- xax + (xf * upper)
	atvals <- xax + (xf * estim)
	# turn off box scaling if CIs are hidden
	if (!boxsizeoverride) {
		boxlefts <- atvals - (0.5*ifelse(ShowCI, boxparmx/stderr, 1))
		boxrights <- atvals + (0.5*ifelse(ShowCI, boxparmx/stderr, 1))
		boxtops <- YLocs + (0.5*ifelse(ShowCI, boxparmy/stderr, boxparmy/boxparmx))
		boxbottoms <- YLocs - (0.5*ifelse(ShowCI, boxparmy/stderr, boxparmy/boxparmx))

		# print((boxrights - boxlefts) * (boxtops - boxbottoms))
	} else {
		boxlefts <- atvals - (0.5*boxparmx) * sqrt(boxsize)
		boxrights <- atvals + (0.5*boxparmx) * sqrt(boxsize)
		boxtops <- YLocs + (0.5*boxparmy) * sqrt(boxsize)
		boxbottoms <- YLocs - (0.5*boxparmy) * sqrt(boxsize)
		cat("Box sizes are overridden, they are NOT inversely proportional to standard error\n")
	}
	
	#  Point Colours
	# PColours <- ThisPlot$PColours
	if (is.null(PColours)) PColours <- rep("black", npoints)
	if (length(PColours) < npoints) PColours <- rep(PColours, ceiling(npoints/length(PColours)))
	
	# # # # disable guidelines where appropriate
	# # # print(length(IsDiamond))
	# # # DiaGui <- force(IsDiamond & DiamondGuidelines)
	# # # print(DiaGui)
	# # # if (Guidelines) ONYXForestGuidelines(npoints=length(IsDiamond), IsDiamond=DiaGui, atvals=atvals, YLocs=YLocs, YLast=YLast, OvlineType="dashed")

	#  Point types
	OpenSquares  <- (Fill==FALSE) & (IsDiamond==FALSE) & (Show==TRUE)
	FillSquares  <- (Fill==TRUE)  & (IsDiamond==FALSE) & (Show==TRUE)
	Squares      <- (IsDiamond==FALSE) & (Show==TRUE)
	CIs 		 <- Squares & ShowCI
	WhiteConf    <- ((lowvals>boxlefts) | (highvals<boxrights)) & FillSquares  & (Show==TRUE) #& ((PColours=="black") | 
	Inside       <- ((lowvals>boxlefts) | (highvals<boxrights)) & FillSquares  & (Show==TRUE)
	Outside      <- (Inside==FALSE) & (Squares==TRUE) & (Show==TRUE)
	BlackConf    <- (WhiteConf==FALSE) & (Squares==TRUE) & (Show==TRUE)
	OpenDiamonds <- (Fill==FALSE) & (IsDiamond==TRUE) & (Show==TRUE)
	FillDiamonds <- (Fill==TRUE) & (IsDiamond==TRUE) & (Show==TRUE)
	
	# print(FillSquares)
	# print(OpenSquares)


	#  Clip where a square's black confidence interval goes outside the axis;
	#  (so white confidence intervals don't get clipped, which looks silly)
	# print(lowvals)
	# print(highvals)
	LowClip      <- (BlackConf==TRUE) & (lowvals < xaxmin)
	HighClip     <- (BlackConf==TRUE) & (highvals > xaxmax)
	# lowvals[LowClip] <- xax + xf*Range[1]
	lowvals[LowClip] <- xaxmin
	# highvals[HighClip] <- xax + xf*Range[2]
	highvals[HighClip] <- xaxmax
	# disable clipping if the box is outside the range 
	LowClip <- ifelse(lowvals < atvals, LowClip, FALSE)
	HighClip <- ifelse(highvals > atvals, HighClip, FALSE)
	
	
	# handle the case where the box is off the plotting range
	SuperLowClip <- lowvals > atvals
	SuperHighClip <- highvals < atvals

	#  Now to finally start plotting
	#  First the boxes
	#  Find the colours
	FillCol <- rep(NA, npoints)
	FillCol[FillSquares] <- PColours[FillSquares]

	#  line colours
	linecol.tmp <- FillCol
	linecol.tmp[linecol.tmp == 0] <- 1
	linecol.tmp[linecol.tmp == "white"] <- "black"
	
	LineCol <- linecol.tmp
	# don't want white lines if the box fill colour is also white
	LineCol[WhiteConf & FillCol != 0 & FillCol != "white"] <- "white"
	
	if (!CISecond) {
		segments(lowvals[CIs], YLocs[CIs], highvals[CIs], YLocs[CIs], col=LineCol[CIs], lend=2, lwd=lwd)
	}

	if (roundedSquares) {
		drawRB <- function(l,r,t,b,fillcol) roundedSquare(c(l,r),c(b,t), col=fillcol, border=1)
		mapply(drawRB, l=boxlefts[Squares], r=boxrights[Squares], t=boxtops[Squares], b=boxbottoms[Squares], fillcol=FillCol[Squares])
		#roundedSquare(c(boxlefts[Squares], boxrights[Squares]), c(boxbottoms[Squares], boxtops[Squares]), col=FillCol[Squares], border="black")
	} else {
		boxbordercol.tmp <- FillCol[Squares]
		# if (boxbordercol.tmp == 0 | boxbordercol.tmp == "white") boxbordercol.tmp <- "black"
		boxbordercol.tmp[boxbordercol.tmp == 0] <- 1
		boxbordercol.tmp[boxbordercol.tmp == "white"] <- "black"
		rect(boxlefts[Squares], boxbottoms[Squares], boxrights[Squares], boxtops[Squares], col=FillCol[Squares], border=boxbordercol.tmp)
	}

	
	# add the lines
	# segments(lowvals[Squares], YLocs[Squares], highvals[Squares], YLocs[Squares], col=LineCol[Squares], lend=2, lwd=lwd)
	if (CISecond) {
		segments(lowvals[CIs], YLocs[CIs], highvals[CIs], YLocs[CIs], col=LineCol[CIs], lend=2, lwd=lwd)
	}
	#  Arrows to indicate clipping
	#  This draws over the confidence intervals, which worries me a little (there will be extra 
	#  hidden lines when someone manipulates the image, I expect)
	ArLen<-1/12
	# print(c(LowClip, HighClip, SuperHighClip, SuperLowClip))
	if (any(LowClip))	arrows(atvals[LowClip], YLocs[LowClip], lowvals[LowClip], YLocs[LowClip], length=ArLen, col=LineCol[LowClip], lend=2, lwd=lwd)
	if (any(HighClip))	arrows(atvals[HighClip], YLocs[HighClip], highvals[HighClip], YLocs[HighClip], length=ArLen, col=LineCol[HighClip], lend=2, lwd=lwd)
	if (any(SuperLowClip))	arrows(highvals[SuperLowClip], YLocs[SuperLowClip], lowvals[SuperLowClip], YLocs[SuperLowClip], length=ArLen, col=LineCol[SuperLowClip], lend=2, lwd=lwd)
	if (any(SuperHighClip))	arrows(highvals[SuperHighClip], YLocs[SuperHighClip], lowvals[SuperHighClip], YLocs[SuperHighClip], length=ArLen, col=LineCol[SuperHighClip], lend=2, lwd=lwd)

	#  Now for the diamonds
	Heights <- rep(80/YLast, npoints)
	FillCol <- rep(NA, npoints)
	FillCol[FillDiamonds] <- PColours[FillDiamonds]
	#  Custom diamond plotting function
	# Diamonds(lowvals[IsDiamond], highvals[IsDiamond], atvals[IsDiamond], YLocs[IsDiamond], Heights[IsDiamond], col=FillCol[IsDiamond])
	Diamonds(lowvals[OpenDiamonds], highvals[OpenDiamonds], atvals[OpenDiamonds], YLocs[OpenDiamonds], Heights[OpenDiamonds], col=FillCol[OpenDiamonds])
	Diamonds(lowvals[FillDiamonds], highvals[FillDiamonds], atvals[FillDiamonds], YLocs[FillDiamonds], Heights[FillDiamonds], col=FillCol[FillDiamonds])

	return(atvals)
}