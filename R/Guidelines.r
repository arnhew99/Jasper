Guidelines <- function(GuideLineType=NULL, GuideLineLocs=NULL) {

	if (!is.null(GuideLineType)) {
	
	  if (is.null(GuideLineLocs)) stop("Error in plot_line: guide lines with no locations")

	  # complex setting of y-values to make the lines run from the axis.
	  # I am not quite sure how this is working, but empirically...
	  guidemin <- par("usr")[3] - par("cxy")[2]
	  guidemax <- par("usr")[4] + par("cxy")[2]
	  segments(GuideLineLocs, guidemin, GuideLineLocs, guidemax, lty=GuideLineType, lend=2)
	}

}