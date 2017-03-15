# optimiseMarginsForAspectRatio <- function(target.aspectratio, l.mar, r.mar, perpage, type, orient, interval=c(0,15)) {

	# getAspectRatio <- function(mar, perpage, type, orient) {
	
		# tmpdir <- normalizePath(Sys.getenv("TEMP"), winslash="/")
		# tmpfilestem <- paste0(tmpdir, "/", floor(runif(1)*1e8))
		# SetPage(type=type, perpage=perpage, orient=orient, filestem=tmpfilestem)
		# par(mar=mar)
		# blankPlot(xlim=c(0,1), ylim=c(0,1), mainfont=1)
		# plot.dimensions <- par("pin")
		# aspect.ratio <- plot.dimensions[1] / plot.dimensions[2]
		# closeFile("PDF", suppress.notice=TRUE)
		# if (type != "GUI") file.remove(paste0(tmpfilestem, ".", type))
		# return(aspect.ratio)
	# }

	# testAspectRatio <- function(tb.mar, l.mar, r.mar, target.aspectratio, perpage, type, orient) {
		# return(abs(getAspectRatio(c(tb.mar, l.mar, tb.mar, r.mar), perpage=perpage, type=type, orient=orient) - target.aspectratio))
	# }
	
	# optimised <- optimise(f=testAspectRatio, interval=interval, target.aspectratio=target.aspectratio, l.mar=l.mar, r.mar=r.mar, perpage=perpage, type=type, orient=orient)
	
	# topbottom.mar <- optimised$minimum
	
	# return(c(topbottom.mar, l.mar, topbottom.mar, r.mar))
# }


optimiseMarginsForAspectRatio <- function(target.aspectratio, l.mar, r.mar, interval=c(0,10), type, orient, ...) {

	# returns a vector of margins such that the displayed aspect ratio of the plotting area
	# is equal to target.aspectratio (width divided by height)
	# user needs to set left margin and right margin
	#
	# note that this is indifferent to the actual user coordinate system
	# it merely sets the plotting area to have the correct aspect ratio 

	getAspectRatio <- function(mar, type, orient, ...) {
	
		tmpdir <- normalizePath(Sys.getenv("TEMP"), winslash="/")
		tmpfilestem <- paste0(tmpdir, "/", floor(runif(1)*1e8))
		SetPage(type=type, orient=orient, filestem=tmpfilestem, verbose=FALSE, ...)
		par(mar=mar)
		blankPlot(xlim=c(0,1), ylim=c(0,1), mainfont=1)
		plot.dimensions <- par("pin")
		aspect.ratio <- plot.dimensions[1] / plot.dimensions[2]
		closeFile("PDF", suppress.notice=TRUE)
		if (type != "GUI") file.remove(paste0(tmpfilestem, ".", type))
		return(aspect.ratio)
	}

	testAspectRatio <- function(tb.mar, l.mar, r.mar, target.aspectratio, type, orient, ...) {
		return(abs(getAspectRatio(c(tb.mar, l.mar, tb.mar, r.mar), type=type, orient=orient, ...) - target.aspectratio))
	}
	
	
	cat("Optimising margins...\n")
	flush.console()
	optimised <- optimise(f=testAspectRatio, interval=interval, target.aspectratio=target.aspectratio, l.mar=l.mar, r.mar=r.mar, type=type, orient=orient, ...)
	cat("Optimisation finished.\n")
	flush.console()
	
	topbottom.mar <- optimised$minimum
	
	calculated.margins <- c(topbottom.mar, l.mar, topbottom.mar, r.mar)
	
	
	# test to see whether the calculated margins give the right aspect ratio
	cat("Testing calculated margins...\n")
	flush.console()
	tmpdir <- normalizePath(Sys.getenv("TEMP"), winslash="/")
	tmpfilestem <- paste0(tmpdir, "/", floor(runif(1)*1e8))
	SetPage(type=type, orient=orient, filestem=tmpfilestem, ...)
	par(mar=calculated.margins)
	blankPlot(xlim=c(0,1), ylim=c(0,1), mainfont=1)
	plot.dimensions <- par("pin")
	aspect.ratio <- plot.dimensions[1] / plot.dimensions[2]
	closeFile("PDF", suppress.notice=TRUE)
	if (type != "GUI") file.remove(paste0(tmpfilestem, ".", type))
	
	cat(paste0("\nRequested aspect ratio: ", target.aspectratio, "\n"))
	cat(paste0("Aspect ratio based on calculated margins: ", aspect.ratio, "\n"))
	cat(paste0("Absolute difference: ", abs(target.aspectratio - aspect.ratio), "\n"))
	flush.console()
	
	if (abs(target.aspectratio - aspect.ratio) > 1e-05) {
		cat("\nWARNING: Calculated aspect ratio not within tolerance, try\nincreasing left or right margins (or both together)\n\n\n")
	}
	
	return(calculated.margins)
}
