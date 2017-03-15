## this function is used to add lines to a plot, with the cleverness of ONYX
## By default, the line is drawn according to the current window's xlim values
## but this can be overridden, explicitly with the origin parameters, 
## or the trim parameters, which provide the indices of the data matrix (linelist) that we'll draw the line to and from

AddLine <- function(intercept, slope, originLeft=NULL, originRight=NULL, trimLeft=NULL, trimRight=NULL, linelist=NULL, ...) {

	# intercept			Line intercept
	# slope				Line slope
	# originLeft		
	# do some sanity checks 
	if (!is.null(originLeft) & !is.null(trimLeft)) stop("originLeft and trimLeft both specified.")
	if (!is.null(originRight) & !is.null(trimRight)) stop("originRight and trimRight both specified")
	
	if ( (!is.null(trimLeft) | !is.null(trimRight)) & is.null(linelist)) stop("trimLeft or trimRight specifed, but no linelist to use")


	# get the current plot window dimensions
	par.usr <- par("usr")
	xlim <- par.usr[1:2]
	
	# xlim defines the where we're going to draw the lines to and from
	# so we adjust these if we need to trim or define an origin
	
	if (!is.null(originLeft)) xlim[1] <- originLeft
	if (!is.null(originRight)) xlim[2] <- originRight
	

	if (!is.null(trimLeft)) xlim[1] <- linelist[[1]]$meanrisk[trimLeft]
	if (!is.null(trimRight)) xlim[2] <- linelist[[1]]$meanrisk[trimRight]
	
	
	lines(xlim, intercept+slope*xlim, lend=2, ...)
	
	
}