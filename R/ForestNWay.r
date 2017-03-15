ForestNWay <- function(rawdata.list, blank.list=NULL, LogScale=TRUE, xNoEffect=NULL, xaxmin, xaxmax, boxparmx=NULL, boxparmy=NULL, Range=c(0.9,2.5), xticks=c(1,1.5,2.5), adjustfactor=2, force.adjustfactor=FALSE, mainfont=1.23, xlabs=c("Data 1", "Data 2", "Data 3"),  ValueLabelsHeader="Hazard ratio (95%)", NLabel=TRUE, boxsizeoverride=TRUE) {

	# rawdata.list 			List of correctly formatted ForestBasic data.frames(). List element i defines the entries in i-th forest plot, from left to right across the page.
	# blank.list			List of blanks data.frames, as in ForestBasic. If NULL, then we assume that there are no blanks required in any of the forest plots.
	# xaxmin, xaxmax		Global left and right parameters, in the 0-100 underlying scale. This function determines how to place the elements.
	# boxparmx, boxparmy	Box size parameters, which will be used in every forest plot, so the box corresponding to N events with SE 1 has the same size in each Forest plot.
	# Range					Common user x-axis range.
	# xticks				Common x-axis ticks
	# adjustfactor			Defines how much we should shrink each plot (in the x-axis direction) to fit them all on the page. Needs to be tweaked as it depends on mainfont in a complex way.
	# mainfont				Common font scaling parameter.
	# xlabs					Vector of strings to use as labels under the x-axes of the Forest plots.

	n <- length(rawdata.list)
	
	if (is.null(boxparmx)) {
		boxparms <- CalcBoxParm(min(sapply(rawdata.list, function(x) return(min(x$stderr)))))
		boxparmx <- boxparms[1]
		boxparmy <- boxparms[2]
	}
	
	
	
	# if no blanks are required anywhere, then create a list 
	# of length n where every element is NULL
	if (is.null(blank.list)) {
		
		blank.list <- lapply(1:n, function(x) return(NULL))
	
	}
	
	# might want different Ranges and xticks in each plot, so detect if we've been given a list - if not, create one
	# the following uses the scope a little weirdly, so we might want to be more careful in the future
	if (!is.list(Range)) {
	
		cur.range <- Range
		Range <- lapply(1:n, function(y) return(cur.range))
	
	}
	
	# similarly xticks
	if (!is.list(xticks)) {
	
		cur.xticks <- xticks
		xticks <- lapply(1:n, function(y) return(cur.xticks))
	
	}
	
	# if xNoEffect is NULL (i.e., not the required list) then generate a list of nulls
	if (is.null(xNoEffect)) xNoEffect = lapply(1:n, function(x) return(NULL))

	# # # this function computes the shrinkage on the b coordinate required
	# # # based upon the adjustfactor parameter, which will need a little user-tuning
	# # adj <- function(a,b,adjfact) {
		# # return(adjfact*b + (1-adjfact)*a)
	# # }
	
	# instead of multiplicative adjustfactor, we attempt to work out the width of "0.00 (-0.00,-0.00)"
	# and turn adjustfactor into the spacing
	# if we have n plots, we need room for n strings
	# and n-1 gaps
	
	# in the above, we adjust for the width of the ValueLabelsHeader
	# but force.adjustfactor will directly override this so we end up with a consistent layout (that depends on mainfont)
	
	if (force.adjustfactor) {
		label.width <- strwidth("0.00 (-0.00,-0.00)", cex=1.2*mainfont)		
	} else {
		#print(n)
		label.width <- max(strwidth("0.00 (-0.00,-0.00)", cex=1.2*mainfont), strwidth(ValueLabelsHeader, cex=1.2*mainfont, font=2))
	}
		#print(label.width)
		
		
		x.room <- xaxmax - xaxmin - (n*label.width) - ((n-1)*adjustfactor)
		
		# so each forest can have x.room/n space
		forest.width <- x.room / n
		#print(forest.width)
		
		x.coords <- xaxmin
		for (i in 2:n) {
		

				x.coords <- c(x.coords, xaxmin + ((i-1)*(forest.width + adjustfactor + label.width)))

		
		}
		print(x.coords)
	
	
	if (length(ValueLabelsHeader)==1) ValueLabelsHeader <- rep(ValueLabelsHeader,n)
	
	# x.coords <- seq(xaxmin, xaxmax, length=(n+1))
	
	res <- list()
	
	for (i in 1:n) {
	
		# res[[i]] <- ForestBasic(rawdata=rawdata.list[[i]], blanks=blank.list[[i]], LogScale=LogScale, boxparmx=boxparmx, boxparmy=boxparmy, xNoEffect=xNoEffect[[i]], xaxmin=x.coords[i], xaxmax=adj(x.coords[i], x.coords[i+1], adjustfactor), Range=Range[[i]], xticks=xticks[[i]], xlab=xlabs[i], mainfont=mainfont,  ValueLabelsHeader=ValueLabelsHeader, NLabel=NLabel, boxsizeoverride=boxsizeoverride)
		# res[[i]]$xstart = x.coords[i]
		# res[[i]]$xend = adj(x.coords[i], x.coords[i+1], adjustfactor)
		res[[i]] <- ForestBasic(rawdata=rawdata.list[[i]], blanks=blank.list[[i]], LogScale=LogScale, boxparmx=boxparmx, boxparmy=boxparmy, xNoEffect=xNoEffect[[i]], xaxmin=x.coords[i], xaxmax=x.coords[i]+forest.width, Range=Range[[i]], xticks=xticks[[i]], xlab=xlabs[i], mainfont=mainfont,  ValueLabelsHeader=ValueLabelsHeader[i], NLabel=NLabel, boxsizeoverride=boxsizeoverride)
		res[[i]]$xstart = x.coords[i]
		res[[i]]$xend = x.coords[i]+forest.width
	
	}
	
	return(res)

}