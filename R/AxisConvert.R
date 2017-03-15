AxisConvert <- function(pts, lim1, lim2, logScale=FALSE, invert=FALSE) {

	relative_scaling <- diff(lim1)/diff(lim2)
	linearmap <- function(x, lim1, lim2) {
		xp <- (x - lim1[1])
		qx <- xp + lim2[1]
	}
	
	expmap <- function(x, lim1, lim2) {
		# this is not obvious
		
	}
	
	if (invert) {
		res <- sapply(pts, linearmap, lim1=lim2, lim2=lim1)
	} else {
		res <- sapply(pts, linearmap, lim1=lim1, lim2=lim2)
	}
	
	return(res)
}
