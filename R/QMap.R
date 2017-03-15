QMap <- function(x, y, xlim, ylim, qxlim, qylim) {
	# maps (x,y) on a xlim by ylim plot onto the page region,
	# as defined by qxlim and qylim
	# convert x into a percentage of xlim
	# convert y into a percentage of ylim
	xp <- (x - xlim[1]) / (xlim[2] - xlim[1])
	yp <- (y - ylim[1]) / (ylim[2] - ylim[1])
	qx <- xp * (qxlim[2] - qxlim[1]) + qxlim[1]
	qy <- yp * (qylim[2] - qylim[1]) + qylim[1]
	
	return(c(qx,qy))
}
