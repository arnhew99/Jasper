roundedSquare <- function(xaxs, yaxs=NULL, y.centre=NULL, amount=0.2, resolution=20, ...) {

	# xaxs is a vector giving the x-axis sides of the box, 
	# y.centre is the centre of the box in the y-direction, we work out from the plot dimensions an appropriate scaling to make the box actually square 
	

	
	if (is.null(yaxs) && is.null(y.centre)) stop("Need some y-axis information")
	
	if (is.null(yaxs) && !is.null(y.centre)) {
		pin <- par("pin")
		usr <- par("usr")
		x.scale <- diff(usr[1:2]) / pin[1]
		y.scale <- diff(usr[3:4]) / pin[2]
		rescale.value <- y.scale/x.scale
		
		yaxs <- (diff(xaxs)*rescale.value)/2 
		yaxs <- c(y.centre - yaxs, y.centre + yaxs)
	}
	#print(yaxs)
	#print(diff(yaxs))

	theta <- seq(pi/2,0,length=resolution)
	corner <- cbind(cos(theta),sin(theta))
	
	amount.minus <- 1-amount
	corner.rescale <- corner * amount
	top.right <- corner.rescale + amount.minus 
	bottom.right <- cbind(corner.rescale[,1]+amount.minus, -corner.rescale[,2]-amount.minus)[resolution:1,]
	bottom.left <- -corner.rescale - amount.minus
	top.left <- cbind(-corner.rescale[,1]-amount.minus, corner.rescale[,2]+amount.minus)[resolution:1,]
	rounded <- rbind(top.right, bottom.right, bottom.left, top.left)
	
	# rescale to lie on [0,1]*[0,1]
	rounded <- (rounded+1)/2

	rounded <- cbind(rounded[,1]*diff(xaxs) + xaxs[1], rounded[,2]*diff(yaxs) + yaxs[1])
	
	polygon(rounded, ...)
	
}