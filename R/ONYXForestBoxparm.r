## Function to calculate the ONYXForest boxparm parameter (controls the size of boxes)

ONYXForestBoxparm <- function(boxparm=NULL, stderr, Ymax=100, YLast) {

	#  Box size parameter...
	#  Boxparm is defined as the size, in inches, in the x-direction, of a point with a standard error of 1.
	#  Boxparm is therefore universally small.  

	#  The x-dimension of a box will be boxparmx/stderr on the 0-Ymax x scale

	#  There are two cases to consider: supplied boxparm, which we use to compute 
	#  boxparmx and boxparmy, and unsupplied boxparm, where we calculate a 'suitable'
	#  value

	# MA: par("pin") is the current plot dimensions in inches
	ParmPin<-par("pin")
	XPin<-ParmPin[1]
	YPin<-ParmPin[2]

	#  Supplied boxparm
	if (!is.null(boxparm))
	{
		boxparmx <- boxparm*Ymax/XPin
		boxparmy <- boxparmx*XPin/YPin
		# boxparm  <- ThisPlot$boxparm
	}


	#  Unsupplied boxparm
	if (is.null(boxparm))
	{
		# Compute boxparmy s.t. the largest box is smaller than a single 
		# y-gap (c.f. the y-location calculation)
		boxparmy <- min(stderr)*(Ymax/YLast)*0.8
		boxparmx <- boxparmy*YPin/XPin
		boxparm  <- boxparmy*YPin/Ymax
	}
	
	return(list(boxparm = boxparm, boxparmx = boxparmx, boxparmy = boxparmy))

}