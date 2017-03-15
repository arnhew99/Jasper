## following a new plot being opened, compute the boxparmx and boxparmy values that will make 
## boxes appear the same size across user defined plots.
##
## NB: the calculation is done using the par() values from the current plotting window, meaning
## that a plotting window needs to have been created first.

CalcBoxParm <- function(min.stderr, xlim=NULL) {
	parm <- par()
	xlen<-parm$pin[1]
	ylen<-parm$pin[2]
	
	xbox<-parm$usr[2]-parm$usr[1]
	ybox<-parm$usr[4]-parm$usr[3]
	
	if (!is.null(xlim)) {
		xbox <- 5*xbox/diff(xlim)
		ybox <- 5*ybox/diff(xlim)
	}
	boxparmx <- (xbox/24)*min.stderr
	# boxparmx <- (1/min.stderr)/(xbox)
	boxparmy<-(boxparmx*(xlen/xbox)*(ybox/ylen))
	return(c(boxparmx, boxparmy))
}
