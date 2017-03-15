convertLineToUser <- function(line, side) {

	# sides are 
	# 1 - bottom
	# 2 - left 
	# 3 - top 
	# 4 - right
	# need to know the current plot size in inches and the user coordinates
	# then convert the margin size lines to inches and then to user coordinates
	# (y direction only) (and bottom margin only)
	if (side %in% c(1,3)) {
		pin <- 2
		usr <- 3:4
	} else {
		pin <- 1
		usr <- 1:2
	}
	marginlines.per.inch <- par("mar")[side] / par("mai")[side]
	inches.per.user <- par("pin")[pin] / diff(par("usr")[usr]) 
	
	return((line/marginlines.per.inch)*(1/inches.per.user))

}