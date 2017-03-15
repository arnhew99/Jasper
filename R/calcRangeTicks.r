calcRangeTicks <- function(z, adj=0.05) {

	ra <- range(z)
	ra <- c(ra[1] - adj*diff(ra), ra[2] + adj*diff(ra))
	
	ticks <- pretty(ra)[pretty(ra) > ra[1] & pretty(ra) < ra[2]]
	
	return(c(list(ra), list(ticks)))

}