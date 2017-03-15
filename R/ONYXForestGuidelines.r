ONYXForestGuidelines <- function(npoints, IsDiamond, atvals, YLocs, YLast, blanks, OvlineType) {

	nptsvec <- 1:npoints
	diamondpts <- nptsvec[IsDiamond]
	
	# attempt to detect when there is 1 diamond and it is at the bottom
	# in this case we probably want a line drawn up through all the points
	if (all(length(diamondpts) == 1, diamondpts[1] == npoints)) {
		lines(x=rep(atvals[diamondpts],2), y=c(YLocs[diamondpts]+(40/YLast), YLocs[1]), lty=OvlineType, lend=2)
	} else {
	
		for (ipoint in seq(2, npoints))
		{
		
			if (all(ipoint %in% diamondpts, !((ipoint-1) %in% diamondpts))) {
				
				# work out where the block ends
				# endpt <- min(nptsvec[nptsvec > max(blanks[blanks < ipoint])])
				testpt <- ifelse(length(blanks[blanks < ipoint]) > 0, max(blanks[blanks < ipoint]), -Inf)
				endpt <- min(nptsvec[nptsvec > testpt])
				
				# or where the next diamond is
				nextdiamond <- ifelse(ipoint == min(diamondpts), 1, max(diamondpts[diamondpts < ipoint]))
				
				endpt <- max(endpt, nextdiamond)
				
				lines(x=rep(atvals[ipoint],2), y=c(YLocs[ipoint]+(40/YLast), YLocs[endpt]), lty=OvlineType, lend=2)
				
			}
			# # # test to see if we need a total line starting from this point
			# # if ((IsDiamond[ipoint]==TRUE) && (IsDiamond[ipoint-1]==FALSE))
			# # {
				# # # We do, so work back to the point after the last diamond, or point 1
				# # jpoint <- ipoint
				# # while (all(jpoint>1, !(IsDiamond[jpoint-1]), jpoint %in% (blanks-1))) {
					# # print(jpoint)
					# # jpoint <- jpoint-1
				# # }

				# # # draw line, from top of diamond to 1/2 diamond height from the midpoint of the 
				# # # square
				# # segments(atvals[ipoint], YLocs[ipoint]+(40/YLast), atvals[ipoint], YLocs[jpoint]+(40/YLast), lty=OvlineType, lend=2)
			# # }
		}
	}
}