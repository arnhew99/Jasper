writePvalue <- function(pvalues, x, y, use.lower.limit=FALSE, lower.limit=1e-100, use.upper.limit=FALSE, upper.limit=0.01, cex=1, col=1, adj=0.5, font=1) {

	np <- length(pvalues)
	
	# So, in the stupidest problem of all time, we need to adjust for the fact that 10^x is much 
	# taller than the other text, so because ONYX uses the middle of the string (vertically) to align
	# we need to take this into account and position as if it wasn't there
	
	# dummy values 
	a.round <- 8
	b <- 88
	height.adjust <- (strheight(bquote(.(a.round) %*% 10^.(b)), cex=cex) - strheight(bquote(.(a.round) %*% 10),cex=cex))/2
	
	if (length(x) == 1) x <- rep(x,np)
	if (length(col) == 1) col <- rep(col, np)
	
	
	nonzero_p <- na.omit(pvalues)
	nonzero_p <- nonzero_p[nonzero_p != 0]
	
	limit_xadjust <- ifelse(all(use.lower.limit, any(nonzero_p < lower.limit)), 0.5*strwidth("<",cex=0.9), 0)
	
	for (i in 1:np) {
	
		# # b <- floor(log10(pvalues[i]))
		# # a <- pvalues[i] * 10^(-b)
		# # #a.round <- round(a,1)
		# # a.round <- sprintf("%.0f", a)
		# # # this rounding bit can lead to > 9.5 being rounded to 10
		# # # which is a bit silly, so if this happens set it to 1 and 
		# # # increase b by 1
		# # if (a.round == "10") {
			# # a.round <- "1"
			# # b <- b + 1
		# # }
		# if missing then skip
		if (is.na(pvalues[i])) next
		
		# if the given P value appears to be zero then skip
		if (pvalues[i] == 0) {
			text(x=x[i], y=y[i], labels="0", cex=cex, col=col[i], adj=adj, font=font[i])
			next
		}
		
		a.round <- writePvalue.format(pvalues[i])[1]
		b <- writePvalue.format(pvalues[i])[2]
		
		if (all(a.round=="1", b==0, !use.upper.limit)) {
			text(x=x[i], y=y[i], labels="1", cex=cex, col=col[i], adj=adj, font=font[i])
		} else if (all(use.upper.limit, pvalues[i] > upper.limit)) {

			# format the string to 2 dp and then write out
			text(x=x[i], y=y[i], labels=sprintf("%.2f", pvalues[i]), cex=cex, font=font[i], col=col[i], adj=adj)


		} else if (all(use.lower.limit, pvalues[i] < lower.limit)) {
		
			# now we need to write, e.g. <1e-100 in the neat format but taking into account alignment
			# what a fun treat for everyone!
			
			if (adj == 0) {
				# left aligned (the < should "pop" over the edge relative to all pvalues bigger than the limit)
				text(x=x[i] - strwidth("<", cex=cex, font=font[i]) + limit_xadjust,
						y=y[i], 
						labels="<1\u00d710", 
						cex=cex, 
						col=col[i], 
						adj=0, 
						font=font[i])
				text(x=x[i] - strwidth("<", cex=cex) + strwidth("<1\u00d710", cex=cex, font=font[i]) + limit_xadjust,
						y=y[i] + (strheight("10", cex=cex, font=font[i])/2),
						labels=-100,
						cex=cex*0.8,
						adj=0,
						col=col[i],
						font=font[i])
			} else if (adj == 1) {
				text(x=x[i] - strwidth("-100", cex=0.8*cex, font=font[i]),
						y=y[i], 
						labels="<1\u00d710", 
						cex=cex, 
						col=col[i], 
						adj=1, 
						font=font[i])
				text(x=x[i],
						y=y[i] + (strheight("10", cex=cex, font=font[i])/2),
						labels=-100,
						cex=cex*0.8,
						adj=1,
						col=col[i],
						font=font[i])
			} else if (adj == 0.5) {
				text(x=x[i],
						y=y[i], 
						labels="<1\u00d710", 
						cex=cex, 
						col=col[i], 
						adj=1, 
						font=font[i])
				text(x=x[i],
						y=y[i] + (strheight("10", cex=cex, font=font[i])/2),
						labels=-100,
						cex=cex*0.8,
						adj=0,
						col=col[i],
						font=font[i])	
			}
			
		} else {
		
			# need to position the formatted string according to the value of adj
			# which is complicated by the fact it's in two bits
			if (adj==0) {
				
				# left aligned...
				text(x=x[i] + limit_xadjust, y=y[i], paste(a.round, "\u00d710", sep=""), cex=cex, col=col[i], adj=0, font=font[i])
				text(x=x[i] + limit_xadjust + strwidth(paste(a.round, "\u00d710", sep=""), cex=cex, font=font[i]), y=y[i]+(strheight("10", cex=cex, font=font[i])/2), labels=b, cex=cex*0.8, adj=0, font=font[i], col=col[i])
				
			} else if (adj==1) {
			
				# right aligned
				text(x=x[i]-strwidth(b, cex=cex*0.8, font=font[i]), y=y[i], paste(a.round, "\u00d710", sep=""), cex=cex, col=col[i], adj=1, font=font[i])
				text(x=x[i], y=y[i]+(strheight("10", cex=cex, font=font[i])/2), labels=b, font=font[i], col=col[i], cex=cex*0.8, adj=1)
				
			} else if (adj==0.5) {
			
				# centre aligned
				# x.adjust <- strwidth(bquote(.(a.round) %*% 10^.(b), font=font[i]), cex=cex)/2
				text(x=x[i], y=y[i], paste(a.round, "\u00d710", sep=""), cex=cex, col=col[i], adj=1, font=font[i])
				text(x=x[i], y=y[i]+(strheight("10", cex=cex, font=font[i])/2), labels=b, cex=cex*0.8, col=col[i], adj=0, font=font[i])
			
			}
		}
	
	}


}

writePvalue.format <- function(p) {

	b <- floor(log10(p))
	a <- p * 10^(-b)
	a.round <- sprintf("%.0f", a)
	# this rounding bit can lead to > 9.5 being rounded to 10
	# which is a bit silly, so if this happens set it to 1 and 
	# increase b by 1
	if (a.round == "10") {
		a.round <- "1"
		b <- b + 1
	}

	return(c(a.round,b))
}