ONYXForestValues <- function(estim, upper, lower, isLogged=TRUE, xaxmax, YLocs, Boldness, mainfont, separator=", ", Show, ShowCI, spacing=0, digits=NULL) {
	
	#  Value labels (adj: c(x-adj, y-adj); 0 for left/bottom, 1 for right/top, 0.5 for centre)

	Font <- mainfont
	
	nlabs <- length(YLocs)
	if (isLogged) {
		estim <- exp(estim)
		upper <- exp(upper)
		lower <- exp(lower)
	}
	
	format.nice <- FormatValueNice

	ValueLabels <- list(sapply(estim, format.nice, digits=digits), sapply(lower, format.nice, zero.check=FALSE, digits=digits), sapply(upper, format.nice, zero.check=FALSE, digits=digits))
	
	CIpositive <- all(ValueLabels[[2]] >= 0, ValueLabels[[3]] >= 0)
	
	
	for (i in 1:nlabs) {
	
		# if Show[i] == 0 then we might have dummy values in the dataset that we don't 
		# want to be printed... try to guess what these might turn out to be
		#
		# otherwise... print the values as it may be that the user wants the point to be 
		# hidden but the value label still to be shown...
		null_values <- c("0", "0.0", "0.00", "1", "1.0", "1.00", "3", "2.7", "2.72")
		if (Show[i] == 0) {
			if ((ValueLabels[[1]][i] %in% null_values) & (ValueLabels[[3]][i] %in% null_values) & (ValueLabels[[3]][i] %in% null_values)) {
				next
			}
			else if ((ValueLabels[[1]][i] %in% null_values) & (ValueLabels[[3]][i] %in% null_values) & (ValueLabels[[3]][i] %in% null_values)) {
				next
			}
			else if ((ValueLabels[[1]][i] %in% null_values) & (ValueLabels[[3]][i] %in% null_values) & (ValueLabels[[3]][i] %in% null_values)) {
				next
			}
		}
	
		xcoord <- xaxmax+spacing
		if (estim[i] < 0) xcoord <- xcoord - strwidth("-", font=Boldness[i], cex=Font)
	
		if (ShowCI[i] == 0) {
			text(x=xcoord+strwidth("0.00", cex=Font, font=Boldness[i]), y=YLocs[i], ValueLabels[[1]][i], font=Boldness[i], adj=c(1, 0.5), cex=Font)
			next
		}
		if (CIpositive) {
		
			ci_part <- paste("(", ValueLabels[[2]][i], separator, ValueLabels[[3]][i], ")", sep="")
			text(x=xcoord+strwidth("0.00", cex=Font, font=Boldness[i]), y=YLocs[i], ValueLabels[[1]][i], font=Boldness[i], adj=c(1, 0.5), cex=Font)
			text(x=xcoord+strwidth("0.00 ", cex=Font, font=Boldness[i]), y=YLocs[i], ci_part, font=Boldness[i], adj=c(0, 0.5), cex=Font)
			
		} else {
		
			# now we need to position the labels manually, which is going to be a massive faff
			zeroprint <- ifelse(is.null(digits), sprintf("%.2f", 0), sprintf(paste("%.", digits, "f", sep=""), 0))
			xadjust <- ifelse(substr(ValueLabels[[1]][i], 1, 1)=="-", strwidth(paste("-", zeroprint, " (", sep=""), font=Boldness[i], cex=Font), strwidth(paste(zeroprint, " (", sep=""), font=Boldness[i], cex=Font))
			
			text(x=xcoord + xadjust, y=YLocs[i], paste(ValueLabels[[1]][i], " (", sep=""), font=Boldness[i], adj=c(1, 0.5), cex=Font)
			
			# position the labels such that we allow for a minus sign
			lci <- paste(ValueLabels[[2]][i], separator, sep="")
			if (substr(lci,1,1) == "-") {
				if (length(grep(".",lci, fixed=TRUE))==1) {
					xcoord <- xaxmax +spacing + strwidth(paste(zeroprint, " (", sep=""), font=Boldness[i], cex=Font)
				} else {
					xcoord <- xaxmax +spacing + strwidth(paste(zeroprint, " (.", sep=""), font=Boldness[i], cex=Font)
				}
			} else {
				if (length(grep(".",lci, fixed=TRUE))==1) {
					xcoord <- xaxmax +spacing + strwidth(paste(zeroprint, " (-", sep=""), font=Boldness[i], cex=Font)
				} else {
					xcoord <- xaxmax +spacing + strwidth(paste(zeroprint, " (-.", sep=""), font=Boldness[i], cex=Font)
				}
			}
			text(x=xcoord, y=YLocs[i], lci, font=Boldness[i], adj=c(0, 0.5), cex=Font)
			
			# now the UCI
			uci <- paste(ValueLabels[[3]][i], ")", sep="")
			if (substr(uci,1,1) == "-") {
				if (length(grep(".", uci, fixed=TRUE)) == 1) {
					xcoord <- xaxmax+spacing + strwidth(paste(zeroprint, " (-", zeroprint, ",", sep=""), font=Boldness[i], cex=Font)
				} else {
					xcoord <- xaxmax+spacing + strwidth(paste(zeroprint, " (-", zeroprint, ",.", sep=""), font=Boldness[i], cex=Font)
				}
			} else {
				if (length(grep(".", uci, fixed=TRUE)) == 1) {
					xcoord <- xaxmax+spacing + strwidth(paste(zeroprint, " (-", zeroprint, ",-", sep=""), font=Boldness[i], cex=Font)
				} else {
					xcoord <- xaxmax+spacing + strwidth(paste(zeroprint, " (-", zeroprint, ",-.", sep=""), font=Boldness[i], cex=Font)
				}
			}
			text(x=xcoord, y=YLocs[i], uci, font=Boldness[i], adj=c(0, 0.5), cex=Font)
			
		}


	}
	
	MidValLab <- xaxmax + spacing + (0.5*max(strwidth("0.00 (0.00, 0.00)", cex=Font)))
	
	return(MidValLab)

}