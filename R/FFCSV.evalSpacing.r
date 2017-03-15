FFCSV.evalSpacing <- function(mainfont,
								orient,
								type, 
								filestem, 
								blank.right.percent, 
								blank.bottom.percent,
								mar,
								rawdata,
								col.ids = -1,
								print.headings="",
								plot.width = 0,
								nforests = 0,
								anyhets = 0,
								HetTrendScale=0.8,
								spacing.text = "a",
								spacing.cex=1,
								ValueDigits=NULL) 
	{
								
	SetPage(orient=orient, perpage=1, type=type, filestem=filestem, blank.right.percent=blank.right.percent, blank.bottom.percent=blank.bottom.percent, verbose=FALSE, attempt_adobe_kill=FALSE)
	par(mar=mar)
	blankPlot(c(0,100), c(0,100), mainfont)
	par(xpd=NA)		
	# need to use the formatted versions of the column headings where ";" has been substituted for "\n"
	if (col.ids[1] == -1) {
		other.cols.widths <- 0 
	} else {
		# print(col.ids)
		other.cols.widths <- sapply(col.ids, function(z) FFCSV.findColWidth(column=as.character(rawdata[,z]), heading=print.headings[z]))
	}
	
	# plotlabel.width <- max(strwidth("0.00 (-0.00,-0.00)", cex=1, font=2), anyhets*strwidth("Heterogeneity: AI=3.7 (p=0.0001)", cex=1*HetTrendScale)) # second part is 0 if there are no tests
	plotlabel.width <- max(strwidth(makeBlankValueLabel(digits=ValueDigits), cex=1, font=2), anyhets*strwidth("Heterogeneity: AI=3.7 (p=0.0001)", cex=1*HetTrendScale)) # second part is 0 if there are no tests
	
	all.widths <- c(other.cols.widths, nforests*plot.width, nforests*plotlabel.width)
	#print(all.widths)
	n.cols <- ifelse(col.ids == -1, 0, length(col.ids))
	
	# we give 0.5 of a spacing to the gap between the forest and the label
	spacing <- (100 - sum(all.widths)) / (length(n.cols)+(nforests*1.5))
	
	a.width <- strwidth(spacing.text, cex=spacing.cex)
	closeFile("PDF", suppress.notice=TRUE)
	
	return(c(spacing, spacing-a.width))
}

FFCSV.findColWidth <- function(column, heading) {

	# print(column)
	# print(sapply(convertUnicode(as.character(c(column, heading))), strwidth, font=2, cex=1))
	# print(max(sapply(convertUnicode(as.character(c(column, heading))), strwidth, font=2, cex=1)))
	return(max(sapply(convertUnicode(as.character(c(column, heading))), strwidth, font=2, cex=1)))
	

}


FFCSV.evalLabelSpacing <- function(mar.test, current.mar, ylocs, orient, mainfont, type, filestem, blank.right.percent, blank.bottom.percent, titlespace, footerspace) {

	new.mar <- c(mar.test,current.mar[2], mar.test, current.mar[4])
	SetPage(orient=orient, perpage=1, type=type, filestem=filestem, blank.right.percent=blank.right.percent, blank.bottom.percent=blank.bottom.percent, verbose=FALSE, titlespace=titlespace, footerspace=footerspace, attempt_adobe_kill=FALSE)
	par(mar=new.mar)
	blankPlot(c(0,100), c(0,100), mainfont)
	par(xpd=NA)		

	
	max.heading.height <- strheight("A sentence with a y and a CAPITAL", font=2)
	
	# want to know the smallest difference between vertically neighbouring labels
	min.heading.gap <- min(abs(diff(ylocs)))
	
	line.gap <- (strheight("A\nA") - strheight("M"))

	closeFile("PDF", suppress.notice=TRUE)
	
	return(c(max.heading.height, min.heading.gap, line.gap))
	
}

FFCSV.optimise.linespace <- function(current.mar, line.spacing, ylocs, orient, mainfont, type, filestem, blank.right.percent, blank.bottom.percent, titlespace, footerspace) 
{

	# function to optimise over
	optim.TBmargin.fn <- function(z) {
		spac <- FFCSV.evalLabelSpacing(mar.test=z, current.mar=current.mar, ylocs=ylocs, orient=orient, mainfont=mainfont, type=type, filestem=filestem, blank.right.percent=blank.right.percent, blank.bottom.percent=blank.bottom.percent, titlespace=titlespace, footerspace=footerspace)
		# return(abs(spac[2] - (spac[1]*ifelse(mainfont < 1.5, 1.5, mainfont)*line.spacing)))
		return(abs(spac[2] - line.spacing*spac[3]))
	}
	optim.TBmargin <- optimise(f=optim.TBmargin.fn, interval=c(0,min(30,30/mainfont)), maximum=FALSE)
	return(optim.TBmargin)

}


FFCSV.evalColWidths <- function(mar.test, current.mar, spacing, spacing.is.inches=FALSE, headings, rawdata, orient, mainfont, type, filestem, blank.right.percent, blank.bottom.percent) {

	new.mar <- c(current.mar[1], mar.test, current.mar[3], mar.test)
	SetPage(orient=orient, perpage=1, type=type, filestem=filestem, blank.right.percent=blank.right.percent, blank.bottom.percent=blank.bottom.percent, attempt_adobe_kill=FALSE)
	par(mar=new.mar)
	blankPlot(c(0,100), c(0,100), mainfont)
	par(xpd=NA)		

	ncols <- length(headings)
	all.col.widths <- sum(sapply(1:ncols, function(z) FFCSV.findColWidth(rawdata[,z], heading=headings[z])))
	# print(all.col.widths)
	
	if (spacing.is.inches) {
		invert.spacing <- function(inches) return(inches*100/(par("pin")[1] - par("mai")[2] - par("mai")[4]))
		spacing <- invert.spacing(spacing)
	}

	
	closeFile("PDF", suppress.notice=TRUE)
	
	target <- abs(100 - all.col.widths - (ncols-1)*spacing)

	return(target)
	
}

FFCSV.optimise.LRmargins <- function(current.mar, spacing, spacing.is.inches=FALSE, headings, rawdata, orient, mainfont, type, filestem, blank.right.percent, blank.bottom.percent) {

	# function to optimise over
	optim.LRmargin.fn <- function(z) {
		xmar <- FFCSV.evalColWidths(mar.test=z, current.mar=current.mar, spacing=spacing, spacing.is.inches=spacing.is.inches, headings=headings, rawdata=rawdata, orient=orient, mainfont=mainfont, type=type, filestem=filestem, blank.right.percent=blank.right.percent, blank.bottom.percent=blank.bottom.percent)
		return(xmar)
	}
	return(optimise(f=optim.LRmargin.fn, interval=c(0,25), maximum=FALSE))
}