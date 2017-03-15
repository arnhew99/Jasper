TableFromCSV <- function(file, 
						orient="PORTRAIT", 
						filestem="test", 
						type="GUI",
						mainfont=1,
						line.spacing=1.25,
						optimise.TBmargins=TRUE,
						optimise.LRmargins=TRUE,
						blank.right.percent=NULL,
						blank.bottom.percent=NULL,
						vertical.alignment="top",
						mar=c(2,2,2,2),
						spacing=1)
{

	# read all in as characters without turning them into factors
	rawdata <- read.csv(file, as.is=TRUE, colClasses="character")
	
	# look for any meta data
	meta.file <- paste(substr(file, 1, nchar(file)-4), "_meta.csv", sep="")
	if (meta.file %in% list.files()) {
		
		cat("Metadata file found. Processing...")
		metadata <- TFCSV.processMeta(meta.file, rawdata=rawdata)
		headings <- metadata$colheadings
		spanheadings <- metadata$spanheadings
		coldecimals <- metadata$coldecimals
		if (!is.null(spanheadings)) {
			nspanheadings <- max(as.numeric(as.character(spanheadings$row)))
		} else {
			nspanheadings <- 0
		}
		cat("done\n")
		print(metadata)

	
	} else {
		headings <- rep("", dim(rawdata)[2])
		nspanheadings <- 0
		spanheadings <- NULL
	}
	
	print(headings)
	
	SetPage(orient=orient, perpage=1, type=type, filestem=filestem, blank.right.percent=blank.right.percent, blank.bottom.percent=blank.bottom.percent)
	par(mar=mar)
	blankPlot(c(0,100), c(0,100), mainfont)
	par(xpd=NA)
	
	
	# compute the rows locations: need a row for the colheadings and then rows for the span headings
	nrows <- dim(rawdata)[1]
	ncols <- dim(rawdata)[2]
	
	max.heading.height <- max(sapply(headings, strheight))
	total.heights <- c(rep(strheight("M"),nrows), max.heading.height, rep(strheight("M"), nspanheadings))

	# vertical.spacing <- (100 - sum(total.heights)) / (length(total.heights) - 1)
	
	c.total.heights <- cumsum(total.heights)
	# all.ylocs <- (c.total.heights - min(c.total.heights)) / (max(c.total.heights) - min(c.total.heights)) * 100
	all.ylocs <- c.total.heights + cumsum(c(0,rep((100-sum(total.heights))/(nrows + nspanheadings), nrows+nspanheadings)))

	
	# all.ylocs <- seq(0, 100, length=nrows+1+nspanheadings)
	ylocs <- rev(all.ylocs[1:nrows])
	
	# work out how many inches the spacing is in the original plot
	get.spacing.inches <- function(spacing) return(spacing*(par("pin")[1] - par("mai")[1] - par("mai")[3])/100)
	spacing.inches <- get.spacing.inches(spacing)
	
	# work out column classes
	col.classes <- sapply(1:ncols, function(z) ColClass(rawdata[,z]))
	col.adjs <- ifelse(col.classes == "numeric", 1, 0)
	
	
	
	# apply any formatting commands found in coldecimals
	if (!is.null(coldecimals)) {
	
		for (i in 1:(dim(coldecimals)[1])) {
			formatted <- sprintf(paste("%.", coldecimals$dp[i], "f", sep=""), as.numeric(rawdata[, coldecimals$col[i]]))
			print(formatted)
			rawdata[, coldecimals$col[i]] <- ifelse(formatted == "NA", " ", formatted)
		}
	
	}
	# work out column widths
	col.widths <- sapply(1:ncols, function(z) FFCSV.findColWidth(rawdata[,z], heading=headings[z]))

	
	# need to close the currently open file before optimising the top and bottom margins
	closeFile("PDF", suppress.notice=TRUE)
	
	if (optimise.TBmargins) {
		cat("Optimising top and bottom margins...")
		optim.TBmargin <- FFCSV.optimise.linespace(current.mar=mar, line.spacing=line.spacing, titlespace=0.1, footerspace=0.1, ylocs=ylocs, orient=orient, mainfont=mainfont, type=type, filestem=filestem, blank.right.percent=blank.right.percent, blank.bottom.percent=blank.bottom.percent)
		cat("done.\n")
		
		# replace mar with our optimised version
		TB.mar <- optim.TBmargin$minimum
		if (vertical.alignment == "top") {
			mar <- c(TB.mar*2, mar[2], 0, mar[4])
		} else if (vertical.alignment == "bottom") {
			mar <- c(0, mar[2], TB.mar*2, mar[4]) 
		} else {
			mar <- c(TB.mar, mar[2], TB.mar, mar[4])
		}
	}
	
	
	# try to optimise LR margins
	print(rawdata)
	if (sum(col.widths) + spacing*(length(col.widths)-1) > 100) {
		cat("Cannot optimise L/R margins as columns already too wide.\nConsider reducing the font size.\n")
	
	} else if (optimise.LRmargins) {
	
		optim.LRmargin <- FFCSV.optimise.LRmargins(current.mar=mar, spacing=spacing, spacing.is.inches=FALSE, headings=headings, rawdata=rawdata, orient=orient, mainfont=mainfont, type=type, filestem=filestem, blank.right.percent=blank.right.percent, blank.bottom.percent=blank.bottom.percent)
		LR.mar <- optim.LRmargin$minimum
		
		# replace mar, but centre aligned
		mar <- c(mar[1], LR.mar, mar[3], LR.mar)
	
	}
	
	print(mar)


	# remake the page
	SetPage(orient=orient, perpage=1, type=type, filestem=filestem, blank.right.percent=blank.right.percent, blank.bottom.percent=blank.bottom.percent)
	par(mar=mar)
	blankPlot(c(0,100), c(0,100), mainfont)
	par(xpd=NA)

	# recompute the col widths for the new margins
	col.widths <- sapply(1:ncols, function(z) FFCSV.findColWidth(rawdata[,z], heading=headings[z]))
	print(col.widths)
	
	
	# write out the data frame
	computed.xcoords <- rep(0,ncols)
	for (i in 1:ncols) {
		
		x.coord <- cumsum(c(0, col.widths))[i+col.adjs[i]] + (i-1)*spacing
		text(x=x.coord, y=ylocs, labels=rawdata[,i], adj=c(col.adjs[i],0))
		# add the heading
		text(x=x.coord, y=all.ylocs[nrows+1]+0.5*strheight("M"), labels=headings[i], font=2, adj=c(col.adjs[i],0))
		
		print(x.coord)
	
		# save the x coordinate
		computed.xcoords[i] <- x.coord
	}
	
	# add a title line
	lines(x=c(0,100), y=rep(all.ylocs[nrows+1],2), lend=2)
	
	# add span headings
	if (!is.null(spanheadings)) {
	
		# now we have some span headings to add
		for (i in 1:length(spanheadings$start)) {
		
			# work out the start and end column numbers
			# and the row on which we are putting things
			current.start <- which(names(rawdata) == as.character(spanheadings$start[i]))
			current.end <- which(names(rawdata) == as.character(spanheadings$end[i]))
			current.row <- nspanheadings - as.numeric(as.character(spanheadings$row[i])) + 1
			print(current.row)
			
			if (col.adjs[current.start] == 1) x.start <- computed.xcoords[current.start] - col.widths[current.start] else x.start <- computed.xcoords[current.start]
			if (col.adjs[current.end] == 0) x.end <- computed.xcoords[current.end] - col.widths[current.end] else x.end <- computed.xcoords[current.end]
			
			# text(x=(x.start+x.end)/2, y=100 + max(sapply(headings, strheight)) + 1.5*strheight("A"), labels=spanheadings$heading[i], adj=c(0.5,0), font=2)
			text(x=(x.start+x.end)/2, y=all.ylocs[nrows+1+current.row], labels=spanheadings$heading[i], adj=c(0.5,1), font=2)
			
		}
	
	}
	
	closeFile(type, suppress.notice=FALSE)
	return(all.ylocs)
	

}