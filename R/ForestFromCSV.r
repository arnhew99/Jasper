ForestFromCSV <- function(file, 
	orient="PORTRAIT", 
	filestem="test", 
	append_datetime=FALSE,
	type="GUI", 
	expandDataLoading=FALSE, 
	StopIfCodeExists=TRUE, 
	mainfont=1, 
	optimise.fontsize=TRUE, 
	auto.shrink.font=TRUE, 
	optimise.line.spacing=TRUE, 
	line.spacing=1.25, 
	vertical.alignment="centre", 
	LogScale=TRUE, 
	ExponentiateDataOnPlot=FALSE, 
	DataLogged=NULL, 
	EstimateCol=NULL,
	StdErrCol=NULL,
	LCICol=NULL,
	UCICol=NULL,
	FormatPValue=NULL,
	pvalue.use.lower.limit=FALSE,
	pvalue.lower.limit=1e-100,
	pvalue.use.upper.limit=FALSE,
	pvalue.upper.limit=0.01,
	ColHeadings=NULL,
	ColDecimalPlaces=NULL,
	plot.width=30, 
	font=NULL,
	forest.Range=NULL,
	forest.xlim=NULL,
	forest.xticks=NULL,
	forest.hideticklabs=FALSE,
	forest.title="",
	forest.titleY=NULL,
	forest.title.axiscentred=FALSE,
	ylabels.offset=0,
	lwd=1,
	xlab="", 
	mar=c(7,2,7,2), 
	blank.labels.font=2, 
	ValueLabels=TRUE, 
	ValueLabelsHeader="", 
	ValueLabelsEffectSE=FALSE,
	ValueDigits=NULL,
	ValueDigitsSE=NULL,
	HetTrendScale=0.8, 
	boxparm.stderr=NULL,
	boxparm.sharedstderr=TRUE,
	boxsizeoverride=FALSE,
	boxTop=FALSE,
	blank.right.percent=NULL, 
	blank.left.percent=NULL, 
	blank.bottom.percent=NULL, 
	titlespace=0.1,
	footerspace=0.1,
	footer.title=NULL, 
	footer.title.cex=NULL,
	mainTitle=NULL,
	pointGroupsFactor=0.9,
	additionalR=NULL,
	suppress.date=FALSE,
	fixLabelsY=FALSE,
	attempt_adobe_kill=TRUE) {
	
	################################################################
	# WARNING 
	#
	# If you aren't MA, this will probably make your eyes bleed.
	# A lot of this needs to be made modular 
	#
	#
	################################################################
	
	
	# check which version of R we're running on... if it's R >= 3.4.0 then 
	# we need to temporarily disable the JIT compiler 
	if (all(as.integer(R.version$major) >= 3L, as.integer(substr(R.version$minor,1,1)) >= 4)) {
		compiler:::enableJIT(0)
		cat("R version >= 3.4.0 detected\n")
		flush.console()
	}
	
	# attempt to kill any open Adobe reader window (Adobe locks files so we can't write to them)
	# subcomponents should not try to kill these windows, and should run SetPage with attempt_adobe_kill=FALSE
	type <- toupper(type)
	if (all(attempt_adobe_kill, type == "PDF", .Platform$OS.type=="windows")) {
		cmd <- paste0("taskkill /fi \"Windowtitle eq ", filestem, ".pdf - Adobe Reader\"")
		system(command=cmd, intern=FALSE, wait=TRUE, show.output.on.console=FALSE)
	}
	
	file <- normalizePath(file, winslash="/")
	
	
	## check to see if we have a TEMP directory available 
	# tmpdir <- Sys.getenv()["TEMP"]
	tmpdir <- ifelse(.Platform$OS.type=="windows", Sys.getenv()["TEMP"], Sys.getenv()["TMPDIR"])
	if (any(is.null(tmpdir), is.na(tmpdir))) stop("No temporary working area available")
	
	
	# generate a random number to append to our temporary working files
	tmp_number <- sample(1e8, size=1)
	
	## check to see that we can write to the TEMP directory
	testwrite <- paste0(tmpdir, "\\", "jasper_temp_test_write_", tmp_number)
	con <- file(testwrite, open="wt")
	writeLines("This is Jasper.", con)
	close(con)
	file.remove(testwrite)
	
	
	
	
	# test to see if there are any existing files in that area, and delete any that do exist
	# tmpdir.filelist <- list.files(tmpdir)
	# tmpdir.jasperfiles <- tmpdir.filelist[grepl("jasper_temp", tmpdir.filelist, fixed=TRUE) | grepl("jasper_code", tmpdir.filelist, fixed=TRUE)]
	# if (length(tmpdir.jasperfiles) > 0) {
		# file.remove(paste0(tmpdir, "\\", tmpdir.jasperfiles))
	# }
	
	
	tmpfiles <- paste("\\jasper_temp_", tmp_number, sep="")
	tmpfilestem <- paste(tmpdir, tmpfiles, sep="")
	
	
	# for the time being, set DataLogged = ExponentiateDataOnPlot
	# they represent the same idea, Expo... seems clearer
	if (is.null(DataLogged)) DataLogged <- ExponentiateDataOnPlot
	else if (!is.null(DataLogged) & !(ExponentiateDataOnPlot)) ExponentiateDataOnPlot <- DataLogged
	
	
	# allow specification of forest.xlim as a more natural option
	if (all(is.null(forest.Range), !is.null(forest.xlim))) forest.Range <- forest.xlim
	
	
	# don't overwrite existing code unless user has changed flag
	if (file.exists(paste(filestem, "_jasper_code.r",sep="")) & StopIfCodeExists) stop("\nTarget file for Jasper code already exists\n(Aborting to protect possible user modifications)\nTo override, set StopIfCodeExists=FALSE")
	
	
	# set a default font if none is supplied
	if (is.null(font)) font <- "sans"
	
	
	# check to see that if boxsizeoverride=TRUE that we have a boxparm.stderr to use
	if (all(boxsizeoverride, is.null(boxparm.stderr))) stop("\nIf using boxsizeoverride=TRUE, a value must be provided for boxparm.stderr\nto set the box size.")
	
	
	
	# check to see what's going on with EstimateCol etc
	if (all(!is.null(EstimateCol), is.null(StdErrCol), is.null(LCICol), is.null(UCICol))) stop("\nIf specifying EstimateCol, must also specify StdErrCol or both LCICol and UCICol")
	
	if (all(!is.null(EstimateCol), !is.null(StdErrCol), length(EstimateCol) != length(StdErrCol))) stop("\nEstimateCol and StdErrCol must be the same length")
	
	if (all(!is.null(EstimateCol), is.null(StdErrCol), !is.null(LCICol), is.null(UCICol))) stop("\nLCICol and UCICol must both be specified")
	if (all(!is.null(EstimateCol), is.null(StdErrCol), is.null(LCICol), !is.null(UCICol))) stop("\nLCICol and UCICol must both be specified")
	
	
	if (all(!is.null(EstimateCol), is.null(StdErrCol), !is.null(LCICol), !is.null(UCICol), length(EstimateCol) != length(LCICol))) stop("\nEstimateCol and LCICol must be the same length.")
	if (all(!is.null(EstimateCol), is.null(StdErrCol), !is.null(LCICol), !is.null(UCICol), length(EstimateCol) != length(UCICol))) stop("\nEstimateCol and UCICol must be the same length.")
	if (all(!is.null(EstimateCol), is.null(StdErrCol), !is.null(LCICol), !is.null(UCICol), length(UCICol) != length(LCICol))) stop("\nUCICol and LCICol must be the same length.")
	
	
	
	
	
	# hopefully all the *Col checks have passed...
	rawdata <- read.csv(file, as.is=TRUE)
	
	# the names provided should actually be column names
	if (all(!is.null(EstimateCol), any(!(EstimateCol %in% names(rawdata))))) stop("\nEstimateCol names should be names of columns in the CSV file")
	if (all(!is.null(StdErrCol), any(!(StdErrCol %in% names(rawdata))))) stop("\nStdErrCol names should be names of columns in the CSV file")
	if (all(!is.null(LCICol), any(!(LCICol %in% names(rawdata))))) stop("\nLCICol names should be names of columns in the CSV file")
	if (all(!is.null(UCICol), any(!(UCICol %in% names(rawdata))))) stop("\nUCICol names should be names of columns in the CSV file")
	
	# check to see if we have a Heading column
	if (!("Heading" %in% names(rawdata))) stop("\nHeading column not found.\nHeading needs to be the first column (NB case-sensitive)")
	
	
	# check to see if there is at least one valid estimate column
	if (is.null(EstimateCol)) {
		valid.names <- c("Estimate1", "RR1", "HR1", "LogRR1")
		test <- sapply(valid.names, function(z) (z %in% names(rawdata)))
		if (!(any(test))) stop("\nValid Estimate1, RR1 or LogRR1 column not found.")
	}
	
	# check to see if there is a StdErr or LCI and UCI columns
	if (is.null(StdErrCol)) {
		test <- c(("StdErr1" %in% names(rawdata)), all(sapply(c("LCI1", "UCI1"), function(z) any(sapply(names(rawdata), function(y) grepl(z, y, fixed=TRUE))))))
		if (!(any(test))) stop("\nValid StdErr1 or LCI1 and UCI1 columns not found.")
	}
	
	
	# if EstimateCol is null then detect the columns and add them to appropriate lists
	possible.types <- c("HR","Estimate","RR","LogRR")
	truncated.names <- substr(names(rawdata),1,nchar(names(rawdata))-1)
	if (is.null(EstimateCol)) {
		# detect the maximum number of forests
		EstimateCol.tmp <- names(rawdata)[which(truncated.names %in% possible.types)]
		inds <- substr(EstimateCol.tmp, nchar(EstimateCol.tmp), nchar(EstimateCol.tmp))
		nforests <- max(inds)
		
		EstimateCol <- c()
		StdErrCol <- c()
		UCICol <- c()
		LCICol <- c()
		
		for (i in 1:nforests) {
			EstimateCol <- c(EstimateCol, names(rawdata)[names(rawdata) %in% paste0(possible.types,i)])
			if (paste0("StdErr", i) %in% names(rawdata)) StdErrCol <- c(StdErrCol, paste0("StdErr", i))
			if (paste0("LCI", i) %in% names(rawdata)) LCICol <- c(LCICol, paste0("LCI", i))
			if (paste0("UCI", i) %in% names(rawdata)) UCICol <- c(UCICol, paste0("UCI", i))
			
			
			if (paste0("LogRR_LCI", i) %in% names(rawdata)) LCICol <- c(LCICol, paste0("LogRR_LCI", i))
			if (paste0("LogRR_UCI", i) %in% names(rawdata)) UCICol <- c(UCICol, paste0("LogRR_UCI", i))
			
			if (paste0("RR_LCI", i) %in% names(rawdata)) LCICol <- c(LCICol, paste0("RR_LCI", i))
			if (paste0("RR_UCI", i) %in% names(rawdata)) UCICol <- c(UCICol, paste0("RR_UCI", i))
			
			if (paste0("HR_LCI", i) %in% names(rawdata)) LCICol <- c(LCICol, paste0("HR_LCI", i))
			if (paste0("HR_UCI", i) %in% names(rawdata)) UCICol <- c(UCICol, paste0("HR_UCI", i))
			
		}		
	}
	
	# check to see if ValueDigits is a list, if it is convert it to a normal vector
	if ((!is.null(ValueDigits)) & is.list(ValueDigits)) {
		ValueDigits <- unlist(ValueDigits)
	}

	
	# detect if we are ignoring any columns 
	ignore.cols <- sapply(names(rawdata), function(z) ifelse(substr(z,1,6) == "IGNORE", TRUE, FALSE))
	ignore.cols <- (1:length(names(rawdata)))[ignore.cols]
	

	# detect if Jasper is running from inside an R package 
	insidePackage <- ifelse("Jasper" %in% loadedNamespaces(), TRUE, FALSE)
	
	
	# detect if we're forcing any numeric formats on particular columns 
	if (!is.null(ColDecimalPlaces)) {
		ColDecimalPlaces <- strsplit(ColDecimalPlaces, "=", fixed=TRUE)
		coldp_names <- sapply(ColDecimalPlaces, function(z) return(z[1]))
		coldp_dps <- sapply(ColDecimalPlaces, function(z) as.numeric(z[2]))
	} else {
		coldp_names <- NULL
		coldp_dps <- NULL
	}
	
	# set up how the data gets loaded, if we need to ignore any columns then add a sentence to do this
	if (insidePackage) {
		MakeForest.command <- paste('# set the working directory\nsetwd(normalizePath(\"', gsub("\\", "\\\\", normalizePath(getwd()), fixed=TRUE), '\"))\n\n# load Jasper\nrequire(Jasper)\n\n', sep="")
	} else {
		MakeForest.command <- paste('# set the working directory\nsetwd(normalizePath(\"', gsub("\\", "\\\\", normalizePath(getwd()), fixed=TRUE), '\"))\n\n# load Jasper\nsource(', deparse(.jasper.load),')\n\n', sep="")
	}
	MakeForest.command <- paste(MakeForest.command, '# read in the raw data\nrawdata <- read.csv(\"', gsub("\\", "\\\\", normalizePath(file), fixed=TRUE), '\", as.is=TRUE)\n', sep="")
	if (length(ignore.cols) != 0) {
		rawdata <- rawdata[,-ignore.cols]
		MakeForest.command <- paste(MakeForest.command, "rawdata <- rawdata[,-(", deparse(ignore.cols),")]\n\n", sep="")
	} else {
		MakeForest.command <- paste(MakeForest.command, "\n", sep="")
	}
	
	
	# detect if we need to format some Pvalues
	if (!is.null(FormatPValue)) {
		pvalue.cols <- FormatPValue
	} else if (!is.null(rawdata$FormatPValue)) {
		pvalue.cols <- as.character(rawdata$FormatPValue[rawdata$FormatPValue != ""])
	} else { pvalue.cols <- -1 }

	
	# check to see if there are columns with special names that we should handle
	other.col.names <- c("Headin", "RR", "LogRR", "Estimate", "HR", "LCI", "UCI", "RR_LCI", "RR_UCI", "LogRR_LCI", "LogRR_UCI", "LogRR_StdErr", "StdErr", "Het", "Trend", "IsDiamond", "FillColour", "Show", "ShowCI", "ColHeading", "FormatPValu", "DiamondGuidelines", "hideNoEffect", "pointGroups")
	if (!is.null(EstimateCol)) other.col.names <- c(other.col.names, substr(EstimateCol,1,nchar(EstimateCol)-1))
	if (!is.null(StdErrCol)) other.col.names <- c(other.col.names, substr(StdErrCol,1,nchar(StdErrCol)-1))
	if (!is.null(LCICol)) other.col.names <- c(other.col.names, substr(LCICol,1,nchar(LCICol)-1))
	if (!is.null(UCICol)) other.col.names <- c(other.col.names, substr(UCICol,1,nchar(UCICol)-1))
	other.cols <- which(!sapply(names(rawdata), function(z) substr(z,1,nchar(z)-1)) %in% other.col.names)
	# if there aren't any other columns, set other.cols <- -1
	if (length(other.cols) == 0) other.cols <- -1

	# if (is.null(EstimateCol)) {	
		# nforests <- sum(sapply(names(rawdata), function(z) substr(z,1,nchar(z)-1)) %in% c("HR", "Estimate", "RR", "LogRR"))
	# } else {
		# nforests <- length(EstimateCol)
	# }
	
	nforests <- length(EstimateCol)
		

	if (!is.null(boxparm.stderr) & length(boxparm.stderr) > 1 & length(boxparm.stderr) != nforests) stop("\nboxparm.stderr mis-specified, either provide 1 value\nor a vector of values, one for each forest.")
	
	if (!is.null(boxparm.stderr) & length(boxparm.stderr) == 1) {
		boxparm.stderr <- rep(boxparm.stderr, nforests)
	}
	
	# detect if ValueDigits is the right length or not
	if (all(!is.null(ValueDigits), length(ValueDigits) == 1)) {
		ValueDigits <- rep(ValueDigits,nforests)
	}		
	if (!is.null(ValueDigits) & length(ValueDigits) > 1) {
		if (length(ValueDigits) != nforests) stop("\nIf a vector of ValueDigits is provided, there must be one for each forest")
	}	
	
	if (all(!is.null(ValueDigitsSE), length(ValueDigitsSE) == 1)) {
		ValueDigitsSE <- rep(ValueDigitsSE,nforests)
	}
	if (!is.null(ValueDigitsSE) & length(ValueDigitsSE) > 1) {
		if (length(ValueDigitsSE) != nforests) stop("\nIf a vector of ValueDigitsSE is provided, there must be one for each forest")
	}
	# check to see if LogScale and ExponentiateDataOnPlot are the right length 
	if (all(length(LogScale) == 1, nforests > 1)) {
		LogScale <- rep(LogScale, nforests)
	}
	if (all(length(ExponentiateDataOnPlot) == 1, nforests > 1)) {
		ExponentiateDataOnPlot <- rep(ExponentiateDataOnPlot, nforests)
	}
	


	# allow specification of multiple Ranges, xticks, xlabs, titles, ValueLabelsHeader
	if (!is.list(forest.Range)) forest.Range <- lapply(1:nforests, function(z) return(forest.Range))
	if (!is.list(forest.xticks)) forest.xticks <- lapply(1:nforests, function(z) return(forest.xticks))
	if (all(!is.list(forest.title), length(forest.title) < nforests)) forest.title <- rep(forest.title[1], nforests)
	if (!is.list(forest.title)) forest.title <- lapply(1:nforests, function(z) return(forest.title[z]))
	if (!is.list(xlab)) xlab <- lapply(1:nforests, function(z) return(xlab))
	if (!is.list(ValueLabelsHeader)) ValueLabelsHeader <- lapply(1:nforests, function(z) return(ValueLabelsHeader))

	
	# parse any R expressions in forest.title
	forest.title <- lapply(forest.title, FFCSV.parseTitle)



	
	# assume the top row gives the plain text column headings and that the real data starts on row2
	HetsTrend.command <- ""
	hets <- rep(FALSE,nforests)
	trends <- rep(FALSE,nforests)
	
	MakeBlanks.command <- ""
	 
	for (i in 1:nforests) {
	
		# decide whether FormatForest should look for certain columns
		getHets <- ifelse((paste("Het",i, sep="") %in% names(rawdata)), TRUE, FALSE) 
		getTrends <- ifelse((paste("Trend",i, sep="") %in% names(rawdata)), TRUE, FALSE) 
		
		# we're going to define foresti
		foresti <- paste("forest", i, sep="")
		
		# print(EstimateCol)
		# print(StdErrCol)
		# print(LCICol)
		# print(UCICol)
		
		# we should have EstimateCol 
		if (i == 1) data.type.1 <- EstimateCol[i]
		data.type.i <- EstimateCol[i]
		current.estimate <- data.type.i
		if (!is.null(StdErrCol)) {
			current.stderr <- StdErrCol[i]
			stderr.in.names <- TRUE
		} else if (all(!is.null(LCICol), !is.null(UCICol))) {
			current.LCI <- LCICol[i]
			current.UCI <- UCICol[i]
			stderr.in.names <- FALSE
		}
		if (substr(data.type.i,1,3) == "Log") {
			logData <- FALSE
			ExponentiateDataOnPlot[i] <- TRUE
		} else if (substr(data.type.i,1,2) %in% c("RR")) {
			logData <- TRUE
			ExponentiateDataOnPlot[i] <- TRUE
		} else if (substr(data.type.i,1,2) %in% c("HR")) {
			logData <- FALSE
			if (is.null(ExponentiateDataOnPlot)) ExponentiateDataOnPlot[i] <- FALSE
		}
		else {
			logData <- FALSE
		}
	
		# need to know if there are any blanks, and if so, where to find them
		if (length(which(is.na(rawdata[, data.type.1])))==0) anyblanks <- FALSE else anyblanks <- TRUE
		
		ForestFormat.command <- FFCSV.writeFormatForest(forest.n=i, 
														estimate.col=current.estimate, 
														use.stderr=stderr.in.names, 
														stderr.col=current.stderr, 
														lci.col=current.LCI, 
														uci.col=current.UCI, 
														logData=logData, 
														getBlanks=anyblanks,
														getTrends=getTrends, 
														getHets=getHets) 
		assign(foresti, eval(parse(text=ForestFormat.command)))
			
		MakeForest.command <- paste(MakeForest.command, "# set up an object that stores the forest and associated metadata\n", foresti, " <- ", ForestFormat.command, "\n", sep="")
		
		# detect if there are any Het or Trend columns and set up the appropriate data frame (using same after= notation as the blanks)
		# only if the expandDataLoading is TRUE
		if ((paste("Het",i, sep="") %in% names(rawdata))) {
			# set up the in-function Het tests
			Het.i <- paste("Het", i, sep="")
			current.Het <- data.frame(after=which((rawdata[,Het.i])[-which(is.na(rawdata[,data.type.i]))] != ""))
			assign(paste("Hets", i, sep=""), current.Het)
			
			# write out the command
			
			if (expandDataLoading) HetsTrend.command <- paste(HetsTrend.command, '# Heterogeneity tests data frame\nHets',i,' <- data.frame(after=which(rawdata$Het',i,'[-which(is.na(rawdata$',data.type.i,'))] != \"\"))\n\n', sep="")
			anyhets <- 1
			hets[i] <- TRUE
		} else {
			if (expandDataLoading) HetsTrend.command <- paste(HetsTrend.command, "Hets", i," <- NULL\n", sep="")
			anyhets <- 0
			Hets1 <- NULL
		}
		
		if ((paste("Trend",i,sep="") %in% names(rawdata))) {
			# set up the in-function Trend tests
			Trend.i <- paste("Trend", i, sep="")
			current.Trend <- data.frame(after=which((rawdata[,Trend.i])[-which(is.na(rawdata[,data.type.i]))] != ""))
			assign(paste("Trends",i,sep=""), current.Trend)
			
			# write out the command to do the same
			if (expandDataLoading) HetsTrend.command <- paste(HetsTrend.command, '# Trend tests data frame\nTrends',i,' <- data.frame(after=which(rawdata$Trend',i,'[-which(is.na(rawdata$',data.type.i,'))] != \"\"))\n\n', sep="")
			anyhets <- 1
			trends[i] <- TRUE
			Trends1 <- NULL
		} else {
			if (expandDataLoading) HetsTrend.command <- paste(HetsTrend.command, "Trends", i," <- NULL\n", sep="")
			anyhets <- 0
			Trends1 <- NULL
		}

		
		# blank rows are indicated by gaps in the HR1 column
		if (length(which(is.na(rawdata[, data.type.1])))==0) {
			anyblanks <- FALSE
			assign(paste("blanks",i,sep=""), NULL)
			blank.rows <- which(is.na(rawdata[, data.type.1]))
			MakeBlanks.command <- paste(MakeBlanks.command, "# identify blank rows and create blanks data frame\nblanks",i," <- NULL\nblank.rows <- which(is.na(rawdata$",ifelse(!is.null(EstimateCol), EstimateCol[1], data.type.1),"[-1]))\n\n", sep="")
		} else {
			anyblanks <- TRUE
			blank.rows <- which(is.na(rawdata[, data.type.1]))
			blanks <- data.frame(after=cumsum(c(ifelse(blank.rows[1]==1,0,blank.rows[1]),diff(blank.rows)-1)))
			assign(paste("blanks",i,sep=""), blanks)
			
			# if we're expanding the top write out the long commands, else do the short version
			# MakeBlanks.command <- paste(MakeBlanks.command, "# identify blank rows and create blanks data frame\nblanks.info <- FormatForest(rawdata=rawdata, EstimateCol=\"", data.type.i, "\", getBlanks=TRUE)\nblanks", i," <- blanks.info$blanks\n", sep="")
			if (i == 1) MakeBlanks.command <- paste(MakeBlanks.command, "blank.rows <- forest1$blank.rows", sep="")	
			MakeBlanks.command <- paste(MakeBlanks.command, "\n\n")
		}



	}

	# could also do with working out what the minimum standard errors are 
	minstderr <- rep(NULL, nforests)
	for (i in 1:nforests) {
	
		cforest <- get(paste("forest",i, sep=""))
		cstderrs <- na.omit(cforest[[1]]$stderr)
		minstderr[i] <- min(cstderrs[cstderrs > 0])
	
	}
	globalminstderr <- min(minstderr)
	
	
	left.labels <- convertUnicode(as.character(rawdata$Heading))
	
	if (is.null(EstimateCol)) {
		hr.cols <- which(sapply(names(rawdata), function(z) substr(z,1,nchar(z)-1)) %in% c("HR", "Estimate", "RR", "LogRR"))
	} else {
		hr.cols <- which(names(rawdata) %in% EstimateCol)
	}
	other.cols.LRofForest <- ifelse(other.cols > which(names(rawdata)==data.type.1), 1, 0)
	
	# reformat the column headings
	if (!is.null(ColHeadings)) {
		print.headings <- parseColHeadings(ColHeadings, rd=rawdata)
	} else {
		print.headings <- parseColHeadings(rawdata$ColHeadings, rd=rawdata)
	}

	
	ColumnsSetup.command <- paste("# find labels to put in the left most column\nleft.labels <- convertUnicode(as.character(rawdata$Heading))\n", "# what other columns should be plotted\nother.cols <- ", deparse(other.cols, width.cutoff=500L), "\n# find column headings\nprint.headings <- parseColHeadings(rawdata$ColHeadings, rd=rawdata)\n\n", sep="")
	


	# work out the amount of space needed for the text in each column, plus the plot width
	spacing <- FFCSV.evalSpacing(mainfont=mainfont, 
									orient=orient, 
									type=type, 
									filestem=filestem, 
									blank.right.percent=blank.right.percent, 
									blank.left.percent=blank.left.percent, 
									blank.bottom.percent=blank.bottom.percent, 
									mar=mar, 
									col.ids=c(1,other.cols), 
									anyhets=anyhets, 
									rawdata=rawdata, 
									HetTrendScale=HetTrendScale, 
									print.headings=print.headings, 
									plot.width=plot.width, 
									coldp_names=coldp_names,
									coldp_dps=coldp_dps,
									pvalue.cols=pvalue.cols,
									nforests=nforests)[1]
	
	
	
	
	if (spacing < 0) cat("SEVERE WARNING: the spacing between columns is negative, overlapping columns are likely\n")
	
	spacing.opt.req <- spacing < 0 & auto.shrink.font
	if (spacing.opt.req | optimise.fontsize) {
		# if we're optimising fontsize then 3 is a realistic maximum?
		cat("Optimising font size...")
		flush.console()
		current.mainfont <- 3
		
		# function to optimise over
		if (other.cols[1] == -1) {
			col.ids <- 1
		} else {
			col.ids <- c(1,other.cols)
		}
		optim.spacing <- function(z) return(abs(FFCSV.evalSpacing(mainfont=z, orient=orient, type=type, filestem=tmpfilestem, blank.right.percent=blank.right.percent, blank.left.percent=blank.left.percent, blank.bottom.percent=blank.bottom.percent, mar=mar, col.ids=col.ids, anyhets=anyhets, rawdata=rawdata, HetTrendScale=HetTrendScale, print.headings=print.headings, plot.width=plot.width, nforests=nforests, ValueDigits=ValueDigits, coldp_names=coldp_names, coldp_dps=coldp_dps, pvalue.cols=pvalue.cols)[2]))
		
		# do the optimisation
		optim.mainfont <- optimise(f=optim.spacing, interval=c(0, current.mainfont),maximum=FALSE)
		mainfont <- optim.mainfont$minimum

		# now set up a page so that we can save the spacing given the optimal mainfont
		SetPage(orient=orient, perpage=1, type=type, filestem=tmpfilestem, blank.right.percent=blank.right.percent, blank.left.percent=blank.left.percent, blank.bottom.percent=blank.bottom.percent, verbose=FALSE, titlespace=titlespace, footerspace=footerspace, attempt_adobe_kill=FALSE)
		par(mar=mar)
		blankPlot(c(0,100), c(0,100), mainfont)
		par(xpd=NA)		
		spacing <- FFCSV.evalSpacing(mainfont=mainfont, 
										orient=orient, 
										type=type, 
										filestem=tmpfilestem, 
										blank.right.percent=blank.right.percent, 
										blank.left.percent=blank.left.percent,
										blank.bottom.percent=blank.bottom.percent, 
										mar=mar, 
										col.ids=col.ids, 
										anyhets=anyhets, 
										rawdata=rawdata, 
										HetTrendScale=HetTrendScale, 
										print.headings=print.headings, 
										plot.width=plot.width, 
										nforests=nforests, 
										coldp_names=coldp_names, 
										coldp_dps=coldp_dps,
										pvalue.cols=pvalue.cols, 
										ValueDigits=ValueDigits)[1]
										
		cat("done\n")
		cat(paste("Spacing is", sprintf("%.4f", spacing), "\n"))
		flush.console()

	} else {
		SetPage(orient=orient, perpage=1, type=type, filestem=filestem, blank.right.percent=blank.right.percent, blank.left.percent=blank.left.percent, blank.bottom.percent=blank.bottom.percent, verbose=FALSE, titlespace=titlespace, footerspace=footerspace, attempt_adobe_kill=attempt_adobe_kill)
		par(mar=mar)
		blankPlot(c(0,100), c(0,100), mainfont)
		par(xpd=NA)
		current.mainfont <- mainfont
	}
	
	cat(paste("Character expansion factor is ", sprintf("%.4f", mainfont), "\n", sep=""))
	
		
	# the horizontal spacing and the vertical spacing are independent
	# so we can optimise the size of the top and bottom margins towards some goal
	# SP suggests 1.25 line height 
	# want to find the max strheight of any of the labels
	# the YLocs layout remains the same even if you change the top and bottom margins, 
	# it's just that the height of strings gets bigger as the margins get bigger
	# but we want to optimise this after choosing the font size needed to squeeze things in horizontally

	if (optimise.line.spacing) {
		cat("Optimising line spacing...")
		flush.console()

		# need to know the forest Y locations
		
		SetPage(orient=orient, perpage=1, type=type, filestem=tmpfilestem, blank.right.percent=blank.right.percent, blank.left.percent=blank.left.percent, blank.bottom.percent=blank.bottom.percent, verbose=FALSE, titlespace=titlespace, footerspace=footerspace, attempt_adobe_kill=FALSE)
		par(mar=mar)
		blankPlot(c(0,100), c(0,100), mainfont)
		forest.locs <- ForestBasic(forest1, LogScale=LogScale[1], ExponentiateDataOnPlot=ExponentiateDataOnPlot[1], xaxmin=1, xaxmax=2, mainfont=1, verbose=FALSE)
		closeFile("PDF",suppress.notice=TRUE)

		# do the optimisation
		optim.TBmargin <- FFCSV.optimise.linespace(current.mar=mar, line.spacing=line.spacing, ylocs=forest.locs$YLocs, orient=orient, mainfont=mainfont, type=type, filestem=tmpfilestem, blank.right.percent=blank.right.percent, blank.left.percent=blank.left.percent, blank.bottom.percent=blank.bottom.percent, titlespace=titlespace, footerspace=footerspace)

		
		# replace mar with our optimised version
		TB.mar <- optim.TBmargin$minimum
		if (vertical.alignment == "top") {
			mar <- c(TB.mar*2, mar[2], 0, mar[4])
		} else if (vertical.alignment == "bottom") {
			mar <- c(0, mar[2], TB.mar*2, mar[4]) 
		} else {
			mar <- c(TB.mar, mar[2], TB.mar, mar[4])
		}

		
		cat("done\n")
		flush.console()
		SetPage(orient=orient, perpage=1, type=type, filestem=filestem, blank.right.percent=blank.right.percent, blank.left.percent=blank.left.percent, blank.bottom.percent=blank.bottom.percent, verbose=FALSE, titlespace=titlespace, footerspace=footerspace, attempt_adobe_kill=FALSE)
		par(mar=mar)
		blankPlot(c(0,100), c(0,100), mainfont)
		par(xpd=NA)	
	}
	
	
	#### OK, ready to start writing out graphics instructions
	
	SetPage.command <- paste('\n\n\n##################################################################################',
								'\n\n# set up the Jasper page\ntype <- \"', type, '\"\nSetPage(orient=\"',orient,'\", perpage=1, type=type, filestem=\"', filestem, '\", append_datetime=', as.character(append_datetime),', suppress.date=', deparse(suppress.date), ",", sep="")
	
	
	if (!is.null(font) & font != "sans") SetPage.command <- paste(SetPage.command, 'font=\"', font,'\",', sep="")
	if (!is.null(blank.right.percent)) SetPage.command <- paste(SetPage.command, 'blank.right.percent=', blank.right.percent,',', sep="")
	if (!is.null(blank.left.percent)) SetPage.command <- paste(SetPage.command, 'blank.left.percent=', blank.left.percent,',', sep="")
	if (!is.null(blank.bottom.percent)) SetPage.command <- paste(SetPage.command, 'blank.bottom.percent=', blank.bottom.percent,',', sep="")
	if (!is.null(footer.title)) SetPage.command <- paste(SetPage.command, 'footer.title=', deparse(footer.title, width.cutoff=500L),', ', sep="")
	if (!is.null(footer.title.cex)) SetPage.command <- paste(SetPage.command, 'footer.title.cex=', footer.title.cex, ', ', sep="")
	if (!attempt_adobe_kill) SetPage.command <- paste(SetPage.command, 'attempt_adobe_kill=FALSE, ', sep="")
	
	
	SetPage.command <- paste0(SetPage.command, ' titlespace=', titlespace, ', footerspace=', footerspace, ')\n\n# Set page margins: format is c(bottom, left, top, right)\npar(mar=', deparse(mar), ")\n")
	SetPage.command <- paste0(SetPage.command, "\nmainfont <- ", sprintf("%.4f", mainfont),"\nblankPlot(c(0,100),c(0,100), mainfont)", "\npar(xpd=NA)\n\nspacing <- ",sprintf("%.4f", spacing),"\n")
	
	# add in a plot.width variable 
	SetPage.command <- paste0(SetPage.command, "plot.width <- ", plot.width, "\n\n")
	
	if (other.cols[1] == -1) {
		other.cols.widths <- 0 
	} else {
		# other.cols.widths <- sapply(other.cols, function(z) max(sapply(convertUnicode(as.character(rawdata[-1,z])), strwidth, cex=1, font=2), strwidth(convertUnicode(print.headings[z]), cex=1, font=2)))
		other.cols.widths <- sapply(other.cols, function(z) FFCSV.findColWidth(column=rawdata[,z], heading=print.headings[z], colname=names(rawdata)[z], coldp_names=coldp_names, coldp_dps=coldp_dps, pvalue.cols=pvalue.cols))
	}
	
	heading.width <- max(sapply(convertUnicode(as.character(rawdata$Heading)), strwidth, cex=1, font=blank.labels.font), strwidth(convertUnicode(print.headings[1]), cex=1, font=2))
	# want to place the forest after the left cols
	
	plotlabel.width <- max(strwidth(makeBlankValueLabel(digits=ValueDigits), cex=1), anyhets*strwidth("Heterogeneity: AI=3.7 (p=0.0001)", cex=1*HetTrendScale))
	
	
	# set up empty commands
	forest.command <- ""
	Het.commands <- ""
	Trend.commands <- ""

	
	
	for (i in 1:nforests) {
		# establish the current data type again
		
		current.forest.cols <- EstimateCol[i]
		data.type <- "Estimate"
		# work out what columns are to the left of this forest		
		other.cols.LRofForest <- ifelse(other.cols > which(names(rawdata)==current.forest.cols), 1, 0)
		# get the widths of the columns to the left (unless there aren't any)
		if (other.cols[1] == -1) other.cols.widths.left <- c() else other.cols.widths.left <- other.cols.widths[other.cols.LRofForest == 0]

		xaxmin <- sum(c(heading.width, other.cols.widths.left)) + (length(other.cols.widths.left)+1)*spacing + (i-1)*(plot.width + plotlabel.width + 1.5*spacing)
		if (i == 1) locs <- "locs" else locs <- paste("locs",i,sep="")
		blanks.part.command <- ifelse(expandDataLoading, paste(" blanks=blanks",i,",\n",sep="") ,"")
		Hets.part.command <- ifelse(expandDataLoading, paste(" Hets=Hets",i,",\n",sep="") ,"")
		Trends.part.command <- ifelse(expandDataLoading, paste(" Trends=Trends",i,",\n",sep="") ,"")
		
		# save xaxmin, xaxmax as variables so user can refer to them later
		xaxmin.i <- paste("xaxmin", i, sep="")
		xaxmax.i <- paste("xaxmax", i, sep="")
		
		if (forest.hideticklabs) {
			ticklabs <- rep("", length(forest.xticks[[i]]))
		} else {
			ticklabs <- forest.xticks[[i]]
		}
		
		forest.command <- paste(forest.command,"# draw the Forest plot(s)\n",
								xaxmin.i, " <- ", sprintf("%.4f", xaxmin), "\n",
								xaxmax.i, " <- ", xaxmin.i, " + plot.width\n", 
								locs," <- ForestBasic(forest",i,",\n", blanks.part.command, Hets.part.command, Trends.part.command, "\tLogScale=", as.character(LogScale[i]), ",\n\tExponentiateDataOnPlot=", as.character(ExponentiateDataOnPlot[i]),
								",\n\txaxmin=", xaxmin.i,
								",\n\txaxmax=", xaxmax.i,								
								",\n\txlim=", deparse(forest.Range[[i]], width.cutoff = 500L),
								",\n\txticks=", deparse(forest.xticks[[i]], width.cutoff = 500L),
								",\n\tticklabs=", deparse(ticklabs, width.cutoff = 500L), 
								',\n\txlab=\"', xlab[[i]], '\"',
								",\n\tNLabel=", as.character(FALSE),
								",\n\tmainfont=1",
								",\n\tValueLabels=", as.character(ValueLabels),
								',\n\tValueLabelsHeader=\"', ValueLabelsHeader[[i]],
								'\",\n\tValueLabelsEffectSE=', ValueLabelsEffectSE,
								',\n\tValueDigits=', deparse(ValueDigits[i], width.cutoff = 500L),
								',\n\tValueDigitsSE=', deparse(ValueDigitsSE[i], width.cutoff = 500L),
								',\n\tlwd=', deparse(lwd, width.cutoff = 500L),
								',\n\tCISecond=', as.character(!boxTop),
								',\n\tspacing=spacing/2',
								',\n\tpointGroupsFactor=', pointGroupsFactor,
								',\n\tverbose=FALSE',
								sep="")
		if (!is.null(boxparm.stderr)) {
			forest.command <- paste(forest.command, ",\n\tboxsizeoverride=", as.character(boxsizeoverride),",\n\tboxparm.stderr = ", boxparm.stderr[i], ")\n\n", sep="") 
		} else if (boxparm.sharedstderr) {
			forest.command <- paste(forest.command, ",\n\tboxparm.stderr = ", globalminstderr, ")\n\n", sep="") 
		}
		else forest.command <- paste(forest.command, ")\n\n", sep="")
		
		# write commands to put a heading over the forest
		if (is.null(forest.titleY)) forest.titleY <- "100+1.5*strheight(\"A\", cex=mainfont)"
		if (forest.title.axiscentred) labelwidth <- "" else labelwidth <- "+ strwidth(\"0.00 (0.00, 0.00)\")"
		forestheading.command <- paste("# add titles above forest (may be left empty, change labels=\"\" to what you will)\ntext(x=", xaxmin.i, " + (", xaxmax.i, " - ", xaxmin.i, " + (spacing/2) ", labelwidth, ")/2, y=", forest.titleY, ", labels=\"", as.character(forest.title[[i]]), "\", adj=c(0.5,0), font=2)\n", sep="")
		forest.command <- paste(forest.command, forestheading.command, "\n\n", sep="")
		
		# write the commands to add Hets and Trends in the appropriate places 
		if (hets[i]) {
			Het.commands <- paste0(Het.commands, FFCSV.writeTests(foresti=i, rawdata=rawdata, type="Het", pretty.label="Heterogeneity", xaxmin=xaxmin, spacing=spacing, plot.width=plot.width, HetTrendScale=HetTrendScale), sep="")
		} else {
			Het.commands <- paste0(Het.commands,"")
		}
		
		if (trends[i]) {
			Trend.commands <- paste0(Trend.commands, FFCSV.writeTests(foresti=i, rawdata=rawdata, type="Trend", pretty.label="Trend", xaxmin=xaxmin, spacing=spacing, plot.width=plot.width, HetTrendScale=HetTrendScale), sep="")
		} else {
			Trend.commands <- paste0(Trend.commands,"")
		}
		
		
		
		
	}
	
	
	
	# the left hand column of labels is anchored at x=0
	LeftLabels.command <- "# left hand column of labels\n"
	LeftLabels.command <- paste0(LeftLabels.command, "ylabels.offset <- ", ylabels.offset, " * strheight(\"A\")\n")

	LeftLabels.command <- paste0(LeftLabels.command, 'text(x=0, y=100, labels=',deparse(print.headings[1]),', adj=c(0,0), font=2, cex=1)\n')
	if (anyblanks) {
		LeftLabels.command <- paste0(LeftLabels.command, "text(x=0, y=", ifelse(fixLabelsY, "adjustYForText(locs$YLocs, left.labels[-blank.rows])", "locs$YLocs"), "+ ylabels.offset, labels=left.labels[-blank.rows], adj=0, cex=1, font=ifelse(forest1$IsDiamond, 2,1))\n")
		LeftLabels.command <- paste0(LeftLabels.command, "text(x=0, y=", ifelse(fixLabelsY, "adjustYForText(locs$BlankLocs, left.labels[blank.rows])", "locs$BlankLocs"), "+ ylabels.offset, labels=left.labels[blank.rows], adj=0, font=", blank.labels.font,", cex=1)\n\n")
	} else {
		LeftLabels.command <- paste0(LeftLabels.command,"text(x=0, y=", ifelse(fixLabelsY, "adjustYForText(locs$YLocs, left.labels)", "locs$YLocs"), "+ ylabels.offset, labels=left.labels, adj=0, cex=1, font=ifelse(forest1$IsDiamond, 2,1))\n\n")
	}
	
	# loop over the other columns, there's a bit of faffing involved in working out which forest a column comes after,
	# and the alignment depends on whether the column is text or numeric
	OtherCols.command <- "# adding other columns\n"

	# check to see if there are any other columns
	if (other.cols[1] != -1) {
		# loop over these extra columns - detecting if they're character or numeric and adjusting their position and alignment accordingly
		for (i in 1:length(other.cols)) {

			colType <- ColClass(rawdata[, other.cols[i]])
			if (names(rawdata)[other.cols[i]] %in% pvalue.cols) adj <- 0 else adj <- ifelse(is.na(colType),1, ifelse(colType == "numeric", 1, 0))

			after.plot.number <- sum(other.cols[i] > hr.cols)

			
			# if i=1 and it's a column of text, then we have a problem
			# each plot requires 1.5 spaces
			if ((i-1+adj) == 0) {
				x.coord <- heading.width + i*spacing + after.plot.number*(plot.width + plotlabel.width + 1.5*spacing)
			} else {
				x.coord <- heading.width + i*spacing + cumsum(other.cols.widths)[i-1+adj] + after.plot.number*(plot.width + plotlabel.width + 1.5*spacing)
			}
			
			# write the header 
			col.header <- print.headings[other.cols[i]]
			
			
			col.i <- paste("column",i,".xpos", sep="")
			OtherCols.command <- paste(OtherCols.command, ifelse(i > 1, "\n",""), col.i, " <- ", sprintf("%.4f", x.coord), "\n", sep="")

			OtherCols.command <- paste(OtherCols.command, "text(x=", col.i, ", y=100, labels=convertUnicode(", deparse(col.header), "), adj=c(", adj, ",0), font=2, cex=1", ")\n", sep="")
			
			# if its a pvalue column then do that instead 
			if (names(rawdata)[other.cols[i]] %in% pvalue.cols) {
				if (anyblanks) {
					OtherCols.command <- paste(OtherCols.command, "writePvalue(pvalues = as.numeric(rawdata[-blank.rows,other.cols[", i, "]]), x=", col.i, ", y=locs$YLocs + ylabels.offset, adj=0, font=ifelse(forest1$IsDiamond, 2, 1),\n\t\t\tuse.lower.limit=", pvalue.use.lower.limit, ", lower.limit=", pvalue.lower.limit, ", use.upper.limit=", pvalue.use.upper.limit, ", upper.limit=", pvalue.upper.limit, ")\n",sep="")
				} else {
					OtherCols.command <- paste(OtherCols.command, "writePvalue(pvalues = as.numeric(rawdata[,other.cols[", i, "]]), x=", col.i, ", y=locs$YLocs + ylabels.offset, adj=0, font=ifelse(forest1$IsDiamond, 2, 1),\n\t\t\tuse.lower.limit=", pvalue.use.lower.limit, ", lower.limit=", pvalue.lower.limit,  ", use.upper.limit=", pvalue.use.upper.limit, ", upper.limit=", pvalue.upper.limit, ")\n",sep="")
				}
			} else {
				# write the lines that are in line with a forest point
				# detect if we need to apply a format 
				# need this check first otherwise complaints about coldp_names being missing...
				if (!is.null(ColDecimalPlaces)) {
					if (all(colType == "numeric", names(rawdata)[other.cols[i]] %in% coldp_names)) {
						cdp <- coldp_dps[which(coldp_names == names(rawdata)[other.cols[i]])]
					} else {
						cdp <- NULL
					}
				} else {
						cdp <- NULL 
				}
				if (anyblanks) {
					OtherCols.command <- paste(OtherCols.command, FFCSV.writeLines(i=i, x.coord=col.i, locs.name="YLocs", blank.rows.name="-blank.rows", adj=adj, dp=cdp, fixLabelsY=fixLabelsY), "\n", sep="")
					
					# now the lines that are in the blanks			
					OtherCols.command <- paste(OtherCols.command, FFCSV.writeLines(i=i, x.coord=col.i, locs.name="BlankLocs", blank.rows.name="blank.rows", font.isdiamond.switch=FALSE, adj=adj, dp=NULL, fixLabelsY=fixLabelsY), "\n", sep="")
				} else {
					OtherCols.command <- paste(OtherCols.command, FFCSV.writeLines(i=i, x.coord=col.i, locs.name="YLocs", blank.rows.name="", adj=adj, dp=cdp, fixLabelsY=fixLabelsY), "\n", sep="")
				}
			}
		}
	}
	
	closeFile("PDF",suppress.notice=TRUE)
	if (!is.null(additionalR)) {
		if (file.exists(additionalR)) {
		additional.command <- paste0("\n\n# including requested additionalR file\nsource(\"", additionalR, "\")\n", sep="")
		} else {
			cat("WARNING: additional R file not found.\n")
			additional.command <- ""
		}
	} else {
		additional.command <- ""
	}
	
	closeFile.command <- "# stop writing to file\ncloseFile(type)"
	
	if (!is.null(mainTitle)) mainTitle.command <- paste('# add the main title\nmainTitle(\"', mainTitle,'\", mainfont=1)\n\n', sep="") else mainTitle.command <- "\n"

	cat("Finished set up.\nWriting out Jasper code...")
	flush.console()
	
	# target_file <- paste(filestem,"_jasper_code.r",sep="")
	target_file <- paste0(tmpdir, "\\", filestem, "_jasper_code.r")
	fileConn <- file(target_file)
	writeLines(paste(MakeForest.command, HetsTrend.command, MakeBlanks.command, ColumnsSetup.command, SetPage.command, forest.command, LeftLabels.command, OtherCols.command, Het.commands, Trend.commands, additional.command, mainTitle.command, closeFile.command,  sep=""), fileConn)
	close(fileConn)
	cat("done\n")
	flush.console()
	
	cat("Testing generated Jasper code...\n")
	flush.console()
	source(target_file)
	cat("done\n")
	flush.console()
	
	# move the generated code to the right place 
	cat("Copying generated Jasper code into working directory...")
	flush.console()
	new_target_file <- paste(filestem,"_jasper_code.r",sep="")
	file.copy(from=target_file, to=new_target_file, overwrite=TRUE)
	cat("done\n")
	flush.console()
	
	# delete the temporary R code file
	file.remove(target_file)
	
	# delete any other files in the temp directory that have our temporary filestem
	tmpfilelist <- list.files(tmpdir, pattern=tmpfiles, full.names=TRUE)
	if (length(tmpfilelist > 0)) file.remove(tmpfilelist)
	
	
	cat("Finished!\n")
	
	# return(list(forest=forest, blanks=blanks))
	return()
}



# function to decide if a data frame column is text or numeric, in the presence of blanks
ColClass <- function(column) {

	# convert the column to numeric via character
	col.char <- as.character(column)
	col.num <- suppressWarnings(as.numeric(col.char))
	
	# if the NAs in col.num correspond to "" in col.char then it's probably numeric
	testvec <- apply(cbind(is.na(col.num), col.char == ""),1,function(z) z[1] == z[2])
	test <- all(na.omit(testvec))
	return(ifelse(test, "numeric", "character"))

}
# ColClass(matt$Age_dth[-1])

format.col.headings <- function(z) {

	return(ifelse(is.na(z),z,gsub(";","\\\n", z)))

}

HetTrendLabel <- function(z, start) {

	z.split <- strsplit(z, ",")[[1]]
	
	return(paste('expression(paste(\"',start, '\", sep=\"\"))',sep=""))

}


parseColHeadings <- function(ColHeadings, rd) {

	headings <- rep("", length(names(rd)))
	# turn ColHeadings back into strings
	ColHeadings <- as.character(ColHeadings)
	# only want non-empty ones
	ColHeadings <- ColHeadings[ColHeadings != ""]
	# split any discovered ColHeadings at the equals

	split.heads <- lapply(ColHeadings, strsplit, split="=")
	
	split.heads <- lapply(split.heads, function(z) z[[1]])
	
	# format the heading with new lines if there are any
	split.heads <- lapply(split.heads, function(z) return(c(z[1], gsub(";","\\\n",z[2]))))
	
	for (z in split.heads) {
	
		# place the parsed headings in the right place
		headings[which(names(rd) == z[1])] <- z[2]
	
	}
	
	return(headings)

}



FFCSV.writeTests <- function(foresti, rawdata, type="Het", pretty.label="Heterogeneity", xaxmin, spacing, plot.width, HetTrendScale) {
	Test.command <- ""
	# get the current test
	cur.test <- paste(type, foresti, sep="")
	rd.cur.test <- rawdata[,cur.test]

	ntests <- length(rd.cur.test[rd.cur.test != ""])

	# work out where the locations are stored
	test.locs <- paste("locs$", type, "Locs", sep="")
	Test.command <- paste(Test.command,"# ", pretty.label, " labels \n",sep="")
	for (j in 1:ntests) {

		# find the entry corresponding to the j-th test
		current.test <- (rd.cur.test[rd.cur.test != ""])[j]

		# decompose the info given in the CSV
		current.test <- strsplit(as.character(current.test), ",")[[1]]

		if (is.na(current.test[1])) next
		# make the command
		if (!(current.test[1] == "blank")) {
			Test.command <- paste(Test.command, 'text(x=', xaxmin+plot.width+(spacing/2), ', y=', test.locs, '[', j, '], labels=expression(paste(\"', pretty.label, ': \", chi[', current.test[1], ']^2, \"=', current.test[2], ' (p', ifelse(substr(current.test[3],1,1) %in% c("<", ">"), "", "="), current.test[3], ')\", sep=\"\")), adj=0, cex=', HetTrendScale, ')\n\n', sep="")
		}
	}
	return(Test.command)
}


FFCSV.writeLines <- function(i, x.coord, locs.name, blank.rows.name, font.isdiamond.switch=TRUE, adj, dp=NULL, fixLabelsY=FALSE) {
	if (font.isdiamond.switch) font.command <- "font=ifelse(forest1$IsDiamond, 2, 1)," else font.command <- ""
	if (!is.null(dp)) {
		return(paste("text(x=", x.coord, ", y=locs$", locs.name, " + ylabels.offset, labels=sprintf(\"%.", dp, "f\", rawdata[,other.cols[",i, "]])[", blank.rows.name, "],", font.command, " adj=", adj, ", cex=1", ")\n", sep=""))
	} else {
		labels_bit <- paste0("convertUnicode(as.character(rawdata[,other.cols[",i, "]]))[", blank.rows.name, "]")
		ypos <- ifelse(fixLabelsY, paste0("adjustYForText(Ys=locs$", locs.name, ", labels=", labels_bit, "),\n\t"), paste0("locs$", locs.name, " + ylabels.offset, "))
		# return(paste("text(x=", x.coord, ", y=locs$", locs.name, ", labels=convertUnicode(as.character(rawdata[,other.cols[",i, "]]))[", blank.rows.name, "],", font.command, " adj=", adj, ", cex=1", ")\n", sep=""))
		return(paste("text(x=", x.coord, ", y=", ypos, " labels=", labels_bit, ", ", font.command, ", adj=", adj, ", cex=1", ")\n", sep=""))
	}
}


makeBlankValueLabel <- function(digits=NULL) {

	if (is.null(digits)) digits <- 2
	zero <- sprintf(paste("%.", digits, "f", sep=""), 0)
	return(paste(zero, " (-", zero, ",-", zero, ")", sep=""))

}

heading_preprocess <- function(heading_vector) {

	for (i in 1:length(heading_vector)){
		if (substr(heading_vector[i],1,4) =="plus") {
			substr(heading_vector[i],1,4) <- "   +"
		}
	}
	return(heading_vector)
}