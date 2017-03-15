### function that formats a forest data frame, or alternatively computes blanks

FormatForest <- function(rawdata, 
							forest.n=1, 
							EstimateCol, 
							StdErrCol=NULL, 
							LCICol=NULL, 
							UCICol=NULL, 
							logData=FALSE, 
							findDiamonds=NULL, 
							findShow=NULL,
							findShowCI=NULL, 
							findFillColour=NULL, 
							findDiamondGuidelines=NULL, 
							findhideNoEffect=NULL, 
							getBlanks=FALSE, 
							getHets=FALSE, 
							getTrends=FALSE
) {

	if (all(is.null(StdErrCol), is.null(LCICol), is.null(UCICol), !getBlanks)) stop("Need to specify StdErrCol or both of LCICol and UCICol")
	
	# two cases: given StdErrCol, or given both of LCICol and UCICol
	newEstCol <- "Estimate"
	
	# set up the forest data frame in the two different ways
	if (!is.null(StdErrCol)) {
	
		forest <- na.omit(rawdata[,c(EstimateCol, StdErrCol)])

		names(forest) <- c(newEstCol, "stderr")
		forest$LCI <- forest[,newEstCol] - 2*forest$stderr
		forest$UCI <- forest[,newEstCol] + 2*forest$stderr
		
		if (logData) forest <- log(forest)
	
	} else if ((!is.null(LCICol)) & (!is.null(UCICol))) {
	
		forest <- na.omit(rawdata[,c(EstimateCol, LCICol, UCICol)])
		
		names(forest) <- c(newEstCol, "LCI", "UCI")
		if (logData) forest <- log(forest)

		if (all(substr(EstimateCol,1,2) == "HR", !logData)) {
			# the special CEU case where we are given hazard ratios 
			# but draw the axis on the linear scale... which means 
			# that we really need to calculate the stderr from the 
			# log estimate and log UCI
			cat("Calculating SE from log HR and log UCI\n")
			flush.console()
			forest$stderr <- (log(forest$UCI) - log(forest[,newEstCol]))/1.96
		} else {
			forest$stderr <- (forest$UCI - forest[,newEstCol])/1.96
		}

		approx_lci <- (forest[,newEstCol] - 1.96*forest$stderr)

		
	
	} else stop("Unknown error.........")
	
	# reserved keywords 
	res_keys <- c("IsDiamond", "Show", "ShowCI", "FillColour", "DiamondGuidelines", "hideNoEffect")
	default <- c(0,1,1,1,1,0)
	
	for (key in res_keys) {
		ckey <- paste0(key, forest.n)
		# need the data with any blank rows omitted
		if (!is.null(na.action(forest))) rawdata.nona <- rawdata[-na.action(forest),] else rawdata.nona <- rawdata
		if (ckey %in% names(rawdata)) {
			# match up the keyword with the forest points
			forest[,key] <- rawdata.nona[!is.na(rawdata.nona[,ckey]), ckey]
		} else {
			forest[,key] <- default[res_keys == key]
			if (key == "ShowCI") forest[,key] <- forest[,"Show"]
		}
	}

	
	if (getHets) {
		current.Hets <- paste("Het", forest.n, sep="")
		Hets <- data.frame(after=which((rawdata[,current.Hets])[-which(is.na(rawdata[,EstimateCol]))] != ""))
	} else {
		Hets <- NULL
	}
	
	if (getTrends) {
		current.Trends <- paste("Trend", forest.n, sep="")
		Trends <- data.frame(after=which((rawdata[,current.Trends])[-which(is.na(rawdata[,EstimateCol]))] != ""))
	} else {
		Trends <- NULL
	}
	
	
	# if we actually want the blanks data.frame, compute that and then return it
	# it's useful later to be able to return forest$IsDiamond directly
	if (getBlanks) {
		blank.rows <- which(is.na(rawdata[,EstimateCol]))
		blanks1 <- data.frame(after=cumsum(c(ifelse(blank.rows[1]==1,0,blank.rows[1]-1),diff(blank.rows)-1)))
		# if (all(!is.null(findhideNoEffect), findhideNoEffect)) blanks1$hideNoEffect <- ifelse((rawdata[,paste("hideNoEffect", forest.n,sep="")])[blank.rows] == 1, TRUE, FALSE)
		if (paste0("hideNoEffect", forest.n) %in% names(rawdata)) {
			blanks1$hideNoEffect <- ifelse((rawdata[,paste("hideNoEffect", forest.n,sep="")])[blank.rows] == 1, TRUE, FALSE)
		}
	}

	
	# pointGroups is somewhat special...
	pointGroupsn <- paste0("pointGroups", forest.n)
	if (pointGroupsn %in% names(rawdata)) {
	
		if (getBlanks) {
			ptgrps <- rawdata[-blank.rows,pointGroupsn]
		} else {
			ptgrps<- rawdata[,pointGroupsn]
		}
		ptgrps <- ifelse(is.na(ptgrps), 0, ptgrps)
		
		# find distinct values
		ptgrps.distinct <- unique(ptgrps[ptgrps != 0])
		
		new_pt_groups <- lapply(ptgrps.distinct, function(z) which(ptgrps == z))	
	
	} else {
		new_pt_groups <- NULL
	}
	
	if (getBlanks) {
		return(list(forest=forest, blank.rows=blank.rows, blanks=blanks1, Hets=Hets, Trends=Trends, IsDiamond=forest$IsDiamond, pointGroups=new_pt_groups))
	}
	return(list(forest=forest, blanks=NULL, Hets=Hets, Trends=Trends, IsDiamond=forest$IsDiamond, pointGroups=new_pt_groups))
}