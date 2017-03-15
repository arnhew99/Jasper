FormatLP <- function(LPdata, GroupCol=NULL, 
						RFLevelCol, 
						EstimateCol, 
						StdErrCol, 
						xlim=NULL, 
						ylim=NULL, 
						axes=NULL, 
						xaxis=NULL, 
						yaxis=NULL, 
						xticks=NULL, 
						yticks=NULL, 
						xaxis.all=NULL,
						xticklabs=NULL, 
						yticklabs=NULL,
						yaxis.all=NULL,
						xlab=NULL,
						xlab.line=NULL,
						ylab=NULL, 
						ylab.line=NULL,
						boxcols=NULL, 
						boxbordercol=NULL, 
						linecols=NULL, 
						linelwds=NULL, 
						lineltys=NULL,
						PointLabelsTop=NULL, 
						PointLabelsBottom=NULL,
						PointLabelsPosAdj=NULL, 
						PointLabelsCol=NULL,
						PointLabelsCEX=NULL, 
						boxparmx=NULL, 
						boxparmy=NULL, 
						boxparm.stderr=NULL, 
						boxsizeoverride=NULL) 
{

	if (is.null(GroupCol)) {
	
		LPdata$DUMMY <- 1
		GroupCol <- "DUMMY"
	
	}

	GroupCol.values <- unique(LPdata[,GroupCol])
	# if RFLevelCol is a character, treat as factors and convert to numeric
	if (is.character(LPdata[,RFLevelCol])) {
	
		LPdata[,RFLevelCol] <- as.numeric(as.factor(LPdata[,RFLevelCol]))
	
	}
	# could do some heuristics here to try and guess (at least) the GroupCol
	# i.e. is.character on the left most such column
	# or such that grouping on a column splits the data into sets of maximum size
	# differentiating Estimate and RFLevelCol might be more difficult, but we
	# might guess that the RFLevelCol has increasing estimates
	# and the standard error col should have values in random order (and of about the same magnitude)
	res <- lapply(GroupCol.values, function(z) return(data.frame(RF_Level = LPdata[LPdata[, GroupCol] == z, RFLevelCol], Estimate = LPdata[LPdata[, GroupCol] == z, EstimateCol], StdErr = LPdata[LPdata[, GroupCol] == z, StdErrCol])))
	params <- c("xlim", "ylim", "xticks", "yticks", "xlab", "xlab.line", "ylab", "ylab.line", "xticklabs", "yticklabs", "linelwds", "axes", "xaxis", "yaxis", "xaxis.all", "yaxis.all", "boxcols", "boxbordercol", "linecols", "linelwds", "lineltys","PointLabelsTop","PointLabelsBottom","PointLabelsPosAdj","PointLabelsCol","PointLabelsCEX","boxparmx","boxparmy","boxparm.stderr","boxsizeoverride")
	
	
	# look for a global Jasper options list and pull info from there...
	# but this function overrides them if options are provided
	if (exists(".jasper.global")) {
	
		for (i in names(.jasper.global)) {
		
			command <- paste("res$params$", i," <- ", deparse(.jasper.global[[i]]), sep="")
			eval(parse(text=command))
		
		}
	
	}
	
							
	for (i in params) {
		# write a command and then execute it 
		command <- paste("if (!is.null(", i, ")) res$params$", i," <- ", i, sep="")
		eval(parse(text=command))
	}
	
	# if (!is.null(ylim)) res$params$xlim <- ylim
	return(res)

}

