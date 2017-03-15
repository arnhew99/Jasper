FFCSV.writeFormatForest <- function(forest.n=1,
									estimate.col,
									use.stderr=TRUE, 
									stderr.col=NULL, 
									lci.col=NULL, 
									uci.col=NULL, 
									logData=FALSE, 
									getBlanks=FALSE,
									getTrends=FALSE, 
									getHets=FALSE) 
{

	# need to write out two versions, apparently
	# one to be added to the R code
	# one to be executed immediately (although I don't really know what the difference is? 

	start_part <- paste0('FormatForest(rawdata=rawdata, forest.n=', forest.n, ', EstimateCol = \"', estimate.col, '\", ')
	end_part <- paste0('logData=', as.character(logData),', getBlanks=', as.character(getBlanks), ', getHets=', as.character(getHets), ', getTrends=', as.character(getTrends),')\n')
	
	mid_part <- ""
	if (use.stderr) {
		mid_part <- paste0('StdErrCol=\"', stderr.col, '\", ')
	} else {
		mid_part <- paste0('LCICol=\"', lci.col, '\", UCICol=\"', uci.col, '\", ')
	}
	
	command <- paste0(start_part, mid_part, end_part)
	# ForestFormat_write.command <- paste0(start_part, ' StdErrCol=\"', ifelse(!is.null(StdErrCol), StdErrCol[i], current.stderr), end_part)
	
	return(command)
	
			



}