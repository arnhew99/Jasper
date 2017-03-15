FFCSV.parseTitle <- function(tit) {

	# allow Jasper to parse some R commands in titles
	
	tit.split <- unlist(strsplit(tit, "#R#", fixed=TRUE))
	
	# any element that's a multiple of two needs parsing and evaluating
	for (i in 1:length(tit.split)) {
	
		if (i %% 2 == 0) {
		
			tit.split[i] <- as.character(eval(parse(text=tit.split[i])))
		
		} else next
	
	}
	return(paste(tit.split, collapse=""))
	

}