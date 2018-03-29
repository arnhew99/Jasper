

FindCSVs <- function(folder=".", verbose=TRUE, upcase.names=FALSE) {

	curwd <- getwd()
	setwd(folder)
	object.names <- c()
	for (i in list.files()) {
	
		if (substr(i, nchar(i)-3, nchar(i)) == ".csv") {
		
			if (verbose) {
				cat(paste("loading", i, "\n"))
				flush.console()
			}
			csvname <- substr(i, 1, nchar(i)-4)
			object.names <- c(object.names, csvname)
			assign(csvname, read.csv(i, as.is=TRUE), envir=.GlobalEnv)
			
			if (upcase.names) {
				eval(parse(text=paste0("names(", csvname, ") <- toupper(names(", csvname, "))")), envir=.GlobalEnv)
			}
		
		}
	
	}
	setwd(curwd)
	invisible(object.names)

}
