readCSVs <- function() {

	valid <- c()
	for (i in list.files()) {
		if (substr(i,nchar(i)-2, nchar(i)) == "csv") {
			cat(paste("loading", i, "\n"))
			assign(substr(i, 1, nchar(i)-4), read.csv(i,as.is=TRUE), envir=.GlobalEnv)
			valid <- c(valid, substr(i, 1, nchar(i)-4))
	}

}
	return(valid)
}