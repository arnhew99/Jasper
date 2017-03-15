# function to detect if a CSV file containing the string npeople exists in the current folder

detectNpeople <- function() {

	grep.res <- grep("npeople", list.files(), fixed=TRUE)
	if (length(grep.res) == 0) return(NULL) else {
	
		# need to check whether the identified file is actually a CSV file
		possibles <- list.files()[grep.res]

		test <- sapply(possibles, function(z) ifelse(substr(z, nchar(z)-3, nchar(z)) == ".csv", TRUE, FALSE))
		
		if (!any(test)) return(NULL) else return((possibles[test])[1])
	
	}

}