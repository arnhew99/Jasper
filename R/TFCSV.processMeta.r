TFCSV.processMeta <- function(file, rawdata) {
	
	# look for keyword-value pairs in the CSV file
	metadata <- read.csv(file, as.is=TRUE, colClasses="character")
	
	keywords <- metadata$keyword
	
	findKeyword <- function(string, keys) {
	
		found <- string %in% keys
		if (found) return(which(keys == string)) else return(-1)
	
	}
	
	res <- list(colheadings=rep("", dim(rawdata)[2]))
	
	# look for colheadings
	colheadings.i <- findKeyword(string="colheading", keys=keywords)
	if (colheadings.i[1] != -1) {
	
		headings <- parseColHeadings(metadata[colheadings.i, 2], rd=rawdata)
		res$colheadings <- headings
	}
	
	# look for spanheadings
	spanheadings.i <- findKeyword(string="spanheading", keys=keywords)
	if (spanheadings.i[1] != -1) {
	
		span.parsed <- lapply(spanheadings.i, function(z) unlist(strsplit(metadata[z,2], "!")))
		
		# the first element needs processing as a column title
		# the second and third need the column names extracted
		
		parseSpanHeadings <- function(z) {
		
			z[1] <- gsub(";","\\\n",z[1])
			z[2:4] <- unlist(lapply(z[2:4], function(y) unlist(strsplit(y, "="))[2]))
			return(z)
		}
		
		span.parsed <- lapply(span.parsed, parseSpanHeadings)
		res$spanheadings <- matrix(unlist(span.parsed), ncol=4, byrow=TRUE)
		res$spanheadings <- as.data.frame(res$spanheadings)
		names(res$spanheadings) <- c("heading", "start", "end", "row")
		
	}
	
	# look for coldecimals
	coldecimals.i <- findKeyword(string="coldecimal", keys=keywords)
	if (coldecimals.i[1] != -1) {
	
		cd <- lapply(coldecimals.i, function(z) unlist(strsplit(metadata[z,2], "=")))
		res$coldecimals <- data.frame(col=sapply(cd, function(z) return(z[1])), dp=sapply(cd, function(z) return(z[2])), stringsAsFactors=FALSE)

	}
	print(res)
	
	return(res)
	
	
}