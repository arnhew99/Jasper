svgz <- function(filestem) {

	# read in an SVG file (which is a text format)
	# write out an SVGZ gzip compressed version

	con <- file(paste0(filename, ".svg"), open="rt")
	contents <- readLines(con)
	close(con)
	
	con <- gzfile(paste0(filename, ".svgz"), open="wt")
	writeLines(text=contents, con)
	close(con)
	

}