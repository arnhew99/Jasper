projectFiles <- function(start_dir="~", filetypes=c("r", "sas"), filename="tmp") {

	# get a list of files in the current directory
	# use recursive=TRUE to follow the directory structure
	start_dir <- path.expand(start_dir)

	cat("searching...")
	flush.console()
	file_list <- list.files(path=start_dir, recursive=TRUE)
	cat("done\n")
	flush.console()
	
	cat("processing...")
	flush.console()
	get_extension <- function(ext, fnames) {
		# use toupper to make case-insensitive
		spl <- strsplit(toupper(fnames), ".", fixed=TRUE)
		lengths <- sapply(spl, length)
		extensions <- sapply(spl, tail, 1)
		return(fnames[ifelse(lengths==1, FALSE, ifelse(extensions == toupper(ext), TRUE, FALSE))])
	}
	
	filetype_list <- lapply(filetypes, get_extension, fnames=file_list)
	filetype_list <- lapply(filetype_list, sort)
	cat("done\n")
	flush.console()
	
	cat("making HTML...")
	flush.console()
	html <- "<html><body>"
	
	for (i in 1:length(filetype_list)) {
		html <- paste0(html, "<p>", filetypes[i], "<br>")
		crows <- paste0("<a href=\"file:///", paste0(start_dir, "/", filetype_list[[i]]), "\">", filetype_list[[i]], "</a>")
		crows <- paste(crows, collapse="<br>")
		html <- paste0(html, crows)
	}
	
	html <- paste0(html,"</body></html>")
	
	con <- file(paste0(filename, ".html"), open="wt")
	writeLines(html, con)
	close(con)
	cat("done\n")
	flush.console()
	
	return(filetype_list)


}