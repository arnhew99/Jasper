projectFiles <- function(start_dir="~", filetypes=c("r", "sas"), filename="tmp", sort_mtime=TRUE) {

	# get a list of files in the current directory
	# use recursive=TRUE to follow the directory structure
	start_dir <- path.expand(start_dir)

	cat("searching...")
	flush.console()
	filetype_list <- lapply(filetypes, function(z) list.files(start_dir, pattern=paste0("\\.", z, "$"), ignore.case=TRUE, recursive=TRUE))
	cat("done\n")
	flush.console()
	
	cat("processing...")
	filetype_list <- lapply(filetype_list, sort)
	filetype_list_mtime <- lapply(filetype_list, function(z) sapply(z, function(y) as.character(file.info(paste0(start_dir, "/",y))$mtime)))

	if (sort_mtime) {

		mtime_order <- lapply(filetype_list_mtime, order, decreasing=TRUE)
		filetype_list <- mapply(function(l, z) l[z], l=filetype_list, z=mtime_order, SIMPLIFY=FALSE)
		filetype_list_mtime <- mapply(function(l, z) l[z], l=filetype_list_mtime, z=mtime_order, SIMPLIFY=FALSE)


	}

	cat("done\n")
	flush.console()
	
	cat("making HTML...")
	flush.console()
	html <- "<html><body>"
	
	for (i in 1:length(filetype_list)) {
		html <- paste0(html, "<p>", filetypes[i], "<br>")
		crows <- paste0("<a href=\"file:///", paste0(start_dir, "/", filetype_list[[i]]), "\">", filetype_list[[i]], "</a>&nbsp&nbsp&nbsplast modified: ", filetype_list_mtime[[i]])
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