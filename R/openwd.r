openwd <- function() {

	# we need to be running on Windows
	if (grepl("WINDOWS", toupper(Sys.info()["sysname"]), fixed=TRUE)) {
		shell.exec(normalizePath(getwd()))
		return(TRUE)
	}

}