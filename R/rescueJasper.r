rescueJasper <- function() {

	# attempt to rescue any aborted file writes by closing all graphics devices
	# and then reloading Jasper using the path .jasper.load
	
	while (!is.null(dev.list())) {
		dev.off()
	}
	
	cat("Reloading Jasper...\n")
	flush.console()
	if (exists(".jasper.load")) source(.jasper.load) else {
	
		if ("package:Jasper" %in% search()) {
			# try to unload the package
			cat("attempting to unload Jasper package\n")
			tryCatch(detach("package:Jasper", unload=TRUE), error=function(e) cat(""))
			tryCatch(unloadNamespace("Jasper"), error=function(e) cat(""))
			
			# try to reload the package 
			cat("attempting to reload Jasper package\n")
			tryCatch(require(Jasper), error=function(e) cat(""))
		
		}
		
		
	}
	cat("done.\n")

}