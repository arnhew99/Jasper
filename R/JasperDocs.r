JasperDocs <- function() {

	if (!exists(".jasper.home", envir=.GlobalEnv)) stop("Jasper documentation location not known.\nAre you using the Jasper R package?\nDocumentation for some functions can be found by prefixing ?\nto the function name")
	loc <- paste(.jasper.home, "\\documentation\\index.html", sep="")
	browseURL(loc)

}