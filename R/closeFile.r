### function to close a graphics file if one has been opened
###		- if the output type is GUI then we don't need to do this
   
closeFile <- function(type="GUI", suppress.notice=FALSE) {
	if (type != "GUI") {
		if (!suppress.notice) {
			cat("Closing graphics file\n")
			flush.console()
		}
		dev.off()
	}
}