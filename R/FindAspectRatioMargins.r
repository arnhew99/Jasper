FindAspectRatioMargins <- function(targetAS, mar=NULL, maxmar=15, xlim, ylim, orient, type, perpage, mainfont) {

	if (is.null(mar)) mar <- par("mar")
	
	tmpfilestem <- paste(Sys.getenv()["TEMP"], "\\jasper_temp", sep="")
	
	optimiseTBMargins <- function(addmar, mar, targetAS) {
	
		SetPage(type=type, filestem=tmpfilestem, perpage=perpage, orient=orient, unload.devices=FALSE, attempt_adobe_kill=FALSE)
		par(mar=c(mar[1]+(addmar/2), mar[2], mar[3]+(addmar/2), mar[4]))
		blankPlot(xlim=xlim, ylim=ylim, mainfont=mainfont)
		pin <- par("pin")
		usr <- par("usr")
		diffpin <- targetAS - ((diff(usr[3:4])/pin[2]) / (diff(usr[1:2])/pin[1]))
		closeFile("PDF", suppress.notice=TRUE)
		return(abs(diffpin))
	
	}
	sol <- optimise(optimiseTBMargins, interval=c((-1)*min(mar[c(1,3)]),maxmar), mar=mar, targetAS=targetAS)$minimum
	return(c(mar[1]+(sol/2), mar[2], mar[3]+(sol/2), mar[4]))
	

}