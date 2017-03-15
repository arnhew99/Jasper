ONYXForestNEvents <- function(xaxmin, YLocs, nevents, Boldness, mainfont) {

	Font <- mainfont

	text(xaxmin, YLocs, labels=nevents, font=Boldness, adj=c(1, 0.5), cex=1.2*Font)
	MidNEvents <- xaxmin - (0.5*max(strwidth(as.character(nevents)), cex=1.2*Font))
	
	return(MidNEvents)

}