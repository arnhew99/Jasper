ForestBlankLabels <- function(blanklist, labelmid, mainfont, bold=2) {
	
	labelmids <- rep(labelmid,dim(blanklist)[1])
	text(x=rep(labelmids,3), y=blanklist$BlankLocs, labels=blanklist$text, cex=1.2*mainfont, font=bold, adj=c(0.5,0))
	
}