## function that adds the value labels in an ONYX forest plot.

ONYXForestValueLabels <- function(ThisCol="Hazard ratio (95% CI)", ValLabTit=NULL, MidValLab, Ymax=100, mainfont) {

	# MA: the following is a little weird, but OK
	if (is.null(ValLabTit)==FALSE) ThisCol <- ValLabTit
	
	ColTitY<-Ymax+(seq(length(ThisCol)-1, 0)*1.5*strheight("2", cex=mainfont))
	text(MidValLab, ColTitY, ThisCol, adj=c(0.5, 0), font=2, cex=mainfont)
	
	return(max(ColTitY))
	
}