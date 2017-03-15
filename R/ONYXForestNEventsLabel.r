ONYXForestNEventsLabel <- function(EventsTit, MidNEvents, mainfont) {

	ThisCol<- EventsTit
	
	bars<-myindex(ThisCol, "|")
	
	#     Need fixed=TRUE because | means something in regular expressions (see help)
	if (bars[1]>-1) ThisCol <- unlist(strsplit(ThisCol, split="|", fixed=TRUE))
	
	Font <- mainfont 
	
	ColTitY<-100+(seq(length(ThisCol)-1, 0)*1.5*strheight("2", cex=1.2*Font))
	
	text(MidNEvents, ColTitY, ThisCol, adj=c(0.5, 0), font=2, cex=1.2*Font)
	#ndeathstop<-max(ColTitY)
	
	return(max(ColTitY))

}