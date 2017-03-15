addTable <- function(dataframe, xleft, ytop, headings, spacing) {


	ncols <- dim(dataframe)[2]
	colwidths <- apply(dataframe,2,function(z) max(strwidth(z)))
	colwidths <- sapply(1:length(colwidths), function(i) return(max(colwidths[i], strwidth(headings[i], font=2))))
	colwidths <- c(0, colwidths)
	
	
	for (i in 1:ncols) {
	
		text(x=xleft + (i-1)*spacing + cumsum(colwidths)[i], y=ytop, labels=paste("\n", paste(dataframe[,i], collapse="\n"), sep=""), adj=c(0,1))
		text(x=xleft + (i-1)*spacing + cumsum(colwidths)[i], y=ytop, labels=headings[i], adj=c(0,1), font=2)
		
	}
	
	


}