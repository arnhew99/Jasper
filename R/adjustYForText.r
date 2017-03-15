adjustYForText <- function(Ys, labels) {

	dropped.letters <- c("g", "j", "p", "q", "y")
	adjust.Ys <- unique(as.vector(unlist(sapply(dropped.letters, function(z) grep(z, labels, fixed=TRUE)))))
	Ys[adjust.Ys] <- Ys[adjust.Ys] - (strheight(expression(g)) - strheight(expression(c)))/2
	return(Ys)

}