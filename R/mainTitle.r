## function that will format and print the main title in the current layout matrix

mainTitle <- function(titles, mainfont=NULL, shrinktofit=TRUE, cex=NULL) {

	# titles 			vector of titles, the first one will be embiggened
	# mainfont			seems to be the main font size (scaling?)

	# allow either mainfont or cex to be given, but mainfont is given priority
	if (is.null(mainfont) & is.null(cex)) stop("ERROR: One of mainfont or cex needs to be supplied")
	
	if (is.null(mainfont) & !is.null(cex)) {
		if (cex == "auto") cex <- 100
		mainfont <- cex
	}
	
	
	
   # Main title
   # First line is larger and in bold, which needs a different line drop
   # (hence the complexity here)
   #ntitle <- length(PageStruct$MainTitle)
   ntitle <- length(titles)
   
   par(mar=c(1, 1, 1, 1))
   plot(c(0, 1), c(0, 1), type='n', axes=F, xlab="", ylab="")

	  
	if (ntitle>=1)
      {
      linedrop1 <- 1.2*max(strheight(titles, cex=1.8*mainfont))
      linedrop <- 1.2*max(strheight(titles, cex=1.5*mainfont))
      }


   if (ntitle<=1) titlespace <- 0
   if (ntitle>1) titlespace <- linedrop1 + ((ntitle-2)*linedrop)
   yhigh <- 0.5+(titlespace/2)

   yat <- yhigh
   if (ntitle > 1) yat <- c(yat, yhigh - linedrop1)
   if (ntitle > 2) yat <- c(yat, (yhigh - linedrop1) - (linedrop*seq(1, ntitle-2)))

   FontSizes <- c(mainfont*1.8, rep(mainfont*1.5, ntitle-1))

   title.width <- strwidth(titles[1], cex=FontSizes[1], font=2)
   if (title.width > 1) {
		cat("Title font size too large, shrinking...\n")
		
		# define a function to test
		test.title.width <- function(z) return(abs(1 - strwidth(titles[1], cex=z*FontSizes[1], font=2)))
		
		optim.cex <- optimise(f=test.title.width, interval=c(0,1), maximum=FALSE)
		FontSizes <- optim.cex$minimum*FontSizes
		
   }
   

   # if (ntitle>=1) text(0.5, yat, PageStruct$MainTitle, adj=0.5, cex=FontSizes, 
       # font=c(2, rep(1, ntitle-1))) 
	if (ntitle>=1) text(0.5, yat, titles, adj=0.5, cex=FontSizes, font=c(2, rep(1, ntitle-1))) 

}