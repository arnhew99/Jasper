SetPage <-
function(orient="PORTRAIT", 
		perpage=NULL, 
		nlong=NULL, 
		nshort=NULL, 
		type="GUI", 
		attempt_adobe_kill=TRUE,
		png_bg="transparent",
		filestem="jasper_output", 
		append_datetime=FALSE,
		label=NULL,
		font=NULL,
		footer.title=NULL, 
		footer.title.cex=1.5, 
		footer.title.allbold=FALSE, 
		display.layout=FALSE, 
		titlespace=0.1, 
		footerspace=0.1, 
		width=1, 
		height=1, 
		footer="", 
		footer.cex=1.3, 
		page_height=NULL,
		page_width=NULL,
		page_dpi=NULL,
		page_pointsize=NULL,
		suppress.date=FALSE, 
		NoLabel=TRUE, 
		ckb.participants=TRUE, 
		verbose=TRUE,
		blank.left.percent=NULL, 
		blank.right.percent=NULL, 
		blank.bottom.percent=NULL,
	  	vertical.scales=NULL, 
		ncols=NULL,
		show.internal.layout=FALSE,
		advanced=FALSE, 
		mat=NULL, 
		widthvec=NULL, 
		heightvec=NULL,
		unload.devices=TRUE)
{
	# Page set up routine
	# One title bar at the top
	# plotting windows below
	# footnote space at the base

	# The windows are used as follows: first the bottom label space, 
	# then the plots in rows, and finally the title space.
	
	# if this is the first time that SetPage has been run in this session, then attempt to 
	# get the current build number off of the network
	if (all(!exists(".jasper.buildnum"), !("Jasper" %in% loadedNamespaces()))) {
		if (file.exists("K:\\NDPHopen\\Stats User Area\\Jasper\\jasper\\builds\\latest.txt")) .jasper.buildnum <<- paste("2-", read.table("K:\\NDPHopen\\Stats User Area\\Jasper\\jasper\\builds\\latest.txt")[1], sep="")
	}
	type <- toupper(type)
	
	if (all(unload.devices, !(type %in% c("GUI","PDF_MULTIPAGE_CONT")))) {
		if (!is.null(dev.list())) {
		
			devnames <- names(dev.list())
			# toclose <-  (dev.list())[which(devnames != "windows")]
			toclose <- dev.list()
			# print(toclose)
			for (j in toclose) {
				if (j != 1) dev.off(j)
			}
		}
	}
	
	# if the file type is PDF and the system is Windows, attempt to kill
	# any running Adobe reader windows with the right title
	if (all(attempt_adobe_kill, type == "PDF", .Platform$OS.type=="windows")) {
		cmd <- paste0("taskkill /fi \"Windowtitle eq ", filestem, ".pdf - Adobe Reader\"")
		system(command=cmd, intern=TRUE, wait=TRUE, show.output.on.console=FALSE)
		# annoyingly it seems that sometimes the lock doesn't get released in time
	}

	# set the default font
	if (is.null(font)) {
		font <- "sans"
	}
	
	if (is.null(page_height)) {
		if (orient=="LANDSCAPE") {
			page_height <- 8.26
		}
		else if (orient=="PORTRAIT") {
			page_height <- 11.69
		}
		else stop("Invalid orientation")
	}	
	if (is.null(page_width)) {
		if (orient=="PORTRAIT") {
			page_width <- 8.26
		}
		else if (orient=="LANDSCAPE") {
			page_width <- 11.69
		}
		else stop("Invalid orientation")
	}
	
	if (append_datetime) {
		filestem <- paste0(filestem, "_", strftime(Sys.time(), "%Y%m%d_%H%M"))
	}
	
	# First open the device
	if (type=="EMF") {
		outfile<-paste(filestem, '.emf', sep='')
		# EMF usually fails if the file currently exists...
		# it's a bit risky but we will probably need to delete the existing filename
		if (file.exists(outfile)) {
			if (!is.null(dev.list())) closeFile("EMF")
			file.remove(outfile)
		}
		if (orient=="LANDSCAPE") win.metafile(outfile, width=page_width, height=page_height, pointsize=8)
		else win.metafile(outfile, width=page_width, height=page_height, pointsize=8)
	}

	if (type=="PS") {
		outfile<-paste(filestem, '.ps', sep='')
		if (orient=="LANDSCAPE") postscript(outfile, width=page_width, height=page_height, pointsize=8)
		else postscript(outfile, width=page_width, height=page_height, pointsize=8, horizontal=FALSE)
	}

	# the logic here is trying to work out if we are running on Windows or not 
	if (type=="GUI") {
		if (is.function(options("device")$device)) {
			gui.supportedargs <- names(formals(options("device")$device))
			if ("rescale" %in% gui.supportedargs) {
				if (orient=="LANDSCAPE") options("device")$device(width=page_width, height=page_height, pointsize=8, xpos=0, ypos=0, rescale="fixed")
				else options("device")$device(width=page_width, height=page_height, pointsize=8, xpos=0, ypos=0, rescale="fixed")
			} else {
				if (orient=="LANDSCAPE") options("device")$device(width=page_width, height=page_height, pointsize=8)
				else options("device")$device(width=page_width, height=page_height, pointsize=8)
			}
		} else if (options("device")$device == "RStudioGD") {
			if (Sys.info()[1] == "Windows") {
				if (orient=="LANDSCAPE") windows(width=page_width, height=page_height, pointsize=8, xpos=0, ypos=0, rescale="fixed")
				else windows(width=page_width, height=page_height, pointsize=8, xpos=0, ypos=0, rescale="fixed")
			} else if (Sys.info()[1] %in% c("Darwin", "DARWIN", "darwin", "AQUA", "aqua", "Aqua")) {
				if (orient=="LANDSCAPE") quartz(width=page_width, height=page_height, pointsize=8)
				else quartz(width=page_width, height=page_height, pointsize=8)
			} else {
				# attempt to default to X11
				if (orient=="LANDSCAPE") x11(width=page_width, height=page_height, pointsize=8)
				else x11(width=page_width, height=page_height, pointsize=8)
			}
		}
	}
	
	# support RStudio plot window (which I fucking hate, designing a plot in that 
	#  window is a recipe for disaster, and you can't even set plot dimensions)
	if (type=="RSGUI") {
		if (orient=="LANDSCAPE") windows(width=page_width, height=page_height, pointsize=8)
		else windows(width=page_width, height=page_height, pointsize=8)
	}

	if (type=="PDF") {
		outfile<-paste(filestem, '.pdf', sep='')
		if (orient=="LANDSCAPE") cairo_pdf(outfile, width=page_width, height=page_height, pointsize=8, family=font)
		else cairo_pdf(outfile, width=page_width, height=page_height, pointsize=8, family=font)
	}	
	if (type=="PDF_NONCAIRO") {
		outfile<-paste(filestem, '.pdf', sep='')
		if (orient=="LANDSCAPE") pdf(outfile, width=page_width, height=page_height, paper="a4r", pointsize=8, family=font)
		else pdf(outfile, width=page_width, height=page_height, paper="a4", pointsize=8, family=font)
	}

	if (type=="PDF_MULTIPAGE") {
		cat("Setting up multipage PDF file.\nUse type=\"PDF_multipage_cont\" in subsequent SetPage to start a new page.\n")
		outfile<-paste(filestem, '.pdf', sep='')
		if (orient=="LANDSCAPE") cairo_pdf(outfile, width=page_width, height=page_height, pointsize=8, onefile=TRUE)
		else cairo_pdf(outfile, width=page_width, height=page_height, pointsize=8, onefile=TRUE)
		# cat("PDF file set up, exiting SetPage.\nUse SetPage with type=\"PDF_multipage_cont\" to set up a layout\n")
		return(0)
	}
	
	if (type=="PDF_MULTIPAGE_CONT") {
	
	}

	if (type=="SVG") {
		outfile <- paste(filestem, ".svg", sep="")
		if (orient=="LANDSCAPE") svg(outfile, width=page_width, height=page_height, pointsize=8, family=font, bg="transparent")
		else svg(outfile, width=page_width, height=page_height, pointsize=8, family=font, bg="transparent") 
	}

	if (type=="PNG") {
		cat("PNG output is experimental\n")
		outfile <- paste(filestem, ".png", sep="")
		if (orient=="LANDSCAPE") png(outfile, width=page_width, height=page_height, units="in", res=576, pointsize=8, bg=png_bg, type="cairo-png", family=font)
		else png(outfile, width=page_width, height=page_height, units="in", res=576, pointsize=8, bg=png_bg, type="cairo-png", family=font) 
	}
	
	if (type=="TIFF") {
		cat("TIFF output is experimental\n")
		flush.console()
		outfile <- paste(filestem, ".tiff", sep="")
		if (is.null(page_dpi)) page_dpi <- 300
		if (is.null(page_pointsize)) page_pointsize <- 8
		if (orient=="LANDSCAPE") tiff(outfile, width=page_width, height=page_height, units="in", res=page_dpi, pointsize=page_pointsize, bg=png_bg, compression="lzw", type="windows", antialias = "cleartype")
		else tiff(outfile, width=page_width, height=page_height, units="in", res=page_dpi, pointsize=page_pointsize, bg=png_bg, compression="lzw", type="windows", antialias = "cleartype") 
	}
	
	if (type == "EPS") {
		cat("EPS output is experimental\n")
		flush.console()
		cairo_ps(file=paste0(filestem, ".eps"), width=page_width, height=page_height, onefile=FALSE)
	}
	
	if (type %in% c("JPG", "JPEG")) {
		cat("JP(E)G output is experimental\n")
		flush.console()
		jpeg(file=paste0(filestem, ".jpg"), width=page_width, height=page_height, units="in", res=300, quality=90, pointsize=10)
	}
	  
	# give a way to override the page layout 
	if (!advanced && is.null(vertical.scales)) {
		if (is.null(perpage)) {

			if (is.null(nlong) | is.null(nshort)) stop("If you leave perpage blank, then you need to provide BOTH of nlong and nshort")
			perpage <- nlong*nshort
		}
		else {
			if (perpage >= 10) perpage<-10
			longlist <-c(1, 2, 3, 2, 3, 3, 4, 4, 5, 5)
			nlong<-longlist[perpage]
			shortlist<-c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2)
			nshort<-shortlist[perpage]
		}
		# titlespace<- 0.1
		# footerspace<-0.1
		#    orient <- "PORTRAIT"

		#     Aspect ratio variation:
		#     Proceed by inserting empty rows and columns between plots of suitable width and height
		#     the empty rows will have width and height of 0 in the default case, which is fine
		#     However, this substantially complicates the computation for both the vectors of row and column sizes, 
		#        and for the matrix that states which cell contains which plot

		#     number of plots in horizontal and vertical...
	
		nhoriz <- nlong
		nvert <- nshort
		if (orient=="PORTRAIT")
		{
			nhoriz <- nshort
			nvert <- nlong
		}

		#     Horizontal space division: one gap at each end, 2 gaps run together between plots
		hwindow <- width / nhoriz
		hgap <- (1 - width)/(2*nhoriz)
		widthvec <- c(hgap, rep(c(hwindow, 2*hgap), nhoriz-1), hwindow, hgap)

		#     Vertical, remembering to leave footerspace at the bottom and titlespace at the top, otherwise like horizontal
		vspace <- 1 - titlespace - footerspace
		vwindow <- vspace*width/nvert
		vgap <- (1-height)*(vspace/(2*nvert))
		heightvec <- c(titlespace, vgap, rep(c(vwindow, 2*vgap), nvert-1), vwindow, vgap, footerspace)

		#     and finally the matrix, which I think is the hardest of all
		#     have (2*nhoriz)+1 columns, (2*nvert)+1 rows

		#     How many blank plot cells should there be at the end?
		nleft <- (nhoriz*nvert) - perpage
		#     List of numbers to go in the plotting cells
		plotvec <- c(seq(2, perpage+1), rep(0, nleft))

		#     No. of cells in the matrix, including the blanks this time
		ncellsh <- (2*nhoriz) + 1
		ncellsv <- (2*nvert) + 1

		#     First 2 rows:
		matvec <- c(rep(perpage+2, ncellsh), rep(0, ncellsh))

		plotstart <- 1
		#     Plot cells rows
		for (irow in seq(1, nvert))
		{
			ToUse<-plotvec[seq(plotstart, plotstart+nhoriz-1)]
			plotstart <- plotstart + nhoriz

			#         Interleave plot numbers with zeroes
			ThisRow <- rep(0, ncellsh)
			ThisRow[seq(2, ncellsh-1, by=2)]<-ToUse

			#         Add this and a row of zeroes to matvec

			matvec <- c(matvec, ThisRow, rep(0, ncellsh))
		}

		#     Add the footer bar
		matvec <- c(matvec, rep(1, ncellsh))
		mat <- matrix(matvec, ncol=ncellsh, byrow=TRUE)
		
		# if we want blank space on the page then adjust the layout 
		if (!is.null(blank.right.percent)) {
			if (verbose) {
				cat(paste("Leaving ",blank.right.percent, "% empty at the right\n",sep=""))
				flush.console()
			}
			n.widthvec <- length(widthvec)
			widthvec[n.widthvec] <- blank.right.percent/100
			widthvec[-n.widthvec] <- widthvec[-n.widthvec] * (1 - (blank.right.percent/100))
		}
		
		# if we want blank space on the page left then adjust layout
		if (!is.null(blank.left.percent)) {
			if (verbose) {
				cat(paste("Leaving ",blank.left.percent, "% empty at the left\n",sep=""))
				flush.console()
			}
			widthvec[1] <- blank.left.percent/100
			widthvec[-1] <- widthvec[-1] * (1 - (blank.left.percent/100))
		}
		
		if (!is.null(blank.bottom.percent)) {
			if (verbose) {
				cat(paste("Leaving ",blank.bottom.percent, "% empty at the bottom\n",sep=""))
				flush.console()
			}
			n.heightvec <- length(heightvec)
			heightvec[(n.heightvec - 1)] <- blank.bottom.percent/100
			headerfooter <- c((n.heightvec - 1),1,n.heightvec)
			# print(headerfooter)
			heightvec[-headerfooter] <- heightvec[-headerfooter] / sum(heightvec[-headerfooter]) * (1 - sum(heightvec[headerfooter]))
			
		
		}
		
		
	}	else if (!is.null(vertical.scales)) {
		
		if (is.null(ncols)) ncols <- 1
		scaling.params <- relative.scalings(vertical.scales, ncols=ncols)
		mat <- scaling.params$layout.mat
		heightvec <- scaling.params$heightvec
		widthvec <- scaling.params$widthvec
		print(mat)
		print(widthvec)
		print(heightvec)
		
		
	}	else {
	
		cat("Warning: using advanced settings.\n")
		if (is.null(mat) && is.null(widthvec) && is.null(heightvec)) stop("If using advanced settings: mat, widthvec, heightvec are ALL required")
		print(mat)
		print(widthvec)
		print(heightvec)
	
	}
	
	if (show.internal.layout) {
		cat("Layout matrix\n")
		print(mat)
		cat("Column width vector\n")
		print(widthvec)
		cat("Row height vector\n")
		print(heightvec)
	}




#     Finally define the layout
      layout(mat, widthvec, heightvec, respect=FALSE)

      # footer and label into the bottom left
      par(mar=c(.1,.1,.1,.1))
      plot(c(0, 1), c(0, 1), type='n', axes=F, xlab="", ylab="")

      # footer
	  if (!suppress.date) {
			user.footer <- footer
			# find out if we have a build number to add
			if ("Jasper" %in% loadedNamespaces()) buildnum <- packageDescription("Jasper")$Version
			else if (exists(".jasper.buildnum")) buildnum <- .jasper.buildnum
			else buildnum <- "2"
			if (ckb.participants & (!is.null(detectNpeople()))) {
				if (verbose) cat("Detected npeople.csv in the current directory, printing in footer.\n")
				footer <- c(paste(date(),". Filename: ", getwd(), "/", filestem, ".", tolower(type), sep=""), paste("Created with Jasper (\uf903\u65af\u73c0) ", buildnum, ". In ", read.csv(detectNpeople()), " participants.", footer[1], sep=""))
			} else {
				footer <- c(paste(date(),". Filename: ", getwd(), "/", filestem, ".", tolower(type), sep=""), paste("Created with Jasper (\uf903\u65af\u73c0) ", buildnum, " ", footer[1], sep=""))
			}
			
		}
			
		nfooter <- length(footer)
		if (nfooter>0)
		{
			cur.xpd <- par("xpd")
			par(xpd=NA)
			linedrop <- strheight('M\nM', cex=footer.cex) - (2*strheight("M", cex=footer.cex))
			# yat <- (1 + 0.5*linedrop) - linedrop*seq(1, nfooter)
			yat <- (1:nfooter - 1)*(linedrop+strheight("M",cex=footer.cex))+linedrop
			if (type %in% c("PDF", "SVG")) {
				text(0, yat, footer, cex=footer.cex, adj=0, family=ifelse(any((Sys.info()[1] %in% c("Darwin", "DARWIN", "darwin", "AQUA", "aqua", "Aqua"))),font,"Arial Unicode MS"))
			} else {
				text(0, yat, footer, cex=footer.cex, adj=0)
			}
			par(xpd=cur.xpd)
		}
		 
		if (!is.null(footer.title)) {
			cur.xpd <- par("xpd")
			par(xpd=NA)
			linedrop <- strheight("M\nM",cex=footer.title.cex) - (2*strheight('M', cex=footer.title.cex))
			nfooter.title <- length(footer.title)
			yat <- 1-(strheight("M",cex=footer.title.cex)+linedrop)*(1:nfooter.title -1)
			# print(yat)
			text(0, 1, footer.title[1], cex=footer.title.cex,adj=c(0,1), font=2)
			if (nfooter.title > 1) {
				text(0, yat[2:nfooter.title], footer.title[2:nfooter.title], cex=footer.title.cex, adj=c(0,1), font=ifelse(footer.title.allbold,2,1))
			}
			par(xpd=cur.xpd)
		}

      # plot label
      nlabel <- length(label)
      yat <- 0.6*max(strheight(label, cex=1))
      if (NoLabel!=TRUE) text(0, yat, label, cex=1, adj=0)
      
      par(mar=c(5, 4, 4, 2) + 0.1, ljoin="mitre")

      if (display.layout) layout.show(perpage+2)         
      }
