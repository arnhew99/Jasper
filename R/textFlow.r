textFlow <- function(x, y, labels, adj=c(0,1), cex=1, available_width=NULL, ignore.linebreaks=TRUE) {

	## Matt Arnold March 2016
	## 
	## 
	## 
	## Function arguments
	## 
	## x, y				Coordinates of top left corner of "text box" in the current
	##					underlying coordinate system (each numeric, length 1)
	## labels			String to be formatted and printed (character, length 1)
	## adj				Ignored...
	## cex				Character expansion factor (numeric, length 1)
	## available_width	Override the detection of the amount of space available and 
	##					force the "text box" to be a certain width (left from x)
	
	## This function probably works best with par(xpd=NA) if you want to place a 
	## label/title around a plot
	
	## adj can only be one of 0 (left aligned) or 1 (right aligned) 
	if (!(adj[1] %in% c(0,0.5,1))) stop("first element of adj not 0, 0.5 or 1")
	
	if (length(adj) == 1) vadj <- 1  # by default hang the text block off a line
	else if (length(adj) == 2) vadj <- adj[2]
	if (!(vadj %in% c(0,0.5,1))) stop("second element of adj not 0, 0.5 or 1")
	
	# check to see whether we were actually given anything to write
	if (length(labels)==0) {
		return(NULL)
	}

	lims <- list(xlim=par("usr")[1:2], ylim=par("usr")[3:4])
	
	
	
	## allow for available_width calculation to be overridden 
	if (is.null(available_width)) {
		## work out how much space we've got
		available_width <- diff(lims$xlim)
		if (adj[1] == 0) {
			available_width <- available_width - (x - lims$xlim[1])
		} else if (adj[1] == 1) {
			available_width <- available_width - (lims$xlim[2] - x)
		} 
	} 
	
	
	## remove any line breaks
	if (ignore.linebreaks) {
		labels <- paste(strsplit(labels, "\n", fixed=TRUE)[[1]], collapse="")
	}
	
	## split up the string into words
	labels_split <- strsplit(labels, " ", fixed=TRUE)[[1]]
	
	
	## work out how wide the string is... if it's too long, break the line
	current_stringwidth <- 0
	current_line <- 0
	bold <- FALSE
	italic <- FALSE
	
	# store the Xs and Ys in a list
	positions <- rep(list(list(x=c(), y=c(), label=c())), 4)
	
	# for (i in 1:length(labels_split)) {
	for (ilabel in labels_split) {
	
		if (findMark(ilabel, "#b#")$mark) {
			bold <- !bold
			ilabel <- findMark(ilabel, "#b#")$string
		}
		if (findMark(ilabel, "#i#")$mark) {
			italic <- !italic
			ilabel <- findMark(ilabel, "#i#")$string
		}
		
		fontstyle <- 1
		if (all(bold, italic)) fontstyle <- 4
		else if (bold) fontstyle <- 2
		else if (italic) fontstyle <- 3
		
		## find out if there is a forced line break ahead of the 
		## current word, as long as we're detecting linebreaks
		forced_linebreak <- FALSE
		nbreaks <- 0
		if (!ignore.linebreaks) {
			if (findMark(ilabel, "\n")$mark) {
				forced_linebreak <- TRUE
				foundMark <- findMark(ilabel, "\n")
				ilabel <- foundMark$string
				nbreaks <- foundMark$mark.count
			}
		}
		
		## annoyingly it looks like we're going to have to place each individual word
		test_stringwidth <- current_stringwidth + strwidth(paste0(" ", ilabel), cex=cex, font=fontstyle)
		if (test_stringwidth > available_width) {
			forced_linebreak <- TRUE
			nbreaks <- 1
		}
		linebreak <- FALSE
		if (any(forced_linebreak)) {
			current_line <- current_line + nbreaks*(strheight("A\nA", cex=cex) - strheight("A", cex=cex))
			current_stringwidth <- 0
			linebreak <- TRUE
		} 
		
		
		# update the position vectors
		positions[[fontstyle]]$x <- c(positions[[fontstyle]]$x, x+current_stringwidth)
		positions[[fontstyle]]$y <- c(positions[[fontstyle]]$y, y-current_line)
		positions[[fontstyle]]$label <- c(positions[[fontstyle]]$label, ilabel)
		
		
		if (linebreak) {
			current_stringwidth <- 0 + strwidth(paste0(ilabel, " "), cex=cex, font=fontstyle)
		} else {
			current_stringwidth <- test_stringwidth
		}
	}
	
	# we need the y positions both to adjust the horizontal alignment
	# and to correct the vertical alignment
	y_positions <- unique(unlist(sapply(positions, function(z) z$y)))
	if (adj[1] != 0) {
	
		# if we're right-aligned we need to recompute the current line
		# positions relative to the left end of the line
		for (j in y_positions) {
			# find the max x position
			# need to keep track of the label and fontstyle associated
			# with the position of the max x, so we can add the labels
			# width to the overall width.
			
			# this is pretty horrible code
			max_x <- c()
			max_label <- c()
			max_type <- c()
			
			# loop over the different font types tring to find
			# where the current line ends...
			for (k in 1:4) {
				if (is.null(positions[[k]]$x)) next
				cx <- positions[[k]]$x[positions[[k]]$y == j]
				if (length(cx) > 0) {
					max_x <- c(max_x, max(cx))
					max_label <- c(max_label, (positions[[k]]$label[positions[[k]]$y == j])[which.max(cx)])
					max_type <- c(max_type, k)
				}
			}
			max_label <- max_label[which.max(max_x)]
			max_type <- max_type[which.max(max_x)]
			max_x <- max(max_x)
			
			max_x <- max_x + strwidth(max_label, font=max_type, cex=cex)
			
			
			for (k in 1:4) {
				# update all x positions with the correction
				positions[[k]]$x[positions[[k]]$y == j] <- positions[[k]]$x[positions[[k]]$y == j] - ifelse(adj[1] == 1, max_x - x, 0.5*(max_x - x))
			}
		}
	}
	
	# correct the vertical alignment:
	# 0 - text sits above a line
	# 0.5 - text distributed evenly above and below a line
	# 1 - text hangs below a line
	if (vadj == 0) {
		correction <- y - min(y_positions)
	} else if (vadj == 0.5) {
		correction <- (y - strheight("A", cex=cex) - min(y_positions))/2
	} else {
		correction <- -strheight("A")
	}
	for (k in 1:4) {
		positions[[k]]$y <- positions[[k]]$y + correction
	}
	
	
	# actually get around to writing the text (!)
	for (i in 1:4) {
		if (is.null(positions[[i]]$x)) next
		text(x=positions[[i]]$x, y=positions[[i]]$y, labels=positions[[i]]$label, cex=cex, adj=c(0,0), font=i)
	}
	
}


findMark <- function(string, mark) {
	# only works on individual words...
	if (string == "") return(list(mark=FALSE, string=""))
	split_string <- strsplit(string, mark, fixed=TRUE)[[1]]
	if (length(split_string) > 1) {
		newstr <- paste0(split_string[which(split_string != "")], collapse="")
		return(list(mark=TRUE, string=newstr, mark.count=sum(split_string=="")))
	}
	else if (split_string[1] == "") return(list(mark=TRUE, string=split_string[1], mark.count=1)) 
	else return(list(mark=FALSE, string=string[1], mark.count=0))
}
