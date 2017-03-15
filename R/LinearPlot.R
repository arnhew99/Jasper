LinearPlot <- function(
						linelist, 
						reglines=NULL, 
						extendLines=FALSE,
						joinPoints=FALSE, 
						YLogScale=FALSE, 
						XLogScale=FALSE, 
						DataLogged=NULL, 
						ExponentiateDataOnPlot=NULL,
						LineLabels=NULL, 
						boxcols=NULL, 
						boxbordercol=NULL, 
						linecols=NULL, 
						linelwds=NULL, 
						lineltys=NULL, 
						new.plot=FALSE, 
						xlim=NULL, 
						ylim=NULL, 
						axes=TRUE,
						xaxis=TRUE,
						xaxis.line=1,
						xticks=NULL, 
						xticklabs=NULL,
						xticklabs.cex=1,
						xlab=NULL,
						yaxis=TRUE,
						yaxis.line=1,
						yticks=NULL, 
						yticklabs=NULL,
						ylab=NULL,
						yticklabs.cex=1,
						mainfont=1, 
						PointLabelsTop=NULL, 
						PointLabelsBottom=NULL,
						PointLabelsPosAdj=NULL, 
						PointLabelsCol=NULL, 
						PointLabelsCEX=1, 
						useArrows=TRUE,
						boxparmx=NULL, 
						boxparmy=NULL, 
						boxparm.stderr=NULL, 
						boxsizeoverride=FALSE, 
						margins=NULL, 
						CIs=TRUE,
						CILineCol=NULL,
						PointCIs=NULL,
						CIsOnTop=FALSE,
						rBox=FALSE) {
  
	# set up the plotting window, and for the moment, don't want to draw outside the region
	current.xpd <- par("xpd")
	par(xpd=NA)
	
	# if linelist is a data frame, then assume that the user hasn't bothered to create a list, so do that for them
	if (is.data.frame(linelist)) {
	
		# if there is a GROUP column then use that to split the data up
		if ("GROUP" %in% toupper(names(linelist))) {
			gp.col <- which(toupper(names(linelist)) == "GROUP")
			gp.unique <- unique(linelist[,gp.col])
			linelist <- lapply(gp.unique, function(z) linelist[linelist[,gp.col] == z,])
		} else {
			linelist <- list(linelist)
		}
	}
	
	
	# sort out the DataLogged issue
	if (is.null(ExponentiateDataOnPlot) & is.null(DataLogged)) DataLogged <- FALSE
	if (!is.null(ExponentiateDataOnPlot) & is.null(DataLogged)) DataLogged <- ExponentiateDataOnPlot
	
	
	
	
	# detect if there is a params element in linelist - if so remove it and store elsewhere
	if ("params" %in% names(linelist)) {
	
		params <- linelist[which(names(linelist)=="params")]$params
		linelist <- linelist[-which(names(linelist)=="params")]
		
		# parse any available elements of params
		for (i in names(params)) {
			assign(i, unlist(params[i]))
		}
		
	}
	
	
	# change RF_Level to meanrisk if it's been given as such
	# change Estimate and StdErr also
	for (i in 1:length(linelist)) {
	
		if (is.null(linelist[[i]]$meanrisk)) {
			# names(linelist[[i]])[which(names(linelist[[i]])=="RF_Level")] <- "meanrisk"
			linelist[[i]]$meanrisk <- linelist[[i]]$RF_Level
			
		}
		
		if (is.null(linelist[[i]]$estimate)) {
			# names(linelist[[i]])[which(names(linelist[[i]])=="Estimate")] <- "estimate"
			linelist[[i]]$estimate <- linelist[[i]]$Estimate
		}
		
		if (all(is.null(linelist[[i]]$stderr), !is.null(linelist[[i]]$StdErr))) {
		
			linelist[[i]]$stderr <- linelist[[i]]$StdErr
		}
		
		if (all(is.null(linelist[[i]]$stderr), !is.null(linelist[[i]]$LCI), !is.null(linelist[[i]]$UCI))) {
		
			linelist[[i]]$stderr <- (linelist[[i]]$UCI - linelist[[i]]$LCI)/4
		}
	}
	
	# if PointLabelsBottom or PointLabelsTop are "" then set them to NULL
	if (!is.null(PointLabelsBottom)) {
		if (PointLabelsBottom == "") PointLabelsBottom <- NULL
	}
	if (!is.null(PointLabelsTop)) {
		if (PointLabelsTop == "") PointLabelsTop <- NULL
	}
	if (is.null(PointLabelsCol)) PointLabelsCol <- 1
	
	
	# old version of Jasper supported "PointCIs"
	if (!is.null(PointCIs)) CIs <- PointCIs
	
	# CIs can be 
	#  1) TRUE / FALSE : all CIs off or all CIs on
	#  2) vector of TRUE / FALSE : CIs for element k of the linelist are all on or all off
	#  3) list of vectors : CIs for individual points are all off or on
	# need to expand the top two to be like the last
	if (is.logical(CIs) & length(CIs) == 1) {
		# expand a single TRUE/FALSE to be like 2)
		CIs <- rep(CIs, length(linelist))
		noCIs <- all(CIs)
	}
	if (is.logical(CIs)) {
		# now expand type 2) to be like type 3)
		CIs <- lapply(1:length(linelist), function(k) rep(CIs[k], dim(linelist[[k]])[1]))
	}
	# CIs should now be a list in every case ...?
	
	# only create a new window if one has been requested (this matches the behaviour of classPlot and other ONYX functions)
	if (new.plot) {

		# if the user hasn't provided xlim or ylim, then guess them from the data
		if (is.null(xlim)) {
			max.x <- max(sapply(linelist, function(z) return(max(z$meanrisk))))
			min.x <- min(sapply(linelist, function(z) return(min(z$meanrisk))))
			cat("xlim not specified (making a guess)\n")
			xlim <- c(min.x, max.x)
			# if (DataLogged) xlim <- exp(xlim)
			if (XLogScale) xlim <- log(xlim)
			print(xlim)
			flush.console()
		} else {
			if (XLogScale) xlim <- log(xlim)
		}
		
		if (is.null(ylim)) {
			max.y <- max(sapply(linelist, function(z) return(max(z$estimate + 2*z$stderr))))
			min.y <- min(sapply(linelist, function(z) return(min(z$estimate - 2*z$stderr))))
			cat("ylim not specified (making a guess)\n")
			ylim <- c(min.y, max.y)
			if (DataLogged) ylim <- exp(ylim)
			if (YLogScale) ylim <- log(ylim)
			print(ylim)
			flush.console()
		} else {
			if (YLogScale) ylim <- log(ylim)
		}
		
		
		# if the user hasn't provided xticks or yticks then guess them 
		if (is.null(xticks) & xaxis) {
			
			if (XLogScale) {
				xticks <- pretty(exp(xlim))
				xticks <- xticks[xticks > 1e-05]
				xlim[1] <- min(log(xticks))
				xlim[2] <- max(log(xticks))
			} else {
				xticks <- pretty(xlim)
				xlim[1] <- min(xticks)
				xlim[2] <- max(xticks)
			}
			
			cat("xticks not specified (making a guess)\n")
			print(xticks)
			flush.console()
		}
		
		if (is.null(yticks) & yaxis) {
			
			if (YLogScale) {
				yticks <- pretty(exp(ylim))
				yticks <- yticks[yticks > 1e-05]
				ylim[1] <- min(log(yticks))
				ylim[2] <- max(log(yticks))
			} else {
				yticks <- pretty(ylim)
				ylim[1] <- min(yticks)
				ylim[2] <- max(yticks)
			}
		
			cat("yticks not specified (making a guess)\n")
			print(yticks)
			flush.console()
		}
		
		# if the user hasn't given any xticklabs or yticklabs then use the values provided for xticks and yticks
		if (is.null(xticklabs)) {
			xticklabs <- xticks
		}
		if (is.null(yticklabs)) {
			yticklabs <- yticks
		}
	
		if (!is.null(margins)) par(mar=margins) else par(mar=c(7,7,6,6))
		par(xaxs='i', yaxs='i', cex=mainfont)
		plot(x=c(0, 100), y=c(0, 100), xlim=xlim, ylim=ylim, axes=FALSE, type="n", xlab="", ylab="", xaxs="i", yaxs="i")
	}
	# need line cols at some point
	if (is.null(linecols)) linecols <- rep("black", length(linelist))
	if (length(linecols)==1) linecols <- rep(linecols, length(linelist))
	# add the "regression" lines, if they exist
	if (!is.null(reglines)) {
		if (is.null(linecols)) linecols <- rep("black", dim(reglines)[1])
		if (is.null(linelwds)) linelwds <- rep(1, dim(reglines)[1])
		if (is.null(lineltys)) lineltys <- rep(1, dim(reglines)[1])
		if (length(linecols)==1) linecols <- rep(linecols, dim(reglines)[1])
		if (length(linelwds)==1) linelwds <- rep(linelwds, dim(reglines)[1])
		if (length(lineltys)==1) lineltys <- rep(lineltys, dim(reglines)[1])
		
		for (i in 1:(dim(reglines)[1])) {
		
			if (extendLines) {
				AddLine(reglines[i,1], reglines[i,2], col=linecols[i],lwd=linelwds[i], lty=lineltys[i])
			} else {
				trimRight = dim(linelist[[i]])[1]
				if (DataLogged) {
					b <- exp(reglines[i,1])
					a <- exp(reglines[i,1] + reglines[i,2]) - b
				} else {
					b <- reglines[i,1]
					a <- reglines[i,2]
				}
				AddLine(b, a, trimLeft = 1, trimRight = trimRight, linelist=linelist[i], col=linecols[i],lwd=linelwds[i], lty=lineltys[i])
			}
		
		}
	}
	
	
	# add bootleft, bootright and faintness if they are missing, via a function
	linelist.struct <- function(list.element) {
		
		n <- dim(list.element)[1]
		if (is.null(list.element$BootLeft)) list.element$BootLeft <- rep(FALSE,n)
		if (is.null(list.element$BootRight)) list.element$BootRight <- rep(FALSE,n)
		if (is.null(list.element$Faintness)) list.element$Faintness <- rep(FALSE,n)
		if (is.null(list.element$nevents)) list.element$nevents <- rep(1,n)
				
		return(list.element)
	
	}
	linelist <- lapply(linelist, linelist.struct)
	
	
	# compute a reasonable size of box, if the user hasn't provided one
	if (is.null(boxparmx)) {
		if (is.null(boxparm.stderr)) {
			MinStderr <- min(sapply(linelist, function(x) min(x$stderr)))
			boxparms <- CalcBoxParm(MinStderr)
			boxparmx <- boxparms[1]
			#  boxparmy is chosen to make the boxes square on the paper
			boxparmy <- boxparms[2]
		} else {
			boxparms <- CalcBoxParm(boxparm.stderr)
			boxparmx <- boxparms[1]
			boxparmy <- boxparms[2]
		}
	
	}
	
	# print(c(boxparmx, boxparmy))
	
	# do the actual drawing
	if (is.null(boxcols)) boxcols <- rep(1, length(linelist))
	if (is.null(boxbordercol)) boxbordercol <- rep(1, length(linelist))
	if (is.null(linelwds)) linelwds <- rep(1, length(linelist))
	
	
	
	# need to update CILineCol 
	if (!is.null(CILineCol)) {
		# if there's only one colour given then expand to a list of the correct length
		if (all(length(CILineCol) == 1, !is.list(CILineCol))) {
			print("here")
			CILineCol <- lapply(linelist, function(z) rep(CILineCol, length=dim(z)[1]))
		}
		
		# if there is a vector the length of the linelist then expand this
		else if (all(length(CILineCol) == length(linelist), !is.list(CILineCol))) {
			CILineCol <- lapply(1:length(linelist), function(i) rep(CILineCol[i], dim(linelist[[i]])[1]))
		}
		
		# else stop if CILineCol is a list but the elements are not the right length
		else if (is.list(CILineCol)) {
			npoints <- sapply(linelist, function(z) return(dim(z)[1]))
			nlinecol <- sapply(CILineCol, function(z) return(length(z)))
			if (!all(npoints == nlinecol)) stop("CILineCol doesn't have the correct number of elements")
		}
	}
	
	
	
	# going to do the plotting differently with some lovely vectorisation
	draw.CIlines <- function(x, y1, y2, linecol, linelwd=1) {
		
		
		if (is.na(x)) return(NULL)
		if (is.na(y1)) return(NULL)
		if (is.na(y2)) return(NULL)
		
		
		if (useArrows) {
			par(xpd=FALSE)
			if (CurrentPlotLims()$ylim[[2]]< y2){ 
				arrows(x, CurrentPlotLims()$ylim[[2]],y1=CurrentPlotLims()$ylim[[2]]-strheight("A"), code=1, col=linecol,lwd=linelwd,lend=2,length=0.1)
			}
			if (CurrentPlotLims()$ylim[[1]]> y1){ 
				arrows(x, CurrentPlotLims()$ylim[[1]],y1=CurrentPlotLims()$ylim[[1]]+strheight("A"), code=1, col=linecol,lwd=linelwd,lend=2,length=0.1)
			}
		}

		lines(c(x,x), c(y1,y2), col=linecol, lwd=linelwd, lend=2)
		par(xpd=NA)
	}
	Square <- function(x1, x2, y1, y2) rect(x1, y1, x2, y2, col=1, lwd=1, border=1, ljoin=1)
	
	if (is.null(PointLabelsPosAdj)) {
		PointLabelsPosAdj <- ifelse(YLogScale, log(2.5), 2.5)
	} 
	# else {
		# PointLabelsPosAdj <- ifelse(YLogScale, log(PointLabelsPosAdj), PointLabelsPosAdj)
	# }
	# we'll do point labels in here too if required
	format.numeric.label <- function(z) {
		
		log10z.rd <- ifelse(log10(z) < 0, ceiling(log10(z)), floor(log10(z)))
		
		return(ifelse(log10(z) >= 2, sprintf("%.0f",z) , sprintf(paste("%.", ifelse((2-log10z.rd) > 3, 2, 2-log10z.rd), "f", sep=""), z)))
	}
	
	for (k in 1:length(linelist))	{
	
		# CBA to change i to linelist[[k]]
		i <- linelist[[k]]
		


		# compute the CI coordinates
		if (DataLogged) {
			# if (XLogScale) trans.x <- function(x) x else trans.x <- exp 
			trans.x <- function(x) x
			if (YLogScale) trans.y <- function(y) y else trans.y <- exp 
		} else {
			trans.x <- function(x) x
			if (YLogScale) trans.y <- log else trans.y <- function(y) y
		}

		# print(par("usr"))
		x.coord <- trans.x(i$meanrisk)
		# if CIs are given then use these instead
		if (all(!is.null(i$LCI), !is.null(i$UCI))) {
			y1.coord <- trans.y(i$LCI)
			y2.coord <- trans.y(i$UCI)
		} else {
			y1.coord <- trans.y(i$estimate - 2*i$stderr)
			y2.coord <- trans.y(i$estimate + 2*i$stderr)
		}
		
		if (boxsizeoverride) i$stderr <- 1
		# compute the boxlimits, using boxparm to guide the size
		boxlefts <- trans.x(i$meanrisk) - (0.5*boxparmx/i$stderr)
		boxrights <- trans.x(i$meanrisk) + (0.5*boxparmx/i$stderr)
		boxtops <- trans.y(i$estimate) + (0.5*boxparmy/i$stderr)
		boxbottoms <- trans.y(i$estimate) - (0.5*boxparmy/i$stderr)
	
		if (all(YLogScale, !DataLogged)) {
			
				# here, evaluate the values then take the log
				# replace the box boundaries and the CI y-limits
				boxtops <- log(i$estimate) + (0.5*boxparmy/i$stderr)
				boxbottoms <- log(i$estimate) - (0.5*boxparmy/i$stderr)
				# y1.coord <- log(y1.coord)
				# y2.coord <- log(y2.coord)

		}		
		if (all(XLogScale, !DataLogged)) {
	
			
				# here, evaluate the values then take the log
				# replace the box boundaries and the CI x-limits
				boxlefts <- log(i$meanrisk) - (0.5*boxparmx/i$stderr)
				boxrights <-  log(i$meanrisk) + (0.5*boxparmx/i$stderr)
				x.coord <- log(x.coord)
				
		}
		
	
	
		# here, we are on a linear scale so just assume the user knows what they are doing
		# because we haven't mucked about changing the coordinate system to the log scale
		# switch between black and white lines if the line is inside the box
		docis <- function() {
			if (is.null(CILineCol)) {
				ci.linecol <- ifelse(boxbottoms > y1.coord, linecols[k], ifelse(boxcols[k] == "white", "black", ifelse(boxcols[k] == 0, "black", "white")))
			} else {
				ci.linecol = CILineCol[[k]]
			}
			if (all(CIs[[k]])) {
				mapply(draw.CIlines, x=x.coord, y1=y1.coord, y2=y2.coord, linecol=ci.linecol, linelwd=linelwds[k])
			} else {

				for (ind in 1:length(CIs[[k]])) {
					if ((CIs[[k]])[ind]) draw.CIlines(x.coord[ind], y1.coord[ind], y2.coord[ind], linecol=ci.linecol[ind], linelwd=linelwds[k])
				}
			}
		}
		if (!CIsOnTop) docis()

		
		# if there are only 1 set of lines then we could interpret boxcols as a vector giving colours.
		if (all(length(linelist)==1, length(boxcols)==length(boxlefts))) {
			rect(boxlefts, boxbottoms, boxrights, boxtops, col=boxcols, lwd=linelwds[k], border=boxbordercol[k])
		} else if (length(boxcols) == sum(sapply(linelist, function(z) return(dim(z)[1])))) {
			# assume that the boxcols refer to each point individually
			linelist_dim <- c(0,cumsum(sapply(linelist, function(z) return(dim(z)[1]))))
			cboxcols <- boxcols[(linelist_dim[k]+1):linelist_dim[k+1]]
			rect(boxlefts, boxbottoms, boxrights, boxtops, col=cboxcols, lwd=linelwds[k], border=boxbordercol[k])
		} else {
			rect(boxlefts, boxbottoms, boxrights, boxtops, col=boxcols[k], lwd=linelwds[k], border=boxbordercol[k])
		}
		
		if (CIsOnTop) docis()
		
		

		
		# do the point labelling whilst we're looping over the linelist
		if (!is.null(PointLabelsTop)) {
			PLT <- i[,PointLabelsTop]
			# work out how to format these 
			if (is.numeric(PLT)) {
			
				# detect if all the numbers are integers, as this probably means 
				# that they represent counts...
				if (all(PLT == as.integer(PLT))) {
					PLT <- sprintf("%.0f", PLT)
				} else {
					PLT <- format.numeric.label(PLT)
				}
			}
			# if (noCIs) {
				# text(x=i$meanrisk, y=mapply(max, boxtops, y2.coord) + PointLabelsPosAdj*max(i$stderr), labels=PLT, adj=c(0.5,0), col=PointLabelsCol, cex=PointLabelsCEX)
			# } else {
				# if (YLogScale) {
					# text(x=i$meanrisk, y=mapply(max, boxtops) + PointLabelsPosAdj, labels=PLT, adj=c(0.5,0), col=PointLabelsCol, cex=PointLabelsCEX)
				# } else {
					# text(x=i$meanrisk, y=mapply(max, boxtops, y2.coord) + PointLabelsPosAdj, labels=PLT, adj=c(0.5,0), col=PointLabelsCol, cex=PointLabelsCEX)
				# }
			# }			
			if (noCIs) {
				plt.ypos <- mapply(max, boxtops, y2.coord) + PointLabelsPosAdj*max(i$stderr)
			} else {
				if (YLogScale) {
					plt.ypos <- mapply(max, boxtops) + PointLabelsPosAdj
				} else {
					plt.ypos <- mapply(max, boxtops, y2.coord) + PointLabelsPosAdj
				}
			}
			text(x=i$meanrisk, y=plt.ypos, labels=PLT, adj=c(0.5,0), col=PointLabelsCol, cex=PointLabelsCEX)
		}
		if (!is.null(PointLabelsBottom)) {
				PLB <- i[,PointLabelsBottom]
			if (is.numeric(PLB)) {
				if (all(PLB == as.integer(PLB))) {
					PLB <- sprintf("%.0f", PLB)
				} else {
					PLB <- format.numeric.label(PLB)
				}
			}
			# if (noCIs) {
				# text(x=i$meanrisk, y=mapply(min, boxbottoms, y1.coord) - PointLabelsPosAdj*max(i$stderr), labels=PLB, adj=c(0.5,1), col=PointLabelsCol, cex=PointLabelsCEX)
			# } else {
				# if (YLogScale) {
					# text(x=i$meanrisk, y=mapply(min, boxbottoms) - PointLabelsPosAdj, labels=PLB, adj=c(0.5,1), col=PointLabelsCol, cex=PointLabelsCEX)
				# } else {
					# text(x=i$meanrisk, y=mapply(min, boxbottoms, y1.coord) - PointLabelsPosAdj, labels=PLB, adj=c(0.5,1), col=PointLabelsCol, cex=PointLabelsCEX)
				# }
			# }			
			if (noCIs) {
				plb.ypos <- mapply(min, boxbottoms, y1.coord) - PointLabelsPosAdj*max(i$stderr)
			} else {
				if (YLogScale) {
					plb.ypos <- mapply(min, boxbottoms) - PointLabelsPosAdj
				} else {
					plb.ypos <- mapply(min, boxbottoms, y1.coord) - PointLabelsPosAdj
				}
			}
			text(x=i$meanrisk, y=plb.ypos, labels=PLB, adj=c(0.5,1), col=PointLabelsCol, cex=PointLabelsCEX)
			
		}
			
	
		if (joinPoints) {
			# here join the points by lines if asked to
			points(i$meanrisk, trans.y(i$estimate), type="l", col=linecols[k], lty=lineltys[k], lend=2)
		}
		
		
	}
	
	
	# if user wants us to make the plotting area, then put in some axes too
	if (axes & new.plot) {
		if (xaxis) {
			if (XLogScale) {
				Xaxis(ticklabs=xticklabs, xticks=log(xticks), mainfont=xticklabs.cex, lwd=linelwds[1], allX=ifelse(exists("xaxis.all"), xaxis.all, TRUE), line=xaxis.line)
			} else {
				Xaxis(ticklabs=xticklabs, xticks=xticks, mainfont=xticklabs.cex, lwd=linelwds[1], allX=ifelse(exists("xaxis.all"), xaxis.all, TRUE), line=xaxis.line)
			}
		}
		if (yaxis) {
			if (YLogScale) {
				Yaxis(LinScale=TRUE, yticks=log(yticks), ticklabs=yticklabs, mainfont=yticklabs.cex, lwd=linelwds[1], allY=ifelse(exists("yaxis.all"), yaxis.all, TRUE), line=yaxis.line)
			} else {
				Yaxis(LinScale=TRUE, ticklabs=yticklabs, yticks=yticks, mainfont=yticklabs.cex, lwd=linelwds[1], allY=ifelse(exists("yaxis.all"), yaxis.all, TRUE), line=yaxis.line)
			}
		}
	}
		
	
	# turns out I'd rather do my own linelabels...
	if (!is.null(reglines) & !is.null(LineLabels)) {
	
		nlabels <- length(LineLabels)
		xmax <- par("usr")[2]
		ycoords <- trans.y(reglines[,1] + xmax*reglines[,2])
		text(x=rep(xmax,nlabels), y=ycoords, labels=LineLabels, pos=4, cex=0.8*mainfont)
	
	} else if (!is.null(LineLabels)) {
	
		nlabels <- length(LineLabels)
		xmax <- par("usr")[2]
		ycoords <- sapply(linelist,function(z) trans.y(z$estimate[which.max(z$meanrisk)]))
		text(x=rep(xmax,nlabels), y=, labels=LineLabels, pos=4, cex=0.8*mainfont)
	
	
	}
	
	if (!is.null(xlab)) AxisLabels(xlab=xlab, ylab="", xline=ifelse(exists("xlab.line"), xlab.line, 2.5+xaxis.line), mainfont=1)
	if (!is.null(ylab)) AxisLabels(xlab="", ylab=ylab, yline=ifelse(exists("ylab.line"), ylab.line, 3+yaxis.line), mainfont=1)
	
}