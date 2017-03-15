## CSV file format should be
# panel
# linegroup
# rf_level
# estimate
# stderr

# if we're given lci and uci columns instead, infer stderr
LinearPlotFromCSV <- function(
	file,
	type="GUI",
	orient="PORTRAIT",
	perpage=NULL,
	filestem,
	cex=1,
	titlespace=0.1,
	footerspace=0.1,
	wls.lines=FALSE,
	join.points=FALSE,
	XLogScale=FALSE,
	XExponentiate=FALSE,
	YLogScale=FALSE,
	YExponentiate=FALSE,
	xlim=NULL,
	ylim=NULL,
	xticks=NULL,
	yticks=NULL,
	show.xticklabs=TRUE,
	show.yticklabs=TRUE,
	xlabs=NULL,
	ylabs=NULL,
	point.labels.top.column=NULL,
	point.labels.bottom.column=NULL,
	point.labels.cex=1,
	boxparm.stderr=1,
	boxsizeoverride=FALSE,
	mar=c(7,7,3,3),
	mar.optimise=FALSE,
	mar.spacing.horiz.target=3,
	mar.force.left=NULL,
	mar.force.right=NULL,
	additionalR=NULL) 
{


	panels <- read.csv(file, as.is=TRUE)

	names(panels) <- toupper(names(panels))

	# count the number of panels
	panels.max <- max(panels$PANEL)

	# check that the read in data file appears to be valid
	if (any(panels$STDERR <= 0)) stop("All STDERR values must be > 0")

	# check to see if the necessary columns are here
	for (column in c("PANEL", "GROUP", "ESTIMATE")) {
		tryCatch(panels[,column], error=function(e) stop(paste0(column, "seems to be missing (check spelling)")))
	}
	
	# check the status of the STDERR or LCI or UCI columns
	se.missing <- sapply(c("STDERR", "LCI", "UCI"), function(z) tryCatch(if (!is.null(panels[,z])) return(FALSE), error=function(e) return(TRUE)))
	if (all(c(se.missing[1], any(se.missing[2:3])))) stop(paste0("STDERR column missing but either LCI or UCI seem to be missing (check spelling)"))

	# check to see that all the columns are the same length
	if (se.missing[1]) {
		columns <- c("PANEL", "GROUP", "ESTIMATE", "LCI", "UCI")
	} else {
		columns <- c("PANEL", "GROUP", "ESTIMATE", "STDERR")
	}
	col.lengths <- sapply(columns, function(z) as.integer(length(panels[,z])))
	if (sum(diff(col.lengths)) != 0L) stop("Unequal column lengths")
	
	
	# check to see that no columns have NAs
	col.na <- sapply(columns, function(z) as.integer(sum(is.na(panels[,z]))))
	if (sum(col.na) > 0) {
		# work out which column has at least one missing entry
		col.miss <- columns[which(col.na > 0)[1]]
		stop(paste0(col.miss, " appears to have a missing entry"))
	}
	
	

	# expand xlabs, ylabs, xlim, ylim, xticks, yticks if they aren't the right length
	LPargs <- c("xlabs", "ylabs", "xlim", "ylim", "xticks", "yticks", "show.xticklabs", "show.yticklabs", "XLogScale", "YLogScale", "XExponentiate", "YExponentiate")
	LPargs.expand.list <- c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
	
	for (i in 1:length(LPargs)) {
		if (exists(LPargs[i])) {
			assign(LPargs[i], value=verifyLPArgs(arg=get(LPargs[i]), argname=LPargs[i], panels.max=panels.max, expand.list=LPargs.expand.list[i]))
		}
	}
	
	
	
	
	command <- ""
	
	
	
	# set the current working directory
	command <- paste0(command, "# Set working directory\nsetwd(", deparse(getwd()), ")\n")
	
	# need to load Jasper, working out if the package was loaded or if we are
	# running directly off the network
	if ("Jasper" %in% loadedNamespaces()) {
		command <- paste0(command, "# Load Jasper\nrequire(Jasper)\n")
	} else {
		command <- paste0(command, "# Load Jasper\nsource(", deparse(.jasper.load),")\n\n\n")
	}
	


	# load the data in the R script 
	command <- paste0(command, "# Load the data\npanels <- read.csv(file=\"", file, "\", as.is=TRUE)\nnames(panels) <- toupper(names(panels))\n")

	if (se.missing[1]) {
		cat(paste0("No STDERR column, but found LCI and UCI\n"))
		panels$STDERR <- (panels$UCI - panels$LCI)/4
		command <- paste0(command, "panels$STDERR <- (panels$UCI - panels$LCI)/4\n\n")
	} else {
		command <- paste0(command, "\n")
	}

	# set up the page
	command <- paste0(command, "# Set up the Jasper page\ntype <- \"", type, "\"\n")
	command <- paste0(command, "SetPage(type=type,\n\t\tfilestem=\"", filestem, "\",\n\t\torient=\"", orient, "\",\n\t\t",
								"titlespace=", titlespace, ",\n\t\t",
								"footerspace=", footerspace, ",\n\t\t")


	# calculate the panel layout if unspecified
	if (is.null(perpage)) {
		nshort <- max(which(panels.max %% (1:floor(sqrt(panels.max))) == 0))
		nlong <- panels.max / nshort
		# if panels.max is prime then this is a problem
		if (all(nshort == 1, panels.max > 4)) {
			# recalculate by adding a dummy panel in 
			nshort <- max(which((panels.max+1) %% (1:floor(sqrt((panels.max+1)))) == 0))
			nlong <- (panels.max+1) / nshort
		}		
		command <- paste0(command, "nshort=", nshort, ",\n\t\tnlong=", nlong, ")\n")
	} else {
		command <- paste0(command, "perpage=", perpage, ")\n")
	}
	
	# attempt to calculate "optimal" margins
	if (all(mar.optimise, is.null(perpage), panels.max > 1)) {
		cat("Margin optimisation is experimental. Attempting optimisation...")
		newmars <- optimiseInternalGap(target=mar.spacing.horiz.target, 
										mar=mar,
										left_space_mar=mar.force.left, 
										right_space_mar=mar.force.right, 
										type=type, orient=orient, nlong=nlong, nshort=nshort,  mainfont=cex)
		cat("done.\n")
		mar <- c(mar[1], round(newmars[1],2), mar[3], round(newmars[2],2))
	}
	
	# expand mar to be a list of the right length
	# and apply the asked for correction to the left and right edges
	mars <- lapply(1:panels.max, function(z) return(mar))
	for (i in 1:panels.max) {
		if (orient == "PORTRAIT") breaks <- nshort 
		breaks <- ifelse(orient=="PORTRAIT", nshort, nlong)
		if (all(i %% nshort == 1, !(is.null(mar.force.left)))) mars[[i]][2] <- mar.force.left
		if (all(i %% nshort == 0, !(is.null(mar.force.right)))) mars[[i]][4] <- mar.force.right
		
	}
	


	# turn on straight ended lines
	command <- paste0(command, "par(lend=2)\n")
	# turn on drawing anywhere
	command <- paste0(command, "par(xpd=NA)\n")



	# add the panel code
	command <- paste0(command, "\n\n\n")
	for (i in 1:panels.max) {

		panels.sub <- panels[panels$PANEL == i,]
		panel.command <- panelCode(panel_index=i,
									groups=panels.sub$GROUP,
									x=panels.sub$RF_LEVEL, 
									ye=panels.sub$ESTIMATE,
									yse=panels.sub$STDERR,
									cex=cex,
									wls.line=wls.lines,
									join.points=join.points,
									boxcols=panels.sub$BOXCOLS,
									boxbordercols=panels.sub$BOXBORDERCOLS,
									pch=panels.sub$PCH,
									XLogScale=XLogScale[i],
									XExponentiate=XExponentiate[i],
									YLogScale=YLogScale[i],
									YExponentiate=YExponentiate[i],
									xlab=xlabs[i],
									ylab=ylabs[i],
									xlim=xlim[[i]],
									ylim=ylim[[i]],
									xticks=xticks[[i]],
									yticks=yticks[[i]],
									show.xticklabs=show.xticklabs[i],
									show.yticklabs=show.yticklabs[i],
									boxparm.stderr=boxparm.stderr,
									boxsizeoverride=boxsizeoverride,
									point.labels.top.column=point.labels.top.column,
									point.labels.bottom.column=point.labels.bottom.column,
									point.labels.cex=point.labels.cex,
									mar=mars[[i]])
		command <- paste0(command, panel.command, "\n\n\n\n\n\n")

	}

	if (!is.null(additionalR)) {
		if (file.exists(additionalR)) {
			command <- paste(command, "source(\"", additionalR, "\")\n", sep="")
		}
	}

	# close the file
	command <- paste0(command, "\ncloseFile(type)")

	target_file <- paste(filestem,"_jasper_code.r",sep="")
	fileConn <- file(target_file)
	writeLines(command, fileConn)
	close(fileConn)

	source(target_file)

}


calculateAxis <- function(lim, ticks, pts, logscale, exponentiate) {

	if (exponentiate) {
		if (!is.null(pts)) pts <- exp(pts)
	}
	# if logscale then only use points > 0
	if (logscale) {
		if (!is.null(pts)) pts <- pts[pts > 0]
		if (all(!is.null(lim), any(lim <= 0))) stop("Given axis limits invalid on log scale")
		if (all(!is.null(ticks), any(ticks <= 0))) stop("Given tick points invalid on log scale")
	}
	

	if (all(is.null(ticks), is.null(lim))) {
		# no x axis information
		ticks <- pretty(c(min(pts), max(pts)))
		lim <- c(min(ticks), max(ticks))
	} else if (is.null(ticks)) {
		# missing ticks but non-missing lim
		ticks <- pretty(lim)
		# want the tick marks to be inside the range given
		ticks <- ticks[ticks > (min(lim) - .Machine$double.eps)]
		ticks <- ticks[ticks < (max(lim) + .Machine$double.eps)]
	} else if (is.null(lim)) {
		# missing lim but non missing ticks
		lim <- c(min(ticks), max(ticks))
	}
	
	return(list(lim=lim, ticks=ticks))

}

verifyLPArgs <- function(arg, argname, panels.max, expand.list) {

	# if it's already NULL that's fine
	if (is.null(arg)) return(NULL) 

	# check to see if a list is expected - if it isn't then make it a list
	if (all(expand.list, typeof(arg) != "list")) {
		arg <- list(arg)
	}

	if (all(!is.null(arg), length(arg)==1)) {
		arg <- rep(arg, panels.max)
	} else if (all(!is.null(arg), length(arg)>=1, length(arg) != panels.max)) {
		stop(paste0(argname, " must be NULL, of length 1, or of length ", panels.max))
	}
	return(arg)
}


panelCode <- function(panel_index, 
						groups, 
						x, 
						ye, 
						yse, 
						cex, 
						wls.line,
						join.points,
						XLogScale=FALSE,
						XExponentiate=FALSE,
						YLogScale=FALSE,
						YExponentiate=FALSE,
						xlim=NULL,
						ylim=NULL,
						xlab=NULL,
						ylab=NULL,
						xticks=NULL,
						yticks=NULL,
						show.xticklabs=TRUE,
						show.yticklabs=TRUE,
						boxcols=NULL,
						boxbordercols=NULL,
						point.labels.top.column=NULL,
						point.labels.bottom.column=NULL,
						point.labels.cex=1,
						pch=NULL,
						boxparm.stderr=1,
						boxsizeoverride,
						mar) {

	Xaxis.calc <- calculateAxis(xlim, xticks, pts=x, logscale=XLogScale, exponentiate=XExponentiate)
	xticks <- Xaxis.calc$ticks
	xlim <- Xaxis.calc$lim
	
	
	Yaxis.calc <- calculateAxis(ylim, yticks, pts=c(ye-2*yse, ye+2*yse), logscale=YLogScale, exponentiate=YExponentiate)
	yticks <- Yaxis.calc$ticks
	ylim <- Yaxis.calc$lim				
	
	# set up ticklabs
	if (show.xticklabs) {
		xticklabs <- as.character(xticks)
	} else {
		xticklabs <- rep("", length(xticks))
	}
	if (show.yticklabs) {
		yticklabs <- as.character(yticks)
	} else {
		yticklabs <- rep("", length(yticks))
	}
	

	command <- paste0("# PANEL ", panel_index, "\n")
	command <- paste0(command, "panel", panel_index, " <- panels[panels$PANEL == ", panel_index, ",]\n\n")
	command <- paste0(command, "# Setting up plotting area\npar(mar=", deparse(mar), ")\n")
	if (XLogScale) {
		xlim <- log(xlim)
		xticks <- log(xticks)
	}
	if (YLogScale) {
		ylim <- log(ylim)
		yticks <- log(yticks)
	}
	command <- paste0(command, "# Setting up empty plot\nblankPlot(xlim=", deparse(xlim), ", ylim=", deparse(ylim), ", mainfont=", cex, ")\n\n")


	ngroups <- max(groups)
	for (j in 1:ngroups) {

		if (is.null(boxcols)) {
			group.boxcols <- rep("black", sum(groups == j))
		} else {
			group.boxcols <- boxcols[groups == j]
		}

		if (is.null(boxbordercols)) {
			group.boxbordercols <- rep("black", sum(groups == j))
		} else {
			group.boxbordercols <- boxbordercols[groups == j]
		}

		if (is.null(pch)) {
			group.pch <- rep(22, sum(groups == j))
		} else {
			group.pch <- pch[groups == j]
		}

		if (boxsizeoverride) {
			cex.group <- deparse(rep(boxparm.stderr, sum(groups == j)), width.cutoff=500L)
		} else {
			# cex.group <- paste0("boxparm.stderr * 1/panel", panel_index, "$STDERR[panel", panel_index, "$GROUP == ", j, "] / max(1/panel", panel_index, "$STDERR[panel", panel_index, "$GROUP == ", j, "])")
			cex.group <- paste0("boxparm.stderr * 1/panel", panel_index, "$STDERR[panel", panel_index, "$GROUP == ", j, "] / max(1/panel", panel_index, "$STDERR)")
		}
		
		if (join.points) {
			type <- "o"
		} else {
			type <- "p"
		}

		command <- paste0(command, "# LINE GROUP ", j, "\n")
		if (wls.line) command <- paste0(command, "# Drawing fitted lines\nAddWLSLine(panel.group=panel", panel_index, "[panel", panel_index, "$GROUP == ", j, ",], col=\"guess\", lty=\"solid\", extend=5, XLogScale=", deparse(XLogScale), ", YLogScale=", deparse(YLogScale), ", XExponentiate=", deparse(XExponentiate), ", YExponentiate=", deparse(YExponentiate), ")\n")
		if (!is.null(point.labels.top.column)) command <- paste0(command, "# Writing top of point labels\nAddPointLabels(panel.group=panel", panel_index, "[panel", panel_index, "$GROUP == ", j, ",], end=\"top\", column=\"", point.labels.top.column, "\", cex=", point.labels.cex, ")\n")
		if (!is.null(point.labels.bottom.column)) command <- paste0(command, "# Writing bottom of point labels\nAddPointLabels(panel.group=panel", panel_index, "[panel", panel_index, "$GROUP == ", j, ",], end=\"bottom\", column=\"", point.labels.bottom.column, "\", cex=", point.labels.cex, ")\n")
		command <- paste0(command, "# Drawing confidence intervals\nAddCIs(panel.group=panel", panel_index, "[panel", panel_index, "$GROUP == ", j, ",], XLogScale=", deparse(XLogScale), ", YLogScale=", deparse(YLogScale), ", XExponentiate=", deparse(XExponentiate), ", YExponentiate=", deparse(YExponentiate), ")\n")
		command <- paste0(command, "# Set reference box size\nboxparm.stderr <- ", boxparm.stderr, "\n")
		command <- paste0(command, "# Draw the points\npoints(",
									"x=", ifelse(all(XLogScale, !XExponentiate), "log(", ""), "panel", panel_index, "$RF_LEVEL[panel", panel_index, "$GROUP == ", j, "]", ifelse(all(XLogScale, !XExponentiate), ")", ""),",\n\t\t",
									"y=panel", panel_index, "$ESTIMATE[panel", panel_index, "$GROUP == ", j, "],\n\t\t",
									"cex=", cex.group, ",\n\t\t",
									"col=", deparse(group.boxbordercols, width.cutoff=500L), ",\n\t\t",
									"bg=", deparse(group.boxcols, width.cutoff=500L), ",\n\t\t",
									"pch=", deparse(group.pch, width.cutoff=500L), ",\n\t\t",
									"type=\"", type, "\")\n\n")

	}



	# add axes
	command <- paste0(command, "# Adding axes\n")
	command <- paste0(command, "Xaxis(xticks=", deparse(xticks, width.cutoff=500L), ", ticklabs=", deparse(xticklabs, width.cutoff=500L), ")\n")
	command <- paste0(command, "Yaxis(yticks=", deparse(yticks, width.cutoff=500L), ", ticklabs=", deparse(yticklabs, width.cutoff=500L), ")\n\n")

	# add axis labels
	command <- paste0(command, "# Writing axis labels (placeholder code if unspecified)\n")
	command <- paste0(command, "AxisLabels(xlab=\"", ifelse(is.null(xlab), "", xlab), "\", ylab=\"", ifelse(is.null(ylab), "", ylab),
								"\", cex=1, xline=3.5, yline=4)")


	return(command)


}





## additional helper functions to simplify the generated code
AddCIs <- function(panel.group, XLogScale, XExponentiate, YLogScale, YExponentiate) {

	names(panel.group) <- toupper(names(panel.group))
	if (is.null(panel.group$CICOL)) {
		cols <- rep("black", dim(panel.group)[1])
	} else {
		cols <- panel.group$CICOL
	}
	
	rf <- panel.group$RF_LEVEL
	y <- panel.group$ESTIMATE
	se <- panel.group$STDERR
	lci <- y-1.96*se
	uci <- y+1.96*se
	
	if (all(XLogScale, !XExponentiate)) rf <- log(rf)
	if (all(YLogScale, !YExponentiate)) {
		y <- log(y)
		lci <- log(lci)
		uci <- log(uci)
	}
	

	mapply(function(x,y,lci,uci,col) lines(c(x,x), c(lci, uci), col=col),
			x=rf,
			y=y,
			lci=lci,
			uci=uci,
			col=cols)
			
	return(NULL)

}

AddWLSLine <- function(panel.group, col="guess", lty="solid", extend=5, XLogScale, XExponentiate, YLogScale, YExponentiate) {

	names(panel.group) <- toupper(names(panel.group))
	
	est <- panel.group$ESTIMATE
	rf <- panel.group$RF_LEVEL
	stderr <- panel.group$STDERR
	
	# adjust for logs
	if (all(XLogScale, !XExponentiate)) rf <- log(rf)
	if (all(YLogScale, !YExponentiate)) est <- log(est)
	
	# the standard error is only used for weighting so 
	# doesn't need adjusting...
	
	params <- coef(lm(est ~ rf, weights=1/(stderr^2)))
	
	# if col = guess take a majority guess at what the line colour should be
	if (all(col == "guess", !is.null(panel.group$CICOL))) {
		col <- names(which.max(table(panel.group$CICOL)))
	} else if (col == "guess") {
		col <- "black"
	}

	xs <- c(min(rf), max(rf))
	xs <- xs + diff(xs)*c(-extend/100, extend/100)
	lines(xs, params[1] + xs*params[2], col=col, lty=lty)
	
	return(params)
 

}

AddPointLabels <- function(panel.group, end, column, cex) {

	names(panel.group) <- toupper(names(panel.group))
	column <- toupper(column)
	xs <- panel.group$RF_LEVEL
	
	if (end == "top") {
		ys <- panel.group$ESTIMATE + 1.96*panel.group$STDERR + strheight("A")
	} else if (end == "bottom") {
		ys <- panel.group$ESTIMATE - 1.96*panel.group$STDERR - strheight("A")

	}
	
	# check to see if the column actually exists 
	if (!(column %in% names(panel.group))) warning("Can't find point labels column")
	else {
		print(xs)
		print(ys)
		text(x=xs, y=ys, labels=panel.group[,column], cex=cex)
	}

}


testPlotSeparation <- function(mar, left_space_mar=NULL, right_space_mar=NULL, type, orient, nlong, nshort, mainfont) {

	SetPage(type=type, orient=orient, nshort=nshort, nlong=nlong, attempt_adobe_kill=FALSE)
	
	mais <- list()
	for_end <- ifelse(orient=="PORTRAIT", nshort, nlong)
	for (i in 1:for_end) {
		cmar <- mar
		if (all(i == 1, !(is.null(left_space_mar)))) cmar[2] <- left_space_mar
		if (all(i == for_end, !(is.null(right_space_mar)))) cmar[4] <- right_space_mar
		par(mar=cmar)
		blankPlot(xlim=c(0,1), ylim=c(0,1), mainfont=mainfont)
		mais <- c(mais, list(par("mai")))
	}
	closeFile("PDF", suppress.notice=TRUE)
	return(mais)
	
}

getHorizAllocation <- function(mais) {

	gaps <- unlist(lapply(mais, function(z) return(z[c(2,4)])))
	
	leftmar <- gaps[1]
	rightmar <- gaps[length(gaps)]
	
	internalgaps <- gaps[-c(1, length(gaps))]
	internalgaps <- mapply(function(x,y) return(gaps[x] + gaps[y]), x=seq(1,length(internalgaps),by=2), y=seq(2,length(internalgaps),by=2))

	return(list(left=leftmar, internal=internalgaps, right=rightmar))	
}

optimiseInternalGap <- function(target, mar, left_space_mar=NULL, right_space_mar=NULL, type, orient, nlong, nshort, mainfont) {
	
	# target is going to be given in cm
	target <- target / 2.54
	
	optimFn2 <- function(newmar, target, mar, left_space_mar, right_space_mar, type, orient, nlong, nshort, mainfont) {

		mais <- testPlotSeparation(
					mar=c(mar[1], ifelse(newmar[1] <= 0L, 0.01, newmar[1]), mar[3], ifelse(newmar[2] <= 0L, 0.01, newmar[2])), 
					left_space_mar=left_space_mar,
					right_space_mar=right_space_mar,
					type=type, orient=orient, nlong=nlong, nshort=nshort, mainfont=mainfont)
		fit <- getHorizAllocation(mais)
		return(sum(abs(fit$internal - target)))

	}
	
	newmars <- optim(par=c(6,3), optimFn2, target=target, mar=mar, left_space_mar=left_space_mar, right_space_mar=right_space_mar, type=type, orient=orient, nlong=nlong, nshort=nshort, mainfont=mainfont)$par
	return(newmars)
	
}