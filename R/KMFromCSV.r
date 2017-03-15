KMFromCSV <- function(file, 
	orient="LANDSCAPE",
	type="GUI",
	filestem="JasperKM",
	fup_max=NULL,
	strata_names,
	strata_totals=NULL,
	strata_counts=NULL,
	sas_input_csv=NULL,
	alloc_var=NULL,
	fup_var=NULL,
	addStrata_events=TRUE,
	xlim=NULL,
	ylim=NULL,
	xticks=NULL,
	xticklabs=NULL,
	xaxis.line=1,
	yticks=NULL,
	yticklabs=NULL,
	yaxis.line=1,
	xlab="",
	ylab="",
	mainTitle,
	mainTitle.cex=1.2,
	lineltys=NULL,
	linecols=NULL,
	linelwds=NULL,
	addLegend=TRUE,
	legendx=NULL,
	legendy=NULL,
	additionalText="",
	additionalTextx=NULL,
	additionalTexty=NULL,
	cex=1.2
) 
{

	km <- read.csv(file, as.is=TRUE)

	strata <- unique(km$STRATUM)
	
	# set some defaults
	if (is.null(lineltys)) lineltys <- strata
	if (is.null(linecols)) linecols <- strata
	if (is.null(linelwds)) linelwds <- rep(1, length(strata))

	
	# reformat the file according to GB's suggestions
	km$X_CENSOR_ <- ifelse(km$fup == 0, 1, km$X_CENSOR_)
	km$minsur <- unlist(lapply(strata, function(z) rep(min(na.omit(km$SURVIVAL[km$STRATUM == z])), sum(km$STRATUM==z))))
	km$SURVIVAL_imp <- ifelse(is.na(km$SURVIVAL), km$minsur, km$SURVIVAL)
	km$one_minus_survival <- 1-km$SURVIVAL_imp
	
	
	if (is.null(fup_max)) fup_max <- min(sapply(strata, function(z) max(na.omit(km$fup[km$STRATUM == z]))))
	if (is.null(xlim)) xlim <- c(0,fup_max)
	if (is.null(xticks)) {
		xticks <- pretty(xlim)
		xticks <- xticks[xticks <= (max(xlim)+0.000001)]
	}
	if (is.null(xticklabs)) xticklabs <- xticks
	
	ymax <- NULL
	if (is.null(ymax)) ymax <- min(sapply(strata, function(z) max(na.omit(km$one_minus_survival[km$STRATUM == z & km$fup < fup_max]))))
	if (is.null(ylim)) ylim <- c(0, ymax)
	if (is.null(yticks)) {
		yticks <- pretty(ylim)
		yticks <- yticks[yticks <= (max(ylim)+0.000001)]
	}
	if (is.null(yticklabs)) yticklabs <- yticks
	
	
	
	SetPage(orient=orient, type=type, filestem=filestem, perpage=1, titlespace=0.01)
	par(mar=c(8,10,6,8))
	blankPlot(xlim=xlim, ylim=ylim, mainfont=cex)

	for (j in strata) {
		points(km$fup[km$STRATUM==j & km$fup <= fup_max], km$one_minus_survival[km$STRATUM==j & km$fup <= fup_max], type="l", col=linecols[j], lty=lineltys[j], lwd=linelwds[j], lend=2)
	}


	Xaxis(xticks=xticks, ticklabs=xticklabs, line=xaxis.line)
	Yaxis(yticks=yticks, ticklabs=yticklabs, LinScale=TRUE, line=yaxis.line)
	AxisLabels(xlab=xlab, ylab=ylab, cex=1, xline=2.5+xaxis.line, yline=3+yaxis.line)

	par(xpd=NA)
	text(x=mean(xlim), y=max(ylim)+3*strheight("A"), labels=mainTitle, font=2, cex=mainTitle.cex, adj=c(0.5,0))


	if (addStrata_events) {
		# calculate number of people with events (?)
		# I think the function will need the total number of people
		# and then write out #total - #events
		if (!is.null(strata_counts)) {
			nevents <- strata_counts 
		} else if (!is.null(sas_input_csv)) {
			input <- read.csv(sas_input_csv, as.is=TRUE)
			strata_totals <- sapply(1:2, function(z) sum(input[,alloc_var] == z))
			nevents <- lapply(1:2, function(z) strata_totals[z] - c(0, sapply(1:fup_max, function(y) sum(input[input[,alloc_var]==z, fup_var] <= y))))
		}
		else {
			nevents <- lapply(strata, function(y) sapply(xticks, function(z) sum(km$X_CENSOR_[km$STRATUM == y & km$fup <= z])))
		}
		if (all(!is.null(strata_totals), is.null(sas_input_csv))) {
			nevents <- lapply(strata, function(y) strata_totals[y] - nevents[[y]])
		}
		lapply(1:length(nevents), function(n) text(xticks, y=0-10*strheight("A")-(n-1)*(strheight("A\nA")-strheight("A")), labels=nevents[[n]]))
		text(x=0-strwidth("\t\t\t"), y=sapply(1:2, function(n) 0-10*strheight("A")-(n-1)*(strheight("A\nA")-strheight("A"))), labels=strata_names, adj=c(1,0.5))
	}

	# add a legend
	
	if (addLegend) {
	
		if (is.null(legendx)) legendx <- xlim[1] + 0.6*diff(xlim) - 2*max(sapply(strata_names, strwidth))
		if (is.null(legendy)) legendy <- ylim[1] + 0.9*diff(ylim)

		par(lend=2)
		legend(x=legendx, y=legendy, legend=strata_names, lty=lineltys, col=linecols, lwd=linelwds, bty="n", seg.len=4)
	}
	
	if (additionalText != "") {
	
		if (is.null(additionalTextx)) additionalTextx <- xlim[1] + 0.4*diff(xlim) - strwidth(additionalText)
		if (is.null(additionalTexty)) additionalTexty <- ylim[1] + 0.5*diff(ylim) + strheight(additionalText)
		
		text(x=additionalTextx, y=additionalTexty, labels=additionalText, adj=c(0,0.5))
		
	
	}


	closeFile(type)


}