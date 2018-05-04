\name{ForestBasic}
\alias{ForestBasic}
\title{ForestBasic}
\description{Plot the central graphic part of a forest plot. Optionally, the text representation of the value and CIs may be added to the right of the forest and an additional column, usually N, may be added to the left.}
\arguments{
\item{rawdata}{Data frame formatted appropriately}
\item{LogScale}{X axis should be on the log scale (default: TRUE)}
\item{ExponentiateDataOnPlot}{rawdata data frame contains data that should be exponentiated when plotted. For instance, the data frame contains log RR but we want to plot RR (default: FALSE)}
\item{blanks}{List of blank rows (default: NULL)}
\item{Hets}{List of details of any heterogeneity tests to plot (default: NULL)}
\item{Trends}{List of details of any trend tests to plot (default: NULL)}
\item{xNoEffect}{X axis value to draw a vertical line, usually indicating the "line of no-effect" (default: NULL)}
\item{NoEffect.lty}{ (default: solid)}
\item{xaxmin}{Left of the forest drawing area, in user coordinates. NLabels column will hang left of this}
\item{xaxmax}{Right of the forest drawing area, in user coordinates. ValueLabels column will hang right of this}
\item{xaxis}{Allow this function to draw an X axis (default: TRUE)}
\item{ymin}{Bottom of the forest, in user coordinates (default: 0)}
\item{ymax}{Top of the forest, in user coordinates (default: par("usr")[4])}
\item{boxparmx}{Specify X length of "unit" box (default: NULL)}
\item{boxparmy}{Specify Y length of "unit" box (default: NULL)}
\item{boxparm.stderr}{Provide the value of the smallest stderr to be used with computing the boxparm, instead of detecting it automatically.  (default: NULL)}
\item{Range}{Deprecated; use xlim instead (default: NULL)}
\item{xlim}{Vector giving min and max value of X range to be plotted (default: NULL)}
\item{xticks}{Vector of tick marks to put on the X axis (default: NULL)}
\item{ticklabs}{Character vector of tick labels to use instead of computed numeric values (default: NULL)}
\item{xticks.ticklength}{ (default: 0.25)}
\item{mainfont}{Font scaling applied to cex (default: 1)}
\item{xlab}{X axis label}
\item{xlab.adjust}{ (default: 0)}
\item{ValueLabels}{Add the values and confidence intervals, anchored to the right of the forest (default: TRUE)}
\item{ValueLabelsHeader}{Heading to put above the ValueLabels column (default: Hazard ratio)}
\item{ValueLabelsEffectSE}{ (default: FALSE)}
\item{ValueDigits}{ (default: NULL)}
\item{ValueDigitsSE}{ (default: NULL)}
\item{separator}{Separator character to use in the confidence intervals in the ValueLabels column (default: NULL)}
\item{NLabel}{Add a column anchored to the left of the forest, usually containing N (default: FALSE)}
\item{NLabelHeader}{Heading to put above the N column (default: N)}
\item{boxsizeoverride}{Disable scaling of boxes by inverse of variance (default: FALSE)}
\item{roundedSquares}{Plot squares with rounded corners (not advised) (default: FALSE)}
\item{spacing}{Space (in user coordinates) between forest and ValueLabels (default: 0)}
\item{verbose}{Print diagnostic information while plotting (default: TRUE)}
\item{drawPoints}{ (default: TRUE)}
\item{AdAM.mode}{Experimental mode for use with XML data (default: FALSE)}
\item{pointGroups}{ (default: NULL)}
\item{pointGroupsFactor}{ (default: 0.9)}
\item{lwd}{Line width (scaling) (default: 1)}
\item{CISecond}{ (default: NULL)}
}
\author{Matt Arnold}
