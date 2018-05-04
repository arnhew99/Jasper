\name{LinearPlot}
\alias{LinearPlot}
\title{LinearPlot}
\description{Plot a `linear' style plot, either in the current panel, meaning that the user must already have handled the setting up of a decent coordinate system; or the next available one, allowing Jasper to compute the underlying coordinate system.}
\arguments{
\item{linelist}{List of data frames, each of which should contain RFLevel, Estimate and StdErr columns. Each data frame defines a "group" of points, which will be plotted with a common style.}
\item{reglines}{Matrix of line parameters with two columns and the same number of rows as line groups. First column gives intercepts, second gives slopes. (default: NULL)}
\item{extendLines}{ (default: FALSE)}
\item{joinPoints}{Join points in the same group by a piecewise line underneath the boxes (default: FALSE)}
\item{YLogScale}{Use a log scale on the Y axis (default: FALSE)}
\item{XLogScale}{Use a log scale on the X axis (default: FALSE)}
\item{DataLogged}{Deprecated; use ExponentiateDataOnPlot instead (default: NULL)}
\item{ExponentiateDataOnPlot}{The contents of the data table should be exponentiated when plotted. For instance, if the data contains log RR but you want to plot RR (default: NULL)}
\item{LineLabels}{Labels to add at the right of a group (default: NULL)}
\item{boxcols}{Vector of box colours, one per group (default: NULL)}
\item{boxbordercol}{Vector of box border colours, one per group (default: NULL)}
\item{linecols}{Vector of line colours (if plotted), one per group (default: NULL)}
\item{linelwds}{Vector of line widths (if plotted), one per group (default: NULL)}
\item{lineltys}{Vector of line types (if plotted), one per group (default: NULL)}
\item{new.plot}{Allow this function to handle setting up a plotting area, either using the values given for xlim etc. or detecting them from the data (default: FALSE)}
\item{xlim}{Vector giving min and max value of X range to be plotted (default: NULL)}
\item{ylim}{Vector giving min and max value of Y range to be plotted (default: NULL)}
\item{axes}{Allow this function to draw axes automatically  (default: TRUE)}
\item{xaxis}{Allow this function to draw an X axis (default: TRUE)}
\item{xaxis.line}{ (default: 1)}
\item{xticks}{Vector of tick marks to put on the X axis (default: NULL)}
\item{xticklabs}{Vector of labels to use at tick marks instead of X axis position of mark (default: NULL)}
\item{xticklabs.cex}{ (default: 1)}
\item{xlab}{X axis label (default: NULL)}
\item{xlab.line}{ (default: NULL)}
\item{yaxis}{Allow this function to draw an Y axis  (default: TRUE)}
\item{yaxis.line}{ (default: 1)}
\item{yticks}{Vector of tick marks to put on the Y axis (default: NULL)}
\item{yticklabs}{Vector of labels to use at tick marks instead of Y axis position of mark (default: NULL)}
\item{ylab}{Y axis label (default: NULL)}
\item{ylab.line}{ (default: NULL)}
\item{yticklabs.cex}{ (default: 1)}
\item{mainfont}{Font scaling applied to cex (default: 1)}
\item{PointLabelsTop}{Name of column in line group that gives labels to put above points (default: NULL)}
\item{PointLabelsBottom}{Name of column in line group that gives labels to put below points (default: NULL)}
\item{PointLabelsPosAdj}{Adjust the position of the point labels. Vector of length 2 giving top and bottom adjustment (default: NULL)}
\item{PointLabelsCol}{Font colour to use for the point labels (default: NULL)}
\item{PointLabelsCEX}{Font scaling applied to the point labels, relative to the panel default (default: 1)}
\item{useArrows}{ (default: TRUE)}
\item{TruncateAtPlotLims}{ (default: TRUE)}
\item{boxparmx}{Specify X length of "unit" box (default: NULL)}
\item{boxparmy}{Specify Y length of "unit" box (default: NULL)}
\item{boxparm.stderr}{Provide the value of the smallest stderr to be used with computing the boxparm, instead of detecting it automatically.  (default: NULL)}
\item{boxsizeoverride}{Disable scaling of boxes by inverse of variance (default: FALSE)}
\item{margins}{Vector of margins to use if drawing a new plot, in format (bottom, left, top, right) (default: NULL)}
\item{CIs}{Can be one of three types of value. TRUE/FALSE turns plotting of CIs on or off globally. A vector of TRUE/FALSE turns CIs on or off per group. A list of TRUE/FALSE vectors turns CIs on and off on a per-point basis (default: TRUE)}
\item{CILineCol}{ (default: NULL)}
\item{PointCIs}{Deprecated; use CIs instead (default: NULL)}
\item{CIsOnTop}{ (default: FALSE)}
\item{rBox}{Draw boxes with rounded corners (not advised) (default: FALSE)}
}
\author{Matt Arnold}
