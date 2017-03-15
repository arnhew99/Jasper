\name{FormatLP}
\alias{FormatLP}
\title{FormatLP}
\description{Format a data frame to be suitable for plotting with LinearPlot, by specifying which columns provide the X and Y coordinates, and various other styling options.}
\arguments{
\item{LPdata}{Data frame to be formatted ready to be plotted using LinearPlot}
\item{GroupCol}{ (default: NULL)}
\item{RFLevelCol}{Name of column in LPdata to be used as the risk factor levels (X axis positions)}
\item{EstimateCol}{Name of column in LPdata to be used as the estimate (Y axis positions)}
\item{StdErrCol}{Name of column in Lpdata to be used as the standard error (Y axis standard errors)}
\item{xlim}{Vector giving min and max value of X range to be plotted (default: NULL)}
\item{ylim}{Vector giving min and max value of Y range to be plotted (default: NULL)}
\item{axes}{Allow this function to draw axes automatically  (default: NULL)}
\item{xaxis}{Allow this function to draw an X axis (default: NULL)}
\item{yaxis}{Allow this function to draw an Y axis  (default: NULL)}
\item{xticks}{Vector of tick marks to put on the X axis (default: NULL)}
\item{yticks}{Vector of tick marks to put on the Y axis (default: NULL)}
\item{xaxis.all}{Extend the axis to cover the whole range of the current panel (default: NULL)}
\item{xticklabs}{Vector of labels to use at tick marks instead of X axis position of mark (default: NULL)}
\item{yticklabs}{Vector of labels to use at tick marks instead of Y axis position of mark (default: NULL)}
\item{yaxis.all}{Extend the axis to cover the whole range of the current panel (default: NULL)}
\item{xlab}{X axis label (default: NULL)}
\item{xlab.line}{Margin line at which to write the X axis label (default: NULL)}
\item{ylab}{Y axis label (default: NULL)}
\item{ylab.line}{Margin line at which to write the Y axis label (default: NULL)}
\item{boxcols}{Vector of box colours, one per group (default: NULL)}
\item{boxbordercol}{Vector of box border colours, one per group (default: NULL)}
\item{linecols}{Vector of line colours (if plotted), one per group (default: NULL)}
\item{linelwds}{Vector of line widths (if plotted), one per group (default: NULL)}
\item{lineltys}{Vector of line types (if plotted), one per group (default: NULL)}
\item{PointLabelsTop}{Name of column in line group that gives labels to put above points (default: NULL)}
\item{PointLabelsBottom}{Name of column in line group that gives labels to put below points (default: NULL)}
\item{PointLabelsPosAdj}{Adjust the position of the point labels. Vector of length 2 giving top and bottom adjustment (default: NULL)}
\item{PointLabelsCol}{Font colour to use for the point labels (default: NULL)}
\item{PointLabelsCEX}{Font scaling applied to the point labels, relative to the panel default (default: NULL)}
\item{boxparmx}{Specify X length of "unit" box (default: NULL)}
\item{boxparmy}{Specify Y length of "unit" box (default: NULL)}
\item{boxparm.stderr}{Provide the value of the smallest stderr to be used with computing the boxparm, instead of detecting it automatically.  (default: NULL)}
\item{boxsizeoverride}{Disable scaling of boxes by inverse of variance (default: NULL)}
}
\author{Matt Arnold}
