\name{LinearPlotFromCSV}
\alias{LinearPlotFromCSV}
\title{LinearPlotFromCSV}
\description{Plot a full-page Jasper figure containing (potentially) multiple panels, applying formatting styles on a per-panel, per-group basis. The R/Jasper code that renders the figure is also generated.}
\arguments{
\item{file}{Path to input CSV file}
\item{type}{One of the valid SetPage file types (default: GUI)}
\item{orient}{Page orientation, LANDSCAPE or PORTRAIT (default: PORTRAIT)}
\item{perpage}{Force a particular number of plots per page rather than detecting it automatically from the CSV file (default: NULL)}
\item{filestem}{String to use as the filename, an appropriate extension is appended automatically}
\item{cex}{Font scaling factor (multiplies the underlying panel "unit" font size) (default: 1)}
\item{titlespace}{Proportion of the page to allocate to a title area at the top of the page (default: 0.1)}
\item{footerspace}{Proportion of the page to allocate to a footer area at the bottom of the page (default: 0.1)}
\item{wls.lines}{Calculate and add fitted weighted least square regression lines on a per-group basis (default: FALSE)}
\item{join.points}{Add line segments between points on a per-group basis (default: FALSE)}
\item{XLogScale}{ (default: FALSE)}
\item{XExponentiate}{ (default: FALSE)}
\item{YLogScale}{ (default: FALSE)}
\item{YExponentiate}{ (default: FALSE)}
\item{xlim}{Vector giving min and max value of X range to be plotted (default: NULL)}
\item{ylim}{Vector giving min and max value of Y range to be plotted (default: NULL)}
\item{xticks}{Vector of tick marks to put on the X axis (default: NULL)}
\item{yticks}{Vector of tick marks to put on the Y axis (default: NULL)}
\item{show.xticklabs}{If FALSE then do not add any tick labels to the X axis ticks (default: TRUE)}
\item{show.yticklabs}{If FALSE then do not add any tick labels to the Y axis ticks (default: TRUE)}
\item{xlabs}{Vector of labels to use as the X axis labels (default: NULL)}
\item{ylabs}{Vector of labels to use as the Y axis labels (default: NULL)}
\item{point.labels.top.column}{Specify a column in the CSV file that gives the labels to add above the upper end of the confidence interval (default: NULL)}
\item{point.labels.bottom.column}{Specify a column in the CSV file that gives the labels to add below the bottom end of the confidence interval (default: NULL)}
\item{point.labels.cex}{Font size scaling applied to point labels (default: 1)}
\item{boxparm.stderr}{Provide the value of the smallest stderr to be used with computing the boxparm, instead of detecting it automatically.  (default: 1)}
\item{boxsizeoverride}{Disable scaling of boxes by inverse of variance (default: FALSE)}
\item{mar}{Vector of margins, in order c(bottom, left, top, right) (default: c(7, 7, 3, 3))}
\item{mar.optimise}{ (default: FALSE)}
\item{mar.spacing.horiz.target}{ (default: 3)}
\item{mar.force.left}{ (default: NULL)}
\item{mar.force.right}{ (default: NULL)}
\item{additionalR}{Path to an additional R script to be added to the generated R code and run as part of the plot (default: NULL)}
}
\author{Matt Arnold}
