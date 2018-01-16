\name{Manhattan}
\alias{Manhattan}
\title{Manhattan}
\description{}
\arguments{
\item{pvals}{Vector of p values to be plotted}
\item{pt_cols}{Either a single colour to be used for all points or a vector of colours of the same length as pvals with one colour for each point (default: black)}
\item{yaxis.max}{Specify the end of the Y axis in displayed units (negative log base 10) (default: 30)}
\item{yaxis.ticks.by}{ (default: 5)}
\item{xlab}{X axis label}
\item{ylab}{Y axis label (default: Negative log10 P value)}
\item{add.bonferroni}{ (default: TRUE)}
\item{font.bonferroni}{ (default: 1)}
\item{new.plot}{Allow this function to handle setting up a plotting area, either using the values given for xlim etc. or detecting them from the data (default: TRUE)}
\item{blanks.after}{ (default: NULL)}
\item{cex}{Font scaling factor (multiplies the underlying panel "unit" font size) (default: 1)}
\item{xaxis.label.line}{Margin line on which to draw the X axis label (default: 0.5)}
\item{margins}{Specify margins for the plot if plot.new = TRUE (default: c(3, 6, 0.1, 2))}
\item{pch}{Specify point symbol to use; to see a list of possible types look at ?points (default: 1)}
\item{pt.lwd}{Specify the line width to use when drawing points (default: 1)}
\item{draw.base.line}{ (default: TRUE)}
}
\author{Matt Arnold}
