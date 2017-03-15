\name{FormatForest}
\alias{FormatForest}
\title{FormatForest}
\description{Format a data frame to be suitable for plotting with ForestBasic.}
\arguments{
\item{rawdata}{Data frame to be formatted ready to be plotted using ForestBasic}
\item{forest.n}{ (default: 1)}
\item{EstimateCol}{Name of column giving estimates}
\item{StdErrCol}{Name of column giving standard errors (default: NULL)}
\item{LCICol}{Name of column giving lower bound of confidence intervals (default: NULL)}
\item{UCICol}{Name of column giving upper bound of confidence intervals (default: NULL)}
\item{logData}{Data is stored in log format in the data frame (default: FALSE)}
\item{findDiamonds}{Look for a Diamonds column for this forest (default: NULL)}
\item{findShow}{Look for a Show column for this forest (default: NULL)}
\item{findShowCI}{ (default: NULL)}
\item{findFillColour}{Look for a FillColour column for this forest (default: NULL)}
\item{findDiamondGuidelines}{Look for a DiamondGuidelines column for this forest (default: NULL)}
\item{findhideNoEffect}{Look for a hideNoEffect column for this forest (default: NULL)}
\item{getBlanks}{Identify blank rows (default: FALSE)}
\item{getHets}{Identify heterogeneity tests (default: FALSE)}
\item{getTrends}{Identify trend tests (default: FALSE)}
}
\author{Matt Arnold}
