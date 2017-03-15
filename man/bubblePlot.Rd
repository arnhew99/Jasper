\name{bubblePlot}
\alias{bubblePlot}
\title{bubblePlot}
\description{When a scatterplot is drawn with a very large number of points, the plot tends to end up as an indecipherable black mass. The "bubble plot" is an attempt to come to neater solution, by assembling a grid and then counting the number of points that fall within each grid cell. A circle is plotted in the centre of the grid cell whose area is proportional to a function of the number of points. This allows the bubble plot to the shape of the data whilst maintaining legibility.}
\arguments{
\item{Z}{Two-dimensional data matrix}
\item{xlim}{Vector giving min and max value of X range to be plotted (default: NULL)}
\item{ylim}{Vector giving min and max value of Y range to be plotted (default: NULL)}
\item{xn}{Number of grid bins in the X direction (default: 20)}
\item{yn}{Number of grid bins in the Y direction (default: 20)}
\item{new.plot}{Allow this function to handle setting up a plotting area, either using the values given for xlim etc. or detecting them from the data (default: TRUE)}
\item{drawGrid}{Draw the underlying grid (default: FALSE)}
\item{addText}{Vector of text labels to add to the bubbles (default: FALSE)}
\item{offsetTextMin}{ (default: 200)}
\item{cexText}{Text labels character expansion (default: 1)}
\item{colText}{Text label colour (default: 1)}
\item{cex.unit}{The size of a bubble containing one observation (default: 1)}
\item{pch}{ (default: 21)}
\item{colBubble}{ (default: 2)}
\item{colBubbleBorder}{ (default: 1)}
\item{mar}{Vector of margins, in order c(bottom, left, top, right) (default: c(4, 4, 4, 4))}
\item{scalefn}{Function (of x) that is used to scale the size of the bubbles according to the number of observations, which multiplies cex.unit. Default is to increase area linearly with number of observations, which is equivalent to scaling the radius by the square root of number of observations. (default: function(x) sqrt(x))}
\item{axes}{Allow this function to draw axes automatically  (default: TRUE)}
\item{xaxis.line}{ (default: 1)}
\item{yaxis.line}{ (default: 1)}
\item{mainfont}{Font scaling applied to cex (default: 1)}
\item{useHardcore}{Instead of using grid to allocated bubbles, lay down a Hardcore point process in the plotting region and compute its Voronoi tesselation. The resulting Voronoi cells define the bubbles. (default: FALSE)}
\item{HCintensity}{Intensity of Hardcore point process, if used (default: 100)}
\item{HCinhibition}{Inhibition distance of Hardcore point process, if used. No points will be closer together than this distance (default: 0.01)}
\item{useParallel}{ (default: TRUE)}
}
\author{Matt Arnold}
