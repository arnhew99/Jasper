\name{ForestFromCSV}
\alias{ForestFromCSV}
\title{ForestFromCSV}
\description{Plot a full-page Jasper figure containing a forest and associated extra columns, direct from an input CSV file. The R/Jasper code that renders the figure is also outputted. This may be edited by the user to include their own modifications.}
\arguments{
\item{file}{Path to input CSV file}
\item{orient}{Page orientation, LANDSCAPE or PORTRAIT (default: PORTRAIT)}
\item{filestem}{String to use as the filename, an appropriate extension is appended automatically (default: test)}
\item{append_datetime}{ (default: FALSE)}
\item{type}{One of the valid SetPage file types (default: GUI)}
\item{expandDataLoading}{Instead of using FormatForest, expand the details of how the ForestBasic object is being created (default: FALSE)}
\item{StopIfCodeExists}{If the code from a previous run would be overwritten, then halt. Avoids accidental deletion of edits (default: TRUE)}
\item{mainfont}{Font scaling applied to cex (default: 1)}
\item{optimise.fontsize}{Automatically choose the largest font size such that the plot as a whole fits on the page (default: TRUE)}
\item{auto.shrink.font}{If mainfont is too large, then automatically reduce it to the optimal (default: TRUE)}
\item{optimise.line.spacing}{Select the top and bottom margins of the panel automatically such that a visually optimal line spacing is used. Useful for squashing forests with only a few rows (default: TRUE)}
\item{line.spacing}{The target line spacing (default: 1.25)}
\item{vertical.alignment}{Align the forest at the top, middle or bottom of the page (default: centre)}
\item{LogScale}{X axis should be on the log scale (default: TRUE)}
\item{ExponentiateDataOnPlot}{The contents of the data table should be exponentiated when plotted. For instance, if the data contains log RR but you want to plot RR (default: FALSE)}
\item{DataLogged}{Deprecated; use ExponentiateDataOnPlot instead (default: NULL)}
\item{EstimateCol}{Vector of CSV column names that provide point estimates (optional, overrides CSV column names) (default: NULL)}
\item{StdErrCol}{Vector of CSV column names that provide point standard errors (optional, overrides CSV column names) (default: NULL)}
\item{LCICol}{Vector of CSV column names that provide point lower confidence bounds (optional, overrides CSV column names) (default: NULL)}
\item{UCICol}{Vector of CSV column names that provide point upper confidence bounds (optional, overrides CSV column names) (default: NULL)}
\item{FormatPValue}{Instead of providing ColHeadings in the CSV file, provide a vector of CSV column names here to be formatted as P value (default: NULL)}
\item{pvalue.use.lower.limit}{ (default: FALSE)}
\item{pvalue.lower.limit}{ (default: 1e-100)}
\item{pvalue.use.upper.limit}{ (default: FALSE)}
\item{pvalue.upper.limit}{ (default: 0.01)}
\item{ColHeadings}{Instead of providing ColHeadings in the CSV file, provide a vector of strings here, in the format "<CSV colname>=<print name>" (default: NULL)}
\item{ColDecimalPlaces}{ (default: NULL)}
\item{plot.width}{Percentage of the page width to use for the drawing area of each forest. Applies to each forest. (default: 30)}
\item{font}{Specify a font name to use on the whole page (default: NULL)}
\item{forest.Range}{Deprecated, use forest.xlim (default: NULL)}
\item{forest.xlim}{X axis limits for each forest. Use a list to give different limits for each forest (default: NULL)}
\item{forest.xticks}{X axis ticks for each forest. Use a list to give different ticks for each forest (default: NULL)}
\item{forest.hideticklabs}{ (default: FALSE)}
\item{forest.title}{Vector of titles to be placed centrally above each forest}
\item{forest.titleY}{Y axis coordinate to align forest titles on (default: NULL)}
\item{forest.title.axiscentred}{If TRUE, centre the title over the axis part only. If FALSE then centre over axis and estimate labels (default: FALSE)}
\item{ylabels.offset}{ (default: 0)}
\item{lwd}{Line width (scaling) (default: 1)}
\item{xlab}{X axis label}
\item{mar}{Vector of margins, in order c(bottom, left, top, right) (default: c(7, 2, 7, 2))}
\item{blank.labels.font}{Font style to use for labels that occur in blank rows of the forest. 1 is normal, 2 is bold, 3 is italic, 4 is bold italic. (default: 2)}
\item{ValueLabels}{Add the ValueLabels column to the right of each forest (default: TRUE)}
\item{ValueLabelsHeader}{Heading to be placed above the ValueLabels column}
\item{ValueDigits}{Number of digits to print after the decimal point. Either, a single number used globally, or a vector of numbers, 1 per forest (default: NULL)}
\item{HetTrendScale}{Font scaling applied to any heterogeneity or trend tests (default: 0.8)}
\item{boxparm.stderr}{Provide the value of the smallest stderr to be used with computing the boxparm, instead of detecting it automatically.  (default: NULL)}
\item{boxparm.sharedstderr}{If TRUE, share box scaling information across all forests. If FALSE the scaling is determined on a per-forest basis. (default: TRUE)}
\item{boxsizeoverride}{Disable scaling of boxes by inverse of variance (default: FALSE)}
\item{boxTop}{ (default: FALSE)}
\item{blank.right.percent}{Percentage of the whole page to leave empty at the right. Useful for squashing figures into the top left (default: NULL)}
\item{blank.bottom.percent}{Percentage of the whole page to leave empty at the bottom. (default: NULL)}
\item{titlespace}{Proportion of the page to allocate to a title area at the top of the page (default: 0.1)}
\item{footerspace}{Proportion of the page to allocate to a footer area at the bottom of the page (default: 0.1)}
\item{footer.title}{Plot title to place at the bottom of the page above the standard footer. Supplying a vector of titles places each element on a new line (default: NULL)}
\item{footer.title.cex}{Footer title scaling (default: NULL)}
\item{mainTitle}{Main title to be added in the page title space (default: NULL)}
\item{pointGroupsFactor}{ (default: 0.9)}
\item{additionalR}{Filename of further R code to be executed after the forest has been drawn but before the file closes. (default: NULL)}
\item{suppress.date}{If TRUE, do not print the datetime or folder information at the bottom left of the page (default: FALSE)}
\item{fixLabelsY}{ (default: FALSE)}
\item{attempt_adobe_kill}{ (default: TRUE)}
}
\author{Matt Arnold}
