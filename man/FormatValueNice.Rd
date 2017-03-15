\name{FormatValueNice}
\alias{FormatValueNice}
\title{FormatValueNice}
\description{Format a numeric value for use in text. This function normally outputs the number formatted to 2dp; although it will increase the number of decimal places used if the number is so close to 0 that 2dp is rendered as 0.00.}
\arguments{
\item{z}{Number to format}
\item{zero.check}{Check to see if the resulting displayed value is just 0.00, and increase the number of decimal places if so (default: TRUE)}
\item{digits}{ (default: NULL)}
}
\author{Matt Arnold}
