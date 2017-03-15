\name{SetPage}
\alias{SetPage}
\title{SetPage}
\description{A Jasper plot page is normally an A4 page, with header and footer areas. The remaining area is divided into panels, into which Jasper can plot. This function sets up the page with the required number of panels. Setting type to GUI makes Jasper output into an R window. Such a plot is not saved when R is closed, but this mode is useful for quick prototyping of plots as the results can be seen instantly. The contents of a GUI window can be written to a file using the saveGUI function, although this is not the recommended method for outputting files, because Jasper computes optimal font sizes and spacings based on the type of file needed. File output formats are currently limited to PDF, EMF and SVG. EMF is a vector-based format suitable for use in Microsoft Office documents, and is especially good for posters because it can be made arbitrarily large without any loss of resolution. SVG is suitable for web graphics. PNG may be specified but the renderer is very slow and the results are not resolution-independent. }
\arguments{
\item{orient}{Page orientation, LANDSCAPE or PORTRAIT (default: PORTRAIT)}
\item{perpage}{Number of panels per page, up to 10 (default: NULL)}
\item{nlong}{Override perpage by specifying the number of panels along the long axis. nshort must also be supplied (default: NULL)}
\item{nshort}{See nlong. (default: NULL)}
\item{type}{One of PDF, EMF, SVG or GUI (default: GUI)}
\item{attempt_adobe_kill}{ (default: TRUE)}
\item{png_bg}{ (default: transparent)}
\item{filestem}{String to use as the filename, an appropriate extension is appended automatically (default: jasper_output)}
\item{label}{Plot label text to display at bottom left (possibly deprecated) (default: NULL)}
\item{font}{ (default: NULL)}
\item{footer.title}{Plot title to place at the bottom of the page above the standard footer. Supplying a vector of titles places each element on a new line (default: NULL)}
\item{footer.title.cex}{Footer title scaling (default: 1.5)}
\item{footer.title.allbold}{By default, only the first element of the footer.title vector is bolded; setting this option to TRUE makes every element bold (default: FALSE)}
\item{display.layout}{Show the computed panel layout (default: FALSE)}
\item{titlespace}{Proportion of the page to allocate to a title area at the top of the page (default: 0.1)}
\item{footerspace}{Proportion of the page to allocate to a footer area at the bottom of the page (default: 0.1)}
\item{width}{Width of panels as fraction of default  (default: 1)}
\item{height}{Height of panels as fraction of default (default: 1)}
\item{footer}{Additional text to display in the footer, along with the date and current working directory}
\item{footer.cex}{Footer text scaling (default: 1.3)}
\item{page_height}{ (default: NULL)}
\item{page_width}{ (default: NULL)}
\item{page_dpi}{ (default: NULL)}
\item{page_pointsize}{ (default: NULL)}
\item{suppress.date}{Turn off footer information (default: FALSE)}
\item{NoLabel}{deprecated (default: TRUE)}
\item{ckb.participants}{If npeople.csv exists in the working directory, print it in the footer (default TRUE) (default: TRUE)}
\item{verbose}{Output information about layout calculations (default TRUE) (default: TRUE)}
\item{blank.left.percent}{ (default: NULL)}
\item{blank.right.percent}{Percentage of the whole page to leave empty at the right. Useful for squashing figures into the top left (default: NULL)}
\item{blank.bottom.percent}{Percentage of the whole page to leave empty at the bottom. (default: NULL)}
\item{vertical.scales}{Vector of relative scalings to allocate space for panels vertically (can be used to make 1 SD on the Y-axis the same paper length in each panel)  (default: NULL)}
\item{ncols}{Number of columns of panels if vertical.scales is being used (default: NULL)}
\item{show.internal.layout}{Display the layout matrix  (default: FALSE)}
\item{advanced}{Switch to manually set the layout matrix (default FALSE). Required TRUE if the next 3 options are to be used (default: FALSE)}
\item{mat}{Layout matrix to use (advanced=TRUE and values for widthvec and heightvec also required) (default: NULL)}
\item{widthvec}{Proportional widths for panels (default: NULL)}
\item{heightvec}{Proportional heights for panels (default: NULL)}
\item{unload.devices}{ (default: TRUE)}
}
\author{Matt Arnold}
