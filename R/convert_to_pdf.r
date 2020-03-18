convert_to_pdf <- function(filename, path_to_libreoffice="K:\\NDPHopen\\Stats User Area\\Jasper\\jasper\\libreoffice\\program") {

	lo_call <- paste0("\"", path_to_libreoffice, "\\soffice\" --headless --invisible --norestore --nolockcheck --convert-to pdf \"", filename, "\"")
		
	system(command=lo_call, ignore.stdout=TRUE, show.output.on.console=FALSE)		

}		

# setwd("K:\\isise\\CKB Topics\\CIMT\\Carotid IMT and body size\\Paper\\Tables")
# convert_to_pdf("Supp Table 3.rtf")