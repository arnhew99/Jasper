Lancet_cols <- function(transparency="ff") {

	# Lancet Oncology colours as per `ggsci' package: Xiao and Li, 2017

	cols <- paste0(c("#00468b","#ed0000","#42b540","#0099b4", "#925e9f","#fdaf91","#ad002a","#adb6b6","#1b1919"), tolower(transparency))
	return(cols)


}