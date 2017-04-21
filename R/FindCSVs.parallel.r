FindCSVs.parallel <- function(directory, verbose=TRUE) {

	require(parallel)

	
	file_list <- list.files(directory)
	csv_list <- file_list[toupper(substr(file_list, nchar(file_list)-3, nchar(file_list))) == ".CSV"]

	# check that we actually have something to do
	if (length(csv_list) == 0) stop(paste0("No CSV files found in directory: ", directory))


	curwd <- getwd()

	cl <- makeCluster(min(detectCores()*2, length(csv_list)), methods=FALSE)
	clusterExport(cl, "curwd", envir=environment(NULL))
	clusterExport(cl, "directory", envir=environment(NULL))
	clusterEvalQ(cl, setwd(curwd))
	clusterEvalQ(cl, setwd(directory))
	

	load_csv <- function(path) {

		dat <- read.csv(path, as.is=TRUE)

	}
	dats <- parLapply(cl, csv_list, load_csv)

	stopCluster(cl)
	rm(cl)


	objnames <- substr(csv_list,1,nchar(csv_list)-4)

	# check to see if we have enough data frames to match the number of names
	if (length(objnames) != length(dats)) stop("CSV loading failed.")

	# now we need to assign the objects in the global environment with the right names
	for (i in 1:length(dats)) {

		if (verbose) {
			cat(paste("assigning", objnames[i], "\n"))
			flush.console()
		}
		assign(x=objnames[i], value=dats[[i]], envir=.GlobalEnv)

	}

	# tidy up by making sure the data frame list is deleted 
	# and forcing a garbage collection to free memory
	rm("dats")
	gc()

	return(objnames)


}