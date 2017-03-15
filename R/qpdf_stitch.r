qpdf_stitch <- function(outfile, filenames, path_to_qpdf) {

	fn_call <- paste0("\"", path_to_qpdf, "\" --empty --stream-data=compress --object-streams=generate --linearize --pages ", paste(paste0(filenames, " 1 "), collapse=" "), " -- ", outfile)
	system(command=fn_call)


}