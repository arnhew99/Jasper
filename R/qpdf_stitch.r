qpdf_stitch <- function(outfile, filenames, path_to_qpdf) {

	# find out if the path is a file or a folder
	if (all(file.exists(path_to_qpdf), !dir.exists(path_to_qpdf))) {

		fn_call <- paste0("\"", path_to_qpdf, "\" --empty --stream-data=compress --object-streams=generate --linearize --pages ", paste(paste0("\"", filenames, "\" 1 "), collapse=" "), " -- \"", outfile, "\"")
		system(command=fn_call)
	} else if (dir.exists(path_to_qpdf)) {
	
		fn_call <- paste0("\"", path_to_qpdf, "\\qpdf.exe\" --empty --stream-data=compress --object-streams=generate --linearize --pages ", paste(paste0("\"", filenames, "\" 1 "), collapse=" "), " -- \"", outfile, "\"")
		system(command=fn_call)
		
		
		# gs_call <- paste0("\"", path_to_qpdf, "\\gs\\bin\\gs.exe\" -sDEVICE=pdfwrite -dCompatibilityLevel=1.5 -dPDFSETTINGS=/printer -dNOPAUSE -dBATCH  -dQUIET -sOutputFile=jasper_gs_tmp.pdf ", outfile, " ", outfile)
		# system(command=gs_call)
		
		# if (file.exists("jasper_gs_tmp.pdf")) {
			# file.remove(outfile)
			# file.rename("jasper_gs_tmp.pdf", outfile)
		# }
		
	} else {
	
		stop("Cannot find qpdf")
	
	}
	



}

gs_stitch <- function(outfile, filenames, path_to_gs) {

	if (dir.exists(path_to_gs)) {
	
		# fn_call <- paste0("\"", path_to_qpdf, "\\qpdf.exe\" --empty --stream-data=compress --object-streams=generate --linearize --pages ", paste(paste0("\"", filenames, "\" 1 "), collapse=" "), " -- \"", outfile, "\"")
		# system(command=fn_call)
		
		rint <- sample.int(1e8, size=1)
		tmpfilename <- paste0("jasper_gs_tmp_", rint, ".pdf")
		
		gs_call <- paste0("\"", path_to_gs, "\\bin\\gs.exe\" -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dNOPAUSE -dBATCH -dQUIET -dAutoRotatePages=/None -sOutputFile=", tmpfilename, " ", paste0(paste0("\"", filenames, "\""), collapse=" "))
		
		print(gs_call)
		system(command=gs_call)		
		
		if (file.exists(tmpfilename)) {
			if (file.exists(outfile)) file.remove(outfile)
			file.rename(tmpfilename, outfile)
		}
		
	} else {
	
		stop("Cannot find gs")
	
	}
	



}