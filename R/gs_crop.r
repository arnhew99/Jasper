gs_crop <- function(filename, outfile, path_to_gs, add_margin=NULL, inflate_to_a4=FALSE) {

	if (dir.exists(path_to_gs)) {
	
		# fn_call <- paste0("\"", path_to_qpdf, "\\qpdf.exe\" --empty --stream-data=compress --object-streams=generate --linearize --pages ", paste(paste0("\"", filenames, "\" 1 "), collapse=" "), " -- \"", outfile, "\"")
		# system(command=fn_call)
		
		rint <- sample.int(1e8, size=1)
		tmpfilename <- paste0("jasper_gs_tmp_", rint, "_", Sys.getpid(), ".pdf")
		
		# gs_call <- paste0("\"", path_to_gs, "\\bin\\gs.exe\" -sDEVICE=bbox -q -dBATCH -dNOPAUSE ", paste0("\"", filename, "\""), collapse=" ")
		
		# print(gs_call)
		# system(command=gs_call)				
		
		dims <- system2(command=paste0(path_to_gs, "\\bin\\gs.exe"), args=paste0("-sDEVICE=bbox -q -dBATCH -dNOPAUSE \"", filename, "\""), stderr=TRUE, wait=TRUE)
		
		# page_size <- system2(command=paste0(path_to_gs, "\\bin\\gs.exe"), args=paste0("-dNODISPLAY -q -sFILE=\"", filename, "\" -dDumpMediaSizes \"", paste0(path_to_gs, "\\toolbin\\pdf_info.ps"), "\""), stdout=TRUE, wait=TRUE)
		page_size <- system2(command=paste0(path_to_gs, "\\..\\pdfinfo"), args=paste0("-box \"", filename, "\""), stdout=TRUE, wait=TRUE)
		
		mediabox <- strsplit(page_size[grepl("MediaBox", page_size)], split=" ")[[1]][-1]
		mediabox <- mediabox[mediabox != ""]
		mediabox <- as.numeric(mediabox)
		print("mediabox")
		print(mediabox)
		
		orientation <- ifelse(as.numeric(mediabox[3]) < as.numeric(mediabox[4]), "PORTRAIT", "LANDSCAPE")
		cat(paste0("current plot is ", orientation, "\n"))
		flush.console()
		
		
		# if (file.exists(tmpfilename)) {
			# if (file.exists(outfile)) file.remove(outfile)
			# file.rename(tmpfilename, outfile)
		# }
		
		edges <- as.numeric(strsplit(dims[2], split=" ", fixed=TRUE)[[1]][2:5])
		print("bounding box")
		print(edges)
		
		new_width <- as.numeric(mediabox[3]) - as.numeric(edges)[1] - (as.numeric(mediabox[3]) - as.numeric(edges[3]))
		print(new_width)
		new_width_with_margin <- new_width + (add_margin*72/2.52)
		
		new_height <- as.numeric(mediabox[4]) - as.numeric(edges)[2] - (as.numeric(mediabox[4]) - as.numeric(edges[4]))
		print(new_height)
		new_height_with_margin <- new_height + (add_margin*72/2.52)
		
		# if (inflate_to_a4) {
		
			# new_width_with_margin <- max(mediabox[3], mediabox[3] + (add_margin*72/2.52))
			# new_height_with_margin <- max(mediabox[4], mediabox[4] + (add_margin*72/2.52))
		
		# }
		
		crop <- system2(command=paste0(path_to_gs, "\\bin\\gs.exe"), args=paste0("-o \"", tmpfilename, "\" -sDEVICE=pdfwrite -q -dBATCH -dNOPAUSE -c \"<</PageOffset [-", edges[1], " -", edges[2], "]>> setpagedevice\" -f \"", filename, "\""), stderr=TRUE, wait=TRUE)
		
		update_files <- function() {
			if (file.exists(tmpfilename)) {
				if (file.exists(outfile)) file.remove(outfile)
				file.rename(tmpfilename, outfile)
			}
		}
		update_files()
		
		
		cropargs <- paste0("-o \"", tmpfilename, "\" -sDEVICE=pdfwrite -dFIXEDMEDIA -dDEVICEWIDTHPOINTS=", new_width, " -dDEVICEHEIGHTPOINTS=", new_height, " \"", outfile, "\"")
		
		print(cropargs)
		
		crop <- system2(command=paste0(path_to_gs, "\\bin\\gs.exe"), args=cropargs, stderr=TRUE, wait=TRUE)
		
		print(crop)
		
		update_files()

		if (!is.null(add_margin)) {
		
		
			cropargs <- paste0("-o \"", tmpfilename, "\" -sDEVICE=pdfwrite -dFIXEDMEDIA -dDEVICEWIDTHPOINTS=", new_width_with_margin, " -dDEVICEHEIGHTPOINTS=", new_height_with_margin, " \"", outfile, "\"")
			crop <- system2(command=paste0(path_to_gs, "\\bin\\gs.exe"), args=cropargs, stderr=TRUE, wait=TRUE)
			update_files()
			print(crop)
			

			
			
			if (inflate_to_a4) {
			
				width_offset <- max(round(0.5*add_margin*72/2.52,3), (new_width_with_margin - (edges[3] - edges[1]))/2)
				height_offset <- max(round(0.5*add_margin*72/2.52,3), (new_height_with_margin - (edges[4] - edges[2]))/2)
			
				crop <- system2(command=paste0(path_to_gs, "\\bin\\gs.exe"), args=paste0("-o \"", tmpfilename, "\" -sDEVICE=pdfwrite -q -dBATCH -dNOPAUSE -c \"<</PageOffset [", width_offset, " ", height_offset, "]>> setpagedevice\" -f \"", outfile, "\""), stderr=TRUE, wait=TRUE)
				update_files()
				print(crop)
				
				# cropargs <- paste0("-o \"", tmpfilename, "\" -sDEVICE=pdfwrite -sPAPERSIZE=a4 -dPDFFitPage \"", outfile, "\"")
				cropargs <- paste0("-o \"", tmpfilename, "\" -sDEVICE=pdfwrite -dFIXEDMEDIA -dDEVICEWIDTHPOINTS=", ifelse(orientation=="LANDSCAPE",841,594), " -dDEVICEHEIGHTPOINTS=", ifelse(orientation=="LANDSCAPE",594,841), " -dPDFFitPage \"", outfile, "\"")
				crop <- system2(command=paste0(path_to_gs, "\\bin\\gs.exe"), args=cropargs, stderr=TRUE, wait=TRUE)
				update_files()
				print(crop)
			} else {
		
				crop <- system2(command=paste0(path_to_gs, "\\bin\\gs.exe"), args=paste0("-o \"", tmpfilename, "\" -sDEVICE=pdfwrite -q -dBATCH -dNOPAUSE -c \"<</PageOffset [", round(0.5*add_margin*72/2.52,3), " ", round(0.5*add_margin*72/2.52,3), "]>> setpagedevice\" -f \"", outfile, "\""), stderr=TRUE, wait=TRUE)
				update_files()
				print(crop)
		
			}

			
		}
		
		
		return(dims)
		
	} else {
	
		stop("Cannot find gs")
	
	}
	



}

# source("K:\\NDPHopen\\Stats user area\\Jasper\\jasper\\load_dev.r")
# setwd("K:\\isise\\CKB Topics\\CIMT\\Code\\plots\\Ischaemic stroke\\Carotid by age")
# gs_path <- "K:\\NDPHopen\\Stats User Area\\Jasper\\jasper\\qpdf\\gs"
# gs_crop(filename="Carotid age stroke associations.pdf", outfile="test.pdf", path_to_gs=gs_path, add_margin=2, inflate_to_a4=TRUE)


# source("K:\\NDPHopen\\Stats user area\\Jasper\\jasper\\load_dev.r")
# setwd("K:\\isise\\CKB Topics\\CIMT\\Code\\plots\\Ischaemic stroke\\Carotid SBP B-nonHDL-C increasing adj\\PDF")
# gs_path <- "K:\\NDPHopen\\Stats User Area\\Jasper\\jasper\\qpdf\\gs"
# gs_crop(filename="eFig risk factors nonadj stroke.pdf", outfile="test.pdf", path_to_gs=gs_path, add_margin=1, inflate_to_a4=TRUE)



# con <- file("Carotid age stroke associations.pdf", encoding="latin1", open="rt")
# contents <- readLines(con, skipNul=TRUE)
# # print(readLines(con, n=1, skipNul=TRUE))
# close(con)


