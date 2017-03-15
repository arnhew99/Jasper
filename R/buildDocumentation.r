buildDocumentation <- function(target) {

	# this should probably be run from a fresh copy of R
	indextext <- "<html><head><title>Jasper version 2</title><link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\"></head><h1>Jasper</h1></br>"
	
	
	# load the standard varnames 
	std_vars <- read.csv(paste(target, "standard_varnames.csv", sep=""), as.is=TRUE)
	
	# load a reasonably current idea of what the "main" functions are and what the supplementary ones are
	# if a function turns up that isn't in that list then add it to an "other" section
	fn_types <- read.csv(paste(target, "function_types.csv", sep=""), as.is=TRUE)
	per_fn_args <- read.csv(paste(target, "per_fn_args.csv", sep=""), as.is=TRUE) 
	
	get_fn_arg <- function(fnname, argname, df) {
		row <- intersect(which(df$fun == fnname), which(df$argname == argname))
		if (length(row) > 0) return(df$desc[row]) else return("")
	}
	
	mainfns <- c()
	extrafns <- c()
	otherfns <- c()
	
	for (i in ls(name=.GlobalEnv)) {
	
		if (is.function(eval(parse(text=i)))) {
		
			htmltext <- paste("<html><html><head><title>Jasper version 2 - ", i, "</title><link rel=\"stylesheet\" type=\"text/css\" href=\"../style.css\"></head><h1>", i, "</h1>", sep="")
			
			# work out if the function is a main/extra/other function
			# if it's a main function then it should also have a description
			if (i %in% fn_types$name) {
				if (fn_types$main[which(fn_types$name == i)] == 1) mainfns <- c(mainfns, i) else extrafns <- c(extrafns, i)
				fn_desc <- fn_types$desc[which(fn_types$name == i)]
			} else {
				otherfns <- c(otherfns, i)
				fn_desc <- ""
			}
			
			
			htmltext <- paste(htmltext, "<p>", fn_desc, "<p>", sep="")
			
			# get the arguments of the current function
			fn.args <- formals(i)
			
			# abort if there are no arguments 
			if (length(fn.args) == 0) {
				htmltext <- paste(htmltext, "<p><a href='../index.html'>Back to main listing</a></html>", sep="")
				con <- file(paste(target, "\\functions\\", i, ".html", sep=""), open="wt")
				writeLines(text=htmltext, con=con)
				close(con)
				next
			}
			
			# get the description of any standard arguments
			descs <- rep("", length(fn.args))
			for (j in names(fn.args)) descs[which(names(fn.args)==j)] <- paste(ifelse(j %in% std_vars$argname, std_vars$desc[std_vars$argname == j], get_fn_arg(i, j, per_fn_args)), sep="")
			
			argstext <- paste("<b>", names(fn.args), "</b>", sapply(fn.args, function(z) ifelse(is.null(z), " = ", ifelse(z == "", " ", " = "))), fn.args, " : ", descs, "</br>", sep="")
			argstext <- paste(argstext, collapse="")
			
			htmltext <- paste(htmltext, argstext, "<p><a href='../index.html'>Back to main listing</a></html>", sep="")
	
			# write out the function HTML file
			con <- file(paste(target, "\\functions\\", i, ".html", sep=""), open="wt")
			writeLines(text=htmltext, con=con)
			close(con)
			

			
		}	
	}

	
	# update the index file text 
	indextext <- paste(indextext, "<b>Main functions</b></br>", paste(paste("<a href='functions/", mainfns, ".html'>", mainfns, "</a></br>", sep=""),collapse=""), sep="")
	indextext <- paste(indextext, "<p><b>Helper functions</b></br>", paste(paste("<a href='functions/", extrafns, ".html'>", extrafns, "</a></br>", sep=""),collapse=""), sep="")
	indextext <- paste(indextext, "<p><b>Other functions</b></br>", paste(paste("<a href='functions/", otherfns, ".html'>", otherfns, "</a></br>", sep=""),collapse=""), sep="")
	
	# write out the index file
	indextext <- paste(indextext, "</html>", sep="")
	con <- file(paste(target, "index.html", sep=""), open="wt")
	writeLines(text=indextext, con=con)
	close(con)
}

# buildDocumentation(target="K:\\NDPHopen\\Stats User Area\\Jasper\\jasper\\documentation\\")
