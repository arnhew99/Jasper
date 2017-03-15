convertUnicode <- function(char.vec) {

	# might need to convert to char in case we get handing a 
	# numeric vector in a column...
	char.vec <- as.character(char.vec)

	# look for the "\\u" string
	has_u <- grepl("\\u", char.vec, fixed=TRUE)
	
	# if there is nothing to do then return the strings unharmed
	if (!any(has_u)) return(char.vec)
	
	# else we need to find the code attached to the strings 
	# and convert it
	
	has_u_index <- which(has_u)
	
	for (j in has_u_index) {
	
		string_split <- strsplit(char.vec[j], "\\u", fixed=TRUE)[[1]]
		
		# so... we might have multiple parts
		for (k in 2:(length(string_split))) {
		
			string_part <- string_split[k]
			
			# the first four are the hex representation of the unicode string
			hex_part <- substr(string_part, 1, 4)
			
			# interpret as hex and convert to integer
			int_part <- strtoi(hex_part, base=16L)
			
			# convert to Unicode
			unicode_part <- intToUtf8(int_part)
			
			# re-paste together the string 
			new_string <- paste0(unicode_part, substr(string_part, 5, nchar(string_part)))
			
			# replace the entry in string_split
			string_split[k] <- new_string
			
		
		}
		
		# replace the char.vec entry
		char.vec[j] <- paste(string_split, collapse="")
		
	
	}
	
	return(char.vec)


}