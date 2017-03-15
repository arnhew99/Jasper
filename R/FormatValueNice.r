FormatValueNice <- function(z, zero.check=TRUE, digits=NULL) {


	if (is.null(digits)) {
		digits <- floor(log10(abs(z)))

		if (digits >= 2) {
			form.val <- sprintf(paste("%.",0,"f", sep=""), z)
		} else if (digits == 1) {
			form.val <- sprintf(paste("%.",1,"f", sep=""), z)
		} else {
			form.val <- sprintf(paste("%.",2,"f", sep=""), z)
		}
		if (substr(form.val, nchar(form.val), nchar(form.val)) == ".") form.val <- substr(form.val, 1, nchar(form.val) - 1)
		
		# if the value ends up being rendered as 0.00 then expand the number of digits allowed until 
		# the rendered value is no longer == 0
		if (zero.check) {
			if (abs(z) < 1 & z != 0L) {
				i <- 0
				# potentially this could break if the cast from character to numeric ends up being zero
				while ((as.numeric(form.val) == 0L) & i < 10) {
					digits <- 3 + i
					form.val <- sprintf(paste("%.",digits,"f", sep=""), z)
					i <- i + 1
				}
			}
		}
		return(form.val)
	} else {
		# else assume the user knows what they're doing for some reason
		form.val <- sprintf(paste("%.",digits,"f", sep=""), z)
		return(form.val)
	}

}