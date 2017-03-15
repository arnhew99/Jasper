WLScoefs <- function(linelist, DataLogged=FALSE) {

	if (is.data.frame(linelist)) linelist <- list(linelist)		
	
	if (DataLogged) logfn <- log else logfn <- function(x) x
	
	return(t(sapply(linelist, function(z) coef(lm(logfn(Estimate) ~ RF_Level, data=z, weights=1/(StdErr^2))))))

}