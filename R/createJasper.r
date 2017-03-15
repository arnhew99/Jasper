createJasper <- function() {
	require(akima)
	perlin2d <- function(n,m) {

		perls <- lapply(c(n),function(i) interp(rep(seq(1,20,length=i),times=i),rep(seq(1,20,length=i),each=i),runif(i^2,-1,1),xo=seq(1,20,length=m),yo=seq(1,20,length=m))$z)
		res <- matrix(0,ncol=m,nrow=m)
		for (i in 1:length(perls)) {

		res <- res + perls[[i]]

		}
		return(res)

	}


	image(perlin2d(c(5,10,20),200),col=heat.colors(50)[1:40], axes=FALSE)

}
