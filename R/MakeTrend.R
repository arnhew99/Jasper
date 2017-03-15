MakeTrend <-
function(estim, stderr, trendlist, d=seq(1, length(trendlist)))
   {
   # Same variable definitions and return
   # No fussing about with total lines here, no totals needed for this calculation
   # only one degree of freedom for all trend tests

   # And not trying quite so much 'clever' stuff with the subscripting either
   
   # This is an ordinal trend test by default- d is simply a count of the number of points, not an 'x-coordinate' 
   # but if necessary, d can be supplied
   
     nfree<-1
     
     # extract out the bits we want for this trend test
     beta<-estim[trendlist]
     se<-stderr[trendlist]
     
     w<-1/(se^2)

     dw<-d*w
     dbar<-sum(dw)/sum(w)

     # next intermediate: w*beta*(d-dbar)
     inter1<-w*beta*(d-dbar)
     # then w*((d-dbar)^2)
     inter2<-w*((d-dbar)^2)

     # trend is then (sum(inter1))^2 / sum(inter2)
     trend<-((sum(inter1))^2) / sum(inter2)

     pval <- 1-pchisq(trend, nfree)

     RetList<-c(trend, nfree, pval)
     
   return(RetList)
   }
