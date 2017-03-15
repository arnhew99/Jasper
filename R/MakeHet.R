MakeHet <-
function(estim, stderr, hetlist)
   {
   # estim   - point estimates in this plot (vector)
   # stderr  - attached standard errors
   # hetlist - a vector making the heterogeneity 
   #           request (eg. c(3, 4, 5, -5) or c(1, 3, 5, 7)

   # return: RetList, a vector of the heterogeneity, degrees of freedom and the p-value


   # hetlists end in a negative number to compute our own 'total' value 
   # to compare with and plot after the point given by the 
   # negative number's absolute value (so after point 5 above)

   # hetlists end in a positive number to use that point's value as the 'total'
   # value and plot after that point

   # returns a list of heterogeneity test results, which will be (at some point!), I hope, 
   # expressions

   # This code is an implementation of the algorithm from combograph, which so 
   # far as I know works properly.

     # degrees of freedom - number of points, less one, less one for the total
     nfree<-length(hetlist)-2
     nlines<-length(hetlist)

     if (hetlist[nlines]<0)
        {
        # very involved!
        # hetlist[-nlines] is hetlist without its last element
        # estim[hetlist] is the elements of estim indicated by hetlist

        # so we take the sum of (estim/stderr^2), dropping the last element
        # (the last element being the call for the test display position only)

        numerat<-sum(estim[hetlist[-nlines]]/((stderr[hetlist[-nlines]])^2.0))
        denom<-sum(1/((stderr[hetlist[-nlines]])^2.0))
        betahat<-numerat/denom
        } else
        {
        betahat<-estim[hetlist[nlines]]
        }

     # Now the actual calculation: sum of ((estim-betahat)/(stderr))^2
     het<-sum(((estim[hetlist[-nlines]]-betahat)/stderr[hetlist[-nlines]])^2)

     pval <- 1-pchisq(het, nfree)

     RetList<-c(het, nfree, pval)
     
   return(RetList)
   }
