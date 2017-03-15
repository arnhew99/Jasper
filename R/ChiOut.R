ChiOut <-
function(chi, nfree, pval, IsHet=TRUE)
   {
   # chi - chi-squared value
   # nfree - number of degrees of freedom
   # pval  - p-value
   # IsHet - is this heterogeneity (if not, assume trend)

   # Using plotmath here because a UTF-8 chi looks wrong

   # chisq to 1 d.p. 
#   chitext<- prettyNum(chi, format="fg", digits=1)
   chitext<- formatC(chi, format="f", digits=1)

   if (pval>=0.1) ptext<-paste(" (p=", prettyNum(pval, format="fg", digits=1), ")", sep="")
   if (pval<0.1&&pval>=0.01) ptext<-paste(" (p=", prettyNum(pval, format="fg", digits=1), ")", sep="")
   if (pval<0.01&&pval>=0.001) ptext<-paste(" (p=", prettyNum(pval, format="fg", digits=1), ")", sep="")
   if (pval<0.001) ptext<-" (p<0.001)"
 
   if (IsHet==TRUE) chiout<-bquote(paste("Heterogeneity test: ", {chi^2} [.(nfree)]==.(chitext), " ", .(ptext))) else
                    chiout<-bquote(paste("Trend test: ", {chi^2} [.(nfree)]==.(chitext), " ", .(ptext)))

   return(chiout)
}
