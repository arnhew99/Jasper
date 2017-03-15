Tick_Pretty <-
function(TickVals)
   {
   # to make a vector of appropriate labels to feed in as axis tick marks
   # important bit is to see how many decimal places we have to use, wanting to get the same number for each label

   FirstPass<-prettyNum(TickVals)

   MaxDecs<-0
   MaxWid<-max(nchar(FirstPass))

   for (i in 1:length(TickVals))
      {
      PostDec<-strsplit(FirstPass[i], ".", fixed=TRUE)[[1]][2]
      if (is.na(PostDec)==FALSE) MaxDecs<-max(MaxDecs, nchar(PostDec))
      }

   Result<-formatC(TickVals, digits=MaxDecs, width=MaxWid, format="f")
   return(Result)
   }
