Diamonds <-
function(left, right, at, yloc, height, border=NULL, col=NULL)
   {
   for (i in 1:length(left)) 
      {
      x<-c(left[i], at[i], right[i], at[i])
      y<-c(yloc[i], yloc[i]+(height[i]/2), yloc[i], yloc[i]-(height[i]/2))
      polygon(x, y, border=border[i], col=col[i])
      }
   }
