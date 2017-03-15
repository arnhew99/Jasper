myindex <-
function(string, pattern)
   {
   return(as.vector(gregexpr(pattern, string, fixed=TRUE)[[1]]))
   # returns -1 if pattern not within string.  Does not capture occurrences of
   # pattern that begin within previous occurrences of pattern (eg. second
   # occurrence of bab within ababababababab starts at character 6, not character
   # 4
   }
