SuffixArray <- function(s){
  
  S <- strsplit(s, split = "")
  T <- list()
  for(i in (1:(length(S[[1]])+1))){
    if(i<= length(S[[1]])){
      
      T <- c(T, list(paste(S[[1]][i:length(S[[1]])],collapse=' ')))
    }
    else{
      T <- c(T, list('$')) 
    }
  }
  
  A <- unlist(T)
  index <- order(A)
  Asorted <- A[index]
  print(Asorted)
  return(index)
  
}
S <- "ABBA"
indexes <- SuffixArray(S)
