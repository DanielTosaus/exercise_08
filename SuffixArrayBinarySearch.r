SuffixArray <- function(s){
  
  S <- strsplit(s, split = "")
  T <- list()
  for(i in (1:(length(S[[1]])+1))){
    if(i<= length(S[[1]])){
      
      T <- c(T, list(paste(S[[1]][i:length(S[[1]])],collapse=' ')))
    }
    else{
      #T <- c(T, list('$')) 
    }
  }
  
  A <- unlist(T)
  index <- order(A)
  Asorted <- A[index]
  print(Asorted)
  return(index)
  
}
S <- "CTAATAATG"
SA <- SuffixArray(S)


InverseSuffixArray <- function(SA){
  
  ISA <- integer(length(SA))
  for(i in (1: length(SA))){
    for(j in (1: length(SA))){
      if(SA[j] == i){
        ISA[i] <- j
      }
    }
  }
  return(ISA)
  
}

ISA <- InverseSuffixArray(SA)

LCPArray <- function(text, SA, ISA){
  
  text <- paste(text, '$', sep = '', collapse = '')
  text <- strsplit(text, split = "")
  n <- length(SA)
  LCP <- integer(n)
  LCP[1] <- -1
  LCP[n + 1] <- -1
  l <- 0
  for(i in (1:n)){
    
    j <- ISA[i]
    if(j > 1){
      k <- SA[j - 1]
      while(identical(text[[1]][k + l], text[[1]][i + l])){
        
        l <- l + 1
      }
      LCP[j] <- l
      l <- max(l-1, 0)
    }
  }
  return(LCP)
  
}
text <- S
LCP <- LCPArray(text, SA, ISA)


BinarySearchSA <- function(pattern, text, SA){

  text <- strsplit(text, split = "")
  minIndex <- 1
  maxIndex <- length(text[[1]])
  
  while(minIndex < maxIndex){
    
    midlIndex <- floor((minIndex + maxIndex) / 2)
    pos <- SA[midlIndex]
    suffix <- paste(text[[1]][pos:length(text[[1]])], sep = '', collapse = '')
    
    if(pattern <= suffix){
      maxIndex <- midlIndex
    }
    else{
      minIndex <- midlIndex + 1
    }
  }
  First <- minIndex
  #print(First)
  maxIndex <- length(text[[1]])
  while(maxIndex > minIndex){
    
    midlIndex <- floor((minIndex + maxIndex) / 2)
    pos <- SA[midlIndex]
    suffix <- paste(text[[1]][pos:length(text[[1]])], sep = '', collapse = '')

    if(suffix <= pattern){
      
      minIndex <- midlIndex + 1
    }
    else{
      
      maxIndex <- midlIndex
    }
  }
  Last <- maxIndex - 1
  print(paste("First", toString(First), "Last", toString(Last)))
  if(Last <- First){
    print("Pattern does not appear in the text")
    return(NULL)
  }
  else{
    return(list(first = First, last = Last))
  }
}

pattern <- "AAT"
text <- "CTAATAATG"
SA <- SuffixArray(text)
Search <- BinarySearchSA(pattern, text, SA)
