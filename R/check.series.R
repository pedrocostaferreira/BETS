#' @title  xxx
#' 
#' @description  xxxx
#' 
#' @param ts xxx
#' @param lag.max xxx
#' @param mode xxx
#' @param ci xxx 
#' 
#' @return xxx
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}


check.series = function(ts){
  
  if(is.list(ts)){
    s = sum(sapply(train, function(x){anyNA(x)}))
    
    if(s != 0){
      msg("There is at least one series with NAs.")
      return(FALSE)
    }
    
    l = length(ts[[1]])
    s = start(ts[[1]])
    e = end(ts[[1]])
    
    for(i in 2:length(ts)){
      
      if(l != length(ts[[i]])){
        msg("Not all series have the same length.")
        return(FALSE)
      }
      
      if(s != start(ts[[i]])){
        msg("Not all series have the same starting period.")
        return(FALSE)
      }
      
      if(e != end(ts[[i]])){
        msg("Not all series have the same ending period.")
        return(FALSE)
      }
    }

  }
  else if(class(ts) == "ts") {
    
    if(anyNA(ts)){
      msg("This series contains NAs.")
      return(FALSE)
    }
  }
  else{
    msg("Argument is not a time series or a list of time series.")
    return(FALSE)
  }
  
  return(TRUE)
}
