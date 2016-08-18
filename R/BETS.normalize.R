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
#' 
#' @export


BETS.normalize = function(series, mode){
  
  if(mode == "maxmin"){
    
    if(is.list(series)){
      return(lapply(series, function(x){(x-min(x))/(max(x)-min(x))}))
    }
    else{
      return((series - min(series))/max(series) - min(series))
    }
    
  }
  
  else if(mode == "scale"){
    
    if(is.list(series)){
      return(lapply(series, function(x){(x - mean(x))/sd(x)}))
    }
    else {
      return((series - mean(series))/sd(series))
    }
  }
  
  return("ERROR")
}
