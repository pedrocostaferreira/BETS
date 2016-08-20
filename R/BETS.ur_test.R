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
#' @import urca

BETS.ur_test = function(..., mode = "DF", level = "5pct"){
  
  df <- ur.df(...)
  cval = as.matrix(df@cval[,level])
  stat = t(df@teststat)
  res = vector(mode = "logical")
  
  for(i in 1:length(stat)){
    
    if(stat[i] > cval[i]){
      res = c(res, "TRUE")
    }
    else {
      res = c(res, "FALSE")
    }
  }
  
  res = as.matrix(res)
  results = data.frame(statistic = stat,crit.val = cval, result = res)
  return(list(results = results, residuals = df@res))
}