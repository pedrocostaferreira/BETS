#' @title  Perform unit root tests
#' 
#' @description This function uses the package 'urca' to perform unit root tests on a pre-defined time series. Unlike urca functions, it returns a meaningful table summarizing the results. 
#' 
#' @param ... Arguments passed on to urca functions
#' @param mode A \code{character}. The type of the test. For now, only the Augmented Dickey-Fuller test is available.
#' @param level A \code{character}. The confidence level. Can be either '1pct','5pct' or '10pct'
#' 
#' @return A \code{list} object. The first element is a \code{data.frame} with the test statistics, the critical values and the test results. The second, the model residuals. 
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
    
    # If the test statistic is less (this test is non symmetrical so we do 
    # not consider an absolute value) than the (larger negative) critical value, 
    # then the null hypothesis of tau2 = 0 is rejected and no unit root is present.
    
    if(stat[i] > cval[i]){
      res = c(res, "no")
    }
    else {
      res = c(res, "yes")
    }
  }
  
  res = as.matrix(res)
  results = data.frame(statistic = stat,crit.val = cval,  rej.H0 = res)
  return(list(results = results, residuals = df@res))
}