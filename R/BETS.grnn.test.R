#' @title  xxxx
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
#' @import grnn forecast


BETS.grnn.test = function(results, test.set, select = TRUE){
  
  if(length(test) < 2 || !check.series(test, "Series list: test.")){
    return(NULL)
  }
  
  test.n_elem = length(test.set[[1]])
  test.n_series = length(test.set) 
  actual = test.set[[1]]
  
  test_mt = matrix(nrow = test.n_elem, ncol = test.n_series)
  
  for(i in 1:test.n_series){
    test_mt[,i] = test.set[[i]]
  }
  
  res = vector(mode = "list")
  res$mape = 1.797693e+308
  
  if(select){
    
    for(i in 1:length(results)){
      
      regs = results[[i]]$regressors
      sub_test = as.matrix(test_mt[,regs])
      
      preds = vector(mode = "numeric")
      
      for(r in 1:nrow(sub_test)){
        preds[r] = guess(results[[i]]$net, t(as.matrix(sub_test[r,])))
      }
      
      if(!any(is.nan(preds))){
        acc = accuracy(preds,actual)[5]
      }
      else{
        acc = res$mape
      }
      
      if(acc < res$mape){
        res$model = results[[i]]$net
        res$mape = acc 
        res$id = results[[i]]$id
        res$sigma = results[[i]]$sigma
        res$mean = ts(preds,start = start(actual), end=end(actual), frequency = frequency(actual))
        res$x = results[[i]]$series
        res$fitted = results[[i]]$fitted
        res$actual = actual
        res$residuals = results[[i]]$residuals
        res$regressors = results[[i]]$regressors
      }
    }
    
  }
  
  return(res)
}