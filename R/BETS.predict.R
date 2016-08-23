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
#' @import forecast dygraphs

BETS.predict = function(..., actual = NULL, main = "", ylab = "", xlim = NULL, style = "dygraphs", knit = F){
  
  l = list(...)
  
  if(is.null(l$object)){
    if(is.null(l$nn)){
      model = l[[1]]
    }
  }
  
  if(class(model) == "arima" || class(model) == "Arima"){
    preds = forecast(...)
  }
  else {
    
    m = vector(mode = "numeric")
    preds = vector(mode = "list")
    
    for(r in 1:nrow(sub_test)){
      m[r] = guess(l[[1]], t(as.matrix(l[[2]][r,])))
    }
    
    preds$mean = ts(p, start = start(actual), end = end(actual), frequency = frequency(actual))

  }
  
 
  
  if(style == "dygraphs"){
    
    dt = as.ts(cbind(fit = preds$mean, upr =  preds$upper[,2], lwr = preds$lower[,2]))
    
    if(is.null(actual)){
      dt = cbind(hist = preds$x, md = dt)
      
      p = dygraph(dt, main = main) %>%
        dySeries("hist", label = "Actual") %>%
        dySeries(c("md.lwr", "md.fit", "md.upr"), label = "Predicted") %>%
        dyRangeSelector(strokeColor = "gray", fillColor = "gray") %>%
        dyAxis("y", label = ylab)
    }
    else {
      dt = cbind(hist = preds$x, md = dt, act = actual)
      
      p = dygraph(dt, main = main) %>%
        dySeries("hist", label = "Series") %>%
        dySeries("act", label = "Actual") %>%
        dySeries(c("md.lwr", "md.fit", "md.upr"), label = "Predicted") %>%
        dyRangeSelector(strokeColor = "gray", fillColor = "gray") %>%
        dyAxis("y", label = ylab)
    }
    
    if(knit == F){
      print(p)
    }
    else {
      return(p)
    }
    
  }
  else if(style == "normal") {

    max = max(c(preds$x,preds$mean)) 
    max = ceiling(max + 0.1*max)
    min = min(c(preds$x, preds$mean))
    min = floor(min - 0.1*min)
    step = floor((max - min)/4)
    
    plot(preds, main = main, ylab = ylab, xlim = xlim, yaxt = "n", xaxp  = c(1900, 2500, 600))
    abline(v = seq(1900,2500,1), col = "gray60", lty = 3)
    axis(side = 2, at = seq(min,max,step))
    par(new = TRUE)
    lines(actual, col = "firebrick3", lwd = 2)
    
  }
  
  
  if(!is.null(actual)){
    
    results = vector(mode = "list")
    acc = accuracy(preds$mean, actual)
    results$accuracy = acc 
    results$predictions = preds
  }
  else {
    results = preds
  }
  
  return(invisible(results))
}