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
  
  lm = forecast(...)
  
  if(style == "dygraphs"){
    
    dt = as.ts(cbind(fit = lm$mean, upr =  lm$upper[,2], lwr = lm$lower[,2]))
    
    if(is.null(actual)){
      dt = cbind(hist = lm$x, md = dt)
      
      p = dygraph(dt, main = main) %>%
        dySeries("hist", label = "Actual") %>%
        dySeries(c("md.lwr", "md.fit", "md.upr"), label = "Predicted") %>%
        dyRangeSelector(strokeColor = "gray", fillColor = "gray") %>%
        dyAxis("y", label = ylab)
    }
    else {
      dt = cbind(hist = lm$x, md = dt, act = actual)
      
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
  else {

    max = max(c(lm$x,lm$mean)) 
    max = ceiling(max + 0.1*max)
    min = min(c(lm$x, lm$mean))
    min = floor(min - 0.1*min)
    step = floor((max - min)/4)
    
    plot(lm, main = main, ylab = ylab, xlim = xlim, yaxt = "n", xaxp  = c(1900, 2500, 600))
    abline(v = seq(1900,2500,1), col = "gray60", lty = 3)
    axis(side = 2, at = seq(min,max,step))
    par(new = TRUE)
    lines(actual, col = "firebrick3", lwd = 2)
    
  }
  
  
  if(!is.null(actual)){
    
    results = vector(mode = "list")
    acc = accuracy(lm$mean, actual)
    results$accuracy = acc 
    results$predictions = lm
  }
  else {
    results = lm
  }
  
  return(invisible(results))
}