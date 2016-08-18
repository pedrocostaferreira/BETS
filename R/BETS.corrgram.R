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
#' @importFrom plotly plotly_build
#' @import forecast ggplot2 

BETS.corrgram = function(ts, lag.max = 12, type = "correlation", mode = "simple", ci = 0.95, style = "plotly", knit = F){
  
  ## Validation
  
  if(!is.numeric(ci) || ci <= 0 || ci >= 1){
    stop("Parameter 'ci' (confidence interval) must be a real number between 0+ and 1-")
  }
  
  if(type != "correlation" && type != "partial"){
    stop("Unknown value for parameter 'type'")
  }
  
  if(mode != "simple" && mode != "bartlett"){
    stop("Unknown value for parameter 'mode'")
  }
  
  if(!is.integer(lag.max)){
    if(is.numeric(lag.max)){
      lag.max = round(lag.max)
    }
    else{
      stop("Parameter 'lag.max' must be an integer")
    }
  }
  
  z = qnorm(ci, 0, 1)
  
  if(type == "correlation"){
    out <- forecast::Acf(ts, plot=F, lag.max = lag.max)
    yaxis = "Correlation"
    corrs <- out$acf[-1, , ]
    lags = out$lag[-1, , ]
  }
  else {
    out <- forecast::Pacf(ts, plot=F, lag.max = lag.max)
    yaxis = "Partial Correlation"
    corrs <- out$acf[ , , ]
    lags = out$lag[ , , ] + 1
  }
  
  step = frequency(ts)
  lim = lag.max - lag.max%%step
  ticks = seq(0,lim,step)
  ticks[1] = 1
  
  var <- vector(mode = "numeric")
  
  N = length(ts)
  var[1] = 1/sqrt(N)
  sum = 0  
  
  if(mode == "bartlett"){
    
    for(i in 2:length(lags)){
      for(j in 1:(i-1)){
        sum = sum + (var[j])^2
      }
      var[i] = sqrt((1 + 2*sum)/N)
    }
    
  } 
  else {
    
    for(i in 2:length(lags)){
      var[i] = var[1]
    }
  }
  
  var = z*var
  data <- as.data.frame(cbind(lags,corrs,var))
  
  gp <- ggplot(data, aes(x = lags, y= corrs)) +
    geom_segment(aes(x=lags, xend=lags, y=0, yend=corrs), data=data, size = 0.5) +
    scale_x_continuous(breaks = ticks) + 
    geom_point(size = 0.5) +
    geom_step(data=data, mapping=aes(x=lags, y=var), color="red", linetype="dashed", size=0.3) +
    geom_step(data=data, mapping=aes(x=lags, y=-var), color="red", linetype="dashed", size=0.3) + 
    labs(list(x="Lag", y= yaxis))
  
  if(style == "plotly"){
    p <- plotly_build(gp)
    
    p$data[[1]]$text <- paste("Lag:", lags)
    p$data[[2]]$text <- paste0(yaxis, ": ", round(corrs,3), " <br> Lag: ", lags)
    p$data[[3]]$text <- paste("CI upper bound:", round(var,3))
    p$data[[4]]$text <- paste("CI lower bound:", -round(var,3))
   
    if(knit){
      return(p)
    }
    else{
      print(p)
    }
  }
  else {
    p <- gp 
    
    if(knit){
      return(p)
    }
    else{
      plot(p)
    }
  }
  
  return(invisible(corrs))
}






