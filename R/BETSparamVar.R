#' @title  BETS.paramVAR
#'  
#' @description 
#'  
#' @usage BETS.paramVaR(x , volume, conf.level )
#'  
#'  
#'  
#'  @param x
#'  @param volume
#'  @param conf.level
#'  
#'  
#'  @import rugarch



BETS.paramVaR = function(x, volume, conf.level){
  require(rugarch)  
  if(typeof(x)!="S4")stop("Erro, the parameter x must been a garch object")        
  
  sig.level=1-conf.level
  z = qnorm(sig.level,lower.tail = TRUE)
  z
    invisible(attach(x@fit))
    #detach(x@fit)
    n = length(coef)
    m_t = 0 
    for(i in 1:n){
      if(substr(names(coef[i]),1,2) == "mu")
        m_t=m_t
      if(substr(names(coef[i]),1,2) == "ar"){
       m_t=m_t + coef[i] 
      }
    }
    
    invisible(detach(x@fit))
    
    invisible(attach(x@model))
    m_t = m_t*modeldata$data[length(modeldata$data)]
   invisible(detach(x@model))
    invisible(attach(x@fit))
    h_t = 0
    
    for(i in 1:n){
      if(substr(names(coef[i]),1,5)=="omega"){h_t=h_t+h_t}
    if(substr(names(coef[i]),1,5)=="alpha"){
      h_t = h_t + ( coef[i]*residuals[length(residuals)])
    }
      if(substr(names(coef[i]),1,4)=="beta"){
        h_t = h_t + (coef[i]*var[length(var)])
          }
    }
    h_t
    r_ast <- m_t + z*sqrt(h_t)
    return((r_ast/100)*volume)
}


