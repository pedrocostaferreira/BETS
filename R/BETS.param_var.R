#' @title  BETS.param_var
#'  
#' @description O Value at Risk (VaR) representa uma medida de perda potencial de um ativo
#' ou carteira de ativos sujeita a riscos de mercado como, por exemplo, 
#' flutuacoes de precos, de taxas de juros e de taxas de cambio. A metodologia
#' continua relevante no cenario financeiro, principalmente pela facil 
#' interpretacao do seu resultado e pela nao dependencia de uma distribuicao 
#' especifica para sua aplicacao. Como se trata de um metodo parametrico, pois 
#' envolve a estimacao de parametros. Com o objetivo de facilitar o calculo do 
#' valor em risco, assume-se que a serie de retornos assume uma distribuicao
#' conhecida, como a distribuicao normal. 
#' 
#' Resumidamente, VaR pode ser a pior perda esperada para certo volume 
#' investido, sob condicoes normais de mercado e dentro de pequeno e 
#' determinado nível de confianca.
#' 
#' 
#'  
#' @usage BETS.param_var(x , volume, conf.level )
#'  
#'  
#'  
#' @param x  a GARCH model 
#' @param volume that will be invested
#' @param conf.level confidence level of the interval
#'  
#'  
#'   
#' @examples 
#' 
#' require(quantmod)
#' 
#' getSymbols('PETR4.SA',src='yahoo')
#' PETR4.SA.Close <- PETR4.SA$PETR4.SA.Close
#' r_PETR4 <- dailyReturn(PETR4.SA.Close,type = "log")
#' r_PETR4_pos2010 <- window(r_PETR4, start = "2010-01-01")
#' 
#' garch11.spec = ugarchspec(mean.model = list(armaOrder = c(1,0),include.mean=TRUE), 
#' variance.model = list(garchOrder = c(1,1), model = "sGARCH"))
#' 
#' garch.fit = ugarchfit(garch11.spec, data = r_PETR4_pos2010*100,fit.control=list(scale=TRUE), 
#' distribution.model = "norm")
#' 
#' BETS.param_var(garch.fit,volume = 30000,conf.level=0.90)
#' BETS.param_var(garch.fit,volume = 30000,conf.level=0.95)
#' BETS.param_var(garch.fit,volume = 30000,conf.level=0.99)
#'  
#' @keywords Econometria de Series Temporais  
#' @import rugarch
#' @export



BETS.param_var = function(x, volume, conf.level){
  
  if(typeof(x)!="S4")stop("Erro, the parameter x must been a garch object")        

  sig.level=1-conf.level
  z = qnorm(sig.level,lower.tail = T)
  z
    
    n = length(x@fit$coef)
    m_t = 0 
    for(i in 1:n){
      if(substr(names(x@fit$coef[i]),1,2) == "mu")
        m_t = m_t
      if(substr(names(x@fit$coef[i]),1,2) == "ar"){
       m_t = m_t + x@fit$coef[i] 
      }
    }
    

    m_t = m_t*x@model$modeldata$data[length(x@model$modeldata$data)]
  
  
    h_t = 0
    
    for(i in 1:n){
      if(substr(names(x@fit$coef[i]),1,5)=="omega"){h_t=h_t+h_t}
    if(substr(names(x@fit$coef[i]),1,5)=="alpha"){
      h_t = h_t + (x@fit$coef[i]*x@fit$residuals[length(x@fit$residuals)])
    }
      if(substr(names(x@fit$coef[i]),1,4)=="beta"){
        h_t = h_t + (x@fit$coef[i]*x@fit$var[length(x@fit$var)])
          }
    }
    h_t
    r_ast <- m_t + z*sqrt(h_t)
    result = (r_ast/100)*volume
    return(as.numeric(result))
}


