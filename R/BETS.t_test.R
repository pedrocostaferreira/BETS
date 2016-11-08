#' @title  Significance of parameter an Arima model
#' 
#' @description  Performs the test of significance of the parameter an 
#' Arima model
#' 
#' 
#'
#' 
#' @param arima_model  Arima model used
#' @param n_x  Number of variables Exogenous
#' 
#' 
#' @examples 
#' data("AirPassengers")
#' fit.air<- Arima(AirPassengers,order = c(1,1,1), 
#'     seasonal = c(1,1,1), method ="ML",lambda=0)
#'     summary(fit.air)
#' 
#' # significance test for model SARIMA(1,1,1)(1,1,1)_12
#' t.test(arima_model = fit.air)
#'
#' 
#' @return Objeto do tipo \code{list}
#' 
#' @author Daiane Mattos \email{daiane.mattos@fgv.br}
#' 
#' @export



BETS.t_test <- function(arima_model, n_x = 0, alpha = 0.05){

  # Esta fun??o faz o teste de Signific?ncia dos par?metros de um modelo ARIMA
  # n_x ? o n?mero de vari?veis ex?genas

  # Estat?stica T
  coef <- arima_model$coef
  se <- sqrt(diag(arima_model$var.coef))
  t <- abs(coef/se)
  
  crit = qt(1 - alpha/2, length(arima_model$x) - sum(arima_model$arma[c(1,2,3,4,6,7)]) - n_x)
  ok <- t > crit
  resul <- data.frame(Coeffs = coef, Std.Errors = se, t = t, Crit.Values = crit, Rej.H0 = ok )
  return(resul)
  
}
