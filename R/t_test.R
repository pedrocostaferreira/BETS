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
#' significance test for model SARIMA(1,1,1)(1,1,1)_12
#' t.test(arima_model = fit.air)
#'
#' 
#' @return Objeto do tipo \code{list}
#' 
#' @author Daiane Mattos \email{daiane.mattos@fgv.br}
#' 
#' @export



t_test <- function(arima_model, n_x = 0){

  # Esta fun??o faz o teste de Signific?ncia dos par?metros de um modelo ARIMA
  # n_x ? o n?mero de vari?veis ex?genas

  # Estat?stica T
  coef <- modelo_arima$coef
  se <- sqrt(diag(modelo_arima$var.coef))
  t <- abs(coef/se)
  
  ok <- t > qt(0.975, length(arima_model$x) - sum(modelo_arima$arma[c(1,2,3,4,6,7)]) - n_x)
  resul <- data.frame(Coef = coef, sd = se, t = t, rej_H0 = ok )
  return(resul)
  
}