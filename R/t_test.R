#' @title  Significancia dos parametro de um modelo Arima
#' 
#' @description  Realiza o teste de significancia dos parametro de um modelo 
#' ARIMA.
#' 
#'
#' 
#' @param arima_model  Modelo arima utilizado
#' @param n_x  Numero de variaveis exogenas
#' 
#' @details 
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