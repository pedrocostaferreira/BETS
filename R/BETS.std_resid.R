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


BETS.std_resid = function(model, alpha = 0.05){
  resid <- resid(model)
  rp <- (resid - mean(resid))/sd(resid)
  plot(rp, col = "royalblue", ylim = c(-0.5 + min(rp),0.5 + max(rp)), ylab = "Standard Residuals")
<<<<<<< HEAD
  abline(h = c(-qnorm(1 - alpha/2),qnorm(1 - alpha/2)), col = "gray", lty = 2)
=======
  abline(h = c(-qnorm(1 - alpha/2),qnorm(1- alpha/2)), col = "gray", lty = 2)
>>>>>>> fdf2ec038ebc6e2d26d362fca8334eb1ce835b73
  return(rp)
}