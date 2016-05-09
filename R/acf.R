#' @title Auto- and Cross- Covariance and -Correlation Function Estimation
#' 
#' @description This function calls the acf function in the stats package and processes 
#' to drop lag-0 of the acf. It only works for univariate time series, so 
#' x below should be 1-dimensional.
#' 
#' @return 
#' An object of class "acf", which is a list with the following elements: 
#' 
#'\itemize{
#'  \item{lag}{ A three dimensional array containing the lags at which the acf is estimated.} 
#'  \item{acf}{ An array with the same dimensions as lag containing the estimated acf.} 
#'  \item{type}{ The type of correlation (same as the type argument).} 
#'  \item{n.used}{ The number of observations in the time series.} 
#'  \item{series}{ The name of the series x.} 
#'  \item{snames}{ The series names for a multivariate time series.} 
#'}
#' 
#' @param x A univariate or multivariate (not ccf) numeric \code{\link[stats]{ts}} object
#'   or a \code{numeric} vector or matrix, or an \code{\link[stats]{acf}} object.
#' @param lag.max  An \code{integer}. Maximum number of lags at which to calculate the acf. 
#'  Default is 10*log10(N/m) where N is the number of observations and m 
#'  the number of series.
#' @param type A \code{character} The type of acf to be computed. 
#'  Allowed values are "correlation" (the default), "covariance" or "partial".
#' @param plot A \code{boolean}. If TRUE (the default) the acf is plotted.
#' @param na.action A \code{function} to be called to handle missing values. na.pass 
#'  can be used.
#' @param demean A \code{boolean}. Should the covariances be about the sample means?
#' @param ... Further arguments to be passed to plot.acf
#'  
#'  
#' @references Original authors of stats:::acf are: Paul Gilbert, Martyn Plummer, 
#'  B.D. Ripley. This wrapper is written by Kung-Sik Chan
#'  
#' @seealso 
#' 
#' \itemize{ 
#' 
#' \item{ \code{\link{plot.acf}}, \code{\link{ARMAacf}} for the exact 
#'  autocorrelations of a given ARMA process. }
#' 
#' \item{ \code{\link[stats]{acf}} - the original function from the package 'stats' }
#'}
#'  
#' @examples 
#'  
#' data(rwalk)
#' model1=lm(rwalk~time(rwalk))
#' summary(model1)
#' acf(rstudent(model1),main='')
#'
#' @keywords methods
#'  
#'  
#' @import stats
#' @export 

acf<-
  function (x, lag.max = NULL, type = c("correlation", "covariance", 
                          "partial")[1], plot = TRUE, na.action = na.fail, demean = TRUE,...) 
  {
    #invisible(
      acf.out <- stats:::acf(x=x, lag.max=lag.max, type=type, plot=F, na.action=na.action,
                           demean=demean,...)
    acf.out$series <- deparse(substitute(x))
      if (type == "correlation") {
        acf.out$acf = acf.out$acf[-1, , , drop = FALSE]
        acf.out$lag = acf.out$lag[-1, , , drop = FALSE]
      }
    if (plot) {
      plot1.acf(acf.out, ...)
      return(invisible(acf.out))
    }
    else{return(acf.out)}
   # )
  }

