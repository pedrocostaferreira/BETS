#' @title Get a BETS series as a data.frame.
#' 
#' @description By default, \code{\link{BETSget}} returns a \code{\link[stats]{ts}} object. However, there are many situations in which is more convenient to work with a data.frame. So, \code{get.data.frame} receives the code of a BETS series and returns a \code{\link[base]{data.frame}} containing the data of the corresponding series. Alternatively, a \code{ts} can be supplied, in which case the BETS databases will not be searched. 
#' 
#' @param code An \code{integer}. The unique identifier of the series within the BETS database.
#' @param ts An \code{ts} object. A time series to be formatted as a data.frame. 
#' 
#' @return A \code{data.frame}. The first column contains the dates. The second, its values.


get.data.frame = function(code, ts = NULL) {
  
  abbr = "values"
  
  if(is.null(ts)){
    ts = BETSget(code)
    abbr = abbreviate(BETSsearch(code = code, view = F)[1,2])
  }
  
  t = as.Date(ts)
  y = as.data.frame(ts)

  series = data.frame(t, y)
  colnames(series) = c("dates",abbr)
  
  return(series)
  
}