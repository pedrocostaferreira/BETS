#' @title Export a time series to excel
#' 
#' @description Writes a time series to a .xlsx (excel) file.
#' 
#' @param code An \code{integer}. The unique identifier of the series within the BETS database. 
#' @param data A \code{data.frame} or a \code{ts}. Contains the data to be written. If \code{data} is supplied, the BETS database will not be searched. 
#' @param file.name A \code{character}. The name of the output file. The default is 'series.xlsx'.
#' 
#' @return None
#' 
#' @examples 
#' 
#' # Exchange rate - Free - United States dollar (purchase)
#'  us.brl <- BETSget(3691)
#'  requires(seasonal)
#'  us.brl.seasonally_adjusted <- seas(us.brl)
#'  BETS.save.xlsx(data = us.brl.seasonally_adjusted,file.name="us.brl.seasonally_adjusted")
#'  # Or
#'  BETS.save.xlsx(code=3691,file.name="us.brl")
#' @import  xlsx
#' @export 


BETS.save.xlsx=function(code, data = NULL, file.name="series"){
  
  local=getwd()
  
  if(is.null(data)){
    y = get.data.frame(code)
  }
  else if(is.data.frame(data)){
    y = data 
  }
  else if(class(data) == 'ts'){
    y = get.data.frame(ts = data)
  }
  else {
    return(msg('Invalid parameters. The parameter "data" must be either a data.frame or a ts object'))
  }
  
  #-- temporario 
  y[,1] = as.character(y[,1])
  write.xlsx(y, paste0(local,"/",file.name,".xlsx"))
}

