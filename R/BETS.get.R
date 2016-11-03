#' @title Get a complete time series from a BETS database
#' 
#' @description Extracts a complete time series from either the Central Bank of Brazil (BCB), the Brazilian Institute of Geography and Statistics (IBGE) or the Brazilian Institute of Economics (FGV/IBRE).
#' 
#' @param code A \code{character}. The unique code that references the time series. This code can be obtained by using the \code{\link{BETS.search}} function.
#' @param  data.frame A \code{boolean}. True if you want the output to be a data frame. True to \code{ts} output.
#'
#' @return A \code{\link[stats]{ts}} (time series) object containing the desired series.
#' 
#' @note Due to the significant size of the databases, it could take a while to retrieve the values. However, it shouldn't take more than 90 seconds. 
#' 
#' @examples 
#'
#'  # Anual series: GDP at constant prices, in R$ (brazilian reais)
#'  BETS.get(1208)
#'  
#'  # International reserves - Cash concept 
#'  int.reserves <- BETS.get("3543")
#'  plot(int.reserves)
#'  
#'  # Exchange rate - Free - United States dollar (purchase)
#'  us.brl <- BETS.get(3691)
#'  require(seasonal)
#'  us.brl.seasonally_adjusted <- seas(us.brl)
#'  plot(us.brl.seasonally_adjusted)
#'  
#' @seealso \code{\link[stats]{ts}}, \code{\link[BETS]{BETS.search}} and \code{\link[seasonal]{seas}}
#' 
#' @keywords get
#' 
#' @import sqldf
#' @export 


BETS.get = function(code, data.frame = FALSE){
  
    if(data.frame){
      return(get.data.frame(code))
    }
  
    data = bacen_v7
    code = as.character(code)
    query = paste("select Periodicity from data where Codes like " ,"\'", code ,"\'",sep="")
    
    freq = sqldf(query)[1,1]
    freq = trimws(as.character(freq))
    
    if(is.na(freq)){
      return(invisible(msg(paste(.MSG_NOT_AVAILABLE,"There is no corresponding entry in the metadata table."))))
    }
    
    aux1 = NULL
    aux2 = NULL
    
    if(freq == "A"){
      database = "ts_anuais"
      freq = 1 
    }
    else if(freq == "Q"){
      database = "ts_trimestrais"
      freq = 4 
    }
    else if(freq == "M"){
      database = "ts_mensais"
      freq = 12
    }
    else if(freq == "W"){
      database = "ts_semanais"
      freq = 52 
    }
    else if(freq == "D"){
      database = "ts_diarias"
      freq = 365 
    }
    else {
      return(invisible(msg(paste(.MSG_NOT_AVAILABLE,"Malformed metadata. The value", freq, "is not valid for 'periodicity'"))))
    }
    
    query = paste("select data, valor from ", database, " where serie like " ,"\'", code ,"\'",sep="")
    aux = sqldf(query)
    
    if(nrow(aux) == 0){
      return(invisible(msg(paste(.MSG_NOT_AVAILABLE,"Series is empty in database", database))))
    }
    
    aux = na.omit(aux)
    
    if(is.factor(aux[,2])){
      aux[,2] <- as.vector(aux[,2])
    }
    
    if(is.factor(aux[,1])){
      aux[,1] <- as.vector(aux[,1])
    }
    
    aux1 = as.numeric(aux[,2])
    aux2 = as.date(aux[,1])
    
    g = duplicated(aux2)
    
    if(any(g)){
      
      for(i in 1:length(aux1)){
        
        if(g[i]==T){
          aux1[i]=NA
          aux2[i]=NA
        }
      }
    }
    
    aux1 = na.omit(aux1)
    aux2 = na.omit(aux2)
    
    if(is.null(aux1)){
      return(invisible(msg(paste(.MSG_NOT_AVAILABLE,"Series contains only NAs."))))
    }
    
    start = get.period(aux2[1],freq)

    ts <- ts(aux1, start = start, freq = freq)
    
    return(ts)
}





