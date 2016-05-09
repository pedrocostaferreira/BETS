#' @title Get a complete time series from a BETS database
#' 
#' @description Extracts a complete time series from either the Central Bank of Brazil (BCB), the Brazilian Institute of Geography and Statistics (IBGE) or the Brazilian Institute of Economics (FGV/IBRE).
#' 
#' @param code A \code{character}. The unique code that references the time series. This code can be obtained by using the \code{\link{BETSsearch}} function.
#' 
#' @return A \code{\link[stats]{ts}} (time series) object containing the desired series.
#' 
#' @note Due to the significant size of the databases, it could take a while to retrieve the values. However, it shouldn't take more than 90 seconds. 
#' 
#' @examples 
#'
#'  # Anual series: GDP at constant prices, in R$ (brazilian reais)
#'  BETSget(1208)
#'  
#'  # International reserves - Cash concept 
#'  x <- BETSget("3543")
#'  plot(x)
#'  
#'  # Exchange rate - Free - United States dollar (purchase)
#'  k <- BETSget(3691)
#'  requires(seasonal)
#'  m <- seas(k)
#'  plot(m)
#'  
#' @seealso \code{\link[stats]{ts}}, \code{\link[BETS]{BETSsearch}} and \code{\link[seasonal]{seas}}
#' 
#' @keywords get
#' 
#' @import sqldf
#' @export 


BETSget=function(code){
  #require(sqldf)
    data=bacen_v7
    code=as.character(code)
    resposta=sqldf(
    paste("select Periodicity from data where Codes like " ,"\'", code ,"\'",sep=""))[1,1]
    resposta=as.character(resposta)
    if(resposta==" A" || resposta=="A"){
    #-------
    aux1=NULL
    aux2=NULL
    aux=sqldf(
      paste("select data, valor, serie from ts_anuais where serie like " ,"\'", code ,"\'",sep="")
    )
    aux=na.omit(aux)
    aux1=as.numeric(aux[,2])
    aux2=as.Date(aux[,1])
    g=duplicated(aux2)
    if(any(g)){
      for(i in 1:length(aux1)){
        if(g[i]==T){
          aux1[i]=NA
          aux2[i]=NA
          }
        }
        aux1=na.omit(aux1)
        aux2=na.omit(aux2)
        start_ano<-as.numeric(substr(aux2[1],1,4))
        start_mes<-as.numeric(substr(aux2[1],6,7))
        if(is.null(aux1))return(msg("Valores não encontrados! :("))
        minha_ts<-ts(aux1,start<-c(start_ano),freq=1)
        return(minha_ts)
      
      }else{
      aux1=na.omit(aux1)
      aux2=na.omit(aux2)
      start_ano<-as.numeric(substr(aux2[1],1,4))
      start_mes<-as.numeric(substr(aux2[1],6,7))
      if(is.null(aux1))return(msg("Valores não encontrados! :("))
      minha_ts<-ts(aux1,start<-c(start_ano),freq=1)
      return(minha_ts)}
    #-------
  }else if(resposta==" M" || resposta=="M"){
    
    #----------------------------------------------------------------------------------------  
    ###############################  mensais
    #-------
    aux1=NULL
    aux2=NULL
    
    aux=sqldf(
      paste("select data, valor, serie from ts_mensais where serie like " ,"\'", code ,"\'",sep="")
    )
    aux=na.omit(aux)
    aux1=as.numeric(aux[,2])
    aux2=as.Date(aux[,1])
    g=duplicated(aux2)
    if(any(g)){
      for(i in 1:length(aux1)){
        if(g[i]==T){
          aux1[i]=NA
          aux2[i]=NA
        }
      }
      aux1=na.omit(aux1)
      aux2=na.omit(aux2)
      start_ano<-as.numeric(substr(aux2[1],1,4))
      start_mes<-as.numeric(substr(aux2[1],6,7))
      if(is.null(aux1))return(msg("Valores não encontrados! :("))
      minha_ts<-ts(aux1,start<-c(start_ano),freq=12)
      return(minha_ts)
    }else{  
      aux1=na.omit(aux1)
      aux2=na.omit(aux2)
      start_ano<-as.numeric(substr(aux2[1],1,4))
      start_mes<-as.numeric(substr(aux2[1],6,7))
      if(is.null(aux1))return(msg("Valores não encontrados! :("))
      minha_ts<-ts(aux1,start<-c(start_ano),freq=12)
      return(minha_ts)
    }
    #-------
  }else if(resposta==" D" || resposta=="D"){
    
    ###############################  Diarias
    #-------
    aux1=NULL
    aux2=NULL
    aux=sqldf(
      paste("select data, valor, serie from ts_diarias where serie like " ,"\'", code ,"\'",sep="")
    )
    
    aux1=as.numeric(aux[,2])
    #aux2=as.Date(sapply(strsplit(aux$data,"/"),function(x) paste(c(x[2:1], 1), collapse="-")), "%Y-%m-%d")
    aux2=as.Date(sapply(aux$data,function(x)
      paste(substr(x,6,9),paste0(0,substr(x,4,4)),substr(x,1,2),sep="-")), "%Y-%m-%d")
    #aux2=as.Date(aux[,1])
    g=duplicated(aux2)
    if(any(g)){
      for(i in 1:length(aux1)){
        if(g[i]==T){
          aux1[i]=NA
          aux2[i]=NA
        }
      }
      aux1=na.omit(aux1)
      aux2=na.omit(aux2)
      start_ano<-as.numeric(substr(aux2[1],1,4))
      start_mes<-as.numeric(substr(aux2[1],6,7))
      start_day<-as.numeric(substr(aux2[1],9,10))
      if(is.null(aux1))return(msg("Valores não encontrados! :("))
      minha_ts<-ts(aux1,start<-c(start_ano,start_mes,start_day),freq=365)
      return(minha_ts)
    }else{  
      aux1=na.omit(aux1)
      aux2=na.omit(aux2)
      start_ano<-as.numeric(substr(aux2[1],1,4))
      start_mes<-as.numeric(substr(aux2[1],6,7))
      if(is.null(aux1))return(msg("Valores não encontrados! :("))
      minha_ts<-ts(aux1,start<-c(start_ano),freq=365)
      return(minha_ts)
    }
    ############################## semanais
    
  }else if(resposta==" W" || resposta=="W"){
    
    aux1=NULL
    aux2=NULL
    
    aux=sqldf(
      paste("select data, valor, serie from ts_semanais where serie like " ,"\'", code ,"\'",sep="")
    )
    aux=na.omit(aux)
    aux1=as.numeric(aux[,2])
    aux2=as.Date(aux[,1])
    g=duplicated(aux2)
    for(i in 1:length(aux1)){
      if(g[i]==T){
        aux1[i]=NA
        aux2[i]=NA
      }
    }
    aux1=na.omit(aux1)
    aux2=na.omit(aux2)
    start_ano<-as.numeric(substr(aux2[1],1,4))
    start_mes<-as.numeric(substr(aux2[1],6,7))
    if(is.null(aux1))return(msg("Valores não encontrados! :("))
    minha_ts<-ts(aux1,start<-c(start_ano),freq=52)
    return(minha_ts)
    
    
    
  }else if(resposta == " Q" || resposta=="Q"){
    
    aux1=NULL
    aux2=NULL
    
    aux=sqldf(
      paste("select data, valor, serie from ts_trimestrais where serie like " ,"\'", code ,"\'",sep="")
    )
    aux=na.omit(aux)
    aux1=as.numeric(aux[,2])
    aux2=as.Date(aux[,1])
    g=duplicated(aux2)
    for(i in 1:length(aux1)){
      if(g[i]==T){
        aux1[i]=NA
        aux2[i]=NA
      }
    }
    aux1=na.omit(aux1)
    aux2=na.omit(aux2)
    start_ano<-as.numeric(substr(aux2[1],1,4))
    start_mes<-as.numeric(substr(aux2[1],6,7))
    if(is.null(aux1))return(msg("Valores não encontrados! :("))
    minha_ts<-ts(aux1,start<-c(start_ano),freq=4)
    return(minha_ts)
  }else{
    return(msg("Série não encontrada, desculpe!:("))
    }
  
}





