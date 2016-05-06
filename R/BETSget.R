#' @title Extrair series temporais do Banco central do Brasil, IBGE  e do 
#' Instituto brasileiro de economia da Fundacao Getulio Vargas.
#' 
#' @description
#' 
#' @param code que referencia a serie temporal
#' 
#' 
#' @return Um objeto do tipo TS
#' 
#' @note Insira o valor ao argumento code como character. 
#'Devido ao grande volume de dados por parte das obeservacoes 
#'da series temporais, pode levar agum tempo para o algoritmo resgatar
#'os valores, no entanto esse tempo e breve nao passando de 90 segundos.
#' 
#' @examples 
#' 
#'  #Aconselha-se a usar a funcao GETsearch() para 
#'  #localizar a serie desejada, obtendo assim o 
#'  #codigo da serie.
#'  
#'  
#'  #serie anual: GDP at constant last year prices in R$
#'  BETSget(1208)
#'  #International reserves - Cash concept 
#'  x<-BETSget("3543")
#'  plot(x)
#'  #Exchange rate - Free - United States dollar(purchase)
#'  k=BETSget(3691)
#'  library(seasonal)
#'  m <- seas(k)
#'  plot(m)
#'  
#' @keywords get
#' 
#' @import sqldf
#' @export BETSget








BETSget=function(codigo){
  #require(sqldf)
    data=bacen_v7
    codigo=as.character(codigo)
    resposta=sqldf(
    paste("select Periodicity from data where Codes like " ,"\'", codigo ,"\'",sep=""))[1,1]
    resposta=as.character(resposta)
    if(resposta==" A" || resposta=="A"){
    #-------
    aux1=NULL
    aux2=NULL
    aux=sqldf(
      paste("select data, valor, serie from ts_anuais where serie like " ,"\'", codigo ,"\'",sep="")
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
      paste("select data, valor, serie from ts_mensais where serie like " ,"\'", codigo ,"\'",sep="")
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
      paste("select data, valor, serie from ts_diarias where serie like " ,"\'", codigo ,"\'",sep="")
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
      paste("select data, valor, serie from ts_semanais where serie like " ,"\'", codigo ,"\'",sep="")
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
      paste("select data, valor, serie from ts_trimestrais where serie like " ,"\'", codigo ,"\'",sep="")
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





