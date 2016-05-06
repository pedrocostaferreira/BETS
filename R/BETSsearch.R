#' @title Search for a brazilian economic time series
#' 
#' @description
#' 
#' @usage 
#' BETSsearch(name,Source,periodicity,unit,code,view=TRUE,data) 
#' 
#' @return 
#' Objeto do tipo list de tamanho 5  com  estetica de um data.frame.
#' 
#' \itemize{
#' 
#'\item{Codes }{Codigos das series.}
#'\item{Descripition}{Descricao das seires.}
#'\item{Periodicity}{Periodicidade das seires.}
#'\item{source}{Fonte des series.}
#'\item{unit}{Unidade da serie.}
#' }
#' 
#' @param name  Descricao ou parte da descricao da serie temporal
#' @param Source    Fonte de onde e produzida
#' @param periodicity   Frequencia da serie temporal
#' @param unit     Unidade da serie temporal
#' @param code     Codigo que referencia a serie temporal
#' @param view     TRUE para visualizar os resultado diretamente
#' @param data     Em qual base sera feita a busca
#' 
#' @return 
#' 
#' @note 
#' 
#' @examples 
#' 
#' # The function is currently defined as
#' BETSsearch(name="sales")
#' #BETS-package: 55 of 12981 time series !
#' searchBETS(code= 4500)
#' #BETS-package: DONE!
#' BETSsearch(Source="Denor")
#' #BETS-package: 1 of 12981 time series !
#' BETSsearch(periodicity="A")
#' #BETS-package: 2308 of 12981 time series!
#'
#' #Como mencionado em 'Note', segue os 
#' #exemplos de pesquisas combinadas:
#' BETSget(name="production",periodicity="M",view=FALSE)
#' BETSget(Source=BCB,periodicity="M",view=TRUE)
#' BETSget(name="production",Source="IBGE",view=TRUE)
#' BETSget(unit="index",periodicity="M",view=FALSE)
#' 
#' @references 
#' Banco Central do Brasil:
#' Time Series Management System - v2.1 (Public module)
#' 
#' @keywords search
#' 
#' 
#' @import sqldf
#' @export BETSsearch

BETSsearch=function(name,Source,periodicity,unit,code,view=TRUE,data){
  data=bacen_v7
  #require(sqldf)
  #verificando para direcionar a funcao
  #if(data == BD){
  if(missing(Source) & missing(periodicity) & missing(code) & missing(unit)){
    #verificacoes:
    if(is.character(name)==F) stop("Erro")
    aux=sqldf(
      paste("select Codes, Description, Periodicity,start, source, unit from data 
            where Description like " ,"\'%", name ,"%\'",sep=""))
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(data)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }else if( missing(periodicity) & missing(name) & missing(code) & missing(unit)){
    #verificacao
    if(is.character(Source)==F) stop("Erro")
    aux=sqldf(
      paste("select Codes, Description, Periodicity,start, source, unit from data 
            where source like " ,"\'%", Source ,"%\'",sep=""))
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(data)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }else if(missing(Source) & missing(name) & missing(code) & missing(periodicity)){
    #verificacao
    if(is.character(unit) == F)stop("Erro")
    aux=sqldf(
      paste("select Codes, Description, Periodicity,start, source, unit from data 
            where unit like " ,"\'%", unit ,"%\'",sep=""))
    
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(data)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }else if(missing(Source) & missing(name) & missing(code) & missing(unit)){
    #verificacao
    if(is.character(periodicity) == F)stop("Erro")
    aux=sqldf(
      paste("select Codes, Description, Periodicity,start, source, unit from data 
            where periodicity like " ,"\'%", periodicity ,"%\'",sep=""))
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(data)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }else if(missing(name) & missing(periodicity) & missing(Source) & missing(unit)){
    #verificacao
    if(is.numeric(code) && (code%%1!=0))stop("Erro")
    
    aux=sqldf(
      paste("select Codes, Description, Periodicity,start, source, unit from data 
            where Codes like " ,"\'", code ,"\'",sep=""))
    Results=aux
    if(view==T){return(View(Results))}else{return(Results)}
    #aux2=matrix(0,nrow=1,ncol=ncol(data),byrow=F)
    #for(i in 1:nrow(data)){
      #if(code == data[i,1]){
        #aux2=data[i,1:ncol(data)]
      #}
    #}
    
    
    #BD=as.caracter(BD)
    #aux=sqldf(
    #paste("select Codigos, Nome, fonte, Unidade, Periodicidade from data 
    #  where Codigos like " ,"\'%", code ,"%\'",sep=""))
    #msg("Results")
    #msg(paste(nrow(aux2)," time series","of",nrow(BD),"!",sep=" "))
    #msg("Done!")
    return(aux2)
  }else if(missing(code) && missing(Source) & missing(unit)){
    
    aux=sqldf(
      
      paste("select Codes, Description, Periodicity,start, source, unit from data 
            where Description like", "\'%" , name ,"%\'", "and Periodicity like",
            "\'%", periodicity ,"%\'" ,sep="")
    )
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(data)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }else if(missing(code) && missing(name) & missing(unit)){
    
    aux=sqldf(
      
      paste("select Codes, Description, Periodicity,start, source, unit from data 
            where source like", "\'%" , Source ,"%\'", "and Periodicity like",
            "\'%", periodicity ,"%\'" ,sep="")
    )
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(data)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }else if(missing(code) && missing(periodicity) & missing(unit)){
    
    aux=sqldf(
      
      paste("select Codes, Description, Periodicity,start, source, unit from data 
            where source like", "\'%" , Source ,"%\'", "and Description like",
            "\'%", name ,"%\'" ,sep="")
    )
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(data)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }else if(missing(Source) && missing(name) & missing(unit)){
    
    aux=sqldf(
      
      paste("select Codes, Description, Periodicity,start, source, unit from data 
            where Codes like", "\'%" , code ,"%\'", "and Periodicity like",
            "\'%", periodicity ,"%\'" ,sep="")
    )
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(data)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }else if(missing(Source) && missing(name) && missing(code)){
    
    aux=sqldf(
      
      paste("select Codes, Description, Periodicity,start, source, unit from data 
            where Periodicity like", "\'%" , periodicity ,"%\'", "and unit like",
            "\'%", unit ,"%\'" ,sep="")
    )
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(data)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }
}
