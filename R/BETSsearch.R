#' @title Search for a Brazilian Economic Time Series
#' 
#' @description Searches the BETS databases for a time series by its name, source, periodicity, code, data, unit of measurement and database name. 
#' 
#' @param name A \code{character}. The complete name or a part of the name of the series.   
#' @param src A \code{character}. The source of the series. See the 'Details' section for a list of the available sources.
#' @param periodicity A \code{character}. The periodicity of the series. See the 'Details' section for a list of possible values.  
#' @param unit A \code{character}. The unit of measurement of the data. See the 'Details' section for a list of possible values.  
#' @param code An \code{integer}. The index of the series within the database. 
#' @param view A \code{boolean}. The default is \code{TRUE}. If set to \code{FALSE}, the results are NOT going to be shown.    
#' @param database A \code{character}. The database from which the series must be retrieved. The default is \code{bacen}. See the 'Details' section for a list of available databases.    
#' 
#' @return A \code{list} that can be interpreted as a \code{data.frame}. The fields are described below.
#' 
#' \tabular{ll}{
#'  code \tab The code/index of the series within the database \cr
#'  description \tab The description of the series \cr
#'  periodicity \tab The periodicity of the series \cr
#'  start \tab Starting date of the series \cr
#'  source \tab The source of the series \cr
#'  unit \tab The unit of measurement of the data
#'}
#'
#' @details 
#' 
#' \itemize{
#' 
#' \item{ Possible values for the parameter \code{src}:
#'    \tabular{ll}{
#'      IBGE \tab Brazilian Institute of Geography and Statistics \cr
#'      BCB \tab Central Bank of Brazil \cr
#'      BCB e Deban \tab desc \cr
#'      BCB e FGV \tab Central Bank of Brazil and Getúlio Vargas Foundation \cr
#'      BCB-Depin \tab desc \cr
#'      BCB-Derin \tab desc \cr
#'      BCB-Desig \tab desc \cr
#'      BCB-Secre \tab desc \cr
#'      BCB-Demab \tab desc \cr
#'      BCB-Denor \tab desc \cr
#'      FGV \tab Getúlio Vargas Foundation \cr
#'      Sisbacen e Abecip \tab desc \cr
#'      BCB-Depec \tab desc
#'    }
#' }
#' 
#' \item{ Possible values for the parameter \code{periodicity}:
#'    \tabular{ll}{
#'      A - \tab anual data \cr
#'      M - \tab monthly data \cr
#'      Q - \tab quaterly data \cr
#'      W - \tab weekly data \cr
#'      D - \tab daily data 
#'    }
#' }
#' 
#' \item{ Possible values for the parameter \code{unit}:
#'    \tabular{ll}{
#'      R$ - \tab brazilian reais \cr
#'      $ - \tab US dolars \cr
#'     \% - \tab percentage 
#'    }
#' }
#' 
#' \item{ Possible values for the parameter \code{database}:
#'    \tabular{ll}{
#'      bacen - \tab Central Bank of Brazil
#'    }
#' }
#' 
#'}
#' 
#' @note 
#' This function uses \code{\link[sqldf]{sqldf}} for optimization. 
#' 
#' @examples 
#' 
#' BETSsearch(name="sales")
#' # Output: BETS-package: 55 of 12981 time series !
#' 
#' BETSsearch(code= 4500)
#' # Output: BETS-package: DONE!
#' 
#' BETSsearch(src="Denor")
#' # Output: BETS-package: 1 of 12981 time series !
#' 
#' BETSsearch(periodicity="A")
#' # Output: BETS-package: 2308 of 12981 time series!
#' 
#' @references 
#' 
#' Central Bank of Brazil. \href{https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSerie}{Time Series Management System - v2.1}
#' 
#' @keywords search
#' 
#' @import sqldf
#' @export 

BETSsearch=function(name,src,periodicity,unit,code,view=TRUE,database){
  database=bacen_v7
  #require(sqldf)
  #verificando para direcionar a funcao
  #if(data == BD){
  if(missing(src) & missing(periodicity) & missing(code) & missing(unit)){
    #verificacoes:
    if(is.character(name)==F) stop("Erro")
    aux=sqldf(
      paste("select Codes, Description, Periodicity,start, source, unit from database 
            where Description like " ,"\'%", name ,"%\'",sep=""))
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(database)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }else if( missing(periodicity) & missing(name) & missing(code) & missing(unit)){
    #verificacao
    if(is.character(src)==F) stop("Erro")
    aux=sqldf(
      paste("select Codes, Description, Periodicity,start, source, unit from database 
            where source like " ,"\'%", src ,"%\'",sep=""))
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(database)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }else if(missing(src) & missing(name) & missing(code) & missing(periodicity)){
    #verificacao
    if(is.character(unit) == F)stop("Erro")
    aux=sqldf(
      paste("select Codes, Description, Periodicity,start, source, unit from database 
            where unit like " ,"\'%", unit ,"%\'",sep=""))
    
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(database)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }else if(missing(src) & missing(name) & missing(code) & missing(unit)){
    #verificacao
    if(is.character(periodicity) == F)stop("Erro")
    aux=sqldf(
      paste("select Codes, Description, Periodicity,start, source, unit from database 
            where periodicity like " ,"\'%", periodicity ,"%\'",sep=""))
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(database)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }else if(missing(name) & missing(periodicity) & missing(src) & missing(unit)){
    #verificacao
    if(is.numeric(code) && (code%%1!=0))stop("Erro")
    
    aux=sqldf(
      paste("select Codes, Description, Periodicity,start, source, unit from database 
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
  }else if(missing(code) && missing(src) & missing(unit)){
    
    aux=sqldf(
      
      paste("select Codes, Description, Periodicity,start, source, unit from database 
            where Description like", "\'%" , name ,"%\'", "and Periodicity like",
            "\'%", periodicity ,"%\'" ,sep="")
    )
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(database)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }else if(missing(code) && missing(name) & missing(unit)){
    
    aux=sqldf(
      
      paste("select Codes, Description, Periodicity,start, source, unit from database 
            where source like", "\'%" , src ,"%\'", "and Periodicity like",
            "\'%", periodicity ,"%\'" ,sep="")
    )
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(database)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }else if(missing(code) && missing(periodicity) & missing(unit)){
    
    aux=sqldf(
      
      paste("select Codes, Description, Periodicity,start, source, unit from database 
            where source like", "\'%" , src ,"%\'", "and Description like",
            "\'%", name ,"%\'" ,sep="")
    )
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(database)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }else if(missing(src) && missing(name) & missing(unit)){
    
    aux=sqldf(
      
      paste("select Codes, Description, Periodicity,start, source, unit from database 
            where Codes like", "\'%" , code ,"%\'", "and Periodicity like",
            "\'%", periodicity ,"%\'" ,sep="")
    )
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(database)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }else if(missing(src) && missing(name) && missing(code)){
    
    aux=sqldf(
      
      paste("select Codes, Description, Periodicity,start, source, unit from database 
            where Periodicity like", "\'%" , periodicity ,"%\'", "and unit like",
            "\'%", unit ,"%\'" ,sep="")
    )
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(database)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }
}
