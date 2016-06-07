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
#' @param language A \code{character}. The Search language, which defaut is "en", but can be "pt".
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
#'      FGV \tab Getulio Vargas Foundation \cr
#'      FGv-IBRE \tab Getulio Vargas Foundation - Brazilian Institute of Economics \cr
#'      BCB e FGV \tab Central Bank of Brazil and Getulio Vargas Foundation \cr
#'      BCB-Deban \tab Cetral Bank of Brazil - Department of Banking and Payments \cr
#'      BCB-Depin \tab Central Bank of Brazil - Department of International Reserves \cr
#'      BCB-Derin \tab Central Bank of Brazil - Department of International Affairs \cr
#'      BCB-Desig \tab Central Bank of Brazil - Department of Financial Monitoring \cr
#'      BCB-Secre \tab Central Bank of Brazil - Executive Secretariat \cr
#'      BCB-Demab \tab Central Bank of Brazil - Department of Open Market Operations \cr
#'      BCB-Denor \tab Central Bank of Brazil - Department of Financial System Regulation \cr
#'      BCB-Depec \tab Central Bank of Brazil - Department of Economics \cr
#'      Sisbacen \tab Central Bank of Brazil Information System \cr
#'      Abecip \tab Brazilian Association of Real Estate Loans and Savings Companies
#'    }
#' }
#' 
#' \item{ Possible values for the parameter \code{periodicity}:
#'    \tabular{ll}{
#'      A \tab anual data    \cr
#'      M \tab monthly data  \cr
#'      Q \tab quaterly data \cr
#'      W \tab weekly data   \cr
#'      D \tab daily data 
#'    }
#' }
#' 
#' \item{ Possible values for the parameter \code{unit}:
#'    \tabular{ll}{
#'      R$ \tab brazilian reais \cr
#'      $ \tab US dolars        \cr
#'     \% \tab percentage 
#'    }
#' }
#'}
#' 
#'
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

BETSsearch=function(name,src,periodicity,unit,code,view=TRUE,lang="en"){
  
  #if(lang=="pt"){database=bacen_v7_port}else{database=bacen_v7}
  ##-----------------------------------------------------------
  if(lang=="pt"){database=base_final_ptv1}else{database=bacen_v7}
  ##-----------------------------------------------------------
  
    
  if(missing(src) & missing(periodicity) & missing(code) & missing(unit)){
    
    if(is.character(name)==F) stop("Erro")
    aux=sqldf(
      paste("select Codes, Description, Periodicity,start, source, unit from database 
            where Description like " ,"\'%", name ,"%\'",sep=""))
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(database)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }else if( missing(periodicity) & missing(name) & missing(code) & missing(unit)){
    
    if(is.character(src)==F) stop("Erro")
    aux=sqldf(
      paste("select Codes, Description, Periodicity,start, source, unit from database 
            where source like " ,"\'%", src ,"%\'",sep=""))
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(database)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }else if(missing(src) & missing(name) & missing(code) & missing(periodicity)){
    
    if(is.character(unit) == F)stop("Erro")
    aux=sqldf(
      paste("select Codes, Description, Periodicity,start, source, unit from database 
            where unit like " ,"\'%", unit ,"%\'",sep=""))
    
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(database)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }else if(missing(src) & missing(name) & missing(code) & missing(unit)){
    
    if(is.character(periodicity) == F)stop("Erro")
    aux=sqldf(
      paste("select Codes, Description, Periodicity,start, source, unit from database 
            where periodicity like " ,"\'%", periodicity ,"%\'",sep=""))
    msg("Results")
    Results=aux
    msg(paste(nrow(aux),"of",nrow(database)," time series","!",sep=" "))
    if(view==T){return(View(Results))}else{return(Results)}
  }else if(missing(name) & missing(periodicity) & missing(src) & missing(unit)){
    
    if(is.numeric(code) && (code%%1!=0))stop("Erro")
    
    aux=sqldf(
      paste("select Codes, Description, Periodicity,start, source, unit from database 
            where Codes like " ,"\'", code ,"\'",sep=""))
   
    ###----------------------------------------------------
     aux1 = sqldf(
      paste("select Codes, Description, Periodicity,start, source, unit from database 
            where Codes like " ,"\'", paste0("ST_",code) ,"\'",sep=""))
    aux <- rbind(aux1,aux2)
    Results=aux
    if(view==T){return(View(Results))}else{return(Results)}
    
    ###---------------------------------------------------
    
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
