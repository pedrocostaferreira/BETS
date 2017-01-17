#' @title Prepare a time series to be exported
#' 
#' @description To be used with BETS.save.spss, BETS.save.sas and others.
#' 
#' @param code An \code{integer}. The unique identifier of the series within the BETS database. 
#' @param data A \code{data.frame} or a \code{ts}. Contains the data to be written. If \code{data} is supplied, the BETS database will not be searched. 
#' @param file.name A \code{character}. The name of the output file. The default is 'series.spss'.
#' @param type A \code{character}. The type of the file (e.g. 'spss' or 'sas').
#' 
#' @return A list with the data frame to be saved and the file name
#' 
#' @importFrom zoo as.Date


BETS.save = function(code = NULL, data = NULL, file.name="series", type = ""){
  
  local=getwd()
  
  if(is.null(data) && !is.null(code)){
    y = BETS.get(code, data.frame = TRUE)
    
    if(file.name == "series"){
      file.name = paste0(file.name, "_", code)
    }
  }
  else if(is.data.frame(data)){
    y = data 
  }
  else if(class(data) == 'ts'){
    y = data.frame(date = as.Date(data), value = data)
  }
  else {
    return(msg('The parameter "data" must be either a data.frame or a ts object'))
  }
  
  file.name = paste0(local,"/",file.name,".",type)
  
  return(list(data = y, file = file.name))
}

