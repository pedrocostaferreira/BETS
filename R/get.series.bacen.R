#' A function to extract BACEN series using their API
#' @param x Bacen series numbers. Either an integer or a numeric vector.
#' @param from A string specifying where the series shall start.
#' @param to A string specifying where the series shall end.
#' @param save A string specifying if data should be saved in csv or xlsx format. 
#' Defaults to not saving.
#' @keywords bacen
#' @author Fernando Teixeira \email{fernando.teixeira@fgv.br}
#' @import RCurl 
#' @examples
#' get.series.bacen(1242,2134)

get.series.bacen <- function(x, from = "", to = "", save = ""){
  
  
  if (missing(x)){
    stop("Need to specify at least one series.")
  }
  
  if (! is.numeric(x)){
    stop("x must be numeric")
  }
  
  if (from == ""){
    data_init = "01/01/1980"
  } else {data_init = from}
  
  if (to == ""){
    data_end = format(Sys.Date(), "%d/%m/%Y")
  } else {data_end = to}
  
  inputs = as.character(x)
  len = seq_along(inputs)
  serie = mapply(paste0, "serie_", inputs, USE.NAMES = FALSE)
  
  
  for (i in len){ assign(serie[i],
                         RCurl::getURL(paste0('http://api.bcb.gov.br/dados/serie/bcdata.sgs.',
                                              inputs[i], 
                                              '/dados?formato=csv&dataInicial=', data_init, '&dataFinal=',
                                              data_end),
                                       ssl.verifyhost=FALSE, ssl.verifypeer=FALSE))}
  
  
  for (i in len){
    texto = utils::read.csv(textConnection(eval(as.symbol(
      serie[i]))), header=T)
    texto$data = gsub(' .*$','', eval(texto$data))
    assign(serie[i], texto)
    
  }
  
  rm(texto)
  
  #if (save != ""){
    if (save == "csv"){
      for(i in len) {utils::write.csv(eval(as.symbol(serie[i])), file = paste0(serie[i], ".csv"))}
  #   } else if (save == "xls" | save == "xlsx") {
  #     for(i in len) {write.xlsx(eval(as.symbol(serie[i])), file = paste0(serie[i], ".xlsx"), 
  #                               row.names = FALSE)}} else{ 
  #                                 stop("save argument must be 'csv' or 'xlsx' ")}
    }

  lista = list()
  ls_df = ls()[grepl('data.frame', sapply(ls(), function(x) class(get(x))))]
  for ( obj in ls_df ) { lista[obj]=list(get(obj)) }
  
  return(invisible(lista))
  
}