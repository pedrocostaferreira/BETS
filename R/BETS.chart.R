#' @title  Create a chart with a pre-defined BETS series
#' 
#' @description  Create a chart with a pre-defined BETS series
#' 
#' @param ts A \code{character}. The ts of the chart. A complete list of tses for available charts is under the 'Details' section.
#' @param lang A \code{character}. The language. For now, only 'en' (english) is available.
#' @param file A \code{character}. The whole path, including a custom name, for the output (an image file). The default value is 'graphs//parameter_ts' (the 'graphs' directory is under the BETS installation directory).
#' @param open  A \code{boolean}.
#' 
#' @details 
#' 
#' \tabular{lll}{
#'  VALUE \tab DESCRIPTION \tab CODE \cr
#'  \bold{'ipca_with_core'} \tab National consumer price index (IPCA) - in 12 months and  Broad national consumer price index - Core IPCA trimmed means smoothed \tab 13522 and 4466 \cr
#'  \bold{'ulc'} \tab Unit labor cost - ULC-US$ - June/1994=100 \tab 11777 \cr
#'  \bold{'eap'} \tab Economically active population \tab 10810 \cr
#'  \bold{'cdb'} \tab Time deposits (CDB/RDB-preset) - Daily return (percentage) \tab 14 \cr
#'  \bold{'indprod'} \tab Prodcution Indicators (2012=100) - General	\tab 21859 \cr
#'  \bold{'selic'} \tab Interest rate - Selic accumulated in the month in annual terms (basis 252) \tab 4189 \cr
#'  \bold{'unemp'} \tab Open unemployment rate - by metropolitan region - Brasil (weekly) \tab 10777\cr
#'  \bold{'vargdp'} \tab GDP - real percentage change in the year \tab 7326 
#'}
#' 
#' 
#' @return If the parameter \code{file} is not set by the user, the chart will be placed in the 'graphs' directory, under the BETS installation directory. 
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}
#' 
#' @importFrom plotly export 
#' @import webshot 
#' @export


BETS.chart = function(ts, file = NULL, open = TRUE, lang = "en"){
  
  if(lang == "en"){
    Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
  }
  else if(lang == "pt"){
    Sys.setlocale(category = "LC_ALL", locale = "Portuguese_Brazil.1252")
  }
  else {
    return(invisible(msg(.MSG_LANG_NOT_AVAILABLE)))
  }
  
  if(!is.null(file)){
    
    dir.create("graphs", showWarnings = F)
    file = paste0("graphs","\\",ts)
    
    if(!grepl("\\.png$", file)) {
      file <- paste(file,".png",sep="")
    }  
  } 
  
  if(class(ts) == "character"){
    
    if(ts == "animal_spirits"){
      p = draw.animal_spirits()
    } else if(ts == "iie_br"){
      p = draw.iie_br()
    } else if(ts == "gdp_vars"){
      p = draw.gdp_vars()
    } else if(ts == "gdp_comps"){
      p = draw.gdp_comps()
    } else if(ts == "misery_index"){
      p = draw.misery_index()
    } else if(ts == "gdp_unemp"){
      p = draw.gdp_unemp()
    } else if(ts == "ei_vars"){
      p = draw.ei_vars()
    } else if(ts == "ei_comps"){
      p = draw.ei_comps()
    } else if(ts == "confidence"){
      p = draw.confidence()
    } else if(ts == "lab_mrkt"){
      p = draw.lab_mrkt()
    } else if(ts == "capacity"){
      p = draw.capacity()
    } else if(ts == "transf_ind"){
      p = draw.transf_ind()
    } else if(ts == "services"){
      p = draw.services()
    } else if(ts == "commerce"){
      p = draw.commmerce()
    } else if(ts == "construction"){
      p = draw.construction()
    } else if(ts == "consumer"){
      p = draw.consumer()
    } else {
      msg(paste("Plot was not created.",.MSG_PARAMETER_NOT_VALID))
    }
    
  } else {
    ## custom chart logic
  }
  
  
  if(!is.null(file)){
    
    tryCatch({
      export(p, file = file, zoom = 4, cliprect = c(20,20,740,500))},
      message = function(e){
        install_phantomjs() 
        export(p, file = file, zoom = 4, cliprect = c(20,20,740,500))
      })
    
    if(open){
      file.show(file)
    }
  }
  else {
    p
  }
}