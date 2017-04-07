#' @title  Create a chart with BETS aesthetics
#' 
#' @description  Create a chart with a professional look, using a pre-defined BETS series or a custom series.
#' 
#' @param ts A \code{character} or a \code{ts} object. A custom time series or the name of a pre-defined series. A complete list of names is under the 'Details' section.
#' @param lang A \code{character}. The language. For now, only 'en' (english) is available.
#' @param file A \code{character}. The whole path, including a custom name, for the output (an image file). The default value is 'graphs//parameter_ts' (the 'graphs' directory is under the BETS installation directory).
#' @param open  A \code{boolean}. Whether to open the file containing the chart.
#' @param params A \code{list}. Parameters for drawing custom charts. See the 'details' section.
#' 
#' @details 
#' 
#' \bold{Names of pre-defined charts:}
#' 
#' \tabular{lll}{
#'  VALUE \tab DESCRIPTION \tab CODE \cr
#'  \bold{'animal_spirits'} \tab \tab (*) \cr
#'  \bold{'iie_br'} \tab \tab ST_100.0 \cr
#'  \bold{'ei_vars'} \tab \tab (*) \cr
#'  \bold{'ei_comps'} \tab  \tab (*) \cr
#'  \bold{'gdp_vars'} \tab \tab (*) \cr
#'  \bold{'misery_index} \tab \tab 13522 plus 24369 \cr
#'  \bold{'gdp_comps'} \tab  \tab (*) \cr
#'  \bold{'gdp_unemp'} \tab  \tab 22109 and 24369 \cr
#'  \bold{'conf_lvl'} \tab  \tab (*) \cr
#'  \bold{'inst_cap'} \tab  \tab (*) \cr
#'  \bold{'lab_mrkt'} \tab  \tab (*) \cr
#'  \bold{'transf_ind'} \tab  \tab (*) \cr
#'  \bold{'servc'} \tab  \tab (*) \cr
#'  \bold{'constr'} \tab  \tab (*) \cr
#'  \bold{'retail'} \tab  \tab (*) \cr
#'  \bold{'consm'} \tab \tab (*) 
#'}
#' 
#' (*) Not in BETS databases yet. But you can find it in .csv files saved under your BETS installation directory.
#' 
#' \bold{Parameters for custom charts:}
#' 
#' @return If parameter \code{file} is not set by the user, the chart will be shown at the standard R ploting area. Otherwise, it is going to be saved on your computer.
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}
#' 
#' @importFrom plotly export 
#' @import webshot 
#' @export


BETS.chart = function(ts, file = NULL, open = TRUE, lang = "en", params = NULL){
  
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
    } else if(ts == "conf_lvl"){
      p = draw.conf_lvl()
    } else if(ts == "lab_mrkt"){
      p = draw.lab_mrkt()
    } else if(ts == "cap_utl"){
      p = draw.cap_utl()
    } else if(ts %in% c("transf_ind","servc","retail","constr","consm")){
      p = draw.survey(ts)
    } else {
      msg(paste("Plot was not created.",.MSG_PARAMETER_NOT_VALID))
    }
    
  } else {
    #p = draw.generic(ts, params)
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