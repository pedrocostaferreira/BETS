#' @title  Create a chart with BETS aesthetics
#' 
#' @description  Create a professional looking chart, using a pre-defined BETS series or a custom series.
#' 
#' @note It is not yet possible to make charts of custom series. 
#' 
#' @param ts A \code{character} or a \code{ts} object. A custom time series or the name of a pre-defined series. A complete list of names is under the 'Details' section.
#' @param style A \code{character}. Should the chart be made with Plotly (style = "plotly") or with R standard library (style = "normal")?
#' @param lang A \code{character}. The language. For now, only 'en' (english) is available.
#' @param file A \code{character}. The whole path, including a custom name, for the output (an image file). The default value is NULL. If left to NULL, the chart will be rendered in the standard R plotting area.
#' @param open  A \code{boolean}. Whether to open the file containing the chart.
#' @param params A \code{list}. Parameters for drawing custom charts. See the 'details' section. 
#' 
#' @details 
#' 
#' \bold{Names of pre-defined charts:}
#' 
#' Business Cycle Dashboard:
#' 
#' \tabular{lll}{
#'  VALUE \tab DESCRIPTION \tab CODE \cr
#'  \bold{'iie_br'} \tab Uncertainty Index \tab ST_100.0 \cr
#'  \bold{'sent_ind'} \tab Economic Sentiment Index (average between several confidence indexes) \tab (*) \cr
#'  \bold{'gdp_mon'} \tab GDP Monthly and Interanual Variation (last values) - GDP Monitor (FGV/IBRE) \tab (*) \cr
#'  \bold{'ei_vars'} \tab Economic Indicators (Leading and Coincident) monthly variation \tab (*) \cr
#'  \bold{'ei_comps'} \tab Economic Indicators (Leading and Coincident) components variation  \tab (*) \cr
#'  \bold{'lei'} \tab Leading Economic Indicator (LEI - FGV/IBRE with The Conference Board) \tab (*) \cr
#'  \bold{'cei'} \tab Coincident Economic Indicator (CEI - FGV/IBRE with the Conference Board)  \tab (*) \cr
#'  \bold{'gdp_vars'} \tab GDP components variation (whole series) - GDP Monitor (FGV/IBRE) \tab (*) \cr
#'  \bold{'misery_index} \tab Misery Index \tab 13522 plus 24369 \cr
#'  \bold{'gdp_comps'} \tab GDP components variation (last values) - GDP Monitor (FGV/IBRE) \tab (*) \cr
#'  \bold{'gdp_unemp'} \tab GDP monthly levels versus Unemployement Rate \tab 22109 and 24369 \cr
#'  \bold{'conf_lvl'} \tab Enterprises Confidence Index versus Consumers Confidence Index \tab (*) \cr
#'  \bold{'inst_cap'} \tab Installed Capacity Index \tab (*) \cr
#'  \bold{'lab_lead'} \tab Labor Leading Indicator \tab (*) \cr
#'  \bold{'lab_coin'} \tab Labor Coincident Indicator \tab (*) \cr
#'  \bold{'transf_ind'} \tab Transformation Industry Confidence Index (Expectations versus Present Situation) \tab (*) \cr
#'  \bold{'servc'} \tab Services Confidence Index (Expectations versus Present Situation) \tab (*) \cr
#'  \bold{'constr'} \tab Construction Confidence Index (Expectations versus Present Situation) \tab (*) \cr
#'  \bold{'retail'} \tab Retail Sellers Confidence Index (Expectations versus Present Situation) \tab (*) \cr
#'  \bold{'consm'} \tab Consumer Confidence Index (Expectations versus Present Situation) \tab (*) 
#'}
#'
#' Macro Situation Dashboard:
#' 
#' #' \tabular{lll}{
#'  VALUE \tab DESCRIPTION \tab CODE \cr
#'  \bold{'ipca_with_core'} \tab National consumer price index (IPCA) - in 12 months and  Broad national consumer price index - Core IPCA trimmed means smoothed \tab 13522 and 4466 \cr
#'  \bold{'ulc'} \tab Unit labor cost - ULC-US$ - June/1994=100 \tab 11777 \cr
#'  \bold{'eap'} \tab Economically active population \tab 10810 \cr
#'  \bold{'cdb'} \tab Time deposits (CDB/RDB-preset) - Daily return (percentage) \tab 14 \cr
#'  \bold{'indprod'} \tab Prodcution Indicators (2012=100) - General	\tab 21859 \cr
#'  \bold{'selic'} \tab Interest rate - Selic accumulated in the month in annual terms (basis 252) \tab 4189 \cr
#'  \bold{'unemp'} \tab Unemployment rate - by metropolitan region (PNAD-C) \tab 10777\cr
#'  \bold{'vargdp'} \tab GDP - real percentage change in the year \tab 7326 
#'}
#' 
#' (*) Not available on BETS databases yet. But you can find it in .csv files saved under your BETS installation directory.
#' 
#' @return If parameter \code{file} is not set by the user, the chart will be shown at the standard R ploting area. Otherwise, it is going to be saved on your computer.
#' 
#' @examples 
#' 
#' # BETS.chart(ts = "sent_ind", file = "animal_spirits", open = T)
#' # BETS.chart(ts = "gdp_mon", file = "gdp_mon.png", open = F)
#' # BETS.chart(ts = "misery_index")
#' # BETS.chart(ts = "transf_ind", file = "transf_ind.png", open = F)
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}
#' 
#' @importFrom plotly export 
#' @import webshot 
#' @export


BETS.chart = function(ts, style = "normal", file = NULL, open = TRUE, lang = "en", params = NULL){
  
  if(lang == "en"){
    Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
  }
  else if(lang == "pt"){
    Sys.setlocale(category = "LC_ALL", locale = "Portuguese_Brazil.1252")
  }
  else {
    return(invisible(msg(.MSG_LANG_NOT_AVAILABLE)))
  }
    
  not.set <- F
  
  if(!is.null(file)){
    
    dir.create("graphs", showWarnings = F)
    file = paste0("graphs","\\",file)
    
    if(!grepl("\\.png$", file) && !grepl("\\.pdf$",file)) {
      not.set <- T
      file <- paste(file,".png",sep="")
    }
  } 
  
  if(class(ts) == "character"){
    
    if(ts == "iie_br"){
      p = draw.iie_br()
    } else if(ts == "sent_ind"){
      p = draw.sent_ind()
    } else if(ts == "gdp_mon"){
      p = draw.gdp_mon()
    } else if(ts == "lab_lead"){
      p = draw.lab_lead()
    } else if(ts == "lab_coin"){
      p = draw.lab_coin()
    } else if(ts == "gdp_vars"){
      p = draw.gdp_vars()
    } else if(ts == "lei"){
      p = draw.lei()
    } else if(ts == "cei"){
      p = draw.cei()
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
    } else if(ts == "cap_utl"){
      p = draw.cap_utl()
    } else if(ts %in% c("transf_ind","servc","retail","constr","consm")){
      p = draw.survey(ts)
    } else {
        
      if(!is.null(file)){
          
        if(not.set){
            file <- sub("\\.png","\\.pdf",file)
        }
          
        if(grepl("\\.png", file)){
          png(file,width=728,height=478, pointsize = 15) 
        }
        else {
          pdf(file, width = 7, height = 4.5)
        } 
      }
      
      if(ts == "ipca_with_core"){
        draw.ipca()
      } else if(ts == "ulc"){
        draw.ulc()
      } else if(ts == "eap"){
        draw.eap()
      } else if(ts == "cdb"){
        draw.cdb()
      } else if(ts == "indprod"){
        draw.indprod()
      } else if(ts == "selic"){
        draw.selic()
      } else if(ts == "unemp"){
        draw.unemp()
      } else if(ts == "vargdp"){
        draw.vargdp()
      } else {
        msg(paste("Plot was not created.",.MSG_PARAMETER_NOT_VALID))
      }
      
      if(!is.null(file)){
        dev.off()
      }
    }
    
  } else {
    #p = draw.generic(ts, params, type)
  }
  
  if(!is.null(file)){
    
    tryCatch({
      export(p, file = file, zoom = 4, cliprect = c(20,20,740,500))},
      message = function(e){
        install_phantomjs() 
        export(p, file = file, zoom = 4, cliprect = c(20,20,740,500))
      },
      error = function(e){
        # do nothing
      })
    
    if(open){
      file.show(file)
    }
  }
  else {
    p 
  }
}