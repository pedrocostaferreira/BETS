#' Search for Sidra Series
#'
#' Searches the Sidra databases for a series by its description or a given table descriptions.
#'
#' @param description A character argument. Function searches the Sidra metadata and prints results in a window.
#' @param code A numeric argument must be provided. The descriptions of the given table are returned.
#' @param browse Logical. If browse is set to TRUE, the description table opens in your browser for better visualization.
#' @examples
#' \dontrun{
#' BETS.sidra.search("pib")
#' BETS.sidra.search(1248)
#' }
#' @keywords sidra IBGE
#' @importFrom utils View 
#' @importFrom htmltools html_print
#' @import xml2 rvest stringr 
#' @export 

BETS.sidra.search <- function(description = "*", code, browse = FALSE) {
    
    
    if(is.character(description) & missing(code)){
    
        description <- stringr::str_replace_all(description, " ", "%20")
        
        tabela <- xml2::read_html(paste0("https://sidra.ibge.gov.br/Busca?q=", description))
        
        tabela <- rvest::html_nodes(tabela,".busca-link-tabela")
        tabela <- rvest::html_text(tabela)
        
        
        tabela <- stringr::str_replace(tabela, "Tabela ", "")
        tabela <- stringr::str_split(tabela, "-", n = 2)
        tabela <- matrix(trimws(unlist(tabela)), ncol = 2, byrow = TRUE)
        
        colnames(tabela) <- c("code", "description")
        msg(paste("Found", nrow(tabela), "results."))
        utils::View(tabela)
        
        
        # return(writeLines(tabela))
    } else if (is.numeric(code)){
    
    
        tabela <- xml2::read_html(paste0("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=", code))
        tabela <- rvest::html_text(tabela)
        
        
        
        
        
        d = strsplit(tabela, split = "\r\n")
        d = trimws(d[[1]])
        d2 = c()
        
        for ( i in seq_along(d)){
            
            if(d[i] != ""){
                
                d2 = c(d2,d[i])
                
            }
            
        }
        
        d3 = paste(d2[10:length(d2)], collapse = "\n")
        
        
        
        
        
        if(browse != FALSE){
        
        shell.exec(paste0("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=", code))
        } else{
            
            # utils::View(d3)
            return(writeLines(d3))
            
        }
    
    
    } else{ stop("Either 'description' or 'code' must be provided as input.") }

}







# library(htmltools); View(html_print(pre(paste0(capture.output(print(mtcars)), collapse="\n"))))
