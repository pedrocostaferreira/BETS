#' Search for Sidra Series
#'
#' Searches the Sidra databases for a series by its description or a given table descriptions.
#'
#' @param x Either a character or a numeric. If character, function searches the Sidra metadata. If a numeric argument is provided the descriptions of the given table are seached .
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

BETS.sidra.search <- function(x, browse = FALSE) {
    
    
    if( is.character(x)){
    
        x <- stringr::str_replace_all(x, " ", "%20")
        
        tabela <- xml2::read_html(paste0("https://sidra.ibge.gov.br/Busca?q=", x))
        
        tabela <- rvest::html_nodes(tabela,".busca-link-tabela")
        tabela <- rvest::html_text(tabela)
        
        
        tabela <- stringr::str_replace(tabela, "Tabela ", "")
        tabela <- stringr::str_split(tabela, "-", n = 2)
        tabela <- matrix(trimws(unlist(tabela)), ncol = 2, byrow = TRUE)
        
        colnames(tabela) <- c("code", "description")
        msg(paste("Found", nrow(tabela), "results."))
        utils::View(tabela)
        
        
        # return(writeLines(tabela))
    } else if (is.numeric(x)){
    
    
        tabela <- xml2::read_html(paste0("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=", x))
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
        
        shell.exec(paste0("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=", x))
        } else{
            
            # utils::View(d3)
            return(writeLines(d3))
            
        }
    
    
    } else{ stop("x must be provided as a character or numeric.") }

}







# library(htmltools); View(html_print(pre(paste0(capture.output(print(mtcars)), collapse="\n"))))
