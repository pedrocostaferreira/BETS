#' Search for Sidra Series
#'
#' Searches the Sidra databases for a series by its description or a given table descriptions.
#'
#' @param x Either a character or a numeric. If character, function searches the Sidra metadata. If a numeric argument is provided the descriptions of the given table are seached .
#' @import xml2 rvest stringr


sidra.aux <- function(x, len, nova_req) {

    tabela <- xml2::read_html(paste0("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=", x))
    tabela <- rvest::html_text(tabela)
    
    
    
    
    
    d = strsplit(tabela, split = "/P/")
    d = strsplit(d[[1]][2], split = ":")
    d = trimws(d[[1]][1])
    
    
    
    if (d1 = stringr::str_count(d, "Ano") == 1){
        
        
        minus = to - from
        minus = floor(minus/ 3)
        
        for(i in len){
            
            tabela = data.frame()
            header2 = NULL
            
            for(j in seq(from,to, by=minus)){
            
                tabela1=RCurl::getURL(paste0("http://api.sidra.ibge.gov.br/values/",
                                        "t/", inputs[i], "/", territory, "/", "p/", 
                                        j, "-", (j+minus-1),  
                                        "/v/", variable[i], "/f/", "u", "/h/", header,
                                        sections[[i]]),
                                        ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
                
                
                t1 = paste("tabela", x, sep="_")
                tabela1 = rjson::fromJSON(tabela1)
                tabela1 = as.data.frame(do.call("rbind", tabela1))
                header2 = tabela1[1,]
                tabela = as.data.frame(do.call("rbind", list(tabela, tabela1[2:nrow(tabela1),])))
            }
            
            
            
        }
        
    } else if (d1 = stringr::str_count(d, "Mês") == 1){
        
        minus = to - from
        minus = floor(minus/ 3)
        
        
        
        for(i in len){
            
            tabela = data.frame()
            header2 = NULL
            
            for(j in seq()){        
        
                tabela=RCurl::getURL(paste0("http://api.sidra.ibge.gov.br/values/",
                                    "t/", inputs[i], "/", territory, "/", "p/", 
                                    from, "-", to,  
                                    "/v/", variable[i], "/f/", "u", "/h/", header,
                                    sections[[i]]),
                             ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
                
            }
            
        }
        

        
    } else if(d1 = stringr::str_count(d, "Trimestre") == 1){
        
        
        tabela=RCurl::getURL(paste0("http://api.sidra.ibge.gov.br/values/",
                                    "t/", inputs[i], "/", territory, "/", "p/", 
                                    from, "-", to,  
                                    "/v/", variable[i], "/f/", "u", "/h/", header,
                                    sections[[i]]),
                             ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
        

        
    }

    
    
    

    return(writeLines(d3))
}
