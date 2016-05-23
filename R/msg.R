#' @title Format and show a console message.
#' 
#' @description Customizes a message and shows it in the console.
#' 
#' @param ... Arguments to be passed to \code{\link[base]{message}}
#' @param skip_before A \code{boolean}. Indicates if a line should be skipped before the message. 
#' @param skip_after A \code{boolean}. Indicates if a line should be skipped after the message. 
#' 
#' @return None
#' 
#' @import stringr


msg <- function(..., skip_before=TRUE, skip_after=FALSE) {
  
  m <- str_c("BETS-package: ", ...)
  if(skip_before) k <- paste0("\n", m)
  if(skip_after) k <- paste0(m, "\n")
  Encoding(k) <- "UTF-8"
  message(k)
  
  invisible(m)
}

