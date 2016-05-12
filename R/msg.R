#' @import stringr


msg <- function(..., skip_before=TRUE, skip_after=FALSE) {

  wrapped <- strwrap(str_c(..., sep = " "),
                     width = getOption("width") - 2)
  k <- str_c("BETS-package: ", wrapped, collapse = "\n")
  if(skip_before) k <- paste0("\n", k)
  if(skip_after) k <- paste0(k, "\n")
  Encoding(k) <- "UTF-8"
  message(k)
}

