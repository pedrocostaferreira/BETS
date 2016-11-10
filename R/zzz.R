.onLoad <- function(lib, pkg)
{
  base::suppressMessages(base::library("mFilter"))
  
  mylib <- base::dirname(system.file(package = "BETS"))
  ver <- utils::packageDescription("BETS", lib = mylib)["Version"]
  txt <- c("\n",
           paste(sQuote("BETS"), "version:", ver),
           "\n",
           paste(sQuote("BETS"),
                 "Brazilian Economic Time Series"),
           "\n",
           paste("See",
                 sQuote("library(help=\"BETS\")"),
                 "for details"),
           "\n",
           paste("BugReports: https://github.com/pedrocostaferreira/BETS/issues"),
           "\n",
           paste("Maintainer: Pedro costa Ferreira <pedro.guilherme@fgv.br>"),
           "\n"
  )
  if(interactive() || getOption("verbose"))
    writeLines(strwrap(txt, indent = 4, exdent = 4))
}

