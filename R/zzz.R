.onLoad <- function(lib, pkg)
{
  mylib <- dirname(system.file(package = "BETS"))
  ver <- packageDescription("BETS", lib = mylib)["Version"]
  txt <- c("\n",
           paste(sQuote("BETS"), "version:", ver),
           "\n",
           paste(sQuote("mFilter"),
                 "Brazilian Economic Time Series"),
           "\n",
           paste("See",
                 sQuote("library(help=\"BETS\")"),
                 "for details"),
           "\n",
           paste("BugReports: https://github.com/pedrocostaferreira/BETS/issues"),
           "\n"
  )
  if(interactive() || getOption("verbose"))
    writeLines(strwrap(txt, indent = 4, exdent = 4))
}

