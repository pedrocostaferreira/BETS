library(BETS)

BETS.dashboard()
BETS.dashboard(saveas = "survey.pdf")

# É pra dar erro
BETS.dashboard(type = "none")

# With text, without logo

parameters = list(author = "FGV/IBRE", 
                  text = "text.txt",
                  url = "http://portalibre.fgv.br/")

BETS.dashboard(type = "macro_situation", parameters = parameters, saveas = "inst/inflation_dashboard.pdf")

# Without text, with logo

parameters = list(author = "FGV/IBRE", 
                  url = "http://portalibre.fgv.br/",
                  logo = "logo_ibre.png")

BETS.dashboard(type = "macro_situation", parameters = parameters, saveas = "inst/inflation_dashboard.pdf")

# With text, with logo

parameters = list(author = "FGV/IBRE", 
                  url = "http://portalibre.fgv.br/",
                  text = "text.txt",
                  logo = "logo_ibre.png")

BETS.dashboard(type = "macro_situation", parameters = parameters, saveas = "inst/inflation_dashboard.pdf")

