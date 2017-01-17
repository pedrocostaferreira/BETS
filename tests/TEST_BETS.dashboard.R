parameters = list(author = "FGV/IBRE", 
                  text = "text.txt",
                  url = "http://portalibre.fgv.br/",
                  logo = "logo_ibre.png")

BETS.dashboard(parameters = parameters, saveas = "inst/inflation_dashboard.pdf")

BETS.chart("ipca_with_core", out = "pdf", open = TRUE)
