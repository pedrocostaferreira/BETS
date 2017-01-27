# Caso base, default

BETS.report()

# Caso base, com codigo e parametros 

parameters = list(
  lag.max = 48,
  n.ahead = 12 ) 

BETS.report(ts = 21864, parameters = parameters)

# Caso base, objeto do tipo ts

BETS.report(ts = BETS.get(21864), parameters = parameters)

# Mais de uma serie, apenas IDs

series = list(4447, 21864)

parameters = list(
  lag.max = 48,
  n.ahead = 12 ) 


BETS.report(ts = series, parameters = parameters)

# Mais de uma serie, apenas objetos do tipo ts

series = list(BETS.get(4447), BETS.get(21864))

parameters = list(
  lag.max = 20,
  n.ahead = 15 ) 


BETS.report(ts = series, parameters = parameters)


# Uma serie, codigo, salvar no Desktop

BETS.report(ts = 4447, parameters = parameters, report.file = "C:/Users/Talitha/Desktop/TESTE")

# Uma serie, objeto ts, salvar no Desktop

BETS.report(ts = BETS.get(4447), parameters = parameters, report.file = "C:/Users/Talitha/Desktop/TESTE2")

# Duas series, lista mista, salvar no Desktop

series = list(4447, BETS.get(21864))

BETS.report(ts = series, parameters = parameters, report.file = "C:/Users/Talitha/Desktop/TESTE")

# Salvar previsoes

BETS.report(ts = 4447, parameters = parameters, series.saveas = "csv")
