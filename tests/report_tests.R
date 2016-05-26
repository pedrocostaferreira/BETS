library(BETS)

## default: producao de bens intermediarios 
BETSreport()


## producao de insumos para construcao civil
parameters = list(
  lag.max = 10,
  n.ahead = 18 
)
BETSreport(code = 21868, parameters = parameters)

## producao de bens de capital
parameters = list(
  lag.max = 20,
  n.ahead = 12 
)
BETSreport(code = 21863, parameters = parameters)

## producao de bens semi-duraveis e nao duraveis
parameters = list(
  lag.max = 20,
  n.ahead = 12 
)
BETSreport(code = 21867, parameters = parameters)

## producao de bens de consumo
parameters = list(
  lag.max = 30,
  n.ahead = 24 
)
BETSreport(code = 21865, parameters = parameters)

## desemprego
parameters = list(
  lag.max = 24,
  n.ahead = 12 
)
BETSreport(code = 10777, parameters = parameters)

## ipca acumulado em 12 meses
parameters = list(
  lag.max = 24,
  n.ahead = 12 
)
BETSreport(code = 13522, parameters = parameters)

## retorno acumulado em fundos de acoes
parameters = list(
  lag.max = 24,
  n.ahead = 12 
)
BETSreport(code = 7834, parameters = parameters)

