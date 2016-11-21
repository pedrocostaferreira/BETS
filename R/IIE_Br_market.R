#' @title Uncertainty Indicator for the Brazilian Economy - Market
#'
#' @name IIE_Br_market
#' 
#' @description This \code{IIE_Br_market} measures the variability of the Brazilian assets market 
#' and its risk degree. Unlike the \link[BETS]{IIE_Br_expectation}, it seeks to capture the sentiments
#' of the market at the present time. It is composed by the last 21 days volatility, the IBOVESPA asset 
#' prices and the credit swap default's 5-year premium 
#'
#' @format A \code{\link[stats]{ts}} object with 185 observations.
#' 
#' @references A shiny app with IIE-Br-expectations plots can be found \href{https://pedroferreira.shinyapps.io/incerteza/}{here}
#' 
#' @author Pedro Costa Ferreira  \email{pedro.guilherme@fgv.br},
#'         Anna Carolina S. Barros \email{anna.barros@fgv.br},
#'         Bruno R de Miranda Neto \email{bruno.neto@fgv.br},
#'         Itaiguara de Oliveira Bezerra  \email{itaiguara.bezerra@fgv.br}.
#' @source 
#' \href{http://portalibre.fgv.br/}{Brazilian Institute of Economics (FGV|IBRE)} 
#'    
#' @keywords market, uncertainty 
NULL
