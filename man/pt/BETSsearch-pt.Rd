\name{BETSsearch}
\alias{BETSsearch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pesquisa nomes, fontes, periodicidades, unidades e codigos de series temporais disponiveis na base de dados do Brasilian Economics Times Siries.
%%  ~~function to do ... ~~
}
\description{

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BETSsearch(name,Source,periodicity,unit,code,view=TRUE,data) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
    Nome completo, ou parte do nome da serie.
  }
  \item{Source}{
    Fonte das series temporais, dais quais sao: IBGE, BCB, BCB e Deban, BCB e FGV, BCB-Depin, BCB-Derin, BCB-Desig, BCB-Secre, BCB-Demab, BCB-Denor, FGV, Sisbacen e Abecip e BCB-Depec.
  }
  \item{periodicity}{
    Todas as series temporais possuem periodicidade, na base de dados ha series mensais, anuais, diarias, trimestrais e semanais.
  }
  \item{code}{
%%     ~~Describe \code{x} here~~
     Como na funcao get.BETS.ts, ha a possibilidade de pesquisa pelo codigo das series.
}
  \item{data}{
      Parametro destinado a base de dados que sera feita a pesquisa. Por defaut, bacen.

  }
    \item{unit}{
        Unidade que e utilizada para medir a serie temporal. Sao em percentual, por unidade, R$, $ e etc.
  }
  \item{view}{ 
     Para mostrar o resultado em uma nova aba ou apenas imprimir no console. Recebe TRUE ou FALSE respectivamente.
  }
  
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Objeto do tipo list de tamanho 5  com  estetica de um data.frame.
Abaixo o que cada dimensao concentra: 
\item{Codes }{Codigos das series.}
\item{Descripition}{Descricao das seires.}
\item{Periodicity}{Periodicidade das seires.}
\item{source}{Fonte des series.}
\item{unit}{Unidade da serie.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
Banco Central do Brasil:
Time Series Management System - v2.1 (Public module)

url: \link[https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSerie]{Time Series Management System - v2.1}

}
\author{
%%  ~~who you are~~

}


\note{
%%  ~~further notes~~
A estrutura da get.BETS.ts usa outras funcoes de pacotes como sqldf (Manipulate R data frames using SQL/G. Grothendieck).

Como se trata de uma funcao para pesquisa e considerando que ela tem 5 filtros. Nao ha ainda a possibilidade de fazer pesquisa com todos os argumentos juntos, no entanto existe os seguintes agrupamentos de filtros de pesquisa: nome e periodicidade, fonte e periodicidade, fonte e nome, codigo e periodicidade e, por fim, periodicidade e unidade.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

## The function is currently defined as
BETSsearch(name="sales")
#BETS-package: 55 of 12981 time series !
searchBETS(code= 4500)
#BETS-package: DONE!
BETSsearch(Source="Denor")
#BETS-package: 1 of 12981 time series !
BETSsearch(periodicity="A")
#BETS-package: 2308 of 12981 time series!

#Como mencionado em 'Note', segue os 
#exemplos de pesquisas combinadas:
BETSget(name="production",periodicity="M",view=FALSE)
BETSget(Source=BCB,periodicity="M",view=TRUE)
BETSget(name="production",Source="IBGE",view=TRUE)
BETSget(unit="index",periodicity="M",view=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{search}
%\keyword{ ~ }% __ONLY ONE__ keyword per line
