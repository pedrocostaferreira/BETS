library(BETS)

## Business Cycle Dashboard

BETS.chart(ts = "animal_spirits", file = "animal_spirits", open = T)
BETS.chart(ts = "animal_spirits", file = "animal_spirits.png", open = F)
BETS.chart(ts = "animal_spirits")

BETS.chart(ts = "ei_vars", file = "ei_vars", open = T)
BETS.chart(ts = "ei_vars", file = "ei_vars.png", open = F)
BETS.chart(ts = "ei_vars")

BETS.chart(ts = "gdp_comps", file = "gdp_comps", open = T)
BETS.chart(ts = "gdp_comps", file = "gdp_comps.png", open = F)
BETS.chart(ts = "gdp_comps")

BETS.chart(ts = "gdp_unemp", file = "gdp_unemp", open = T)
BETS.chart(ts = "gdp_unemp", file = "gdp_unemp.png", open = F)
BETS.chart(ts = "gdp_unemp")

BETS.chart(ts = "gdp_vars", file = "gdp_vars", open = T)
BETS.chart(ts = "gdp_vars", file = "gdp_vars.png", open = F)
BETS.chart(ts = "gdp_vars")

BETS.chart(ts = "iie_br", file = "iie_br", open = T)
BETS.chart(ts = "iie_br", file = "iie_br.png", open = F)
BETS.chart(ts = "iie_br")

BETS.chart(ts = "misery_index", file = "misery_index", open = T)
BETS.chart(ts = "misery_index", file = "misery_index.png", open = F)
BETS.chart(ts = "misery_index")

BETS.chart(ts = "transf_ind", file = "transf_ind.png", open = T)
BETS.chart(ts = "transf_ind", file = "transf_ind", open = F)
BETS.chart(ts = "transf_ind")

BETS.chart(ts = "servc", file = "servc.png", open = T)
BETS.chart(ts = "servc", file = "servc", open = F)
BETS.chart(ts = "servc")

BETS.chart(ts = "retail", file = "retail.png", open = T)
BETS.chart(ts = "retail", file = "retail", open = F)
BETS.chart(ts = "retail")

BETS.chart(ts = "constr", file = "constr.png", open = T)
BETS.chart(ts = "constr", file = "constr", open = F)
BETS.chart(ts = "constr")

BETS.chart(ts = "consm", file = "consm.png", open = T)
BETS.chart(ts = "consm", file = "consm", open = F)
BETS.chart(ts = "consm")

BETS.chart(ts = "lab_mrkt", file = "lab_mrkt.png", open = T)
BETS.chart(ts = "lab_mrkt", file = "lab_mrkt", open = F)
BETS.chart(ts = "lab_mrkt")

BETS.chart(ts = "cap_utl", file = "cap_utl.png", open = T)
BETS.chart(ts = "cap_utl", file = "cap_utl", open = F)
BETS.chart(ts = "cap_utl")

BETS.chart("sent_ind", file = "sent_ind.png", open = T)
BETS.chart("gdp_mon", file = "gdp_mon.png", open = T)
BETS.chart("lei", file = "lei.png", open = T)

## Macro Situation Dashboard

BETS.chart("ipca_with_core", file = "ipca_with_core.png", open = F)
BETS.chart("ipca_with_core", file = "ipca_with_core.pdf", open = F)
BETS.chart("ipca_with_core", open = T)

BETS.chart("ulc", file = "ulc.png", open = F)
BETS.chart("ulc", file = "ulc.pdf", open = F)

BETS.chart("unemp", file = "unemp.png", open = F)
BETS.chart("unemp",  file = "unemp.pdf", open = F)

BETS.chart("vargdp", file = "vargdp.png", open = F)
BETS.chart("vargdp", file = "vargdp.pdf", open = F)

BETS.chart("indprod", file = "indprod.png", open = F)
BETS.chart("indprod", file = "indprod.pdf", open = F)

BETS.chart("eap", file = "eap.png", open = F)
BETS.chart("eap", file = "eap.pdf", open = F)

BETS.chart("cdb", file = "cdb.png", open = F)
BETS.chart("cdb", file = "cdb.pdf", open = F)

BETS.chart("selic", file = "selic.png", open = F)
BETS.chart("selic", file = "selic.pdf", open = F)


## Custom Charts

ts <- window(BETS.get(4537),start = c(2006,1))
    
params <- list(
  type = "lines",
  title = "General Government Debt",
  arr.len = 6,
  ylim = c(30,82),
  subtitle = "% GDP",
  legend = c("Gross", "Net"),
  extra = window(BETS.get(4536),start = c(2006,1)),
  extra.arr.ort = 'h',
  extra.arr.len = 1 
)

BETS.chart(ts = ts, style = "normal", file = "debt.png", open = T, params = params)

ts <- window(BETS.get(3545),start = 2006)/100

params <- list(
    type = "bar",
    title = "International Reserves",
    subtitle = "Total, US$ Billions",
    trend = T,
    colors = 'chocolate1'
)

BETS.chart(ts = ts, style = "normal", file = "int_res.pdf", open = T, params = params)

# par(mar = c(5.1,4.1,4.1,2.1))
# series = ts(1:20, start = 2000, end = 2019, frequency = 1)
# b = barplot(as.vector(series), names.arg = as.vector(time(series)), xlab = "", ylab = "",  xpd = FALSE)
# lines(y = as.vector(series), lwd = 2.5, lty = 2)
