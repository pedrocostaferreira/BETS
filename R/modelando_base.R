#---------------------------------------------------------------------#
#script para reorganizar a estrutura das series temporais do IBRE|FGV #
#---------------------------------------------------------------------#



modelando_base <- function(dados){
datas = dados[,1]
data_certa= NULL
for(i in 1:length(datas)){
  ano = substr(datas[i],1,4)
  mes = substr(datas[i],5,6)
  dia = substr(datas[i],7,8)
  data_certa[i] = paste(ano,mes,dia,sep="-")
}

data = data_certa
codigos = colnames(dados)
codigos = codigos[2:length(codigos)]
base = dados[,-1]
linhas = nrow(base)
colunas = ncol(base)


ts_mensal_ibre = data.frame(NA,NA,NA)
colnames(ts_mensal_ibre) = c("data","valor","serie")
ts_mensal_ibre = na.omit(ts_mensal_ibre)

for(j in 1:colunas ){
  valor = NULL
  serie = NULL
  for(i in 1:linhas){
    print(paste("linha",i,sep=" "))
      serie = rep(codigos[j],time = linhas)
      valor[i] = base[i,j]
      data = data_certa
  }
  aux = data.frame(data,valor,serie)
  ts_mensal_ibre = rbind(ts_mensal_ibre,aux)
  }
  ts_mensal_ibre
}





