gdp = window(BETS.get(4382),start = c(1998,1))
ipca = window(BETS.get(13522),start = c(1998,1))


gdp_real = BETS.deflate(gdp, ipca, type = "point.perc")
gdp_real_norm = BETS.normalize(gdp_real, mode = "scale")
ipca_norm = BETS.normalize(ipca, mode = "scale")

mean.ipca = mean(ipca)
sd.ipca = sd(ipca)

lag.max = 2

series = vector(mode = "list")
series[[1]] = ipca_norm
series[[2]] = gdp_real_norm

complete = vector(mode = "list")
complete[[1]] = ipca_norm

lag.max = 2


nvars = length(series)
for(i in 1:nvars){
  s = 1 + (i-1)*lag.max
  for(j in 1:lag.max){
    complete[[s + j]] = lag(series[[i]],-j)
  }
}

train = vector(mode = "list")
test = vector(mode = "list")

for(i in 1:length(complete)){
  train[[i]] = window(complete[[i]], start = c(1999,1), end = c(2015,2))
  test[[i]] = window(complete[[i]], start = c(2015,3), end = c(2016,2),frequency=12)
}

## nao precisa rodar daqui pra frente

check = test[[1]]

sigma = 0.9
data_train = cbind(train[[1]],train[[2]], train[[3]],train[[4]],train[[5]])
data_train = data_train[-(1:2),]
data_test = cbind(test[[1]],test[[2]],test[[3]],test[[4]],test[[5]])

BETS.grnn(train,test, sigma = c(0.9,1))

nn = smooth(learn(data_train),sigma)
guess(nn, t(as.matrix(data_test[8,])))
