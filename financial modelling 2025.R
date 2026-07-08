rm(list=ls())
library("quantmod")
library("forecast")
library("lmtest")
install.packages("vrtest")
library("vrtest")
source("expforecast.R")


#getSymbols("^GSPC", from='2010-01-01',to='2025-01-01')
load("GSPC.RData")#to imporrt prices from yahoo finance

head(GSPC)
prices <- log(GSPC$GSPC.Adjusted) #to get adjusted closing prices
plot(prices)
Acf(prices)#Autocorrelation function

ln_ret <- diff(log(prices))
ln_ret <- ln_ret[-1, ]

plot(ln_ret)

Acf(ln_ret)

Box.test(ln_ret, lag = 36, type = c("Ljung-Box"))#test forRW3

Lo.Mac(ln_ret, kvec=c(2,3,5))

auto.arima(ln_ret)
auto.arima(ln_ret, d = 0, D = 0, max.p = 5, max.q = 5)
#right here we cheated beacause we chose the best model for forecasting in the full sample including future data
#while in real life we forecast using only past data/half sample not the full sample because realistically we won't have future data
model <- arima(ln_ret, order=c(4,0,4))
print(model)
coeftest(model)

Box.test(model$residuals, lag = 36, type = c("Ljung-Box"))

Lo.Mac(model$residuals, kvec=c(2,3,5))

half_sample_size <- floor(length(ln_ret) / 2)

#err_ar4ma4 <- expforecast (ln_ret, 4, 4, half_sample_size, 1)
load("err_ar4ma4.RData")
lnret_oos <- ln_ret[(half_sample_size+1):length(ln_ret)]  

mean_in_sample <- mean(lnret_oos)  

model_mse <- sum(err_ar4ma4^2)                 
naive_mse <- sum((lnret_oos - mean_in_sample)^2)    

r2_oos <- 1 - (model_mse / naive_mse)

print(r2_oos) #Take results with a bit of care