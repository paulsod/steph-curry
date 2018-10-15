MadeShots <- ts(CurryShots$X3)
plot.ts(MadeShots)
acf(MadeShots)
pacf(MadeShots)

diff1=diff(MadeShots)
plot(diff1)
acf(diff1)
pacf(diff1)

diff2=diff(diff1)
plot.ts(diff2)

library(forecast)
auto.arima(MadeShots)
fit <- auto.arima(MadeShots)
arima112=arima(MadeShots, order = c(1,1,2))
summary(arima112)
t(confint(arima112))

arima212=arima(MadeShots, order= c(2, 1, 2))
summary(arima212)
t(confint(arima212))

arima213=arima(MadeShots,order = c(2,1,3))
summary(arima213)
t(confint(arima213))

arima312=arima(MadeShots, order = c(3,1,2))
summary(arima312)
t(confint(arima312))

arima113=arima(MadeShots, order = c(1,1,3))
summary(arima113)
t(confint(arima113))

ShotRes = arima112$residuals
plot.ts(ShotRes)
plot(rstandard.Arima(arima112))
acf(ShotRes)
pacf(ShotRes)
hist(ShotRes, freq=FALSE)
t <- -100:1000/10
lines(t, dnorm(t, 0, 2.1), col = 'darkgreen')
qqnorm(ShotRes); qqline(ShotRes)
shapiro.test(ShotRes)
ks.test(ShotRes,pnorm,alternative = c("two.sided", "less", "greater"),exact = NULL)

Box.test(ShotRes, lag = 1, type="Ljung")
Box.test(ShotRes, lag = 50, type="Ljung")
Box.test(ShotRes, lag = 100, type="Ljung")
Box.test(ShotRes, lag = 150, type="Ljung")
Box.test(ShotRes, lag = 200, type="Ljung")
Box.test(ShotRes, lag = 250, type="Ljung")


Altres=arima212$residuals
plot.ts(Altres)
plot(rstandard.Arima(arima212))
acf(Altres)
pacf(Altres)
hist(Altres)
qqnorm(Altres); qqline(Altres)
shapiro.test(Altres)
ks.test(Altres,pnorm,alternative=c("two.sided", "less","greater"),exact=NULL)

Box.test(Altres, lag = 1, type="Ljung")
Box.test(Altres, lag = 50, type="Ljung")
Box.test(Altres, lag = 100, type="Ljung")
Box.test(Altres, lag = 150, type="Ljung")
Box.test(Altres, lag = 200, type="Ljung")
Box.test(Altres, lag = 250, type="Ljung")

library(tseries)
adf.test(diff1, alternative=c("stationary"))

library(TSA)
McLeod.Li.test(arima112)
McLeod.Li.test(arima212)


library(stats)
predict.Arima(arima112)

fcast <- forecast(fit)
plot(fcast)
grid()
fcast
