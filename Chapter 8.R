library(fpp2)
library(forecast)
library(rdatamarket)
library(tseries)
## 2 ##
ggtsdisplay(ibmclose)

## 3 ##

## usnetelec ##
autoplot(usnetelec)
Box.test(diff(usnetelec), type = "Ljung-Box")
kpss.test(diff(usnetelec))

## usgdp ##
autoplot(usgdp)
Box.test(diff(usgdp), type = "Ljung-Box")
autoplot(diff(usgdp))
ndiffs(usgdp)
Box.test(diff(diff(usgdp)), type = "Ljung-Box")
kpss.test(diff(diff(usgdp)))

## 7 ## 
autoplot(wmurders)
autoplot(diff(wmurders))
autoplot(diff(diff(wmurders)))
kpss.test(diff(diff(wmurders)))

wmurder_arima <- arima(wmurders, order = c(0, 2, 2))
checkresiduals(wmurder_arima)

forc_wmurder <- forecast(wmurder_arima, h = 3)
autoplot(forc_wmurder)

fc_auto <- forecast(auto.arima(wmurders),h = 3)
autoplot(fc_auto)

## 8 ## 

austa_arima <- forecast(auto.arima(austa), h =10)
autoplot(austa_arima)

fc_arima.0.1.1 <- forecast(Arima(austa, order = c(0, 1, 1)), h = 10)
autoplot(fc_arima.0.1.1)

fc_arima.2.1.3 <- forecast(Arima(austa, order = c(2, 1, 3)), h = 10, include.drift = TRUE, include.constant = FALSE)
autoplot(fc_arima.2.1.3)

fc_arima.0.0.1 <- forecast(Arima(austa, order = c(0, 0, 1)), h = 10)
autoplot(fc_arima.0.0.1)

fc_arima.0.2.1 <- forecast(Arima(austa, order = c(0, 2, 1)), h = 10)
autoplot(fc_arima.0.2.1)

## 9 ##
autoplot(usgdp)

Box.test(diff(diff(usgdp)), type = "Ljung-Box")
kpss.test(diff(diff(usgdp)))

gdp_lambda <- BoxCox.lambda(usgdp)
auto_gdp <- auto.arima(usgdp, lambda = gdp_lambda)

autoplot(usgdp, series = "Data") +
  autolayer(auto_gdp$fitted, series = "Fitted")

auto_gdp
accuracy(auto_gdp)

fc_auto <- forecast(auto_gdp)
autoplot(fc_auto)

## 10 ##

autoplot(austourists)
ggAcf(austourists)
ggPacf(austourists)

ggtsdisplay(diff(austourists, lag = 4))
ggtsdisplay(diff(diff(austourists, lag = 4)))

tourist_auto <- forecast(auto.arima(austourists))
autoplot(tourist_auto)
accuracy(tourist_auto)
checkresiduals(tourist_auto)

## 12 ##
autoplot(mcopper)
copper_lambda <- BoxCox.lambda(mcopper)
copper_arima <- auto.arima(mcopper, lambda = copper_lambda)
copper_arima
checkresiduals(copper_arima)
fc_copper_arima <- forecast(copper_arima)
autoplot(fc_copper_arima)


## 13 ##
autoplot(qauselec)
ndiffs(qauselec)
qauselec_lambda <- BoxCox.lambda(qauselec)
qauselec_auto <- auto.arima(qauselec, lambda = qauselec_lambda)
qauselec_auto
checkresiduals(qauselec_auto)
accuracy(qauselec_auto)

## 14 ##
fc_qauselec <- stlf(
  qauselec, lambda = BoxCox.lambda(qauselec),
  s.window = 5, robust = TRUE, method = "arima",
  h = 8)
autoplot(fc_qauselec)
