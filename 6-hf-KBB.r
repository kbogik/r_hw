##HOMEWORK## Toltsd be a kovetkezo idosort:
volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
#### Valtoztasd idosorra, plottold, majd valtoztasd kozel stacionerrĂ©.
#### Valaszd le az idosor utolso 20 elemet, a maradekra
#### illessz egy ARIMA modellt, es jelezz elore vele 20 idopontnyi tavolsagra.
#### Abrazold (hasznalhatod az alap plot fuggvenyeket vagy ggplotot) az elorejelzett es a valodi ertekeket

library(tseries)
library(forecast)

volcano <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
vol=volcano[1:(length(volcano)-20)]
#volcano=volcano[(length(volcano)-20): length(volcano)]

#stac. test:
tseries::adf.test(vol)
tseries::kpss.test(vol)
#mindketto alapjan stacioner
plot(vol)
acf(vol) 
pacf(vol)

# auto.arima helyett :
#acf alapján ránézésre van benne MA tag és a stac. vizsgálat alapján
#az ARIMA() modellben a d értéke 0.
#,include.mean=T nélkül
mod1=arima (vol, order=c(0,0,1)
mod2=arima (vol, order=c(0,0,2)
mod3=arima (vol, order=c(1,0,1)
mod4=arima (vol, order=c(1,0,2)
#akaike és bayesi információs krit alapján nem egyértelmű az MA tag rendje
AIC(mod1,mod2,mod3,mod4)
BIC(mod1,mod2,mod3,mod4) 

auto.arima(vol)
#arima(1,0,2)

#autokorreláltak-e a reziduumok?
acf (mod4$resid)
#nem
jarque.bera.test (mod4$resid) #mondjuk nem norm. eloszlású:(

#sarima.for(vol,1,0,2, n.ahead=20)
forecasted_dust=sarima.for(vol,1,0,2, n.ahead=20)
plot(forecasted_dust)
lines(volcano,col="green")

######################

arima_vol <- arima(vol,order=c(1,0,2))
forecast_vol <- forecast(arima_vol, h=20)

################
#ha nem lett volna stac, differenciáztam volna vagy "differenciázás és logaritmizálás"
tseries::adf.test(diff((vol)))
tseries::kpss.test(diff((vol)))
