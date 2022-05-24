 

#regressziót kell felírni, hogy a részvény hozamát egy szektor index hozamával
#más fajta statisztikákat is fel lehet használni

#Töröld ki a 2008-2009-es válságra vonatkozó adatokat a !részvény! idősorából 
#A feladat hogy regressziós modell segítségével megpróbáljuk backfillelni (visszabecsülni) a részvény (napi) hozamait a válság alatti időszakra
#A regresszióhoz magyarázóváltozóként használjátok az S&P500 hozamát és a szektor index hozamát, illetve felhasználhattok a részvényárfolyamból számolt későbbi statisztikákat is (pl.: szezonalitás, realizált volatilitás), vagy az indexek hasonló statisztikáit
#training(+teszt/validáció) halmaznak használjátok a 2009 utáni adatokat

#ábrázoljátok a visszabecsült és az eredeti hozamokat idősorosan

#Szorgalmi: tegyük fel hogy nem csak a hozamokat szeretnénk visszatölteni, hanem az árfolyam szintjét is, mi változik? Dolgozz ki egy olyan becslést, ami 'hihetően' összeköti a 2008 előtti adatokat a 2009 utániakkal

library(readr)
library(tseries)
library(forecast)

#period: 2000-2021.04.28
T <- read_csv("C:/Users/kbogi/Downloads/T.csv") #AT&T Inc. legnagyobb telekommunikációs váll.
SPY <- read_csv("C:/Users/kbogi/Downloads/SPY.csv") #SPDR S&P 500 ETF Trust (SPY)
X_IXTC <- read_csv("C:/Users/kbogi/Downloads/^IXTC.csv") #NASDAQ Telecommunications Index
plot(T$Date,T$Close)

logreturnT=diff(log(T$Close))
logreturnX=diff(log(X_IXTC$Close))
logreturnS=diff(log(SPY$Close))

dateTS=T$Date[2:length(T$Date)]
plot(dateTS,logreturnT) #daily logreturn
adf.test(logreturnT) #stac.
adf.test(logreturnS)
adf.test(logreturnX)

enddate=as.Date("2009-12-31")
LBdate=as.Date("2008-09-14")

training_SPY=rev(logreturnS[SPY$Date>enddate])
training_T=rev(logreturnT[T$Date>enddate])
training_X_IXTC =rev(logreturnX[X_IXTC$Date>enddate])

test_SPY=rev(logreturnS[enddate>=SPY$Date & SPY$Date>LBdate])
test_T=rev(logreturnT[enddate>=T$Date& T$Date>LBdate])
test_X_IXTC =rev(logreturnX[enddate>=X_IXTC$Date&X_IXTC$Date>LBdate])

#feladat:regressziós modell segítségével megpróbáljuk backfillelni (visszabecsülni)

training_data=data.frame(training_SPY,training_X_IXTC,training_T)
fit=lm(training_T~training_SPY+training_X_IXTC, data=training_data)

#prediction_T=predict(fit,data=test_data)

#bena voltam az elnevezeseknel
training_SPY=test_SPY
training_X_IXTC=test_X_IXTC
test_data=data.frame(training_SPY,training_X_IXTC)
#forecast_T <- forecast(fit,test_data,h=328)

#Biroga Bence nyomán:
estimated=fit$coefficients[1]+fit$coefficients[2]*test_SPY+fit$coefficients[3]*test_X_IXTC
ts.plot(test_T,col="green")
lines(estimated)
cor(estimated,test_T)
#ez a fordított irányból ábrázolt még azábrán

############


plot(forecast_T)
lines(rev(test_T)) #vissza

############




