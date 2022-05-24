#5) -----------------
# Szimulálj véletlen mintát a kövektező idősorokból:

# AR(2):  x_t=0.3*x_t-1-0.5*x_(t-2) + e_t

# Bilinear ARMA: x_t=0.4*x_(t-1)-0.3*x_(t-2)+0.5*x_(t-1)*e_(t-1)+e_t

# Nemlineáris MA: x_t=e_t-0.3*e_(t-1)+0.2*e_(t-2)+0.4*e_(t-1)*e_(t-2)-0.25*(e_(t-2))^2


#Illessz mindhárom idősorra

# - ARIMA modellt
# - Neurális hálót különböző paraméterekkel (az nnfor package mlp függvénye könnyen használható erre).

# Hasonlítsd össze a modellek egylépéses előrejelzési teljesítményét a szokásos training+validáció+teszt halmazok használatával!
library(tseries)
library(forecast)
library(astsa)
library(nnfor)

l<-500 #length for the generated ts

#functions for the DGPes
AR2_generator<-function(length,beta1,beta2){
  # generating ts with the given length and beta parameters
  # AR(2):  x_t=beta1*x_t-1-beta2*x_(t-2) + e_t
  ar<-0
  ar[1]<-rnorm(1, mean=0, sd=1)
  ar[2]<-beta1*ar[1]+rnorm(1, mean=0, sd=1)
  for (k in 3: length){
    ar[k]=beta1*ar[k-1]+beta2*ar[k-2]+rnorm(1, mean=0, sd=1)
  }
  return(ts(ar))
}

bilinear_generator<-function(length,beta1,beta2,beta3){
  # generating ts with the given length and beta parameters
  #Bilinear ARMA: x_t=beta1*x_(t-1)+beta2*x_(t-2)+beta3*x_(t-1)*e_(t-1)+e_t
  
  rnorm(length, mean=0, sd=1) 
  e_t<-rnorm(length, mean=0, sd=1)
  ts<-0
  ts[1]<-e_t[1]
  ts[2]<-beta1*ts[1]+beta2*e_t[1]*ts[1]+e_t[2]
  for (k in 3: length){
    ts[k]=beta1*ts[k-1]+beta2*ts[k-2]+beta3*ts[k-1]*e_t[k-1]+e_t[k]
  }
  return(ts(ts))
}

nonlin_MA_generator<-function(length,beta1,beta2,beta3,beta4){
  # generating ts with the given length and beta parameters
  #Nonlinear MA: x_t=e_t+beta1*e_(t-1)+beta2*e_(t-2)+beta3*e_(t-1)*e_(t-2)-beta4*(e_(t-2))^2
  e_t<-rnorm(length, mean=0, sd=1)
  ts<-0
  ts[1]<-e_t[1]
  ts[2]<-beta1*ts[1]+e_t[2]
  for (k in 3: length){
    ts[k]=e_t[k]-beta1*e_t[k-1]+beta2*e_t[k-2]+beta3*e_t[k-1]*e_t[k-2]-beta4*(e_t[k-2])^2
  }
  return(ts(ts))
}

#basic function for the characteristics of the ts

adfkpss_plotacf<-function(ts){
  #plot, acf, pacf, stationarity, uniroot
    
  #plotting the data generating processes:
  ts.plot(ts)
  acf(ts) #autocorrelation
  pacf(ts)  #partial autocorrelation
  
  adf<-tseries::adf.test(ts)
  kpss<-tseries::kpss.test(ts)
  
  return(data.frame(adf[["p.value"]], kpss[["p.value"]]))
}


###############
ts_1<-AR2_generator(l,0.3,-0.5)
ts_2<-bilinear_generator(l,0.4,-0.3,0.5)
ts_3<-nonlin_MA_generator(l,-0.3,0.2,0.4,0.25)

adfkpss_plotacf(ts_1)
adfkpss_plotacf(ts_2)
adfkpss_plotacf(ts_3)

##############
#training & validation &test: 

#training: "A set of examples used for learning, that is to fit the parameters of the classifier."
training_index=seq(1,l*0.6,1)
#validation: "A set of examples used to tune the parameters of a classifier, for example to choose the number of hidden units in a neural network."
val_index=seq(l*0.6+1,l*0.8,1) 
#test: "A set of examples used only to assess the performance of a fully-specified classifier."
test_index=seq(l*0.8+1,l,1) 



ml_arma_fit<-function(ts, train, val, test,step){
  #with given training, validation and test datasets multilayer perceptron
  #train& val-> "uses a 20% validation set (randomly) sampled to find the best number of hidden nodes"
  mlp_ts<-0
  MSE_ML<-0
  
  #ML
  #we can choose more lags with increasing computational time
   mlp_ts<-mlp(ts(ts[c(train,val)]),hd.auto.type="valid", lags=1:2, difforder=0) 
   #hd: nr of hidden nodes->hd.auto.type optimizes the best hidden note
   MSE_ML<-mlp_ts[["MSE"]]
   
   k<-1
   MSE_arma<-0
   best_index<-0
   arma_val<-0
   MSE_ARIMA<-0
   arma_p<-0
   arma_q<-0
   
   
  #ARIMA
   adf<-tseries::adf.test(ts)
   if (adf[["p.value"]]>0.01){
     adfdiff<-tseries::adf.test(diff(ts))
     if (adfdiff[["p.value"]]>0.01){
       d<-2}
     else {d<-1
       }
   }
   else{
     d<-0
   }
   
   
   for (p in (1:5)){
     for (q in (1:5)){
       arma<-arima(ts[train],c(p,d,q))
       MSE_arma[k]<-mean((arma[["residuals"]])^2)
       k<-k+1
       arma_p[k]<-p
       arma_q[k]<-q
         }
      }
  best_index<-which.min(MSE_arma)
  arma_val<-arima(ts[val],c(arma_p[best_index],d,arma_q[best_index]))
  
  MSE_ARIMA<-mean(arma_val[["residuals"]]^2)
  
  forecastedml<-forecast(mlp_ts, step, y=ts[test-1])
  ml_t1<-forecastedml_1[["mean"]][["t+1"]]
  
  forecastedarima=sarima.for(ts[test-1],arma_p[best_index],d,arma_q[best_index], n.ahead=step)
  arima_t1<-forecastedarima[["pred"]]
    return(data.frame(MSE_ARIMA,MSE_ML,arma_p[best_index],d,arma_q[best_index],ml_t1,arima_t1))
}



mlp_ts1=ml_arma_fit(ts_1,training_index,val_index,test_index,1)




#END
##################

#forecastedml_1<-forecast(mlp_ts, y=ts_1[test_index])


#on the validation data: we optimize hyperparameters

#on the test data : we evaluate the final model for comparison with other models

################xx
#ARMA 
#auto.arima(ts_1[test_index]) 
#auto.arima(ts_2[test_index])
#auto.arima(ts_3[test_index]) 

arima_reg<-function(ts,step){
b<-auto.arima(ts)
forecasted_ts=sarima.for(ts[val_index],b[["arma"]][1],b[["arma"]][2],0, n.ahead=step)
plot.ts(forecasted_ts)
return(forecasted_ts)
#lines(ts[(l-1):l],col="green")
}

forecast1<-arima_reg(ts(ts_1[test_index]),1)



forecasted_ts=sarima.for(ts(ts_1[val_index]),1,1,1, n.ahead=1)


#####################
#orai:
mlp(ts_1, lags=1:24) #lag selection
mlp(x, model=fit1) #existing model
mlp(x, model=fit1, retrain=TRUE) #existing model retrain ("if a previous model is provided, retrain the network or not.")

#######

k<-1
arma_p<-0
arma_q<-0

for (p in (1:5)){
  for (q in (1:5)){
    arma<-arima(ts(ts_1[train]),c(p,d,q))
    MSE_arma[k]<-mean((arma[["residuals"]])^2)
    k<-k+1
    arma_p[k]<-p
    arma_q[k]<-q
  }
}