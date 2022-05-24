library(AmesHousing)
#library(pracma)
#install.packages("AmesHousing")
data("ames_raw")
data_ames=as.data.frame(ames_raw)

#for a holdout_sample use these 3:
#holdout_index=seq(20,dim(data_ames)[1],20)
#test_index=seq(1,dim(data_ames)[1],1)
#test_index[! test_index %in% holdout_index]

#for another test and validation data
holdout_index=seq((dim(data_ames)[1])/2,(dim(data_ames)[1]),1)
test_index=seq(1,dim(data_ames)[1],1)
test_index[! test_index %in% holdout_index] 

#funtion for calculating Rsquare of a model
linreg <- function(formula, data, indices) { #4.ora R
  d <- data[indices,]
  fit <- lm(formula, data=d)
  first=summary(fit)$r.square
  #second=predict(fit, data=data_ames[holdout_index])
  return(first)
}
#calculating r square for 2 models:
R_test=linreg(SalePrice~`Overall Qual`,data=data_ames,test_index)
R_test=linreg(SalePrice~data_ames[,56],data=data_ames,test_index) #TotRmsAbvGrd

#simple fit for each variable
#omit Order, PID and SalePrice, MS SubClass, MS Zoning
R_simple_reg=0
AIC_simple_reg=0
MSE_simple=0
index=0
k=1

for (j in 5:(dim(data_ames)[2]-1)){
  
  if (is.numeric(data_ames[,j])){
 model_test=lm(SalePrice~data_ames[,j],data=data_ames[test_index,])

 prediction_simple=predict(model_test,data_ames[holdout_index,])
 
 MSE_simple[k]=mean((data_ames$SalePrice -  prediction_simple)^2) #calculate MSE for the predicted and real values
 AIC_simple_reg[k]=AIC(model_test) #akaike inf.crit.
 R_simple_reg[k]=summary( model_test)$r.square #a modell illeszt�s R^2
 index[k]=j #identify which variable
 
 k=k+1
  }
}
#max(R_simple_reg)
min(MSE_simple)
simple_reg=data.frame(index,R_simple_reg,AIC_simple_reg,MSE_simple)
#best MSE & Rsquare for overall quality


model_test=lm(SalePrice~data_ames[,5],data=data_ames[test_index,])
prediction_wNA=prediction_simple=predict(model_test,data_ames[holdout_index,]) #NA->??


#factor:
#data_ames<-transform(data_ames, Utilities=factor(Utilities, labels = c("NoSeWa","AllPub","NoSewr")))
data_ames<-transform(data_ames, 
                     House.Style=factor(House.Style, labels = c("1Story","2Story","SFoyer","1.5Fin","SLvl","2.5Fin","2.5Unf","1.5Unf" )))

#model_wfactor=lm(SalePrice~data_ames$Utilities,data=data_ames[test_index,])
model_wfactor=lm(SalePrice~data_ames$`House Style`,data=data_ames[test_index,])
summary(model_wfactor)


#bootstrap
#ames.index <- seq(dim(data_ames)[1])
#boot.ames.index <- sample(ames.index, 2*dim(data_ames)[1], replace=TRUE)
#boot.ames <- data.frame("Overall Qual"=data_ames$'Overall Qual'[boot.ames.index], "SalePrice"=data_ames$SalePrice[boot.ames.index])


#multiple
ames_multiple= lm(formula=SalePrice ~data_ames[,19] + data_ames[,56],data=data_ames[test_index,])
summary(ames_multiple)
AIC(ames_multiple)

#forecast MSE
prediction_simple=predict(ames_multiple,data_ames[holdout_index,])
MSE_multiple=mean((data_ames$SalePrice -  prediction_simple)^2) 


#logistic regression
ames_log = glm(formula=SalePrice ~data_ames[,19] + data_ames[,56],data=data_ames[test_index,])
summary(ames_log)
MSE_multiple=mean((data_ames$SalePrice - predict(ames_log,data_ames[holdout_index,]))^2) 

#anova(ames_multipler,ames_log)

##################
#SalePrice
#Gr Liv Area: Above grade (ground) living area square feet
#HeatingQC: Heating quality and condition
#TotRmsAbvGrd: Total rooms above grade (does not include bathrooms)
#Pool Area: Pool area in square feet
# Misc Val: $Value of miscellaneous feature
#Exter Qual: Evaluates the quality of the material on the exterior
#Exter Cond: Evaluates the present condition of the material on the exterior
#Overall Qual: Rates the overall material and finish of the house
#Overall Cond: Rates the overall condition of the house
##

#H�zi feladat:

#K�sz�ts regresszi�t a k�vetkez� adatt�bl�n. T�ltsd be R-ben az "AmesHousing" csomagot, majd ebben
#h�vd meg az "ames_raw" adatt�bl�t. Mivel ez egy 'tibble' adatstrukt�ra (ezt m�g nem tanultuk), haszn�ld
#az as.data.frame parancsot, hogy data.frame legyen bel�le, mentsd el �gy. Ez egy ingatlan-adatb�zis,
#amelynek a 'SalePrice' v�ltoz�j�t szeretn�nk regresszi�val modellezni. 
#B�rmelyik m�sik v�ltoz�kat
#haszn�lhatj�tok ehhez, lehet pr�b�lkozni transzform�ci�kkal, v�ltoz�-�sszevon�sokkal. Haszn�ljatok
#kategorikus v�ltoz�kat is (as.factor lesz a kulcssz�), illetve pr�b�lj�tok ki az �r�n tanult teszteket,
#kiv�laszt�si met�dusokat, hogy hat�konyabb legyen a modellszelekci�! 
#V�lassz�tok k�l�n minden 20-
#  adik sor�t az adatb�zisnak (a 20-adikt�l kezdve) a holdout sample megk�pz�s�hez, de a training setben
#is k�pezzetek testing (validation) halmazt, hogy a modell ne illeszkedjen t�l.