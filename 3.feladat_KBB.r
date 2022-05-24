
# --Legyenek x és y független normális eloszlású változók, várhatóértékük legyen 2 és 5, szórásuk 4 és 2.
# --Legyen z =  3x + 2y +epsilon, ahol epsilon sztenderd normális. 
# Tekintsünk egy olyan regressziót, ahol x-szel és y-nal akarjuk z-t magyarázni (ezek a megfigyelhető magyarázóváltozók, epsilon a zaj/nem megfigyelhető rész).
# Írj egy függvényt/algoritmust, ami 'numerikusan' becsli meg a z = beta1*x+beta2*y regressziót, azaz indulj ki valamilyen tetszőleges (véletlen) beta1 és beta2 értékből, számold ki ebből a becsült z értékeket és számold ki az átlagos négyzetes hibát. Ezután növeld vagy csökkentsd valamilyen egységgel az egyik (vagy mindkét) béta értéket és ismét számold ki a négyzetes hibát és vizsgáld meg hogy hogyan változott az előző becsléshez képest. Határozz meg valamilyen logikát/szabályt, hogy az eredmények függvényében (azaz nőtt-e vagy csökkent-e a hiba) merre változzanak a béta értékek, majd ismételd meg a lépéseket addig amíg valamilyen optimális(nak vélt) megoldást találsz. 

# Szorgalmiként lehet kisérletezni más függvényformákkal (pl z = e^x + 2y + epsilon és hasonlók) / több változóval és érdemes megvizsgálni a háromdimenziós "hibatérképet" is, azaz hogy adott (beta1,beta2) kombináció mellett mekkora az átlagos négyzetes hiba.

length<-500
#mean: 2, sd:4 
x<-rnorm(length, mean=2,sd=1)
#mean: 5, sd:2
y<-rnorm(length, mean=2,sd=1)
z<-3*x+2*y+rnorm(length, mean=0, sd=1)

numeric_estimator<-function(mse_maxval, increment, windowsize, originalts){
  #mse_maxval: the MSE of the regressed ts and the original regression will be lower than this value
  #increment: the increment for the different betas in the regression beta(t)=beta(t-1)+increment
  #window_size: the algorithm will check in each step for the optimal beta value in the range of [beta-windowsize/2, beta+windowsize/2]
  #originalts: the original z = beta1*x+beta2*y regression (which should be regressed with random betas)
  
  beta1<-runif(1, 0, 10) #adding a random value for each beta
  beta2<- runif(1, 0, 10)
  MSE<-mse_maxval+1 
  z<-originalts
  
  while (MSE>mse_maxval) {
    k<-1
    MSE_new<-MSE
    beta1_new<-0
    beta2_new<-0
    
    for (i in seq((beta1-(windowsize/2)),(beta1+(windowsize/2)),increment)){ 
      for (j in seq((beta2-(windowsize/2)),(beta2+(windowsize/2)),increment)){ 
        z_new=i*x+j*y
        MSE_new[k]=mean((z -  z_new)^2) 
        beta1_new[k]<-i
        beta2_new[k]<-j
        k<-(k+1)
      }
    }
    # more indexes with the same value: which(MSE_new == min(MSE_new))
    #chossing the first min index:
    best_index<-which.min(MSE_new)
    MSE<-min(MSE_new)
    beta1<-beta1_new[best_index]
    beta2<-beta2_new[best_index]
  }
  return(data.frame(beta1,beta2,MSE))
}

solution<-numeric_estimator(1,0.1,1,z)
solution
