#HW: 

# Luke Skywalker X-wing gépével a Halálcsillag szellőzőnyílásába akar beletalálni. Nem tud pontosan
# célozni, de a síkot ismeri, amire merőlegesen kell lövéseket leadnia, és minden lövése megfelelő
# szögben csapódik be. A lövések viszont véletlenszerűen oszlanak el a szellőző körül, az 
#x tengely
# vonalán a szellőző pontjának helyén vett várható értékkel, de 1,5 méter szórással vett normális eloszlás
# alapján, 
#míg az y tengelyen egyenletes eloszlás alapján a szellőző 5 méter sugarú körében.
# a. Szimulálj 100 lövést Luke űrhajójával, és rajzold ki a lövések pontjait egy plot paranccsal!
#   Ábrázold a szellőző helyét is egy másik színnel! (Points parancs, pl.)
# b. Próbáljuk meg szimulációval megbecsülni, hogy mekkora valószínűséggel talál be Luke
# legalább egyszer a szellőző 10 cm-es környezetébe (ekkor már elpusztul a Halálcsillag), ha 100
# lövést ad le. És ha 100000-et?
#   c. Készíts egy algoritmust, ami azt az optimális (értsd, minél kevesebb) számú lövést keresi,
# amivel Luke már legalább 50% biztonsággal eltalálja ezt a környzetet 1szer. Használj
# szimulációkat, rendezd egy listába a szimulált lövéssorozatokat, listaelemenként értékeld ki,
# hogy hányszor talált be a körbe!
#   Szorgalmik közé számít:
#   d. Ismételd meg a feladatot úgy, hogy Luke számára 1 óra áll rendelkezésre a lövések leadására,
# és egy Poisson-folyamat szerint fogja ezeket megtenni, amelynek lambda paramétere 1000.
# Szimulálj ilyen folyamatokat, megtartva a térbeli eloszlás tulajdonságait, és számold ki újra,
# hogy mekkora valószínűséggel talál be legalább egyszer a szükséges területre Luke. Ha nem
# megy a megoldás, írj és segítek!


#simulate 100 times
#step 1: x coordinates with 1.5 sigma ~N(0,1.5)
x_coordinates=rnorm(100,0,1.5)

#step2: y coordinates between -5 and 5
y_coordinates=runif(100,-5,5)
shots=data.frame(x_coordinates,y_coordinates)

#step 3: plot shots of Luke
plot(shots,main="Shots of Luke Skywalker") 
par(new=TRUE)
points(0,0, col="green",bg="green",lwd=4)

############
#b) radius of the circle: 0.1

#100
valid_shots=matrix(NA,100,1)

for (j in 1:100){
  x_coordinates=rnorm(100,0,1.5)
  y_coordinates=runif(100,-5,5)
  shots=data.frame(x_coordinates,y_coordinates) 
valid_shots[j]<- nrow(shots[(shots$x_coordinates > -0.1 & shots$x_coordinates < 0.1 & shots$y_coordinates < 0.1 & shots$y_coordinates > -0.1) ,])
probability=valid_shots/100 #probability of shots in the circle
}

prob=sum(valid_shots>=1)/j #probability of min.1 shot

#100000 shots

valid_shots_plus=matrix(NA,1000,1)

for (j in 1:1000){
  x_coor=rnorm(100000,0,1.5)
  y_coor=runif(100000,-5,5)
  shots_plus=data.frame(x_coor,y_coor)
  valid_shots_plus[j]<- nrow(shots_plus[(shots_plus$x_coor > -0.1 & shots_plus$x_coor < 0.1 & shots_plus$y_coor < 0.1 & shots_plus$y_coor > -0.1), ])
}

prob_plus=sum(valid_shots_plus>=1)/j
#pr(x>=1)

##################
#c) simulations to find n for probaility>=0.5

valid_shots<-c()
n=0 #num of simulations needed for P>=0.5

repeat{
  n=n+1
  x_coordinates=rnorm(n,0,1.5)
  y_coordinates=runif(n,-5,5)
  shots=data.frame(x_coordinates,y_coordinates) 
  valid_shots[n]<- nrow(shots[(shots$x_coordinates > -0.1 & shots$x_coordinates < 0.1 & shots$y_coordinates < 0.1 & shots$y_coordinates > -0.1) ,])
  prob=sum(valid_shots>=1)/n
  if (prob>=0.5)
  {break}
}
print(n)

##########

#d
library(scatterplot3d)

time_coordinates=rpois(100,1000) 
shots_3d=data.frame(x_coordinates,y_coordinates,time_coordinates)
perps(shots_3d)
scatterplot3d(shots_3d)

###################

#1st 'valid' shot of Luke =sim_num
sim_num=matrix(NA,nrow=100,1)
for (j in 1:100){
  repeat{x_coordinates=rnorm(n,0,1.5)
  y_coordinates=runif(n,-5,5)
  shots=data.frame(x_coordinates,y_coordinates)
  valid_shots<- nrow(shots[(shots$x_coordinates > -0.1 & shots$x_coordinates < 0.1 & shots$y_coordinates < 0.1 & shots$y_coordinates > -0.1) ,])
  n=n+1
  if (valid_shots==1){break}
  }
  sim_num[j]<-n
}

