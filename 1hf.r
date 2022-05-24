#Az első feladatban a quicksort algoritmust kell implementálni, ami egy rekurzív sorbarendező algoritmus.
#Garantáltan O(nlog(n)) időben fut, gyorsnak számít. Lépései a következők:
#  
#  A lista első eleménél kisebb elemeket egy bal, a nagyobb-egyenlő elemeket (az első elem kivételével) pedig egy jobb listába kell tenni
#A bal és a jobb listát sorba kell rendezni
#A két sorbarendezett féllistát és az elkülönített első elemet egyesíteni kell a megfelelő módon

quicksort2 <- function(x){ 
  if (typeof(x)!= "double"){print('Listat adj meg!') 
    break} 
  a <- c(x[1])
  b=0
  j=0
  quick=a #pivot
  for (i in 2:(length(x))) {#1.lepes balra és jobbra sorolom
    if (x[i]<a) {
      quick= c(x[i],quick)# kisebb elemek-> bal
      b=b+1} #bal lista hossza->b
     else{quick=c(quick,x[i])#nagyobbakat jobbra teszem
     j=j+1}
  }
  for (k in 1:(b-1)){for (i in 2:b)#sorbabal
  {if (quick[i-1]>quick[i]){
    l<-quick[i-1]
    quick[i-1]=quick[i]
    quick[i]=l}
  }}
  for (k in 1:(j-1)){for (i in 1:j)#sorbajobb 
  {if (quick[length(x)-i]>quick[length(x)-i+1]){
    l<-quick[length(x)-i]
    quick[length(x)-i]=quick[length(x)-i+1]
    quick[length(x)-i+1]=l}
  }}
  return(quick)
}


k=c(4,2,1,3,6,7,0,4,-4,4,5,5,3,2)

d=quicksort2(k)
d

##############2.heti hf kezdet########################
#HW: a) Hányféleképpen tudunk felváltani egy dollárt 50, 25, 10, 5 és 1 centesekkel? 

#Készíts egy függvényt, ami megmondja, egy adott "a" mennyiséget hányféleképpen tudunk felváltani "k"-
#féle váltópénzzel, amelyek értékei ("d_1, ..., d_k")?
#  Tipp: gondolj arra, hányféle megoldás lenne az első féle érme nélkül, illetve hányféle lenne "a-d_1"
#összeg esetén, és találd meg ezek okos összegét ahhoz, hogy gyorsan számoljon a függvényed!
#  Használd a System.time-függvényt ahhoz, hogy lemérd, mennyi idő alatt számítja ki a függvény azt,
#mikor "a=150000", "(d_1, d_2, d_3, d_4, d_5, d_6) = (3000, 1000, 150, 15, 3, 1)". A leggyorsabb
#megoldással rendelkező(k) kedvezményt kapnak a harmadik háziban!
#  b). Ez alapján hányféleképpen tudunk felváltani 1000 forintot?

#a-ra favágós mgo:
change<- function(x,d1,d2,d3,d4,d5){#minden input legyen centben!
  #inputként megadjuk, hogy mit és mikkel kell felváltani
  d1_max=x/d1
  d2_max=x/d2
  d3_max=x/d3
  d4_max=x/d4
  d5_max=x/d5
  num=0
  for (i_d1 in 0: d1_max){ #alapötletem: egymásba ágyazott 5 db for ciklus (a felváltandók száma)
    for (i_d2 in 0: d2_max){ 
      for (i_d3 in 0: d3_max){
        for (i_d4 in 0: d4_max){
          for (i_d5 in 0: d5_max){
            #sum kombinaciok, ha kiadjak az összeget, akkor 1 féleképpennel nő
            if (i_d1*d1+i_d2*d2+i_d3*d3+i_d4*d4+i_d5*d5==x){num=num+1}
  }}}}}
  #outputként egy számot kellene várni
  return(num)
}
change(100,50,25,10,5,1)

#általánosra arguments(x,...)
#length(list(...)