##############2.heti hf########################
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
changebasic<- function(x,d1,d2,d3,d4,d5){#minden input legyen centben!
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
changebasic(100,50,25,10,5,1)

#b: GG által beküldött YT videó alapján csináltam: https://www.youtube.com/watch?v=L27_JpN6Z1Q&t=1s

change<- function(x, ...){
  rows=(length(list(...)))  #elsőként megnézem, hogy mennyi coin-nal kell felváltani
  columns=(x+1) #a mátrix oszlopainak száma pedig x+1 (van 0)
  possiblesums=(0:x)
  sortedcoins= sort(unlist(list(...)))
  changes=matrix(1,nrow=rows,ncol=columns) #ebbe fogom feltölteni a lehetőségeket
  #colnames(changes)=list(possiblesums)
  #alapból 1-esekkel töltöm fel, hogy az első oszlopot megspóroljam
  
  for( i in 1:rows){ #soronként szeretném feltölteni
    for (j in 2:columns){
      if (i==1){ 
        if ((possiblesums[j]%%sortedcoins[1])==0){ 
          changes[i,j]=1}
        else{changes[i,j]=0}
      }
      else{
        if ((possiblesums[j]-sortedcoins[i])>=0){ 
          changes[i,j]=changes[i,(possiblesums[j]-sortedcoins[i]+1)]+changes[i-1,j]}
        else{changes[i,j]=changes[i-1,j]} #felette levő sor
      }
    }
  }
  return(changes[rows,columns]) #csak azt adom meg, hogy összességében hányféleképpen
}

change(150000,3000, 1000, 150, 15, 3, 1) 
system.time(change(150000,3000, 1000, 150, 15, 3, 1))

# HW: Write a function which decides if an integer is a prime number

prime<- function(x){
  if (x<=0) {
    stop("Pozitív számot adj meg")}
  if (typeof(x)!= "double") {
    stop("Számot adj meg")}
  
  y=(1:(x-1))
  
  if (x==2) {return("Prímszám") }
  for (i in 2:(x-1)){ #2-től indul az index, de a 2-t külön kellene emaitt kezelni
    if (x%%y[i]==0){
      return("Nem prím")}
    else{
      j=1}
  }
  if(j==1){
    print("Prímszám")}
}