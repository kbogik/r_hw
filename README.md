
Homeworks for R course
Házi feladatok az R kurzusra:

1. hf.: implementation of quicksort algorithm

2. hf.: 
a) Hányféleképpen tudunk felváltani egy dollárt 50, 25, 10, 5 és 1 centesekkel? 
Készíts egy függvényt, ami megmondja, egy adott "a" mennyiséget hányféleképpen tudunk felváltani "k"-féle váltópénzzel, amelyek értékei ("d_1, ..., d_k")?
Tipp: gondolj arra, hányféle megoldás lenne az első féle érme nélkül, illetve hányféle lenne "a-d_1" összeg esetén, és találd meg ezek okos összegét ahhoz, hogy gyorsan számoljon a függvényed!
Használd a System.time-függvényt ahhoz, hogy lemérd, mennyi idő alatt számítja ki a függvény azt,
mikor "a=150000", "(d_1, d_2, d_3, d_4, d_5, d_6) = (3000, 1000, 150, 15, 3, 1)". 
b). Ez alapján hányféleképpen tudunk felváltani 1000 forintot?

3.hf.:
Luke Skywalker X-wing gépével a Halálcsillag szellőzőnyílásába akar beletalálni. Nem tud pontosan célozni, de a síkot ismeri, amire merőlegesen kell lövéseket leadnia, és minden lövése megfelelő szögben csapódik be. A lövések viszont véletlenszerűen oszlanak el a szellőző körül, az  x tengely vonalán a szellőző pontjának helyén vett várható értékkel, de 1,5 méter szórással vett normális eloszlás
alapján, míg az y tengelyen egyenletes eloszlás alapján a szellőző 5 méter sugarú körében.
a. Szimulálj 100 lövést Luke űrhajójával, és rajzold ki a lövések pontjait egy plot paranccsal!
Ábrázold a szellőző helyét is egy másik színnel! (Points parancs, pl.)
b. Próbáljuk meg szimulációval megbecsülni, hogy mekkora valószínűséggel talál be Luke legalább egyszer a szellőző 10 cm-es környezetébe (ekkor már elpusztul a Halálcsillag), ha 100 lövést ad le. És ha 100000-et?
   c. Készíts egy algoritmust, ami azt az optimális (értsd, minél kevesebb) számú lövést keresi,
amivel Luke már legalább 50% biztonsággal eltalálja ezt a környzetet 1szer. Használj szimulációkat, rendezd egy listába a szimulált lövéssorozatokat, listaelemenként értékeld ki, hogy hányszor talált be a körbe!

3. feladat: 
Legyenek x és y független normális eloszlású változók, várhatóértékük legyen 2 és 5, szórásuk 4 és 2.
Legyen z =  3x + 2y +epsilon, ahol epsilon sztenderd normális. 
Tekintsünk egy olyan regressziót, ahol x-szel és y-nal akarjuk z-t magyarázni (ezek a megfigyelhető magyarázóváltozók, epsilon a zaj/nem megfigyelhető rész).
Írj egy függvényt/algoritmust, ami 'numerikusan' becsli meg a z = beta1*x+beta2*y regressziót, azaz indulj ki valamilyen tetszőleges (véletlen) beta1 és beta2 értékből, számold ki ebből a becsült z értékeket és számold ki az átlagos négyzetes hibát. Ezután növeld vagy csökkentsd valamilyen egységgel az egyik (vagy mindkét) béta értéket és ismét számold ki a négyzetes hibát és vizsgáld meg hogy hogyan változott az előző becsléshez képest. Határozz meg valamilyen logikát/szabályt, hogy az eredmények függvényében (azaz nőtt-e vagy csökkent-e a hiba) merre változzanak a béta értékek, majd ismételd meg a lépéseket addig amíg valamilyen optimális(nak vélt) megoldást találsz. 

4.hf.:
Készíts regressziót a következő adattáblán. Töltsd be R-ben az "AmesHousing" csomagot, majd ebben
hívd meg az "ames_raw" adattáblát. Mivel ez egy 'tibble' adatstruktúra (ezt még nem tanultuk), használd
az as.data.frame parancsot, hogy data.frame legyen belőle, mentsd el így. Ez egy ingatlan-adatbázis,
amelynek a 'SalePrice' változóját szeretnénk regresszióval modellezni. 
Bármelyik másik változókat
használhatjátok ehhez, lehet próbálkozni transzformációkkal, változó-összevonásokkal. Használjatok
kategorikus változókat is (as.factor lesz a kulcsszó), illetve próbáljátok ki az órán tanult teszteket,
kiválasztási metódusokat, hogy hatékonyabb legyen a modellszelekció! 
Válasszátok külön minden 20-adik sorát az adatbázisnak (a 20-adiktól kezdve) a holdout sample megképzéséhez, de a training setben
is képezzetek testing (validation) halmazt, hogy a modell ne illeszkedjen túl.

5.hf.:
regressziót kell felírni, hogy a részvény hozamát egy szektor index hozamával
más fajta statisztikákat is fel lehet használni
Töröld ki a 2008-2009-es válságra vonatkozó adatokat a !részvény! idősorából 
A feladat hogy regressziós modell segítségével megpróbáljuk backfillelni (visszabecsülni) a részvény (napi) hozamait a válság alatti időszakra
A regresszióhoz magyarázóváltozóként használjátok az S&P500 hozamát és a szektor index hozamát, illetve felhasználhattok a részvényárfolyamból számolt későbbi statisztikákat is (pl.: szezonalitás, realizált volatilitás), vagy az indexek hasonló statisztikáit
training(+teszt/validáció) halmaznak használjátok a 2009 utáni adatokat
ábrázoljátok a visszabecsült és az eredeti hozamokat idősorosan

5.feladat:
Szimulálj véletlen mintát a kövektező idősorokból:
AR(2):  x_t=0.3*x_t-1-0.5*x_(t-2) + e_t
Bilinear ARMA: x_t=0.4*x_(t-1)-0.3*x_(t-2)+0.5*x_(t-1)*e_(t-1)+e_t
Nemlineáris MA: x_t=e_t-0.3*e_(t-1)+0.2*e_(t-2)+0.4*e_(t-1)*e_(t-2)-0.25*(e_(t-2))^2
Illessz mindhárom idősorra
- ARIMA modellt
- Neurális hálót különböző paraméterekkel (az nnfor package mlp függvénye könnyen használható erre).

Hasonlítsd össze a modellek egylépéses előrejelzési teljesítményét a szokásos training+validáció+teszt halmazok használatával

6.hf.:
Toltsd be:
volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
Valtoztasd idosorra, plottold, majd valtoztasd kozel stacionerré
Valaszd le az idosor utolso 20 elemet, a maradekra
illessz egy ARIMA modellt, es jelezz elore vele 20 idopontnyi tavolsagra.
Abrazold (hasznalhatod az alap plot fuggvenyeket vagy ggplotot) az elorejelzett es a valodi ertekeket
