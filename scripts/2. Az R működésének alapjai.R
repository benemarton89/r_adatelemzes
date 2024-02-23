### AZ R MŰKÖDÉSÉNEK ALAPJAI ###



## -------------------------------------------------------------------------------------------
# írjuk be a konzolba a teszt szót idézőjelek nélkül

teszt


## ----------------------------------------------------------------------------------------------------
# írjuk be a konzolba a teszt szót idézőjelekkel

"teszt"


## ----------------------------------------------------------------------------------------------------
# írjuk be idézőjelek nélkül, de nagybetűkkel, hogy `TRUE`, majd utána, hogy `FALSE`

TRUE

FALSE


## ---------------------------------------------------------------------------------------
# írjuk be idézőjelek nélkül, de kisbetűkkel, hogy `TRUE`, majd utána, hogy `FALSE`

true

false


## ----------------------------------------------------------------------------------------------------
# írjunk be egy nagy `T` és egy nagy `F`betűt

T
F


## ----------------------------------------------------------------------------------------------------
# a c () funkció segítségével jelenítsük meg az 1,4,5,6,7 számsort

c(1,4,5,6,7)


## ----------------------------------------------------------------------------------------------------
#  hozzunk létre egy adatsort az Ádám, Béla, Cecil, Dénes szavakból. Ne felejtsük el az idézőjelet, hiszen itt szöveges adatokról van szó.

c("Ádám", "Béla", "Cecil", "Dénes")


## ----------------------------------------------------------------------------------------------------
# hozzunk létre egy adatsort a T, F, F, T, T logikai értékekből

c(T,F,F,T,T)


## -------------------------------------------------------------------------------------------
# futtassuk le az 1,4,5,6,7 számsort

1,4,5,6,7


## ------------------------------------------------------------------------------------------
# futtassuk úgy le a c () vektorképző funkcióval az 1,4,5,6,7 számsort, hogy kihagyjuk a vesszőket

c(1 4 5 6 7)


## ----------------------------------------------------------------------------------------------------
# mentsük el x objektumnéven az 1-es értéket

x<-1


## ----------------------------------------------------------------------------------------------------
# futtassuk le az x objektumot, hogy a konzolon megjelenjen annak tartalma, az 1-es érték

x


## ----------------------------------------------------------------------------------------------------
# mentsük el y objektumnéven a "teszt" szöveges értéket, majd futtassuk le az y objektumot 

y<- "teszt"
y


## ----------------------------------------------------------------------------------------------------
# mentsük el w objektumnéven a 3-5 kivonás eredményét

w<-3-5


## ----------------------------------------------------------------------------------------------------
# a w objektum futtatásával győződjünk meg arról, hogy az ojbektum nem a képletet, hanem annak eredményét, a -2 értéket tartalmazza

w


## ----------------------------------------------------------------------------------------------------
# mentsük el num objektumnéven az imént létrehozott számsort, names néven a szöveges és log néven a logikai értékekből álló adatsort

num<-c(1,4,5,6,7) 
names<-c("Ádám", "Béla", "Cecil", "Dénes")
log<-c(T,F,F,T,T)


## ----------------------------------------------------------------------------------------------------
# hívjuk be a num, a names és a log objektumokat (idézőjelek nélkül)
num
names
log


## ----------------------------------------------------------------------------------------------------
# hívjuk be a num objektum második elemét (4-es érték)

num[2]


## ----------------------------------------------------------------------------------------------------
# hívjuk be a names objektum első ("Ádám") és harmadik ("Cecil") elemét

names[c(1,3)]


## ----------------------------------------------------------------------------------------------------
# próbáljuk ki az előző két parancsot negatív előjellel

num[-2]
names[-c(1,3)]


## ----------------------------------------------------------------------------------------------------
# mentsük el `names2` néven azt a vektort, amiben Ádám és Cecil neve nem szerepel, tehát ahol a names vekotor első és a harmadik eleme el van távolítva.

names2<-names[-c(1,3)]


## ----------------------------------------------------------------------------------------------------
# mix objektumnéven mentsük el az alábbi heterogén vektort, majd hívjuk is be
mix<- c(1,"Ádám", 4, TRUE)
mix


## ----------------------------------------------------------------------------------------------------
mix2<- c(1, 4, TRUE)
mix2


## ----------------------------------------------------------------------------------------------------
# na néven mentsük az alábbi adatsort, melynek harmadik és ötödik eleme nem ismert, majd hívjuk is be az objektumot

na<-c(1,2,NA,3,NA,8)
na


## ----------------------------------------------------------------------------------------------------
# rögzítsük react objektumnéven a fb adattábla num_reactions változóját

react<-fb$num_reactions


## ----------------------------------------------------------------------------------------------------
# kérjük le a fb adattábla 45. sorában és 11. oszlopában szereplő érték, azaz a 45. poszt reakcióinak a számát

fb[45,11]


## ----------------------------------------------------------------------------------------------------
# react2 objektumnéven mentsük reakciók számát mérő változót indexálással, azaz a fb adattábla 11-es oszlopát az összes sor megtartásával. A vessző előtti üres terület jelzi, hogy minden sort meg akarunk őrizni. 

react2<-fb[,11]


## ----------------------------------------------------------------------------------------------------
# interaction néven hozzunk létre egy objektumot, ami a fb adattábla interakciós mutatókat rögzítő változóit, tehát az adatbázis 11-20 oszlopait, tartalmazza az összes vizsgált esetre

interaction<-fb[,c(11:20)]


## ----------------------------------------------------------------------------------------------------
# mentsük el interaction2 néven a fb adatbázis interakciós változóit (11–20. oszlop), illetve a politikus nevét (26. oszlop) és pártját (29. oszlop) oly módon, hogy a név és a párt kerüljön előre az új adattáblában

interaction2<-fb[,c(26,29,11:20)]


## ----------------------------------------------------------------------------------------------------
# listázzuk ki a fb adattábla változóneveit

colnames(fb)


## ----------------------------------------------------------------------------------------------------
# mentük el fb2 néven külön adattáblában a fb adattábla első 50 esetét, azaz sorát. 

fb2<-fb[c(1:50), ] 


## ----------------------------------------------------------------------------------------------------
# töröljük a workspace-ről az `interaction` és a `react2` objektumokat. 

rm(interaction, react2)


## ----------------------------------------------------------------------------------------------------
# végezzük el az alábbi műveleteket a számtani operátorok segítségével

1+5
8/2
2^4


## ----------------------------------------------------------------------------------------------------
# mentsük el az alábbi műveleteket muv1 és muv2 néven objektumként 

muv1<-(3+4)*2
muv2<-3^(5+4)



## ------------------------------------------------------------------------------------------
# szöveges adatokkal nem lehet számtani műveleteket végezni, az alábbi műveletre ezért hibaüzenetet kapunk


„feri” + „zsolt”


## ----------------------------------------------------------------------------------------------------
# végezzük el az alábbi műveletet, hogy kiderüljön, hogy hány TRUE szerepel a képletben

T+T+F+F+F+T


## ----------------------------------------------------------------------------------------------------
# adjunk hozzá a korábban mentett num vektor minden eleméhez hármat, és ezt a múveletet mentsük el num2 néven, majd hívjuk is be

num2<-num+3
num2


## ----------------------------------------------------------------------------------------------------
# számoljuk ki mindegyik posztnál, hogy  a reakciók számra a követők számának hány százalékának felel meg

fb2$num_reactions/fb2$FB_follow_end


## ----------------------------------------------------------------------------------------------------
# a fb2 adattábla új változójaként eng_rate néven mentsük el az imént kiszámolt értéksort

fb2$eng_rate<-fb2$num_reactions/fb2$FB_follow_end


## ----------------------------------------------------------------------------------------------------
# futtassuk le az alábbi logikai képleteket

4>2
3==2
4!=3
2<=2 



## ---------------------------------------------------------------------------------------------
## # alkalmazzuk a relációs operátort a react objektumnál oly módon, hogy megkapjuk, hogy mely esetek kaptak 100-nál több reakciót.
 
react>100




## -------------------------------------------------------------------------------------------
react[c(1:50)]


## ----------------------------------------------------------------------------------------------------
# válasszuk ki a react adatbázisból azokat az eseteket, amelyek 10000-nél több reakciót váltottak ki. 

react[react>10000]


## ----------------------------------------------------------------------------------------------------
# mentsük el react_10000 objektumnéven az 57 db 10 000-nél több reakciót kiváltó esetet

react_10000<-react[react>10000]


## ----------------------------------------------------------------------------------------------------
# készítsünk egy új adattáblát `fb_10000` néven, amely csak a 10 000-nél több reakciót kiváltó posztokat tartalmazza - a feltételünk az adattáblának csak a num_reactions változójára vonatkozik, ezért a relációs képletnél azt kell specifikálni

fb_10000<-fb[fb$num_reactions>10000,]  


## ----------------------------------------------------------------------------------------------------
# hozzunk létre az ess adattáblából egy `hun` elnevezésű adattáblát, ami csak a magyar válaszadókat tartalmazza, tehát azokat, amelyeknél az `ess$cntry` változó értéke egyenlő a `HU`-val - ne feleddjük, hogy az egyenlőséget az R-ben dupla `=` jellel jelöljük, a szöveges kategóriákat pedig mindig idézőjelek közé kell tenni 

hun<-ess[ess$cntry=="HU", ]


## ----------------------------------------------------------------------------------------------------
# hozzuk létre `fb_10000_fid` egy olyan adattáblát, ami a fb adattáblából csak a 10 000-nél több reakciót kiváltó, Fidesz-KNDP politikusok által közzétett posztokat tartalmazza  

fb_10000_fid<-fb[fb$num_reactions>10000 & fb$party== "Fidesz-KDNP",]  


## ----------------------------------------------------------------------------------------------------
# a fb_10000_fid_jobb adattáblában rögzítsük a fb adattábla Fidesz-KDNP vagy Jobbik politikus által közzétett, 10 000-nél több reakciót kiváltó eseteit

fb_10000_fid_jobb<-fb[fb$num_reactions>10000 & 
                        (fb$party=="Fidesz-KDNP" | fb$party=="Jobbik"),]      


## ----------------------------------------------------------------------------------------------------
# fb_10000_fid_jobb_neg adattáblában rögzítsük a baloldali politikusok 10000-nél kevesebb reakciót kiváltó posztjait a fb adattáblából

fb_10000_fid_jobb_neg<-fb[!fb$num_reactions>10000 & 
                            !(fb$party=="Fidesz-KDNP" | fb$party=="Jobbik"),]


## ----------------------------------------------------------------------------------------------------
# Korábban létrehoztunk egy `num` és egy `names` elnevezésű vektor objektumot is, ha ezek már nincsenek meg, akkor hozzuk létre újra őket

num<-c(1,4,5,6,7) 
names<-c("Ádám", "Béla", "Cecil", "Dénes")



## ----------------------------------------------------------------------------------------------------
# állapítsuk meg a num vektoron belül az 1 és az 5, valamint a names vektoron belül a "Béla" és "Dénes" értékek helyét 

num%in%c(1,5)
names%in%c("Béla", "Dénes")


## ----------------------------------------------------------------------------------------------------
# fb_jobb néven mentsük el azt az adattáblát, ami a fb adatbázisból csak a Fidesz-KDNP és a Jobbik politikusai által közzétett posztokat tartalmazza - ne felejtsül el a `c()` vektorképző funkciót a felsorolás miatt, az idézőjeleket a karakteradat miatt, illetve a vesszőt a kétdimenziós struktúra miatt!

fb_jobb<-fb[fb$party%in%c("Fidesz-KDNP", "Jobbik"),]


## ----------------------------------------------------------------------------------------------------
# végezzük el a fentebbi feladatot relációs operátorokkal

fb_jobb2<- fb[fb$party=="Fidesz-KDNP" | fb$party=="Jobbik",]


## ----------------------------------------------------------------------------------------------------
# Első lépésben hozzunk létre egy külön vektort, ahol csak ez a 15 név van felsorolva karakteradatként, azaz idézőjelek közé helyezve. 

kultbiz<-c("Dúró Dóra", "Kucsák László", "Pósán László", "Kunhalmi Ágnes", "Hoffmann Rózsa", "Ikotity István", "Demeter Zoltán", "Dunai Mónika", "Halász János", "Törő Gábor", "Vinnai Győző", "Hiller István", "Farkas Gergely", "Gaal Gergely", "Szabó Szabolcs") 


## ----------------------------------------------------------------------------------------------------
# fb_kult néven mentsük el azt az adattáblát, ami csak a kultbiz objektumban rögzített politikusok neveit tartalmazza.

fb_kult<-fb[fb$name%in%kultbiz,]


## ----------------------------------------------------------------------------------------------------
# kérjük le az 1,4,5,6,7 számsor átlagát a `mean ()` funkció segítségével - figyeljünk a zárójelekre, hiszen mind a `mean ()` funkció használata, mind a `c ()` vektorképző funkció használata zárójelet igényel, így duplazárójellel kell lezárni a parancsot.  

mean(c(1,4,5,6,7)) 


## ----------------------------------------------------------------------------------------------------
# A fenti adatsort korábban elmentettük vektorként `num` néven, futtassuk most le az átlagot erre az objektumra is

mean(num)


## ----------------------------------------------------------------------------------------------------
# nézzük meg, hogy egy poszt átlagosan mennyi reakciót vált ki a `fb` adattábla `num_reactions` változójának alkalmazásával

mean(fb$num_reactions)


## ---------------------------------------------------------------------------------------------
## # kérjük le a mean funkció súgólapját úgy, hogy a funkció neve elé beírunk egy kérdőjelet
## 
?mean


## ----------------------------------------------------------------------------------------------------
# hozzunk létre egy x objektumot a 0-tól 10-ig terjedő egész számokból és az 50-es számból

x <- c(0:10, 50)


## ----------------------------------------------------------------------------------------------------
# `xm` objektumként mentsük el ennek a vektornak az átlagát, de semmilyen beállítást ne használjunk az átlag kiszámításánál.

xm<-mean(x)


## ----------------------------------------------------------------------------------------------------
# jelenítsük meg egymás mellett a számtani és a 10%-os trimmelt átlagot

c(xm, mean(x, trim=0.10))


## ----------------------------------------------------------------------------------------------------
# jelenítsük meg külön a számtani és a 10%-os trimmelt átlagot

mean(x)
mean(x, trim=0.10) 


## ---------------------------------------------------------------------------------------
# hozzuk létre újra a korábban már használt na vektort és kérjük le annak átlagát

na<-c(1,2,NA,3,NA,8)
mean(na)



## ----------------------------------------------------------------------------------------------------
# kérjük le az na objektumra az átlagot oly módon, hogy a hiányzó adatokat a művelet ne vegye figyelembe - nem kell kiírni a `TRUE`-t, hiszen a sima `T` betű az R-nyelvben ugyanezt jelenti

mean (na, na.rm=T)


## ----------------------------------------------------------------------------------------------------
# kérjük le az x vektor 10%-os trimmelt átlagát az argumentum megnevezése nélkül 

mean (x, 0.1)


## ----------------------------------------------------------------------------------------
# hibás pozíciós argumentumhasználat a mean () funkciónál

mean (na, T) 


## ----------------------------------------------------------------------------------------------------
# helyes pozíciós argumentumhasználat a mean () funkciónál

mean(na, 0, T)


## ----------------------------------------------------------------------------------------------------
# a fenti parancs tehát ugyanazt jelenti, mint az alábbiak:

mean(na, trim=0, na.rm=T)

mean(na, na.rm=T) 



## -------------------------------------------------------------------------------------------
## # nézzük meg az oszlopdiagramot készítő barplot () súgólapját
 
?barplot


## ----------------------------------------------------------------------------------------------------
#  az 1-4 számsort ismételjük meg négyszer

rep(1:4,4)


## ----------------------------------------------------------------------------------------------------
# a fenti parancs az argumentum neveinek leírásával

rep(1:4, times=4)


## ----------------------------------------------------------------------------------------------------
# nézzük meg a 4-szer megismételt 1-4 sorozat számsorának átlagát 

mean(rep(1:4,4))

