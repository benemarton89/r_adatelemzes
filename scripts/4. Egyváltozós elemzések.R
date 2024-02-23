### EGYVÁLTOZÓS ELEMZÉSEK ###



## ---------------------------------------------------------------------------------------
## # adjuk meg a munkakönyvtárat, korlátozzuk a számok normálalakos megjelenítését, és töltsük be a szokásos (tidyverse, summarytools) package-eket
## 
## setwd("SAJÁT MAPPA")
load("datasets.RData")
options(scipen=999)
library(tidyverse)
library(summarytools)


## ----------------------------------------------------------------------------
library(sjPlot)


## ----------------------------------------------------------------------------------------------------
# missingeljük a polintr változó "Nem tudja", "Nem válaszol" és "Nem alkalmazható" kategóriáknak megfeleltethető 7,8,9 értékeket

ess$polintr<-ess$polintr %>% 
  replace(ess$polintr%in%c(7,8,9), NA)

# missingeljük a politikai bizalom változók "Nem tudja", "Nem válaszol" és "Nem alkalmazható" kategóriáinak megfeleltethető 77,88,99 értékeket

ess$trstprl<-ess$trstprl %>% 
  replace(ess$trstprl%in%c(77,88,99), NA) 
ess$trstlgl<-ess$trstlgl %>% 
  replace(ess$trstlgl%in%c(77,88,99), NA) 
ess$trstplc<-ess$trstplc %>% 
  replace(ess$trstplc%in%c(77,88,99), NA) 
ess$trstplt<-ess$trstplt %>% 
  replace(ess$trstplt%in%c(77,88,99), NA)
ess$trstprt<-ess$trstprt %>% 
  replace(ess$trstprt%in%c(77,88,99), NA)
ess$trstep<-ess$trstep %>% 
  replace(ess$trstep%in%c(77,88,99), NA)
ess$trstun<-ess$trstun %>% 
  replace(ess$trstun%in%c(77,88,99), NA)

# hozzuk létre polintr_f néven a polintr változó felcímkézett faktorváltozatát

ess$polintr_f<- ess$polintr %>% 
  factor(labels = c("Nagyon érdekli", "Eléggé érdekli", "Alig érdekli", 
                    "Egyáltalán nem érdekli"))


## ------------------------------------------------------------------------------------------
## # vizsgáljuk meg az ess adattábla változóit és adatok néven mentsük el egy html fájlban
## 
view(dfSummary(ess), file="adatok.html")


## ----------------------------------------------------------------------------------------------
## # végezzük el a gyorselemzést a politikai bizalommal kapcsolatos változókra és az eredményeket az adatok2 html fájlban mentsük el
## 
ess %>% select(trstprl:trstun) %>%
   dfSummary() %>%
   view(file="adatok2.html")


## ----------------------------------------------------------------------------------------------------
# kérjük le a polintr_f változó medián értékét

median(as.numeric(ess$polintr_f), na.rm=T) 

# ugyanez pipe-ok segítségével

ess$polintr_f %>% as.numeric() %>% median(na.rm=T)  


## ----------------------------------------------------------------------------------------------------
# kérjük le a polintr_f változó gyakorisági tábláját

freq(ess$polintr_f)


## ----------------------------------------------------------------------------------------------------
# a politikaiérdeklődés-változóra oly módon hozzuk létre a táblázatot, hogy a sorok csökkenő sorrendben jelenjenek meg, ahol a leggyakoribb eset van legfelül, és az "Analysis weight"-tel súlyozott eredményeket mutassuk, az adattáblát pedig `polintr_f_freq` néven mentsük el

polintr_f_freq<-tb(freq(ess$polintr_f, order= "freq", weights=ess$anweight))

# pipe segítségével ugyanez

polintr_f_freq<-ess$polintr_f %>% 
  freq(order="freq", weights=ess$anweight) %>% tb()


## ------------------------------------------------------------------------------------------
## # mentsük el a polintr_f_freq gyakorisági táblát .docx formátumban polintr_f néven
## 
tab_df(polintr_f_freq, file="polintr_f.docx")


## ----------------------------------------------------------------------------------------------------
# számoljuk ki az "alig érdekel" kategória populációs arányának konfidenciaintervallumát, ahol a kategóriához tartozók (súlyozott) elemszáma 15021,36, az érvényes választ adóké pedig 42361,72

prop.test(15021.36, 42361.72)


## ----------------------------------------------------------------------------------------------------
# készítsük el a `stby ()` funkció segítségével külön a férfiakra (`gndr`= 1) és külön a nőkre (`gndr` = 2) a politikai érdeklődés súlyozott gyakorisági táblázatait.

stby(ess$polintr_f, ess$gndr, freq, weights=ess$anweight)


## ----------------------------------------------------------------------------------------------------
# jellemezzük a trstplt változót súlyozás nélkül

descr(ess$trstplt)


## ----------------------------------------------------------------------------
# kérjük le a descr () funkció segítségével a polintr_f változó súlyozott medián és medián szórás értékeit

descr(as.numeric(ess$polintr_f), weights=ess$anweight, stats=c("med", "MAD")) 


## ----------------------------------------------------------------------------------------------------
# hasonlítsuk össze a politikai bizalmat mérő hét változó leíró statisztikáit

ess %>% select(trstprl, trstlgl, trstplc, trstplt, trstprt, trstep, trstun) %>% 
  descr()


## ----------------------------------------------------------------------------------------------------
# a hét politikai bizalmat mérő változó szerepeljen a táblázat soraiban, a mutatók pedig az oszlopokban

ess %>% select(trstprl, trstlgl, trstplc, trstplt, trstprt, trstep, trstun) %>% 
  descr(transpose=T)


## ----------------------------------------------------------------------------------------------------
# nézzük meg (súlyozatlan formában) a trstplt változó külön a férfiakra és külön a nőkre vonatkozó leíró statisztikáit

stby(ess$trstplt, ess$gndr, descr) 


## ----------------------------------------------------------------------------------------------------
# nézzük meg (súlyozatlan formában) a politikai bizalom hét változójának külön a nőkre és külön a férfiakra vonatkozó leíró statisztikáit

ess %>% 
  select(trstprl, trstlgl, trstplc, trstplt, trstprt, trstep, trstun) %>% 
  stby(ess$gndr, descr)


## ----------------------------------------------------------------------------------------------------
# mentsük el „tidy” táblázatként `polbiz_nem` objektum néven az előbbi táblázatot

polbiz_nem<-ess %>% 
  select(trstprl, trstlgl, trstplc, trstplt, trstprt, trstep, trstun) %>% 
  stby(ess$gndr, descr) %>% tb()


## -------------------------------------------------------------------------------------------
## # mentsük el a polbiz_nem táblázatot .docx formátumba
 
tab_df(polbiz_nem, file="polbiz_nem.docx")


## ----------------------------------------------------------------------------------------------------
# nézzük meg a politikusokba vetett bizalom átlagának standard hibáját

mean_se(ess$trstplt)


## ----------------------------------------------------------------------------------------------------
# nézzük meg a trstplt populációs átlagának 95%-os konfidenciaintervallumát

mean_cl_normal(ess$trstplt)


## ----------------------------------------------------------------------------------------------------
# # nézzük meg a trstplt populációs átlagának 99%-os konfidenciaintervallumát


mean_cl_normal(ess$trstplt, conf.int=.99)


## ----------------------------------------------------------------------------------------------------
# kérjük le a trstplt változó 0%-os, 20%-os, 40%-os, 60%-os és 80%-os és 100%-os kvantiliseit

quantile(ess$trstplt, c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm=T)


## ----------------------------------------------------------------------------------------------------
# mentsük el x néven az összes érték kvantilisét

x<-ecdf(ess$trstplt)

# kérjük le, hogy a 7-es értékhez milyen kvantilis tartozik
x(7)


## ----------------------------------------------------------------------------------------------------
# nézzük meg, hogy hány olyan válaszadónk van, aki 7-esnél nagyobb értéket adott meg a trstplt változón

sum(ess$trstplt>7, na.rm=T)


## ----------------------------------------------------------------------------------------------------
# ábrázoljuk oszlopdiagrammal a polintr_f változót 

ggplot(ess, aes(polintr_f)) + 
    geom_bar()


## ----------------------------------------------------------------------------------------------------
# alakítsuk úgy át az ábránkat, hogy az áttetszőség 0,3-as legyen, az oszlopok körvonalai sárgák legyenek, az oszlopok pedig sorrendben kék, piros, rózsaszín, zöld és lila színben jelenjenek meg, és kérjünk vékonyabb oszlopokat is 0,7-es szinten.

ggplot(ess, aes(polintr_f)) + 
    geom_bar(alpha=0.3, color="yellow", 
             fill=c("blue","red", "pink", "green", "purple"), 
             width=0.7)


## ----------------------------------------------------------------------------------------------------
# a polintr_f változót súlyozott formában ábrázoljuk oszlopdiagramként

ggplot(ess, aes(polintr_f, weight=anweight)) + 
  geom_bar()


## ----------------------------------------------------------------------------------------------------
# készítsük el újra a tidy formátumú gyakorisági táblát a polintr_f változóra

polintr_f_freq<-ess$polintr_f %>% 
  freq(weights=ess$anweight) %>% 
  tb()


## ------------------------------------------------------------------------------------------
# készítsünk oszlopdiagramot a polintr_f_freq gyakorisági tábla százalékos arányaira

ggplot(polintr_f_freq, aes(polintr_f, pct_valid)) + 
  geom_bar(stat="identity")


## ----------------------------------------------------------------------------------------------------
# mentsük el az ábránk jelenlegi állapotát `plot` néven

plot<- ggplot(polintr_f_freq, aes(polintr_f, pct_valid)) + 
  geom_bar(stat="identity")


## -----------------------------------------------------------------------------------------
# alakítsuk át az Y tengelyt, hogy 0%-tól 50%-ig terjedjen, 10%-os közöket adva, és a % jel is jelenjen meg az elnevezésben, és mentsük el az új objektumot `plot2` néven

plot2<-plot+ scale_y_continuous(limits=c(0,50), 
                                breaks=c(0,10,20,30,40,50), 
                                labels=c("0%", "10%", "20%", "30%", 
                                         "40%", "50%")) 


## ----------------------------------------------------------------------------------------------------
# az előző ábrából távolítsuk el az NA oszlopot

polintr_f_freq %>% filter(polintr_f!="<NA>") %>%  
  ggplot(aes(polintr_f, pct_valid))+
  geom_bar(stat="identity")


## ----------------------------------------------------------------------------------------------------
# távolítsuk el oly módon az `NA` kategóriát, hogy nem soroljuk fel, majd az érdeklődési kategóriák nevéből tüntessük el az „érdeklődik” szót, és csak a „nagyon”, az „eléggé”, az alig” és az „egyáltalán nem” jelenjen meg az X tengelyen - adjuk hozzá ezt az új réteget a `plot2` ábrához, és mentsük el az új réteggel bővített ábrát `plot3` néven.

plot3<- plot2 +
  scale_x_discrete(limits=c("Nagyon érdekli", "Eléggé érdekli", 
                            "Alig érdekli", "Egyáltalán nem érdekli"),
                   labels=c("Nagyon", "Eléggé", "Alig", "Egyáltalán nem"))


## ----------------------------------------------------------------------------------------------------
# a feliratokat rögzítő három új réteget adjuk hozzá a plot3 ábrához, és mentsük el plot4 néven

plot4<-plot3 +
ggtitle("A politikai érdeklődés szintjei Európában","ESS-2019") +
  xlab('"Mennyire érdeklődik Ön a politika iránt?"') + 
  ylab("Százalék")


## -----------------------------------------------------------------------------------------
# jelenítsük meg a százalékos értékeket is a plot4 oszlopdiagramon

plot4+geom_text(aes(label = pct_valid))


## ------------------------------------------------------------------------------------------
# kerekítsük két tizedesjegyre a százalékokat

plot4+geom_text(aes(label = round(pct_valid,2)))


## ------------------------------------------------------------------------------------------
# adjuk hozzá a százalékokhoz a százalékjelet

plot4+geom_text(aes(label = paste(round(pct_valid,2), "%")))


## -----------------------------------------------------------------------------------------
# igazítsuk az ábrához a százalékos arányokat mutató feliratokat, és mentsük 95 néven az ábrát

plot5<-plot4+geom_text(aes(label = paste(round(pct_valid,2), "%"), vjust=-0.5))


## ----------------------------------------------------------------------------------
## # a végleges ábra teljes parancssora
## 
 ggplot(polintr_f_freq, aes(polintr_f, pct_valid))+
   geom_bar(stat="identity")+
   scale_y_continuous(limits=c(0,50), breaks=c(0,10,20,30,40),
                      labels=c("0%", "10%", "20%", "30%", "40%"))+
   scale_x_discrete(limits=c("Nagyon érdekli", "Eléggé érdekli",
                             "Alig érdekli", "Egyáltalán nem érdekli"),
                    labels=c("Nagyon", "Eléggé", "Alig", "Egyáltalán nem"))+
   ggtitle("A politikai érdeklődés szintjei Európában","ESS-2019") +
   xlab('"Mennyire érdeklődik Ön a politika iránt?"') +
   ylab("Százalék")+
   geom_text(aes(label = paste(round(pct_valid,2), "%"), vjust=-0.5))


## ---------------------------------------------------------------------------------------
# adjunk az ábránkhoz egy új réteget, ami a feliratokat piros színű, „mono” betűstílusú és 11-es méretű feliratokká alakítja

plot5+theme(text=element_text(family="mono", colour="red", size=11))


## ----------------------------------------------------------------------------------------------------
# a telepítés után töltsük be a ggthemes package-et

library(ggthemes) 


## ----------------------------------------------------------------------------------------
# készítsük el az ábránkat az Economist hetilap formátumában

plot5+theme_economist()


## -------------------------------------------------------------------------------------------
# forgassuk el 90 fokkal az ábránkat

plot5+coord_flip()


## -------------------------------------------------------------------------------------------
# az alábbi parancs segítségével készítsünk kördiagramot a polintr_f változóra, és mentsünk azt el `plot_pie1` néven

plot_pie1<-ggplot(polintr_f_freq, aes(x="", y=pct_valid, fill=polintr_f)) +
  geom_bar(stat="identity") +
  coord_polar("y", direction=-1)+
  theme_void()


## -------------------------------------------------------------------------------------------
# a színes szeleteken fekete színű felirattal jelöljük a százalékokat, méretnek pedig a 4-es számot adtuk meg - Az új réteggel bővített `plot_pie1` ábrát mentsük el `plot_pie2` néven

plot_pie2<-plot_pie1+
    geom_text(aes(label=paste(round(pct_valid,2), "%")), 
              position = position_stack(vjust=0.5), 
              size=4, color="black")


## -------------------------------------------------------------------------------------------
# változtassuk meg a plot_pie2 kördiagram szeleteinek színeit sorrendben piros, sárga, kék és lila színekre 

plot_pie2+scale_fill_manual(values=c("red", "yellow", "blue", "purple"))


## -------------------------------------------------------------------------------------------
# változassuk meg a plot_pie2 kördiagram szeleteinek színeit a lila és kék árnyalatokból álló 3-as színpaletta segítségével, úgy hogy a sötétebb színektől haladjunk a világosabbak felé 

plot_pie2+
  scale_fill_brewer(palette=3, direction=-1)


## ----------------------------------------------------------------------------------------------------
# plot_pie3 néven mentsük el a plot_pie2 ábra színmagyarázattal kiegészített változatát

plot_pie3<-plot_pie2+
  scale_fill_brewer(palette=3, direction=-1, 
                    name="Politikai érdeklődés", 
                    labels=c("Nagyon", "Eléggé", "Alig", "Egyáltalán nem"))


## -------------------------------------------------------------------------------------------
# a plot_pie3 ábrát változtassuk meg úgy, hogy a jelmagyarázat az ábra alatt jelenjen meg

plot_pie3+theme(legend.position="bottom")


## ----------------------------------------------------------------------------------
 # a kördiagramunk teljes parancssora:
 
 ggplot(polintr_f_freq, aes(x="", y=pct_valid, fill=polintr_f)) +
   geom_bar(stat="identity") +
   coord_polar("y", direction=-1)+
   theme_void()+
   geom_text(aes(label=paste(round(pct_valid,2), "%")),
                          position = position_stack(vjust=0.5),
                          size=4, color="black")+
   scale_fill_brewer(palette=3, direction=-1,
                     name="Politikai érdeklődés",
                     labels=c("Nagyon", "Eléggé", "Alig", "Egyáltalán nem"))+
   theme(legend.position="bottom")


## -------------------------------------------------------------------------------
# ábrázoljuk hisztogrammal a trstplt változót

ggplot(ess, aes(x=trstplt))+
  geom_histogram()


## -------------------------------------------------------------------------------------------
# kérjünk minden értékhez külön oszlopot és súlyozzuk is adatainkat

ggplot(ess, aes(x=trstplt, weight=anweight))+
  geom_histogram(binwidth=1)


## -------------------------------------------------------------------------------------------
# legyenek az oszlopaink fehérek, míg a körvonalak feketék

ggplot(ess, aes(x=trstplt, weight=anweight))+
  geom_histogram(binwidth=1, color="black", fill="white")


## ----------------------------------------------------------------------------------------------------
# adjuk hozzá a sűrűséget az ábrához és mentsük el plot_hist1 néven

plot_hist1<-ggplot(ess, aes(x=trstplt, weight=anweight))+
  geom_histogram(aes(y=..density..),binwidth=1, color="black", fill="white")


## --------------------------------------------------------------------------
# hozzuk létre a leíró súlyozott statisztikákat rögzítő objektumot és mentsük el `trstplt_descr` néven tidy formátumban a `tb ()` funkciót is alkalmazva

trstplt_descr<-descr(ess$trstplt, weight=ess$anweight) %>%  tb ()


## -------------------------------------------------------------------------------------------
# illesszünk normálgörbét az adatainkra

plot_hist1+geom_function(fun = dnorm,
                         args = list(mean = trstplt_descr$mean,
                                     sd = trstplt_descr$sd))


## -------------------------------------------------------------------------------------------
# ábrázoljuk a normális eloszlás görbéjét piros színnel a `color=` argumentum segítségével

plot_hist1+geom_function(fun = dnorm, colour = "red", 
                         args = list(mean = trstplt_descr$mean,
                                     sd = trstplt_descr$sd))


## -------------------------------------------------------------------------------------------
# ábrázoljuk a fb adattábla num_reactions változóját hisztogramként 

ggplot(fb, aes(x=num_reactions))+
  geom_histogram()


## ----------------------------------------------------------------------------------------------------
# vizsgáljuk meg a num_reactions változó alapstatisztikáit

descr(fb$num_reactions)


## ----------------------------------------------------------------------------------------------------
# nézzük meg, hogy 80%-nál 90%-nál, 95%-nál és 99%-nál milyen érték található

quantile(fb$num_reactions, c(0.8, 0.9, 0.95, 0.99), na.rm=T) 


## ----------------------------------------------------------------------------------------------------
# derítsük ki, hogy a posztok hány százaléka váltott ki maximum 5000 reakciót, illetve összesen hány olyan poszt van, ami 5000-nél több reakciót ért el

x<-ecdf(fb$num_reactions)
x(5000)
sum(fb$num_reactions>5000)


## -------------------------------------------------------------------------------------------
# a num_reaction változó hisztogramját korlátozzuk a 0 és 5000 közötti reakciót elérő posztokra

ggplot(fb, aes(x=num_reactions))+
  geom_histogram()+
  xlim(0,5000)


## -------------------------------------------------------------------------------------------
# csökkentsük az ylim () funkcióval az Y tengely terjedelmét a 0-1000 tartományra.

ggplot(fb, aes(x=num_reactions))+
  geom_histogram()+xlim(0,5000)+
  ylim(0,10000)


## -------------------------------------------------------------------------------------------
# adjunk az ábrához megjegyzést a korlátozott terjedelem alkalmazásáról

ggplot(fb, aes(x=num_reactions))+
  geom_histogram()+xlim(0,5000)+ylim(0,10000)+
  labs(caption="Az 5000 reakciónál többet kiváltó posztokat az ábra
  nem jeleníti meg. A posztok 0,4%-a, összesen 146 poszt 
  tartozik ebbe a kategóriába")


## -------------------------------------------------------------------------------------------
# ábrázoljuk hisztogramokkal a politikusokba vetett bizalom eloszlását országonként

ggplot(ess, aes(x=trstplt, weight=anweight))+
  geom_histogram(aes(y=..density..),binwidth=1,
                 color="black", fill="white")+
  facet_wrap(.~cntry)


## ----------------------------------------------------------------------------------------------------
# készítsünk külön ábrát az országgyűlésbe (`trstprl`), a jogrendszerbe (`trstlgl`), a rendőrségbe (`trstplc`), a politikusokba (`trstplt`), a politikai pártokba (`trstprt`), az Európai Parlamentbe (`trstep`) és az ENSZ-be (`trstun`) vetett bizalomra, és mentsük el mindegyiket külön néven

plot_prl<-ggplot(ess, aes(x=trstprl, weight=anweight))+
  geom_histogram(aes(y=..density..),binwidth=1,
                 color="black", fill="white")
plot_lgl<-ggplot(ess, aes(x=trstlgl, weight=anweight))+
  geom_histogram(aes(y=..density..),binwidth=1,
                 color="black", fill="white")
plot_plc<-ggplot(ess, aes(x=trstplc, weight=anweight))+
  geom_histogram(aes(y=..density..),binwidth=1,
                 color="black", fill="white")
plot_plt<-ggplot(ess, aes(x=trstplt, weight=anweight))+
  geom_histogram(aes(y=..density..),binwidth=1,
                 color="black", fill="white")
plot_prt<-ggplot(ess, aes(x=trstprt, weight=anweight))+
  geom_histogram(aes(y=..density..),binwidth=1,
                 color="black", fill="white")
plot_ep<-ggplot(ess, aes(x=trstep, weight=anweight))+
  geom_histogram(aes(y=..density..),binwidth=1,
                 color="black", fill="white")
plot_un<-ggplot(ess, aes(x=trstun, weight=anweight))+
  geom_histogram(aes(y=..density..),binwidth=1,
                 color="black", fill="white")


## ------------------------------------------------------------------------------
# telepítés után hívjuk be a gridExtra package-et

library(gridExtra)

# ábrázoljuk együtt az imént elkészített ábrákat
grid.arrange (plot_prl, plot_lgl,plot_plc, plot_plt, plot_prt, plot_ep, plot_un)


## -------------------------------------------------------------------------------------------
# készítsünk kernelsűrűség-ábrát a trstplt változóra

ggplot(ess, aes(trstplt, weight=anweight)) +
  geom_density()


## -------------------------------------------------------------------------------------------
# tegyük "nyersebbé" a sűrűség megjelenítését a 2-es adjust-érték segítségével

ggplot(ess, aes(trstplt, weight=anweight)) +
  geom_density(adjust=2)


## -------------------------------------------------------------------------------------------
# töltsük ki a vonal alatti területet kék színnel

ggplot(ess, aes(trstplt, weight=anweight)) +
  geom_density(adjust=2, fill= "blue ", alpha=0.3)


## -------------------------------------------------------------------------------------------
# ábrázoljuk sűrűségábrával a politikusba vetett bizalom politikai érdeklődési szintek szerinti eltérését

ggplot(ess, aes(trstplt, weight=anweight)) +
  geom_density(aes(color=polintr_f), adjust=2)


## -------------------------------------------------------------------------------------------
# tüntessük el az ábráról az NA kategóriát

ess %>% filter(!is.na(polintr_f)) %>% 
  ggplot(aes(trstplt, weight=anweight)) + 
  geom_density(aes(color=polintr_f), adjust=2)


## -------------------------------------------------------------------------------------------
# színezzük be az egyes érdekődési szintekhez tartozó területeket, és tegyük őket áttetszővé

ess %>% filter(!is.na(polintr_f)) %>% 
  ggplot(aes(trstplt, weight=anweight)) +
  geom_density(aes(color=polintr_f, fill=polintr_f), adjust=2, alpha=0.2)


## -------------------------------------------------------------------------------------------
# adjuk hozzá az ábrához a teljes mintára vonatkozó sűrűséggörbét fekete és vastagabb vonallal

ess %>% filter(!is.na(polintr_f)) %>% 
  ggplot(aes(trstplt, weight=anweight)) + 
  geom_density(aes(color=polintr_f,fill=polintr_f), adjust=2, alpha=0.2)+
  geom_density(color="black", size=2, adjust=2)


## -------------------------------------------------------------------------------------------
# jelenítsük meg egy ábrán, de külön Kernelsűrűség-görbével a politikai bizalom minden dimenzióját

ggplot(ess, aes(trstprl, weight=anweight))+
  geom_density(aes(color="parlament"),adjust=2)+
  geom_density(aes(trstlgl,color="jogrendszer"), adjust=2)+
  geom_density(aes(trstplc, color="rendőrség"), adjust=2)+
  geom_density(aes(trstplt, color="politikusok"), adjust=2)+
  geom_density(aes(trstprt, color="pártok"), adjust=2)+
  geom_density(aes(trstep, color="EP"), adjust=2)+
  geom_density(aes(trstun, color="ENSZ"), adjust=2)+
  scale_color_manual(values=c("black","red", "lightblue", "navyblue",
                              "purple", "darkgrey", "green"))


## ----------------------------------------------------------------------------------------------------
# adjunk hozzá egy kernelsűrűségi vonalat a politikusokba vetett bizalmat ábrázoló hisztogramhoz
ggplot(ess, aes(x=trstplt, weight=anweight))+
  geom_histogram(aes(y=..density..),binwidth=1,color="black",fill="white")+
  geom_density(adjust=2)


## -------------------------------------------------------------------------------------------
# hozzunk létre boxplot ábrát a trstplt változóra

ggplot(ess, aes(x="", y = trstplt, weight=anweight)) + 
  geom_boxplot()


## -------------------------------------------------------------------------------------------
# távolítsuk el az NA kategóriát az ábráról

ess %>% filter(!is.na(polintr_f)) %>% 
  ggplot(aes(x=polintr_f, y = trstplt, weight=anweight)) + 
  geom_boxplot()


## -------------------------------------------------------------------------------------------
# színekkel jelöljük az egyes kategóriákat, de a színmagyarázat ne jelenjen meg

ess %>% filter(!is.na(polintr_f)) %>% 
  ggplot(aes(x=polintr_f, y = trstplt, weight=anweight, fill=polintr_f)) + 
  geom_boxplot()+
  theme(legend.position="none")


## ----------------------------------------------------------------------------------------------------
# mentsük el az ess hosszú adatformátumú változatát long néven, a key változó pedig legyen trust, az értékváltozó pedig val

long<-ess %>% 
  gather(trstprl, trstlgl, trstplc, trstplt,trstprl,trstep, trstun, 
         key="trust", value="val")


## -------------------------------------------------------------------------------------------
# jelenítsük meg egy ábrán, de külön boxplotokkal a politikai bizalom minden dimenzióját

ggplot(long, aes(x=trust, y = val, fill=trust, weight=anweight)) + 
  geom_boxplot()+
  theme(legend.position="none")

