### TÖBBVÁLTOZÓS ELEMZÉSEK ###


## -----------------------------------------------------------------------------------
## # adjuk meg a munkakönyvtárat, hívjuk be a hun adattáblát, korlátozzuk a számok normálalakos megjelenítését, és a telepítésük után töltsük be a fejezetben használt, illetve a szokásos (tidyverse, summarytools) package-eket
 
## setwd("SAJÁT MAPPA")
 load("hun.RData")
 options(scipen=999)
 library (effects)
 library (performance)
 library (car)
 library (sjPlot)
 library (gridExtra)
 library (interactions)
 library (wdm)
 library (weights)
 library (corrplot)
 library (sjstats)
 library (lmtest) # csak generalizált lineáris regressziónál
 library (DescTools) # csak generalizált lineáris regressziónál
 library (MASS) # csak ordinális logisztikus és negatív binomiális regressziónál
 library (nnet) # csak multinomiális logisztikus regressziónál
 library (PResiduals) # csak ordinális logisztikus regressziónál
 library (summarytools)
 library (tidyverse)


## ----------------------------------------------------------------------------------------------------
# a magas mérési szintű változókat a descr () funkció segítségével vizsgáljuk meg. Mivel súlyozott adatokkal dolgozunk, a súlyváltozót is adjuk hozzá a parancshoz

hun %>% select(stfeco, stfgov, happy, income) %>% 
  descr (weights=hun$pspwght)


## -------------------------------------------------------------------------------------------
# kérjünk le  kernelsűrűség-ábrát az összes változónkra – nem felejtve el a tényt, hogy a jövedelem skálája kicsit eltér a másik három változóétól, hiszen itt csak 10 kategóriából áll a változó.

ggplot(hun, aes(stfeco, weight=pspwght))+
  geom_density(aes(color="gazdaság"),adjust=2)+
  geom_density(aes(stfgov,color="kormány"), adjust=2)+
  geom_density(aes(happy,color="boldogság"), adjust=2)+
  geom_density(aes(income, color="jövedelem"), adjust=2)+
  scale_color_manual(values=c("black","red", "green", "blue"))


## ----------------------------------------------------------------------------------------------------
# elemezzük az alacsony mérési szintű változókat a freq() funkcióval

freq(hun$polintr_f, weights = hun$pspwght)
freq(hun$gndr_f, weights = hun$pspwght)
freq(hun$dom, weights = hun$pspwght)


## ----------------------------------------------------------------------------------------------------
# polintr_n néven hozzunk létre numerikus politikaiérdeklődés-változót a polintr_f változóból, dom2 néven pedig a város referenciakategóriát alkalmazó településtípus változót a dom változóból.

hun <-hun %>% mutate(polintr_n=as.numeric(polintr_f), 
                     dom2=relevel(dom, ref=2)) 


## ----------------------------------------------------------------------------------------------------
# m1 néven mentsük el a lineáris regressziómodellünket

m1<-lm(stfeco~happy + stfgov + income + polintr_n + gndr_f + dom2, 
       data=hun, weights=pspwght)


## ----------------------------------------------------------------------------------------------------
# mentsük el x objektumnévvel, hogy mely értékek számítanak kiugró értéknek az m1 modell alapján

x<-abs(rstandard(m1))>3


## ----------------------------------------------------------------------------------------------------
# xx objektumnéven mentsük el a kiugró eseteink adatbázison belüli sorszámát

xx<-as.numeric(names(x)[x])


## ----------------------------------------------------------------------------------------------------
# m1_data néven készítsük el a modellben használt adatbázist, amiből hiányoznak az alapmodell alapján definiált kiugró értékek

m1_data<-hun[-xx,]


## ----------------------------------------------------------------------------------------------------
# m1.1 néven mentsük el a kiugró értékeket nem tartalmazó m1_data adatbázison futtatott lineáris regressziómodellt

m1.1<-lm(stfeco~happy + stfgov + income + polintr_n + gndr_f + dom2,
         data=m1_data, weights=pspwght)


## ----------------------------------------------------------------------------------------------------
# a kiugró értékek eltávolításának teljes parancssora

x<-abs(rstandard(m1))>3
xx<-as.numeric(names(x)[x])
m1_data<-hun[-xx,]


## ----------------------------------------------------------------------------------------------------
# kérjük le a modell feltételeinek teszteléséhez használható ábrákat

check_model (m1.1, detrend=F)


## ------------------------------------------------------------------------------
# teszteljük vizuálisan a linearitás feltételének teljesülését. A négy ábrát p1, p2, p3, p4 néven mentsük el.

p1<-m1_data %>% ggplot(aes(happy, stfeco))+
  geom_point(position = "jitter")+
  geom_smooth(color="red")+
  geom_smooth(method=lm)
p2<-m1_data %>% ggplot(aes(stfgov, stfeco))+
  geom_point(position = "jitter")+
  geom_smooth(color="red")+
  geom_smooth(method=lm)
p3<-m1_data %>% ggplot(aes(income, stfeco))+
  geom_point(position = "jitter")+
  geom_smooth(color="red")+
  geom_smooth(method=lm)
p4<-m1_data %>% ggplot(aes(polintr_n, stfeco))+
  geom_point(position = "jitter")+
  geom_smooth(color="red")+
  geom_smooth(method=lm)

# ábrázoljuk együtt a négy ábrát

grid.arrange(p1,p2,p3,p4)


## ----------------------------------------------------------------------------------------------------
# teszteljük a homoszkedaszticitás feltételét

check_heteroscedasticity(m1.1)


## ----------------------------------------------------------------------------------------------------
# teszteljük a homoszkedaszticitás feltételét a politikai érdeklődést nem tartalmazó modellen

check_heteroscedasticity(lm(stfeco~happy + stfgov + income + gndr_f + dom2, 
                            data=m1_data, weights=pspwght))


## ------------------------------------------------------------------------------
# ellenőrizzük az m1.1 modellhez tartozó Durbin-Watson-teszt pontos értékét

dwt (m1.1)


## ----------------------------------------------------------------------------------------------------
# ellenőrizzük az m1.1 modell hibáinak normális eloszlását

check_normality (m1.1)


## ----------------------------------------------------------------------------------------------------
# ábrázoljuk az m1.1 modell reziduálisait hisztogrammal, 20 részre bontva az adatokat

hist(m1.1$residuals, breaks=20)


## ----------------------------------------------------------------------------------------------------
# nézzük meg az m1.1 modell reziduálisainak leíró statisztikáit a descr () funkcióval

descr(m1.1$residuals)


## ----------------------------------------------------------------------------------------------------
# ellenőrizzük az m1.1 modell VIF-értékeit

check_collinearity(m1.1)


## --------------------------------------------------------------------------------------------
## # jelenítsük meg az m1.1 modell eredményeit táblázatos formában
 
tab_model (m1.1)


## -------------------------------------------------------------------------------------------
## # a létrehozott modellünket jelenítsük meg úgy, hogy:
 
## #-	a standard hiba megjelenik, viszont együtt a regressziós együtthatókkal (`show.se=T`, `collapse.se=T`)
## #-	a konfidenciaintervallum nem jelenik meg (`show.ci=F`)
## #-	robusztus standard hibákat kérünk le (`vcov.type="HC3"`)
## #-	megjelenítjük a standardizált beta-értékeket is (`show.std=T`)
## #-	a p-érték csillagos jelöléssel jelenik meg (`p.style="stars"`)
## #-	változóinknak magyar nevet adunk (`pred.labels=c(" (intercept)", "boldogság", "gazd. elégedettség", "jövedelem", "pol. érdeklődés", "nem (nő) ", "település:falu", "település:város")`, illetve `dv.labels="gazdasággal való elégedettség"`)
## 
tab_model(m1.1, show.se=T, collapse.se=T, show.ci=F,
           vcov.type="HC3", show.std=T, p.style="stars",
           pred.labels=c("(intercept)", "boldogság", "korm. elégedettség",
                         "jövedelem","pol. érdeklődés", "nem (nő)",
                         "település:falu", "település:város"),
           dv.labels = "gazdasággal való elégedettség")


## ----------------------------------------------------------------------------------------------------
# nézzük meg az m1.1 modell F-statisztikáját a summary () funkció segítségével 

summary (m1.1)


## ----------------------------------------------------------------------------------------------------
# nézzük meg az m1.1 modell alapján a gazdasággal való elégedettség becsült értékeit a kormánnyal való elégedettség különböző szintjein a többi változó rögzítése mellett

predictorEffect("stfgov", m1.1)


## ----------------------------------------------------------------------------------------------------
# nézzük meg az m1.1 modell alapján a gazdasággal való elégedettség predikált értékeit a kormánnyal való elégedettség a 0, a 3, a 7 és a 10-es szintjein

predictorEffect("stfgov", m1.1, c(0,3,7,10))


## ----------------------------------------------------------------------------------------------------
# nézzük meg az m1.1 modell alapján a gazdasággal való elégedettség predikált értékeit a kormánnyal való elégedettség három egyenlő nagyságú csoportjának szakaszhatárain

predictorEffect("stfgov", m1.1, focal.levels = 3)


## ----------------------------------------------------------------------------------------------------
# nézzük meg az m1.1 modell alapján a kormánnyal való elégedettség marginális hatását a gazdasággal való elégedettségre robosztus standard hibák alkalmazásával

plot_model(m1.1, type="eff", terms="stfgov", robust=T)


## ----------------------------------------------------------------------------------------------------
# nézzük meg az m1.1 modell alapján a boldogság marginális hatását a gazdasággal való elégedettségre robusztus standard hibák alkalmazásával

plot_model(m1.1, type="eff", terms="happy", robust=T)


## ------------------------------------------------------------------------------
# mentsük el a kormánnyal való elégedettség marginális hatását mutató ábrát p1, a boldogság marginális hatását ábrázoló ábrát pedig p2 néven, és mindkét ábrán a gazdasággal való elégedettség teljes skáláját jelenítsük meg.

p1<-plot_model(m1.1, type="eff", terms="stfgov", robust=T, axis.lim = c(0,10))
p2<-plot_model(m1.1, type="eff", terms="happy", robust=T, axis.lim = c(0,10))

# ábrázoljuk együtt a két hatást

grid.arrange(p1,p2, ncol=2)


## ---------------------------------------------------------------------
## # ábrázoljuk az összes magyarázó változó marginális hatását együtt
## 
plot_model(m1.1, type="eff", grid=T, axis.lim = c(0,10), robust = T)+xlab("")


## ------------------------------------------------------------------------------
# ábrázoljuk az m1.1 modell alapján településtípus marginális hatását

plot_model(m1.1, type="eff", terms="dom2", robust=T)


## ----------------------------------------------------------------------------------------------------
# ábrázoljuk az m1.1 modell forest plotját a beta értékek megjelenítésével, és a 0,3-as value.offset értékkel

plot_model(m1.1, type="std", show.values = T, robust=T, value.offset=0.3)


## ----------------------------------------------------------------------------------------------------
# nas objektumnéven mentsük el az m1.1 modellben hiányzó értéknek számító esetek sorszámát

nas<-as.numeric(names(m1.1$na.action))


## ----------------------------------------------------------------------------------------------------
# hozzuk létre m1_data_same néven a modelladatbázist, amelyben a modell alapján hiányzó értékként kezelt esetek nem szerepelnek

m1_data_same<-m1_data[-nas,]


## ----------------------------------------------------------------------------------------------------
# m1.1a, m1.1b és m1.1c néven mentsük el a három modellünket

m1.1a<-lm(stfeco~polintr_n + gndr_f + dom2, 
          data=m1_data_same, weights=pspwght)

m1.1b<-lm(stfeco~happy + income + polintr_n + gndr_f + dom2, 
          data=m1_data_same, weights=pspwght)

m1.1c<-lm(stfeco~ stfgov + happy + income + polintr_n + gndr_f + dom2, 
          data=m1_data_same, weights=pspwght)


## ----------------------------------------------------------------------------------------------------
# hasonlítsuk össze az m1.1a, az m1.1b és az m1.1c modellek magyarázóerejét

test_performance(m1.1a, m1.1b, m1.1c)


## --------------------------------------------------------------------------------------------
## # jelenítsük meg az m1.1a, az m1.1b és az m1.1c modellek eredményeit táblázatos formában
 
tab_model(m1.1a, m1.1b, m1.1c, show.se=T, collapse.se=T,
           show.ci=F, vcov.type="HC3", show.std=T, p.style="stars")


## ----------------------------------------------------------------------------------------------------
# m1.2a, m1.2b és m1.2c néven mentsük el a gazdasággal, az oktatással és az egészségüggyel való elégedettséget magyarázó modelleket

m1.2a<-lm(stfeco~happy + income + stfgov + polintr_n + gndr_f + dom2, 
          data=m1_data, weights=pspwght)

m1.2b<-lm(stfedu~happy + income + stfgov + polintr_n + gndr_f + dom2, 
          data=m1_data, weights=pspwght)

m1.2c<-lm(stfhlth~happy + income + stfgov + polintr_n + gndr_f + dom2,
          data=m1_data, weights=pspwght)

## --------------------------------------------------------------------------------------
## # jelenítsük meg a három modellt táblázatos formában
 
 tab_model(m1.2a, m1.2b, m1.2c, show.se=T, collapse.se=T,
           show.ci=F, vcov.type="HC3", show.std=T, p.style="stars")


## ----------------------------------------------------------------------------------------------------
# készítsünk egy közös forest plot ábrát a három modellnek úgy, hogy a beta-értékeket jelenítsük meg, nevezzük el a modelleket „gazdaság”, „oktatás” és „egészségügy” néven, illetve kék, piros és zöld színek jelöljék az egyes modelleket

plot_models(m1.2a, m1.2b, m1.2c, std.est=T,
            m.labels=c("gazdaság", "oktatás", "egészség"), 
            colors=c("blue", "red", "green"))


## ------------------------------------------------------------------------------
# készítsük el az alapmodellt és mentsük el m1.3 néven
m1.3<-lm(stfeco~nwspol+happy + stfgov + income + polintr_n + gndr_f + dom2, 
       data=hun, weights=pspwght)
# távolítsuk el a kiugró értékeket az alapmodell alapján
x<-abs(rstandard(m1.3))>3
xx<-as.numeric(names(x)[x])
m1.3_data<-hun[c(-xx),]
# készítsük el a végleges modellt
m1.3<-lm(stfeco~nwspol+happy + stfgov + income + polintr_n + gndr_f + dom2, 
       data=m1.3_data, weights=pspwght)
# ellenőrizzük a gazdasággal való elégedettség és a hírfogyasztás kapcsolatát
m1.3_data %>% ggplot(aes(nwspol,stfeco))+
  geom_point(position = "jitter")+
  geom_smooth(color="red")+
  geom_smooth(method=lm)


## ------------------------------------------------------------------------------
# jelenítsük meg a gazdasággal való elégedettség és a hírfogyasztás kapcsolatát a hírfogyasztással 0-180 percet töltő válaszadók esetében 
m1.3_data %>% ggplot(aes(nwspol,stfeco))+
  geom_point(position = "jitter")+
  geom_smooth(color="red")+
  geom_smooth(method=lm)+ 
  scale_x_continuous(limits=c(0,180))


## ----------------------------------------------------------------------------------------------------
# a hírfogyasztás hiányzó értékeit nem tartalmazó modelladatbázis elkészítése
m1.3_data<-m1.3_data %>% filter(!is.na(nwspol))
# a logaritmizált hírfogyasztást tartalmazó modellspecifikáció elkészítése
m1.3b<-lm(stfeco~log(nwspol+1)+happy + stfgov + 
            income + polintr_n + gndr_f + dom2, 
          data=m1.3_data, weights=pspwght)
# a másodfokú polinomot tartalmazó modellspecifikáció elkészítése
m1.3c<-lm(stfeco~happy +poly(nwspol,2)+ stfgov + 
            income + polintr_n + gndr_f + dom2, 
          data=m1.3_data, weights=pspwght)
# a harmadfokú polinomot tartalmazó modellspecifikáció elkészítése
m1.3d<-lm(stfeco~poly(nwspol,3)+happy + stfgov + 
            income + polintr_n + gndr_f + dom2, 
          data=m1.3_data, weights=pspwght)


## --------------------------------------------------------------------------------------------
## # a három modell táblázatos megjelenítése
 tab_model(m1.3,m1.3b,m1.3c,m1.3d)


## ----------------------------------------------------------------------------------------------------
# kérjük le a gazdasággal való elégedettség becsült értékeit a hírfogyasztás 10 különböző szintjén
predictorEffect("nwspol", m1.3d, focal.levels = 5)

# kérjük le a hírfogyasztás marginális hatását mutató ábrát
plot_model(m1.3d, type="eff", terms="nwspol")


## ----------------------------------------------------------------------------------------------------
# futtassuk le az alapmodellt és mentsük el m1.4 néven 
m1.4<-lm(nwspol~income + polintr_n + gndr_f + dom2, 
        data=hun, weights=pspwght)
# távolítsuk el a kiugró értékeket
x<-abs(rstandard(m1.4))>3
xx<-as.numeric(names(x)[x])
m1.4_data<-hun[-xx,]
# hozzuk létre a modellt a modelladatbázison
m1.4<-lm(nwspol~income + polintr_n + gndr_f + dom2, 
       data=m1.4_data, weights=pspwght)
# ellenőrizzük a modell feltételeit
check_model(m1.4, detrend=F)


## ----------------------------------------------------------------------------------------------------
# m1.4b néven hozzunk létre egy olyan modellspecifikációt, ahol a függő változót logaritmizált formában léptetjük be a modellbe – mivel a függő változónak van 0 értéke, ezért adjuk hozzá az 1 konstanst is.
m1.4b<-lm(log(nwspol+1)~income + polintr_n + gndr_f + dom2, 
       data=m1.4_data, weights=pspwght)
# ellenőrizzük a modell feltételeit
check_model(m1.4b, detrend=F)


## ----------------------------------------------------------------------------------------------
## # vessük össze a két modellt
 tab_model(m1.4, m1.4b, robust=T)


## ----------------------------------------------------------------------------------------------------
# a predictorEffect () funkció esetében a függő változó eredeti skálájában a becsült értékeket az alábbi módon kapjuk meg a jövedelemváltozó 10 szintjén
predictorEffect("income", m1.4b, focal.levels = 10, transformation=list(link=log, inverse=exp))


## ------------------------------------------------------------------------------
# ábrázoljuk a jövedelem marginális hatását a hírfogyasztásra
plot_model(m1.4b, type="pred", terms="income")


## ----------------------------------------------------------------------------------------------------
# m2a néven mentsük el az egészségüggyel kapcsolatos elégedettséget magyarázó alapmodellt

m2a<-lm(stfhlth~health_n+income+happy + stfgov  + polintr_n + gndr_f + dom2, 
        data=hun, weights=pspwght)

# m2b néven mentsük el az m2a modell a jövedelem és az egészségi állapot interakciójával kibővített változatát

m2b<-lm(stfhlth~health_n+income+happy + stfgov  + polintr_n + gndr_f + dom2 + 
          income*health_n, data=hun, weights=pspwght)


## --------------------------------------------------------------------------------------------
## # nézzük meg modellünket a `tab_model ()` funkció szokásos beállításaival
 
 tab_model(m2a,m2b, show.se=T, collapse.se=T, show.ci=F,
           vcov.type="HC3", show.std=T, p.style="stars")


## ------------------------------------------------------------------------------
# ábrázoljuk az m2b modell interakciós hatását robusztus standard hibák alkalmazásával

plot_model(m2b, "int", robust=T)


## ------------------------------------------------------------------------------
# ábrázoljuk az m2b modell interakciós hatását robusztus standard hibák alkalmazásával és a függő változó teljes skálája megjelenítésével

plot_model(m2b, "int", robust=T, axis.lim = c(0,10))


## ----------------------------------------------------------------------------------------------------
# ábrázoljuk az egészségüggyel való elégedettség predikált értékeit az egészségi állapot 5 különböző szintjén

predictorEffect("health_n", m2b, focal.levels = 5)


## ------------------------------------------------------------------------------
# ábrázoljuk az interakciós hatást a jövedelem 5,6,7,8 szintjein

plot_model(m2b, "pred", robust=T, terms=c("health_n", "income[5,6,7,8]")) 


## ------------------------------------------------------------------------------
# kérjük le úgy az interakciós ábrát, hogy az egészségi állapotot ábrázoljuk moderáló változóként, mégpedig a két szélső értékével, az 1 és 5 értékekkel

plot_model(m2b, "pred", robust=T, terms=c("income", "health_n[1,5]"))


## ------------------------------------------------------------------------------
# nézzük meg a sim_slopes () funkcióval az interakciós hatást a jövedelem 1,3,5,7,10 értékein

sim_slopes(m2b, health_n, income, modx.values = c(1,3,5,7,10), 
           robust=T, jnplot =T)


## ------------------------------------------------------------------------------
# ábrázoljuk a sim_slopes által megjelenített adatokat az interakciós hatásról 

plot(sim_slopes(m2b, health_n, income, modx.values = c(1,3,5,7,10), 
                robust=T, jnplot =T))


## ----------------------------------------------------------------------------------------------------
# Hozzunk létre egy `m2c` elnevezésű modellobjektumot a két változó közötti interakcióval, majd táblázatban foglaljuk össze az eredményeket a korábbi modellekkel együtt, és ábrázoljuk az interakciós hatást.

m2c<-lm(stfhlth~income + health_n +happy + stfgov  + polintr_n + gndr_f +
          dom2 + stfgov*polintr_n, data=hun, weights=pspwght)

## --------------------------------------------------------------------------------------------
 tab_model(m2a,m2b, m2c, show.se=T, collapse.se=T, show.ci=F,
           vcov.type="HC3", show.std=T, p.style="stars")


## ----------------------------------------------------------------------------------------------------
plot_model(m2c, "int", robust=T)


## -------------------------------------------------------------------------------------------
# készítsük el a clsprty és a gndr dummy változók numerikus változatait
hun$clsprty_n<-as.numeric(hun$clsprty)-1
hun$gndr_n<-as.numeric(hun$gndr_f)-1

# futtassuk le a korrelációelemzést és a kereszttábla-elemzést 
cortable<-hun %>% select(clsprty_n, eduyrs, gndr_n ,netinum, ppltrst) %>%  
  wtd.cor(weight=hun$pspwght)
corrplot.mixed(cortable$correlation, upper="circle", lower="number", 
               p.mat=cortable$p.value, insig="blank", bg="yellow")


## -------------------------------------------------------------------------------
 tab_xtab(hun$clsprty, hun$dom, hun$pspwght,
          show.col.prc = T)


## ----------------------------------------------------------------------------------------------------
# hozzuk létre a dom2 változót, ahol a város a referenciakategória
hun <-hun %>% mutate(dom2=relevel(dom, ref=2)) 


## -------------------------------------------------------------------------------------------
# futtassuk az alapmodellt

m1<-glm(clsprty~eduyrs+gndr_f+netinum+ppltrst+dom2, 
        data=hun, family=binomial(), weights=pspwght)


## ----------------------------------------------------------------------------------------------------
# azonosítsuk a kiugró értékeket

x<-abs(rstandard(m1))>3
xx<-as.numeric(names(x)[x])


## ----------------------------------------------------------------------------------------------------
# ellenőrizzük a modell feltételeit

check_model (m1)
vif (m1)
dwt (m1)


## ----------------------------------------------------------------------------------------------------
# netinum_k és ppltrst_k változónéven hozzuk létre a netinum és ppltrst változók +1 konstansal kiegészített változatát 

hun$netinum_k<-hun$netinum+1
hun$ppltrst_k<-hun$ppltrst+1


## ----------------------------------------------------------------------------------------------------
# készítsük el a logaritmizált interakciós együtthatókat tartalmazó változókat logedu_int, lognetinum_int és logppltrst_int néven 

hun$logedu_int<-hun$eduyrs*log(hun$eduyrs)
hun$lognetinum_int<-hun$netinum_k*log(hun$netinum_k)
hun$logppltrst_int<-hun$ppltrst_k*log(hun$ppltrst_k)


## -------------------------------------------------------------------------------------------
# futtassuk a linearitás ellenőrzésére alkalmazott modellt, ami tartalmazza a logaritmizált interakciós együtthatókat, majd táblázatos formában jelenítsük is meg

m1_l<-glm(clsprty~eduyrs+netinum_k+ppltrst_k+
            logedu_int+lognetinum_int+logppltrst_int, 
          data=hun, family=binomial(), weights = pspwght)


## --------------------------------------------------------------------------------
 tab_model (m1_l)


## ----------------------------------------------------------------------------------------------------
# hozzuk létre a modelladatbázist, ami nem tartalmazza a modellben hiányzó értékként kezelt eseteket 

nas<-as.numeric(names(m1$na.action))
m1_data<-hun[-nas,]


## -------------------------------------------------------------------------------------------
# hozzuk létre a linearitás feltételének tesztelésére használt ábrákat

p1<-m1_data %>%
  drop_na(clsprty) %>%
  group_by(eduyrs) %>%
  count(clsprty, wt=pspwght) %>%
  mutate(prop = n/sum(n)) %>%
  filter(clsprty == "pártos") %>%
  summarise(log_odds = log(prop/(1 - prop+0.01)+0.01)) %>%
  ggplot(aes(x = eduyrs, y = log_odds)) +
  geom_point() +
  ylab("Log odds of clsprty")+
  geom_smooth(color="red")+geom_smooth(method=lm)

p2<-m1_data %>%
  drop_na(clsprty) %>%
  group_by(netinum) %>%
  count(clsprty, wt=pspwght) %>%
  mutate(prop = n/sum(n)) %>%
  filter(clsprty == "pártos") %>%
  summarise(log_odds = log(prop/(1 - prop+0.01)+0.01)) %>%
  ggplot(aes(x = netinum, y = log_odds)) +
  geom_point() +
  ylab("Log odds of clsprty")+
  geom_smooth(color="red")+geom_smooth(method=lm)

p3<-m1_data %>%
  drop_na(clsprty) %>%
  group_by(ppltrst) %>%
  count(clsprty, wt=pspwght) %>%
  mutate(prop = n/sum(n)) %>%
  filter(clsprty == "pártos") %>%
  summarise(log_odds = log(prop/(1 - prop+0.01)+0.01)) %>%
  ggplot(aes(x = ppltrst, y = log_odds)) +
  geom_point() +
  ylab("Log odds of clsprty")+
  geom_smooth(color="red")+geom_smooth(method=lm)

# jelenítsük meg egyben a három ábrát

grid.arrange(p1, p2, p3)


## -------------------------------------------------------------------------------------------
# hozzuk létre a három magas mérési szintű magyarázó változóra a binned reziduálisakat tartalmazó ábrákat

p4<-plot(binned_residuals(m1, term="eduyrs", ci_type = "gaussian"))
p5<-plot(binned_residuals(m1, term="netinum", ci_type = "gaussian"))
p6<-plot(binned_residuals(m1, term="ppltrst", ci_type = "gaussian"))

# a három ábrát egyben jelenítsük meg

grid.arrange(p4, p5, p6)


## -------------------------------------------------------------------------------------------
# futtassuk újra a modellt az m1_data adatbázison és mentsük el m1 néven

m1<-glm(clsprty~ eduyrs + gndr_f+netinum+ppltrst+dom2, 
        data=m1_data, family=binomial(), weights=pspwght)


## -------------------------------------------------------------------------------------------
# futtassuk le a likelihood ratio-tesztet a modell szignifikanciájának ellenőrzésére

lrtest (m1)


## -----------------------------------------------------------------------------
## # jelenítsük meg táblázatos formában az m1 modellt

tab_model(m1, show.se = T)


## -------------------------------------------------------------------------------------------
# vizsgáljuk meg a pszeudo R^2^ értékeket

PseudoR2(m1, which="all")


## ----------------------------------------------------------------------------------------------------
#  A három szignifikánsnak bizonyult változóra vonatkozóan vizsgáljuk meg a predikált valószínűségeket. Az iskolázottságnál 4 csoporthatárnál nézzük meg a becsült valószínűségeket, a politikai bizalomnál pedig 11 csoportnál, hogy a változó mindegyik egész értékénél látható legyen a vonatkozó valószínűség.

predictorEffect("gndr_f", m1)

predictorEffect("eduyrs", m1, focal.levels = 4)

predictorEffect("ppltrst", m1, focal.levels = 11)


## --------------------------------------------------------------------------------------------
## # jelenítsük meg az m1 modell összes marginális hatását együtt
 
 plot_model(m1, type="eff", grid=T)+xlab("")


## -------------------------------------------------------------------------------------------
# m1a, m1b és m1c néven mentsünk el alternatív modelleket, ahol a netinum változó (a) kétfokú polinomja, (b) háromfokú polinomja és (c) logaritmizált változata szerepel  

m1a<-glm(clsprty~eduyrs+gndr_f+poly(netinum, 2)+ppltrst+dom2, 
         data=m1_data, family=binomial(), weights=pspwght)
m1b<-glm(clsprty~eduyrs+gndr_f+poly(netinum, 3)+ppltrst+dom2, 
         data=m1_data, family=binomial(), weights=pspwght)
m1c<-glm(clsprty~eduyrs+gndr_f+log(netinum+1)+ppltrst+dom2, 
         data=m1_data, family=binomial(), weights=pspwght)

# m1d, m1e és m1d néven mentsünk el alternatív modelleket, ahol az eduyrs változó (a) kétfokú polinomja, (b) háromfokú polinomja és (c) logaritmizált változata szerepel

m1d<-glm(clsprty~poly(eduyrs, 2)+gndr_f+netinum+ppltrst+dom2, 
         data=m1_data, family=binomial(), weights=pspwght)
m1e<-glm(clsprty~poly(eduyrs, 3)+gndr_f+netinum+ppltrst+dom2, 
         data=m1_data, family=binomial(), weights=pspwght)
m1f<-glm(clsprty~log(eduyrs)+gndr_f+netinum+ppltrst+dom2, 
         data=m1_data, family=binomial(), weights=pspwght)


## ---------------------------------------------------------------------------------
## # jelenítsük meg együtt az m1 modellt és az m1a, m1b, m1c alternatív modelleket
 
 tab_model(m1, m1a, m1b, m1c)


## ----------------------------------------------------------------------------------------------------
# hasonlítsuk össze az m1 modell és az m1a, m1b, m1c alternatív modellek illeszkedési mutatóit 

compare_performance (m1, m1a, m1b, m1c)


## ----------------------------------------------------------------------------------------------------
# likelihood ratio teszttel nézzük meg, hogy az m1c modell szignifikánsan jobb illeszkedést produkál-e, mint az m1 modell

lrtest (m1, m1c)


## --------------------------------------------------------------------------------
## # jelenítsük meg együtt az m1 modellt és az m1d, m1e, m1f alternatív modelleket
 
 tab_model(m1, m1d, m1e, m1f)


## ----------------------------------------------------------------------------------------------------
# hasonlítsuk össze az m1 modell és az m1d, m1e, m1f alternatív modellek illeszkedési mutatóit 

compare_performance (m1, m1d, m1e, m1f)

# likelihood ratio teszttel nézzük meg, hogy az alternatív modellek szignifikánsan jobb illeszkedést produkálnak-e, mint az m1 modell

lrtest(m1, m1d)
lrtest(m1, m1e)
lrtest(m1, m1f)


## ----------------------------------------------------------------------------------------------------
# jelenítsük meg az eduyrs változó m1f alternatív modell alapján becsült valószínűségét 4 csoporthatárnál, és ábrázoljuk a változó marginális hatását

predictorEffect("eduyrs", m1f, focal.levels = 4)
plot_model(m1f, "eff", terms="eduyrs")


## --------------------------------------------------------------------------------------------
## # kereszttáblával ellenőrizzük a párthovatartozás és a nem, illetve a településtípus kapcsolatát
 tab_xtab(hun$party_f, hun$gndr_f, hun$pspwght,
          show.col.prc = T)
 tab_xtab(hun$party_f, hun$dom, hun$pspwght,
          show.col.prc = T)


## ----------------------------------------------------------------------------------------------------
# ANOVA-val ellenőrizzük a párthovatartozás és az iskolázottság, a jövedelem, illetve az emberekbe vetett bizalom kapcsolatát
means_by_group(hun, eduyrs, party_f, weights=pspwght)
means_by_group(hun, netinum, party_f, weights=pspwght)
means_by_group(hun, ppltrst, party_f, weights=pspwght)


## ----------------------------------------------------------------------
# hozzuk létre party_f2 néven a party_f változónak egy olyan változatát, ahol a „nem szavazó” a referenciakategória

hun <-hun %>% mutate(party_f2=relevel(party_f, ref=3)) 

# m1 néven mentsük el az alapmodellt

m1<-multinom(party_f2~ eduyrs+gndr_f+netinum+ppltrst+dom2, 
             data=hun, weights=pspwght)


## ------------------------------------------------------------------------------------------
## # jelenítsük meg táblázatos formában az alapmodellt
 
 tab_model (m1, show.se=T)


## -----------------------------------------------------------------------
# netinum2 néven hozzuk létre a netinum változó standardizált változatát

hun$netinum2<-scale(hun$netinum)

# futtassuk újra az m1 modellt az új standardizált netinum2 változóval

m1<-multinom(party_f2~ eduyrs +gndr_f +netinum2+ppltrst+dom2, 
             data=hun, weights=pspwght)


## ----------------------------------------------------------------------------------------------------
# mentsük el x objektumként a scale () funkció segítségével létrehozott standardizált reziduálisokat

x<-scale(m1$residuals)


## ----------------------------------------------------------------------------------------------------
# xx objektumnéven mentsük el, hogy mely esetekre igaz, hogy 3-nál magasabb a standardizált reziduális abszolút értéke (logikai értékeket rögzítő objektum) 

xx<-abs(x)>3


## ----------------------------------------------------------------------------------------------------
# xxx objektumnéven mentsük el a logikai értékek összegét mindegyik esetnél

xxx<-rowSums(xx)


## ----------------------------------------------------------------------------------------------------
# a kiugró értékek sorszámát mentsük el out objektumnéven

out<-as.numeric(names(xxx)[xxx])


## ----------------------------------------------------------------------------------------------------
# A modelladatbázis létrehozása m1_data néven

nas<-as.numeric(names(m1$na.action))
m1_data<-hun[-nas,]


## -------------------------------------------------------------------------------------------
# az autokorrelációt vizsgáló Durbin-Watson-teszt futtatása a három reziduálisra

dwt(m1$residuals[,1])
dwt(m1$residuals[,2])
dwt(m1$residuals[,3])



## ----------------------------------------------------------------------------------------------------
# a multikollineraritást vizsgáló VIF-teszt futtatása

# a modell lineáris regresszióként való futtatása
m1_lin<-lm(as.numeric(party_f2)~ eduyrs +gndr_f +netinum2+ppltrst+dom2, 
             data=hun, weights=pspwght)
# a vif-értékek lekérése az m1_lin modellre
vif(m1_lin)


## -----------------------------------------------------------------------
# készítsük el a netinum2 változó csak 0-nál magasabb értékeket tartalmazó változatát az 1,59 konstans hozzáadásával és mentsük el netinum2_k változónéven
m1_data$netinum2_k<-m1_data$netinum2+1.59

# hozzuk létre a netinum2_k változó interakciós terminusát a linearitás feltételének teszteléséhez. 

m1_data$lognetinum2_int<-m1_data$netinum2_k*log(m1_data$netinum2_k)

# ugyanezt a két változót hozzuk létre a hun adatbázison is, hogy későbbi modellek számára is rendelkezésre álljanak

hun$netinum2_k<-hun$netinum2+1.59
hun$lognetinum2_int<-hun$netinum2_k*log(hun$netinum2_k)

# Futtassuk a linearitás tesztelésére használt modellt az interakciós változókkal (a jövedelemnél a csak pozitív értéket tartalmazó netinum2_k változót kell alkalmazni)

m1_l<-multinom(party_f2~eduyrs+netinum2_k+ppltrst_k+
             logedu_int+lognetinum2_int+logppltrst_int, 
             data=m1_data, weights = pspwght)


## ---------------------------------------------------------------
## # Jelenítsük meg a modellt táblázatos formában
 
 tab_model (m1_l)


## ----------------------------------------------------------------------------------------------------
# hozzuk létre gov objektumnéven az ellenzékieket nem tartalmazó adatbázist

gov<-m1_data %>% filter(party_f2!="ellenzéki")

# távolítsuk el az ellenzéki faktorszintet is a „party_f2” változóból

gov$party_f2<-droplevels(gov$party_f2)

# hozzuk létre opp objektumnéven a kormánypártiakat nem tartalmazó adatbázist

opp<-m1_data %>% filter(party_f2!="kormánypárti")

# távolítsuk el a kormánypárti faktorszintet is a „party_f2” változóból

opp$party_f2<-droplevels(opp$party_f2)


## ------------------------------------------------------------------------------
# vizuálisan ellenőrizzük a kormánypártiakra vonatkozó modell linearitásra vonatkozó feltételét a jövedelemváltozó kapcsán

p1<-gov %>%
  drop_na(party_f2) %>%
  group_by(netinum2) %>%
  count(party_f2, wt=pspwght) %>%
  mutate(prop = n/sum(n)) %>%
  filter(party_f2 == "kormánypárti") %>%
  summarise(log_odds = log(prop/(1 - prop+0.01)+0.01)) %>%
  ggplot(aes(x = netinum2, y = log_odds)) +
  geom_point() +
  ylab("Log odds of kormánypárti")+
  geom_smooth(color="red")+geom_smooth(method=lm)

# vizuálisan ellenőrizzük az ellenzékiekre vonatkozó modell linearitásra vonatkozó feltételét a jövedelem változó kapcsán

p2<-opp %>%
  drop_na(party_f2) %>%
  group_by(netinum2) %>%
  count(party_f2, wt=pspwght) %>%
  mutate(prop = n/sum(n)) %>%
  filter(party_f2 == "ellenzéki") %>%
  summarise(log_odds = log(prop/(1 - prop+0.01)+0.01)) %>%
  ggplot(aes(x = netinum2, y = log_odds)) +
  geom_point() +
  ylab("Log odds of ellenzéki")+
  geom_smooth(color="red")+geom_smooth(method=lm)

# jelenítsük meg együtt a két ábrát

grid.arrange(p1, p2)


## -----------------------------------------------------------
# m1 néven mentsük el az m1_data objektumon futtatott multinomiális logisztikus regressziómodellt

m1<-multinom(party_f2~eduyrs+gndr_f+netinum2+ppltrst+dom2, 
             data=m1_data, weights=pspwght)

# m1a néven mentsük el a netinum2 változó másodfokú polinomát tartalmazó alternatív modellt

m1a<-multinom(party_f2~eduyrs+gndr_f+poly(netinum2,2)+ppltrst+dom2, 
              data=m1_data, weights=pspwght)

# m1b néven mentsük el a netinum2 változó harmadfokú polinomát tartalmazó alternatív modellt

m1b<-multinom(party_f2~eduyrs+gndr_f+poly(netinum2,3)+ppltrst+dom2, 
              data=m1_data, weights=pspwght)

# m1c néven mentsük el a netinum2 változó logaritmizált változatát tartalmazó alternatív modellt

m1c<-multinom(party_f2~eduyrs+gndr_f+log(netinum2_k)+ppltrst+dom2, 
              data=m1_data, weights=pspwght)


## ---------------------------------------------------------------------------------------------
## # Jelenítsük meg mindegyik modellre a regressziós táblázat netinum2 változóra vonatkozó részét
 
 tab_model(m1, keep=c("netinum2"))
 tab_model(m1a, keep=c("netinum2"))
 tab_model(m1b, keep=c("netinum2"))
 tab_model(m1c, keep=c("netinum2"))


## ----------------------------------------------------------------------------------------------------
# Hasonlítsuk össze a modellek illeszkedési mutatóit

compare_performance(m1, m1a, m1b, m1c)


## -------------------------------------------------------------------------------------------
# Értékeljük, hogy az m1c modell szignifikánsan jobb-e az alapmodellnél

lrtest (m1, m1c)


## ------------------------------------------------------------------------------
# Értékeljük, hogy az m1c modell szignifikáns-e

lrtest (m1c)


## --------------------------------------------------------------------------------------------
## # jelenítsük meg az m1c modellt táblázatos formában
 
 tab_model(m1c, show.se=T)


## ------------------------------------------------------------------------------
# vizsgáljuk meg a jövedelemváltozó becsült értékeit 4 csoportra bontva

predictorEffect("netinum2_k", m1c, focal.levels = 4)

# ábrázoljuk a jövedelemváltozó marginális hatását

plot_model (m1c, type="eff", terms = "netinum2_k")


## ----------------------------------------------------------------------------------------------------
# ellenőrizzük a függő változó és a független változó közötti kétváltozós kapcsolatokat

indep_test(as.numeric(hun$polintr_f), hun$eduyrs, 
           weights=hun$pspwght, method="spearman")
indep_test(as.numeric(hun$polintr_f), hun$netinum, 
           weights=hun$pspwght, method="spearman")
indep_test(as.numeric(hun$polintr_f), hun$ppltrst, 
           weights=hun$pspwght, method="spearman")


## --------------------------------------------------------------------------------------------
 tab_xtab(hun$polintr_f, hun$gndr_f, hun$pspwght,
          show.col.prc = T)
 tab_xtab(hun$polintr_f, hun$dom, hun$pspwght,
          show.col.prc = T)


## ------------------------------------------------------------------------------
# m1 objektumnéven mentsük el az ordinális logisztikus regressziós alapmodellt

m1<- polr(polintr_f ~ eduyrs+ gndr_f+netinum2+ppltrst+dom2, 
          data = hun, weights = pspwght, Hess = TRUE)


## ----------------------------------------------------------------------------------------------------
# készítsük el a modell adattáblát m1_data néven

nas<-as.numeric(names(m1$na.action))
m1_data<-hun[-nas,]


## ----------------------------------------------------------------------------------------------------
# x objektumba rögzítsük, hogy mely esetekre igaz az, hogy a probability-scale reziduálisának abszolút értéke nagyobb, mint 0,95

x<-abs(presid(m1))>0.95


## ----------------------------------------------------------------------------------------------------
# mentsük el az x vektor kiugró értékeinek sorszámát az `xx` vektorba

xx<-which(x==TRUE)

# távolítsuk el az xx vektorban rögzített kiugró értékeket az m1_data adatbázisból

m1_data<-m1_data[-xx,]


## ------------------------------------------------------------------------------
# futtassuk újra az m1 modellt az m1_data modelladatbázison

m1<- polr(polintr_f ~ eduyrs+gndr_f+netinum2+ppltrst+dom2, 
          data = m1_data, weights = pspwght, Hess = TRUE)


## ------------------------------------------------------------------------------
# ellenőrizzük az ordinális logisztikus regressziómodell feltételeinek teljesülését

vif(m1)
dwt(presid(m1))
m1l<- polr(polintr_f ~ eduyrs+netinum2_k+ppltrst_k+logedu_int+
             lognetinum2_int+logppltrst_int, 
           data = m1_data, weights = pspwght,Hess = TRUE)


## ----------------------------------------------------------------------
 tab_model(m1l)


## ------------------------------------------------------------------------------
# ellenőrizzük vizuálisan a linearitás feltételének teljesülését a politikai bizalom változója kapcsán

p1<-m1_data %>%
  drop_na(polintr_f) %>%
  group_by(ppltrst) %>%
  count(polintr_f, wt=pspwght) %>%
  mutate(prop = n/sum(n)) %>%
  filter(polintr_f == "egyáltalán nem") %>%
  summarise(log_odds = log(prop/(1 - prop))) %>%
  ggplot(aes(x = ppltrst, y = log_odds)) +
  geom_point() +
  ylab("Log odds of egyáltalán nem")+
  geom_smooth(color="red")+geom_smooth(method=lm)

p2<-m1_data %>%
  drop_na(polintr_f) %>%
  group_by(ppltrst) %>%
  count(polintr_f, wt=pspwght) %>%
  mutate(prop = n/sum(n)) %>%
  filter(polintr_f == "alig") %>%
  summarise(log_odds = log(prop/(1 - prop))) %>%
  ggplot(aes(x = ppltrst, y = log_odds)) +
  geom_point() +
  ylab("Log odds of egyáltalán nem")+
  geom_smooth(color="red")+geom_smooth(method=lm)

p3<-m1_data %>%
  drop_na(polintr_f) %>%
  group_by(ppltrst) %>%
  count(polintr_f, wt=pspwght) %>%
  mutate(prop = n/sum(n)) %>%
  filter(polintr_f == "eléggé") %>%
  summarise(log_odds = log(prop/(1 - prop))) %>%
  ggplot(aes(x = ppltrst, y = log_odds)) +
  geom_point() +
  ylab("Log odds of egyáltalán nem")+
  geom_smooth(color="red")+geom_smooth(method=lm)

p4<-m1_data %>%
  drop_na(polintr_f) %>%
  group_by(ppltrst) %>%
  count(polintr_f, wt=pspwght) %>%
  mutate(prop = n/sum(n)) %>%
  filter(polintr_f == "nagyon") %>%
  summarise(log_odds = log(prop/(1 - prop))) %>%
  ggplot(aes(x = ppltrst, y = log_odds)) +
  geom_point() +
  ylab("Log odds of egyáltalán nem")+
  geom_smooth(color="red")+geom_smooth(method=lm)

# jelenítsük meg együtt a négy ábrát
grid.arrange(p1,p2,p3,p4)


## ----------------------------------------------------------------------------------------------------
# futtassuk a Brant-tesztet

poTest (m1)


## ------------------------------------------------------------------------------
# ellenőrizzük az m1 modell szignifikanciáját

lrtest (m1)

## ----------------------------------------------------------------------
## # kérjük le az m1 modellhez tartozó táblázatot
 
 tab_model (m1, show.se=T)


## ------------------------------------------------------------------------------
# ábrázoljuk az iskolai végzettség marginális hatását

plot_model(m1, type="eff", terms="eduyrs")


## ------------------------------------------------------------------------------
# vizsgáljuk meg a különböző iskolai végzettségi szintekhez tartozó predikált értékeket négy iskolai szintnél

predictorEffect("eduyrs", m1, focal.levels = 4)


## ----------------------------------------------------------------------------------------------------
# vizsgáljuk meg előzetesen a két változó jellemzőit a `descr ()` funkció segítségével, viszont a súlyt hagyjuk ki ezen a ponton, hogy az alakzati mutatókat is lássuk

hun %>% select(polpart, nwspol) %>% descr()


## ----------------------------------------------------------------------------------------------------
# ellenőrizzük a kétváltozós kapcsolatokat Spearman rho-teszttel és ANOVA-teszttel

indep_test(hun$polpart, hun$eduyrs, method="spearman")
indep_test(hun$polpart, hun$netinum, method="spearman")
indep_test(hun$polpart, hun$ppltrst, method="spearman")
indep_test(hun$polpart, as.numeric(hun$gndr_f), method="spearman")
means_by_group(hun, polpart, dom2, weights=pspwght)
indep_test(hun$nwspol, hun$eduyrs, method="spearman")
indep_test(hun$nwspol, hun$netinum, method="spearman")
indep_test(hun$nwspol, hun$ppltrst, method="spearman")
indep_test(hun$nwspol, as.numeric(hun$gndr_f), method="spearman")
means_by_group(hun, nwspol, dom2, weights=pspwght)


## ------------------------------------------------------------------------------
# m1 objektumnéven mentsük el a Poisson-regresszió alapmodelljét

m1<-glm(polpart~ eduyrs+gndr_f+netinum2+ppltrst+dom2, family=poisson(), 
        data=hun, weights = pspwght)

# m2 objektumnéven mentsük el a negatív binomiális regresszió alapmodelljét

m2<-glm.nb(nwspol~ eduyrs+gndr_f+netinum2+ppltrst+dom2, 
           data=hun, weights = pspwght)


## ------------------------------------------------------------------------------
# hun2 objektumnéven mentsünk el egy olyan adatbázist, amiben a netinum2 változón 5-nél nagyobb értékkel bíró válaszadók nem szerepelnek

hun2<-hun %>% filter(netinum2<5)


## ----------------------------------------------------------------------------------------------------
# futtassuk újra a negatív binomiális regressziómodellt a hun2 adatbázison

m2<-glm.nb(nwspol~ eduyrs+gndr_f+netinum2+ppltrst+dom2, 
           data=hun2, weights = pspwght)


## ----------------------------------------------------------------------------------------------------
# azonosítsuk az m1 modell hiányzó értékeit

nas<-as.numeric(names(m1$na.action))

# azonosítsuk az m1 modell kiugró értékeit

x<-abs(rstandard(m1))>3
xx<-as.numeric(names(x)[x])

# m1_data néven hozzuk létre a modelladatbázist a hun adatbázisból az m1 modell hiányzó és kiugró értékeinek eltávolításával

m1_data<-hun[-c(nas, xx),]

# azonosítsuk az m2 modell hiányzó értékeit

nas2<-as.numeric(names(m2$na.action))

# azonosítsuk az m2 modell kiugró értékeit

x2<-abs(rstandard(m2))>3
xx2<-as.numeric(names(x2)[x2])

# mivel kiugró értékek nincsenek, ezért az m2_data modelladatbázis létrehozásához a hun2 adatbázisból csak a modell hiányzó értékeit távolítsuk el 

m2_data<-hun2[-nas2,]


## ----------------------------------------------------------------------------------------------------
# futtassuk újra a modelleket a saját modelladatbázisukon

m1<-glm(polpart~ eduyrs+gndr_f+netinum2+ppltrst+dom2, family=poisson(), 
        data=m1_data, weights = pspwght)

m2<-glm.nb(nwspol~ eduyrs+gndr_f+netinum2+ppltrst+dom2, 
           data=m2_data, weights = pspwght)


## ------------------------------------------------------------------------------
# ellenőrizzük a túlszórás létét mindkét módszerrel az m1 modellnél

check_model (m1)


## ------------------------------------------------------------------------------
check_overdispersion (m1)


## ------------------------------------------------------------------------------
# ellenőrizzük a túlszórás létét mindkét módszerrel az m2 modellnél

check_model (m2)


## ------------------------------------------------------------------------------
check_overdispersion (m2)


## ------------------------------------------------------------------------------
# készítsünk alternatív modelleket m1b és m2b néven, ahol az ellentétes regressziós metódust alkalmazzuk

m1b<-glm.nb (polpart~ eduyrs+gndr_f+netinum2+ppltrst+dom2, 
             data=m1_data, weights = pspwght)

m2b<-glm(nwspol~ eduyrs+gndr_f+netinum2+ppltrst+dom2, 
         family=poisson (), data=m2_data, weights = pspwght)

# teszteljük, hogy javult-e a modellek illeszkedése az alternatív modellspecifikációnak köszönhetően

lrtest (m1, m1b)
lrtest (m2, m2b)


## ----------------------------------------------------------------------------------------------------
# Durbin-Watson-teszttel ellenőrizzük a hibák függetlenségének feltételét az m1 és m2 modellek kapcsán

dwt (m1)
dwt (m2)

# ellenőrizzük a linearitás feltételét a logisztikus-interakciós tagok segítségével

m1_l<-glm(polpart~eduyrs+netinum2_k+ppltrst_k+
            logedu_int+lognetinum2_int+logppltrst_int, 
          family=poisson(), data=m1_data, weights=pspwght)

m2_l<-glm.nb(nwspol~eduyrs+netinum2_k+ppltrst_k+
               logedu_int+lognetinum2_int+logppltrst_int, 
             data=m2_data, weights=pspwght)


## ---------------------------------------------------------------------------------------------
 tab_model (m1_l, m2_l)


## ------------------------------------------------------------------------------
# az m2 modellnél ellenőrizzük vizuálisan is a linearitás feltételét 

p1<-m2_data %>%drop_na(nwspol) %>% 
  group_by(ppltrst) %>% 
  summarize(log_mean=log(weighted.mean(nwspol+1, pspwght))) %>% 
  ggplot(aes(x = ppltrst, y = log_mean)) +
  geom_point() +
  ylab("Log mean of nwspol")+
  geom_smooth(color="red")+geom_smooth(method=lm)

p2<- m2_data %>%drop_na(nwspol) %>% 
  group_by(eduyrs) %>% 
  summarize(log_mean=(weighted.mean(nwspol+1, pspwght)))%>% 
  ggplot(aes(x = eduyrs, y = log_mean)) +
  geom_point() +
  ylab("Log mean of nwspol")+
  geom_smooth(color="red")+geom_smooth(method=lm)

p3<- m2_data %>%drop_na(nwspol) %>% 
  group_by(netinum2) %>% 
  summarize(log_mean=(weighted.mean(nwspol+1, pspwght)))%>% 
  ggplot(aes(x = netinum2, y = log_mean)) +
  geom_point() +
  ylab("Log mean of nwspol")+
  geom_smooth(color="red")+geom_smooth(method=lm)

# jelenítsük meg együtt a három ábrát

grid.arrange(p1,p2,p3)


## ----------------------------------------------------------------------------------------------------
# értékeljük az m1 és m2 modell szignifikanciáját

lrtest (m1)
lrtest (m2)


## --------------------------------------------------------------------------------------------
## # jelenítsük meg az m1 és m2 modelleket táblázatos formában a standard hibával együtt
## 
## tab_model (m1, m2, show.se=T)


## ----------------------------------------------------------------------------------------------------
# hozzunk létre egy freetime változót a wrhtot változó segítségével

m2_data<-m2_data %>% mutate(freetime=(168 -m2_data$wkhtot)/7)


## ----------------------------------------------------------------------------------------------------
# m2b objektumnéven mentsük el a szabadidőt offset-változóként tartalmazó modellt. Mivel a változónak lehet 0 értéke is, és az `offset ()` funkcióba a kitettség változót logaritmizált formában kell beléptetni, ezért egy 1 értékű konstanst is adjunk hozzá a változóhoz 

m2b<-glm.nb(nwspol~gndr_f+eduyrs+netinum2+ppltrst+dom2+offset(log(freetime+1)), 
            data=m2_data, weights = pspwght)


## -------------------------------------------------------------------------------------------
## # Hasonlítsuk össze a kitettség-változót tartalmazó `m2b` modellt az eredeti `m2` modellel!
 
 tab_model (m2, m2b)

