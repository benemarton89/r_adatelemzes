### KÉTVÁLTOZÓS ELEMZÉSEK ###



## --------------------------------------------------------------------------------------------
## # adjuk meg a munkakönyvtárat, töltsük be a használt adatbázist, korlátozzuk a számok normálalakos megjelenítését, és töltsük be a szokásos (tidyverse, summarytools) package-eket
## 
## setwd("SAJÁT MAPPA")
load("datasets/hun.RData")
options(scipen=999)
library(tidyverse)
library(summarytools)


## -------------------------------------------------------------------------------
# töltsük be a kereszttábla-elemzéshez szükséges package-eket

library (sjPlot)
library (descriptio)


## ----------------------------------------------------------------------------------------------
## # nézzük meg a party_f és a dom változó függetlensége esetén várható értékeket
## 
 tab_xtab(hun$party_f, hun$dom, hun$pspwght,
          show.exp = T, show.obs = F)


## ---------------------------------------------------------------------------------------------
## # kereszttábla-elemzéssel vizsgáljuk meg a party_f és a dom változó kapcsolatát és adjunk címet is a táblázatnak a title= argumentummal
## 
 tab_xtab(hun$party_f, hun$dom, hun$pspwght,
          show.col.prc = T,
          title="A településtípus és a szavazói viselkedés kapcsolata")


## ----------------------------------------------------------------------------------------------------
# z-teszt segítségével nézzük meg, hogy a kormánypártok falusi és városi támogatottsága közötti eltérés szignifikáns-e

prop.test(c(163,186), c(429,519))


## ----------------------------------------------------------------------------------------------------
# nézzük meg, hogy a kormánypártok falusi és nagyvárosi támogatottsága közötti eltérés szignifikáns-e

prop.test(c(163,160), c(429,484))


## --------------------------------------------------------------------------------------------
## # futtassuk újra a kereszttáblát, most azonban sorszázalékot kérjünk le az oszlopszázalék helyett
 
 tab_xtab(hun$party_f, hun$dom, hun$pspwght,
          show.row.prc = T,
          title="A településtípus és a szavazói viselkedés kapcsolata")


## --------------------------------------------------------------------------------------------
## # futtassuk újra a kereszttáblát teljes százalékokkal
 
 tab_xtab(hun$party_f, hun$dom, hun$pspwght,
          show.cell.prc = T,
          title="A településtípus és a szavazói viselkedés kapcsolata")


## ----------------------------------------------------------------------------------------------------
# készítsünk egy csoportosított oszlopdiagramot a party_f és a dom változó kapcsolatáról az esetszámok és a total oszlop kihagyásával, illetve a statisztikai adatok feltüntetésével

plot_xtab(hun$dom, hun$party_f, margin="row", 
          weight.by = hun$pspwght, show.n=F, 
          show.total=F, show.summary=T)


## ------------------------------------------------------------------------------
# az előbb létrehozott ábra színeit beépített paletta segítségével állítsuk be a scale_fill_brewer () funkció segítségével, használjuk a 8. színskálát, témának pedig a theme_light () beépített témát.

plot_xtab(hun$dom, hun$party_f, margin="row", 
          weight.by = hun$pspwght, show.n=F, 
          show.total=F, show.summary=T)+
  scale_fill_brewer(palette=8)+
  theme_light() 


## -------------------------------------------------------------------------------
# telepítés után töltsük be a vcd package-et

library (vcd)


## ----------------------------------------------------------------------------------------------------
# a `weighted.table ()` funkció segítségével mentsük el korábbi kereszttáblánkat az R-ben `table1` néven - oszlopszázalékokat kérjünk le, és mivel később ábrázolni szeretnénk a táblázatot, ezért tüntessük el a total sort és oszlopot.
 
table1<-weighted.table(hun$party_f, hun$dom, weights=hun$pspwght, 
               stat="cprop", mar=F)


## ----------------------------------------------------------------------------------------------------
# változtassuk meg a table1 adattábla változóneveit

names(dimnames(table1))<-c("szavazás", "településtípus")


## ----------------------------------------------------------------------------------------------------
# készítsük el a party_f és a dom változó kapcsolatát ábrázoló mozaikábrát

mosaic(table1, labeling=labeling_values())


## ----------------------------------------------------------------------------------------------------
# változtassuk meg a mozaikábra színeit úgy, hogy az első oszlop sötétszürke, a második szürke, a harmadik pedig világosszürke legyen

mosaic(table1, labeling=labeling_values(),
       gp = gpar(fill = c(rep("darkgrey", 3), 
                          rep("grey", 3), 
                          rep("lightgrey", 3))))


## --------------------------------------------------------------------------------
# töltsük be a korrelációelemzéshez szükséges package-ket

library(wdm)
library(weights)
library(correlation)
library(corrplot)


## -----------------------------------------------------------------------------
# ábrázoljuk pontdiagrammal, illetve az egyenes és a Loess-görbe hozzáadásával az emberekbe vetett és a politikusokba vetett bizalom kapcsolatát

ggplot(hun, aes(ppltrst, trstplt, weight=pspwght))+
  geom_point(position="jitter")+
  geom_smooth(method="lm")+
  geom_smooth(method="loess", color="red")


## -------------------------------------------------------------------------------------------
# ellenőrizzük a ppltrst változó eloszlását hisztogram és sűrűséggörbe segítségével

ggplot(hun, aes(ppltrst, weight=pspwght))+ 
  geom_histogram(aes(y=..density..), binwidth=1, 
                 color="black", fill="white")+
  geom_density(adjust=2)


## -------------------------------------------------------------------------------------------
# ellenőrizzük a trstplt változó eloszlását hisztogram és sűrűséggörbe segítségével

ggplot(hun, aes(trstplt, weight=pspwght))+ 
  geom_histogram(aes(y=..density..),binwidth=1,
                 color="black",fill="white")+
  geom_density(adjust=2)


## -----------------------------------------------------------------------------
# gyorsparancsok segítségével hozzunk létre hisztogramot a ppltrst és a trstplt változókra

qplot(hun$ppltrst, weight=hun$pspwght)
qplot(hun$trstplt, weight=hun$pspwght)


## --------------------------------------------------------------------------------
# gyorsparancsok segítségével hozzunk létre sűrűségábrát a ppltrst és a trstplt változókra

qplot(hun$ppltrst, weight=hun$pspwght, geom="density")
qplot(hun$trstplt, weight=hun$pspwght, geom="density")


## ----------------------------------------------------------------------------
# telepítés után töltsük be a DescTools package-et

library(DescTools)


## ----------------------------------------------------------------------------------------------------
# x néven mentsük el a hun adatbázis olyan változatát, ami nem tartalmazza a ppltrst változón NA értékkel rendelkező eseteket 

x<-hun %>% filter(!is.na(ppltrst))

# y néven mentsük el a hun adatbázis olyan változatát, ami nem tartalmazza a trstplt változón NA értékkel rendelkező eseteket 

y<-hun %>% filter(!is.na(trstplt))

# kérjük le a ppltrst és a trstplt súlyozott alakzati mutatóját a változók hiányzó értékeit nem tartalmazó x illetve y objektum alkalmazásával

Kurt(x$ppltrst, x$pspwght)
Skew(x$ppltrst, x$pspwght)
Kurt(y$trstplt, y$pspwght)
Skew(y$trstplt, y$pspwght)


## ----------------------------------------------------------------------------------------------------
# elemezzük korrerációelemzéssel a ppltrst és a trstplt változók kapcsolatát

indep_test(hun$ppltrst, hun$trstplt, weights=hun$pspwght)


## ----------------------------------------------------------------------------------------------------
# a with funkció segítségével egyszerűsítsük a fenti parancsot

with(hun, indep_test(ppltrst, trstplt, weights=pspwght))


## ----------------------------------------------------------------------------------------------------
# vizsgáljuk meg a ppltrst és trstplt változók kapcsolatát Spearman-féle és Kendall-féle rangkorrelációs eljárással is

indep_test(hun$ppltrst, hun$trstplt, weights=hun$pspwght, method="spearman")
indep_test(hun$ppltrst, hun$trstplt, weights=hun$pspwght, method="kendall") 


## ----------------------------------------------------------------------------------------------------
# telepítés után töltsük be a correlation package-et

library(correlation)

# vizsgáljuk meg súlyozatlan formában a ppltrst és a trstplt változók kapcsolatát a Hoeffding-féle D segítségével
correlation(hun, select="ppltrst", select2="trstplt", method="hoeffding")


## ----------------------------------------------------------------------------------------------------
# azonosítsuk a netinum változó kiugró értékeit

boxplot(hun$netinum)$out


## ----------------------------------------------------------------------------------------------------
# állítsuk sorrendbe a netinum változó kiugró értékeit

sort(boxplot(hun$netinum)$out)


## ----------------------------------------------------------------------------------------------------
# out néven mentsük a hun adatbázisnak azt a változatát, ami csak azokat a válaszadókat tartalmazza, akik a netinum változón 310000 alatti értéket vesznek fel

out<-hun %>% filter(netinum<310000) 


## ------------------------------------------------------------------------------
# ellenőrizzük a linearitás feltételét a netinum és a happy változó kapcsolatára vonatkozóan a netinum változó kiugró értékű eseteit nem tartalmazó out adattáblán

ggplot(out, aes(netinum, happy, weight=pspwght))+
  geom_point(position="jitter")+ 
  geom_smooth(method="lm")+ 
  geom_smooth(method="loess", color="red")


## ---------------------------------------------------------------------------
# ellenőrizzük a netinum változó eloszlását a kiugró értékű eseteket nem tartalmazó out adattáblán 

qplot(out$netinum, weights= out $pspwght)
qplot(out$netinum, weights= out$pspwght, geom="density")
Kurt(out$netinum, out$pspwght)
Skew(out$netinum, out$pspwght)


## ------------------------------------------------------------------------------
# ellenőrizzük a happy változó eloszlását az out adattáblán 

qplot(out$happy, weights=out$pspwght)
qplot(out$happy, weights=out$pspwght, geom="density")
Kurt(out$happy, out$pspwght)
Skew(out$happy, out$pspwght)


## ----------------------------------------------------------------------------------------------------
# mivel itt a feltétel teljesülése kérdéses, mindhárom korrelációs együtthatót kérjük le a netinunm és happy változó kapcsolatára

indep_test(out$netinum, out$happy, method="pearson")
indep_test(out$netinum, out$happy, method="spearman")
indep_test(out$netinum, out$happy, method="kendall")


## -------------------------------------------------------------------------------------------
# telepítés után hívjuk be a GGally package-et

library(GGally)

## --------------------------------------------------------------------------
# szűkítsük le pipe-ok és a `select ()` funkció segítségével a minket érdeklő változókra az adatbázist, majd ezen az adatbázison alkalmazzuk a ggpair () funkciót, az esztétikájában (`aes ()`) definiálva a súlyváltozónkat, az alsó panelen pedig a Loess-görbét alkalmazva

hun %>% select(ppltrst, trstprl:trstun, gndr_f) %>% 
  ggpairs(aes(weight=hun$pspwght),
          lower=list(continuous="smooth_loess"))  


## ----------------------------------------------------------------------------------------------------
# a problémásnak tűnő trstlgl, trstplc, trstplt és trstprt változóknál ellenőrizzük a csúcsosság- és ferdeségértékeket

hun %>% select(trstlgl, trstplc, trstplt, trstprt) %>%
  descr()


## ----------------------------------------------------------------------------------------------------
# gndr_d néven mentsük a gndr_f változó dummy változatát

hun$gndr_d<-as.numeric(hun$gndr_f)-1


## -------------------------------------------------------------------------------------------
# cortable néven mentsük el a több változónkra vonatkozó korrelációs mátrixot

cortable<-hun %>% 
  select(ppltrst, trstprl:trstun, gndr_d) %>%  
  wtd.cor(weight=hun$pspwght)


## ----------------------------------------------------------------------------------------------------
# kérjük le a cortable p-értékeit

cortable$p.value %>% 
  round(3)


## ----------------------------------------------------------------------------------------------------
# nézzük meg a trstplt, trstprt és a trstplc változók közötti kapcsolatokat a szignifikanciateszt szempontjából leginkább robusztus Kendall-féle rangkorrelációs eljárással

indep_test(hun$trstplt, hun$trstprt, weights= hun$pspwght, method="kendall")
indep_test(hun$trstplt, hun$trstplc, weights= hun$pspwght, method="kendall")
indep_test(hun$trstprt, hun$trstplc, weights= hun$pspwght, method="kendall") 


## ----------------------------------------------------------------------------------------------------
# kérjük le a cortable korrelációs együtthatóit

cortable$correlation %>% round(2)


## ----------------------------------------------------------------------------------------------------
# készítsünk egy olyan ábrát a `corrplot.mixed ()` funkcióval, ahol az alsó panelen a korrelációs együtthatókat látjuk, a felső panelen a körök mérete jelöli a kapcsolat erősségét, a nem szignifikáns értékek pedig üresen maradnak

corrplot.mixed(cortable$correlation, upper="circle", lower="number", 
               p.mat=cortable$p.value, insig="blank")


## -------------------------------------------------------------------------------------------
# a telepítés után hívjuk be a cocor package-et

library(cocor)


## ----------------------------------------------------------------------------------------------------
# hozzuk létre előbb a két almintát, `m` névvel jelölve azt az adattáblát, ami csak a férfiak, és `f` névvel azt, ami csak a nők válaszait tartalmazza

m<-hun %>% filter(gndr_f=="férfi")
f<-hun %>% filter(gndr_f=="nő")


## ----------------------------------------------------------------------------------------------------
# az `indep_test ()` funkcióval mindkét minta kapcsán nézzük meg az emberekbe és a politikusokba vetett bizalom korrelációs összefüggését

indep_test(m$ppltrst, m$trstplt, weights=m$pspwght)
indep_test(f$ppltrst, f$trstplt, weights=f$pspwght)


## ----------------------------------------------------------------------------------------------------
# vessük össze a két független mintában megfigyelt korrelációs koefficienseket

cocor.indep.groups(0.32, 0.25, 590, 778) 


## ----------------------------------------------------------------------------------------------------
# vessük össze a trstplt és a trstprt változók közötti, valamint a trstep és a trstun változók közötti korrelációs koefficienseket

cocor.dep.groups.nonoverlap(0.87, 0.81, 0.46, 0.43, 0.49, 0.44, 1661)


## ----------------------------------------------------------------------------------------------------
# vessük össze a a trstplt és a trstprl és a trstprt és a trstprl változók között korrelációs koefficienseket

cocor.dep.groups.overlap(0.64, 0.62, 0.87, 1661) 


## -------------------------------------------------------------------------------------------
# töltsük be a t-teszthez szükséges package-ket

library (tidyverse)
library (summarytools)
library (sjstats)


## -------------------------------------------------------------------------------------------
# ellenőrizzük az stfeco változó eloszlását a nem két kategóriájára

ggplot(hun, aes(stfeco, weight=pspwght)) +
  geom_density(aes(color=gndr_f), adjust=2)


## ----------------------------------------------------------------------------------------------------
# m néven mentsünk el egy adatbázist, ami csak a férfiakat és stfeco változón nem hiányzó értékekkel rendelkező eseteit tartalmazza

m<-hun %>% filter(!is.na(stfeco)&gndr_f=="férfi")

# f néven mentsünk el egy adatbázist, ami csak a nőket és stfeco változón nem hiányzó értékekkel rendelkező eseteit tartalmazza

f<-hun %>% filter(!is.na(stfeco)&gndr_f=="nő")

## ----------------------------------------------------------------------------------------------------
# hívjuk be a DescTools package-t
library(DescTools)

# ellenőrizzük az m és az f adattáblán is az stfeco változó súlyozott alakzati mutatóit

Kurt(m$stfeco, m$pspwght)
Skew(m$stfeco, m$pspwght)
Kurt(f$stfeco, f$pspwght)
Skew(f$stfeco, f$pspwght)


## ----------------------------------------------------------------------------------------------------
# vizsgáljuk meg független mintás t-teszttel a nők és férfiak gazdasággal való elégedettségének eltéréseit

weighted_ttest(stfeco~gndr_f+pspwght, hun)


## ----------------------------------------------------------------------------------------------------
# számoljuk ki a két csoport átlagértéke közötti különbség nagyságát, az r-t

sqrt(0.44^2/(0.44^2+1502))


## ----------------------------------------------------------------------------
# telepítés után töltsük be az effectsize package-et

library (effectsize)

# kérjük le az elvégzett független mintás t-teszthez tartozó r-értéket

t_to_r(0.44, 1502)


## ----------------------------------------------------------------------------------------------------
# boxplot ábrával vessük össze a két csoport gazdasággal való elégedettségét

hun %>% ggplot(aes(x=gndr_f, y = stfeco, weight=pspwght)) + 
  geom_boxplot()+
  stat_summary(fun.data="mean_cl_normal", geom="errorbar", 
               width=0.1, color="red")



## ----------------------------------------------------------------------------------------------------
# vizsgáljuk meg súlyozott Mann-Whitney teszttel a nők és férfiak gazdasággal való elégedettségének eltéréseit

weighted_mannwhitney(stfeco~gndr_f+pspwght, hun)


## ----------------------------------------------------------------------------------------------------
# számoljuk ki a két csoport átlagértéke közötti különbség nagyságát, az r-t

0.40/sqrt(1615)


## ----------------------------------------------------------------------------------------------------
# kérjük le az elvégzett Mann-Whitney teszthez tartozó r-értéket

z_to_r(0.40,1615)


## ----------------------------------------------------------------------------------------------------
# vizsgáljuk meg súlyozatlan Mann-Whitney teszttel a nők és férfiak gazdasággal való elégedettségének eltéréseit

mwu(hun, stfeco, gndr_f)


## ----------------------------------------------------------------------------------------------------
# hozzunk létre  diff néven egy objektumot, ami minden esetnél az stfgov és az stfeco változó különbségét rögzíti

diff<-hun$stfgov-hun$stfeco


## -------------------------------------------------------------------------------------------
# ellenőrizzük a diff objektum eloszlását a qplot () funkción keresztül elérhető Kerlel-sűrűség ábrával

qplot(diff, geom="density", adjust=3)


## ----------------------------------------------------------------------------------------------------
# ellenőrizzük a diff objektum alakzati mutatóit

descr(diff)


## ----------------------------------------------------------------------------------------------------
# súlyozott páros t-teszttel ellenőrizzük, hogy van-e különbség az stfeco és az stfgov átlagai között

weighted_ttest(hun, stfeco, stfgov, weight=pspwght, paired=T)


## ----------------------------------------------------------------------------------------------------
# az stfeco és stfgov átlagértékeinek eléréséhez futtassuk újra a weighted_ttest () funkciót a paired=T argumentum nélkül

weighted_ttest(hun, stfeco, stfgov, weight=pspwght)


## ----------------------------------------------------------------------------------------------------
# számoljuk ki a két csoport átlagértéke közötti különbség nagyságát, az r-t

sqrt(6.36^2/(6.36^2+1571))
t_to_r(6.36, 1571)


## ----------------------------------------------------------------------------------------------------
# futtassuk újra súlyozatlan formában is a páros t-tesztet

weighted_ttest(hun, stfeco, stfgov, weight=NULL, paired=T)


## ----------------------------------------------------------------------------------------------------
# Wilcoxon-teszttel ellenőrizzük az stfeco és stfgov átlagértékei közötti különbséget 

wilcox.test(hun$stfeco, hun$stfgov, paired=T, correct=F)


## ----------------------------------------------------------------------------------------------------
# számoljuk ki a Wilcoxon-teszthez tartozó z-értéket

qnorm(0.0000000003208/2)


## ----------------------------------------------------------------------------------------------------
# számoljuk ki a két csoport átlagértéke közötti különbség nagyságát, az r-t

-6.288378/sqrt(1572)

z_to_r(-6.288378, 1572)


## ----------------------------------------------------------------------------------------------------
# ábrázoljuk kernel-féle sűrűségdiagrammal az stfeco és az stfgov változók eloszlását 

ggplot(hun, aes(stfeco, weight=pspwght))+
  geom_density(aes(color="gazdaság"),adjust=2)+
  geom_density(aes(stfgov,color="kormány"), adjust=2)


## ----------------------------------------------------------------------------------------------------
# a boxplot-ábrához alakítsuk át az stfeco és stfgov változókat long-formátumba 

long<-hun %>% 
  gather(stfeco, stfgov,
         key="elegedettseg", value="val")



## -------------------------------------------------------------------------------------------
# ábrázoljuk boxplottal az stfeco és az stfgov változók eloszlását a két változó átlaga és annak konfidenciaintervalluma megjelenítésével

ggplot(long, aes(x=elegedettseg, y = val, fill=elegedettseg, weight=pspwght)) + 
  geom_boxplot()+
  stat_summary(fun.data="mean_cl_normal", geom="errorbar", 
               width=0.1, color="black")+
  theme(legend.position="none")


## ----------------------------------------------------------------------------
# töltsük be a varianciaelemzéshez szükséges package-eket

library(tidyverse)
library(summarytools)
library (sjstats)
library (misty)
library(rstatix)
library (DescTools)


## --------------------------------------------------------------------------------------------
## # a `freq ()` funkció segítségével nézzük meg a csoportjaink nagyságát
## 
freq(hun$party_f, weights = hun$pspwght)


## -------------------------------------------------------------------------------------------
# a normális eloszlás feltételének teszteléséhez ábrázoljuk kernelsűrűség-ábrával az stfeco eloszlását a party_f mindhárom kategóriájára

hun %>% filter(!is.na(party_f)) %>% 
  ggplot(aes(stfeco, weight=pspwght)) +
  geom_density(aes(color=party_f), adjust=2)


## ----------------------------------------------------------------------------------------------------
# ellenőrizzük a varianca homogenitásának feltételét a Levene-teszt segítségével

LeveneTest(stfeco~party_f, hun)


## ----------------------------------------------------------------------------------------------------
# végezzük el a varianciaelemzést az stfeco és a party_f változó kapcsolatára

means_by_group(hun, stfeco, party_f, weights=pspwght)

#`a` objektumnéven mentsük el a varianciaelemzés eredményét az aov () funkció segítségével

a<-aov(stfeco~party_f, hun, weight=pspwght)


## ----------------------------------------------------------------------------------------------------
# az aov () funkció a objektumban mentett eredményeit tekintsük át a summary () funkcióval 

summary (a)


## ----------------------------------------------------------------------------------------------------
# vessük össze egymással az egyes csoportátlagokat a Bonferroni post-hoc teszt segítségével 

PostHocTest(a, method="bonferroni")


## ----------------------------------------------------------------------------------------------------
# ellenőrizzük az stfeco és a party_f változók kapcsolatát Welch-teszttel is

test.welch(stfeco~party_f, hun, posthoc = F)


## ----------------------------------------------------------------------------------------------------
# ellenőrizzük az stfeco és a party_f változók kapcsolatát Kruskal-Wallis teszttel is

weighted_mannwhitney(stfeco~party_f+pspwght, hun)


## ----------------------------------------------------------------------------------------------------
# ellenőrizzük az stfeco és a party_f változók kapcsolatát súlyozatlan Kruskal-Wallis teszttel is

kruskal.test(stfeco~party_f, hun)


## ----------------------------------------------------------------------------------------------------
# nézzük meg a súlyozatlan Kruskal-Wallis teszthez tartozó hatásnagyságot

kruskal_effsize(hun, stfeco~party_f)


## ----------------------------------------------------------------------------------------------------
# vessük össze egymással az egyes csoportátlagokat a Dunn post-hoc teszt segítségével 

DunnTest(stfeco~party_f, hun, method="bonferroni")


## -------------------------------------------------------------------------------------------
# ábrázoljuk a csoportok közötti különbségeket boxplot segítségével

hun %>% filter(!is.na(party_f)) %>% 
  ggplot(aes(x=party_f, y = stfeco, weight=pspwght)) + 
  geom_boxplot()+
  stat_summary(fun.data="mean_cl_normal", geom="errorbar", 
               width=0.1, color="red")

