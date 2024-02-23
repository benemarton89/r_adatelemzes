### ADATBÁZIS-MENEDZSMENT ###



## -------------------------------------------------------------------------------------------
## # telepítsük a tidyverse package-gyűjteményt
## install.packages("tidyverse")


## ----------------------------------------------------------------------------------------------------
# hívjuk be a tidyverse package-t

library("tidyverse")


## ----e----------------------------------------------------------------------------------------
## # nézzük meg a stats package filter () funkciójának súgóját
## 
?stats::filter


## ----------------------------------------------------------------------------------------------------
# hozzunk létre egy számsort a rep () funkció segítségével, ahol az 1 és 4 közötti értékek ismétlődnek négyszer, majd ennek a számsornak az átlagát nézzük meg

mean(rep(1:4,4))


## ----------------------------------------------------------------------------------------------------
# hozzunk létre egy számsort, ami hatszor tartalmazza rep () funkcióval létrehozott számsor átlagát

rep(mean(rep(1:4,4)),6)


## ----------------------------------------------------------------------------------------------------
# első lépésben x néven hozzunk létre egy objektumot a számsorunk rögzítésére a rep () funkció segítségével

x<-rep(1:4, 4)

# második lépésben z objektumban mentsük x számsor átlagát

z<-mean(x)

# harmadik lépésben z objektum értékét hatszor ismételjük meg a rep () funkció segítségével.
rep(z, 6)


## ----------------------------------------------------------------------------------------------------
# hozzunk létre egy számsort a pipe segítségével, ahol az 1-4 értéksor négyszer ismétlődik.

1:4 %>% rep(4)


## ----------------------------------------------------------------------------------------------------
# kérjük le a négyszer ismételt 1-4 sorozat átlagát a pipe segítségével

1:4 %>% rep(4) %>% mean()


## ----------------------------------------------------------------------------------------------------
# ismételjük meg hatszor a négyszer ismételt 1-4 értéksorozat átlagát pipe segítségével

1:4 %>% rep(4) %>% mean()%>% rep(6)


## ----------------------------------------------------------------------------------------------------
# tegyük áttekinthetőbbé az előző parancsot oly módon, hogy a művelet minden szeletét új sorba helyezzük

1:4 %>%
  rep(4) %>% 
  mean()%>% 
  rep(6) 


## ----------------------------------------------------------------------------------------------------
# korlátozzuk a számok normálalakos kifejezését

options(scipen=999)



## -------------------------------------------------------------------------------------------
## # töltsük be az ESS9e03_2.csv fájlt

ess<-read_csv("ESS9e03_2.csv")


## ----------------------------------------------------------------------------------------------------
# ess.RData néven mentsük el az ess adattáblát

save(ess, file="ess.RData")


## ----------------------------------------------------------------------------------------------------
# az ess.RData fájl behívása előtt töröljük az aktuálisan a Workspace-en lévő ess objektumot

rm(ess)

## ----------------------------------------------------------------------------------------------------
# ezt követően betölthetjük az RData-fájlt
load("ess.RData")


## ---------------------------------------------------------------------------------------------
## # töltsük be a munkakönyvtárban található datasets.RData fájlt
load("datasets.RData")


## ----F------------------------------------------------------------------------------------------
## # írjuk ki az ess adattáblát pontosvesszős elválasztást alkalmazó MS Excel-kompatibilis csv-fájlba ess_export néven a write_excel_csv2 () funkció segítségével
## 
write_excel_csv2(ess, "ess_export.csv")


## ------------------------------------------------------------------------------
## # telepítsük, majd töltsük be a janitor package-t
## 
## install.packages(janitor)
library (janitor)


## ----include=F---------------------------------------------------------------------------------------
library(janitor)



## ---------------------------------------------------------------------------------------------
## # töltsük be az ads.csv adatbázist és mentsük el ads objektumnéven
## 
ads<-read_csv2("datasets/ads.csv")


## ---------------------------------------------------------------------------------------
# végezzük el ezt az adatbázis tisztítást az `ads` adatbázison, oly módon, hogy a biztonság kedvéért ads2 néven mentsük el az új objektumot.

ads2<-ads %>% 
  remove_empty()


## ---------------------------------------------------------------------------------------
# adjuk hozzá az előző parancssorhoz pipe-pal a duplikátumok eltávolítására szolgáló distinct () funkciót

ads2<-ads %>% 
  remove_empty()%>% 
  distinct() 


## ----------------------------------------------------------------------------------------------------
# szűrjünk ki minden ismétlődő id értéket az fb adatbázisból és ezt mentsük fb2 néven

fb2<- fb %>% 
  distinct(id)


## ----------------------------------------------------------------------------------------------------
# távolítsuk el az első sort az ads2 adatbázisból, és ads3 néven mentsük az adattáblát

ads3<-ads2[-1,]


## ----------------------------------------------------------------------------------
# a korábbi adattisztító parancsot folytassuk az első sort változónévvé alakító funkcióval

ads2<-ads %>% 
  remove_empty()%>% 
  distinct() %>% 
  row_to_names(1) 


## ----------------------------------------------------------------------------------------
# a korábbi adattisztító parancsot folytassuk az változóneveket javító funkcióval

ads2<-ads %>% 
  remove_empty()%>% 
  distinct() %>% 
  row_to_names(1) %>% 
  clean_names () 


## -------------------------------------------------------------------------------------
# az adattisztító adatsor pipe nélkül így nézne ki

ads2<-clean_names(row_to_names(
  distinct(remove_empty(ads)),1)) 


## -------------------------------------------------------------------------------------------
# a tisztított adatbázissal írjuk felül az ads adattáblát, azaz ads néven mentsük el a tisztítás eredményét

ads<-ads %>% 
  remove_empty()%>% 
  distinct() %>% 
  row_to_names(1) %>% 
  clean_names ()

# töröljük az ads adattáblát, ami így már azonos az ads adattáblával

rm(ads2)


## ----------------------------------------------------------------------------------------------------
# nevezzük át a közzétett politikai hirdetés szövegét tartalmazó creative_link_body változót "text" névre, a hirdetés linkjét tartalmazó ad_snapshot_url változót pedig "link"-re

ads<-ads %>% 
  rename(text=creative_link_description,
         link=ad_snapshot_url)


## ----------------------------------------------------------------------------------------------------
# listázzuk ki az adatbázis összes változójának nevét

colnames(ads)


## ----------------------------------------------------------------------------------------------------
# listázzuk ki az adatbázis 8., 9., 10. és 11 . változójának nevét
colnames(ads)[8:11]


## ----------------------------------------------------------------------------------------------------
# indexálással változtassuk meg az ads adattábla 8., 9., 10. és 11. oszlopainak változóneveit "title", "capt", "source" és "text" nevekre

colnames(ads)[8:11]<-c("title", "capt",
                       "source","text")


## ----------------------------------------------------------------------------------------------------
# változtassuk meg az ads adattábla összes változónevét az alábbiakra

colnames(ads)<-c("creat","sp_lower",
                "impr_lower","start","sp_upper",
                "impr_upper", "stop", "title",
                "capt", "source", "text",
                "link", "id", "name",
                "curr","funder")


## ----------------------------------------------------------------------------------------------------
# nézzük meg először a cntry változó tartalmát a table () funkció segítségével - ne feledjük, hogy a változónkat az adattáblával együtt tudjuk meghatározni a $ operátor használatával.

table(ess$cntry)


## ----------------------------------------------------------------------------------------------------
# hun néven mentsük el azt az adattáblát, ami az ess adattáblából csak a magyar, tehát a cntry változón "HU" értékkel rendelkező válaszadókat tartalmazza

hun<-ess %>% 
  filter (cntry== "HU")


## ----------------------------------------------------------------------------------------------------
# hun_60 néven mentsük el azt az adatbázist, ami az ess válaszadói közül csak a közéletről legalább napi 60 percet tájékozódó magyar válaszadókat tartalmazza

hun_60<-ess %>% 
  filter (cntry== "HU" & nwspol>=60)


## ----------------------------------------------------------------------------------------------------
# hun_pl néven mentsük el azt az adatbázist, ami az ess válaszadói közül csak magyar és lengyel válaszadókat tartalmazza

hun_pl<-ess %>% 
  filter (cntry== "HU" | cntry=="PL")


## ----------------------------------------------------------------------------------------------------
# pc néven mentük el azt az adatbázist, ami az ess válaszadói közül csak a posztszocialista európai országból származó válaszadókat tartalmazza

pc<-ess %>% 
  filter (cntry%in%c("HU", "PL", "BG",
                     "CZ","SK", "SI",
                     "EE","LV","LT" ))


## ----------------------------------------------------------------------------------------------------

# old_eu néven mentsük el azt az adatbázist, ami az ess válaszadói közül nem tartalmazza azokat, akik posztszocialista európai országból származnak

old_eu<-ess %>% 
  filter (!cntry%in%c("HU", "PL", "BG",
                      "CZ","SK","SI",
                      "EE","LV","LT" ))


## ----------------------------------------------------------------------------------------------------
# ess_1000 néven mentsük el az ess adatbázis első ezer sorát

ess_1000<-ess %>% slice(1:1000)


## ----------------------------------------------------------------------------------------------------
# egy szűkített, trust elnevezésű adattáblába mentsük el szükséges változókat

trust<-ess %>% 
  select(trstprl, trstlgl, trstplc, trstplt,
         trstprt, trstep, trstun, vote,
         cntry,agea)


## ----------------------------------------------------------------------------------------------------
# egy szűkített, trust elnevezésű adattáblába mentsük el szükséges változókat, a bizalmi változóknál azok pozícióját adjuk meg

trust<-ess %>% 
  select(18:24, vote, cntry, agea)


## ----------------------------------------------------------------------------------------------------
# egy szűkített, trust elnevezésű adattáblába mentsük el szükséges változókat, de a bizalmi változók sorozatát a változónevekkel jelöljük ki

trust<-ess %>% 
  select(trstprl:trstun, vote, cntry, agea)


## ----------------------------------------------------------------------------------------------------
# egy szűkített, trust elnevezésű adattáblába mentsük el szükséges változókat úgy, hogy bizalmi változókat a változónév közös része alapján azonosítjuk 

trust<-ess %>% 
  select(starts_with("trst"), 
         vote, cntry, agea)


## ----------------------------------------------------------------------------------------------------
# egy szűkített, trust2 elnevezésű adattáblába mentsük el az összes trst-vel és ppl-lel kezdődő változót

trust2<-ess %>% 
  select(starts_with(c("trst", "ppl")))


## ----------------------------------------------------------------------------------------------------
# egy szűkített, trust2 elnevezésű adattáblába mentsük el az összes trst-vel és ppl-lel kezdődő változót, de távolítsuk el a ppldsrv változót

trust2<-ess %>% 
  select(starts_with(c("trst", "ppl")),
         -"ppldsrv")


## ----------------------------------------------------------------------------------------------------
 # hun_trust néven mentsünk egy olyan adattáblát, ami csak 300 magyar válaszadó emberi és politikai bizalmi adatait tartalmazza


hun_trust<-ess %>% 
  filter(cntry=="HU") %>% 
  select(starts_with(c("trst", "ppl")),
         -ppldsrv) %>% 
  slice(1:300)


## ----------------------------------------------------------------------------------------------------
# állítsuk sorrendbe a fb adatbázis sorait a num_reactions változó alapján

fb<- fb %>% 
  arrange(num_reactions)


## ----------------------------------------------------------------------------------------------------
# állítsuk csökkenő sorrendbe a fb adatbázis sorait a num_reactions változó alapján

fb<- fb %>% 
  arrange(desc(num_reactions))


## ----------------------------------------------------------------------------------------------------
# top_react néven mentsük el az 1000 legtöbb reakciót kiváltó posztot a fb adattáblából

top_react<- fb %>% 
  arrange(desc(num_reactions)) %>%
  slice(1:1000)


## ----------------------------------------------------------------------------------------------------
# rendezzük át a fb adattábla sorait előbb a name, majd a num_reactions változó (utóbbinál csökkenő sorrendben) alapján

fb<-fb %>% arrange(name, desc(num_reactions)) 


## ----------------------------------------------------------------------------------------------------
# rendezzük át a fb adattábla sorait előbb az EVK_megye, aztan a name, majd a num_reactions változó (utóbbinál csökkenő sorrendben) alapján 
fb<-fb %>% arrange(EVK_megye, name, desc(num_reactions)) 


## ----------------------------------------------------------------------------------------------------
# kérjük le a fb változó oszlopneveit

colnames(fb) 


## ----------------------------------------------------------------------------------------------------
# fb1 néven mentsük el a pozíciós meghatározással átrendezett fb adattáblát

fb1<-fb %>% 
  relocate(2, 26, 11:20) 


## ----------------------------------------------------------------------------------------------------
# # fb1 néven mentsük el a névmeghatározással átrendezett fb adattáblát


fb1<-fb %>% 
  relocate(id, name, 
           num_reactions:num_special) 


## ----------------------------------------------------------------------------------------------------
# vegyítsük a pozíció- és névmeghatározós módszert a fb adattábla átrendezésénél

fb1<-fb %>%
  relocate(2, name, 11:20)


## ----------------------------------------------------------------------------------------------------
# helyezzük a name és a party változót egymás mellé az adattábla első változói közé, viszont nem a tábla legelejére, hanem  a pageid után, a statusid elé.

fb1<-fb %>% 
  relocate(name,party, .after=pageid)

# vagy
fb1<-fb %>% 
  relocate(name,party, .before=pageid)



## ----------------------------------------------------------------------------------------------------
# nézzük meg a típusát a fb adattábla következő változóinak: num_reactions (a poszt reakcióinak a száma), party (a posztot közzétevő politikus pártja), reactmean (az adott politikus által kiváltott reakciók átlagos száma)

class(fb$num_reactions)
class(fb$party)
class(fb$reacmean)


## ----------------------------------------------------------------------------------------------------
# egy új, party_f névre keresztelt változóként hozzuk létre a párthovatartozás faktorváltozóját

fb$party_f<-as_factor(fb$party)


## ----------------------------------------------------------------------------------------------------
# ellenőrizzük le a party_f változó típusát

class (fb$party_f)


## ----------------------------------------------------------------------------------------------------
# nézzük meg a party és party_f változó kategóriáit

table(fb$party)
table(fb$party_f)


## ----------------------------------------------------------------------------------------------------
# a party_f változó értékssorendje a pártok választási eredményét kövesse

fb$party_f<-fb$party_f %>%
  fct_relevel("Fidesz-KDNP", "Jobbik",
              "MSZP-Párbeszéd", "LMP", 
              "DK", "Momentum","MKKP",
              "Együtt", "Független")

## ----------------------------------------------------------------------------------------------------
# ellenőrizzük az új sorrendet a table () funkcióval

table(fb$party_f)


## ----------------------------------------------------------------------------------------------------
# alakítsuk át úgy a party_f változó értéksorrendjét, hogy a független képviselők kategóriája legyen az első kategória, a többi párt pedig maradjon a korábban definiált sorrendben

fb$party_f<-fb$party_f %>%
  fct_relevel("Független")


## ----------------------------------------------------------------------------------------------------
# helyezzük a Fidesz-KDNP-t és a Jobbikot a sorrend végére helyezzük - az after után a 9-es számot írhatjuk, hiszen összesen 9 szintből áll a változónk

fb$party_f<-fb$party_f %>% 
  fct_relevel("Fidesz-KDNP","Jobbik", after=9)


## ----------------------------------------------------------------------------------------------------
# a pártok kategóriáit az elemszám alapján rendezzük sorba

fb$party_f<-fb$party_f %>% 
  fct_infreq()


## ----------------------------------------------------------------------------------------------------
# a pártok kategóriáit az elemszám alapján rendezzük sorba, de növekvő sorrendben

fb$party_f<-fb$party_f %>% 
  fct_infreq() %>% 
  fct_rev()


## ----------------------------------------------------------------------------------------------------
# rendezzük csökkenő sorrendbe a party_f változó kategóriáit a pártok által kiváltott reakciók száma alapján

fb$party_f<-fb$party_f %>% 
  fct_reorder(fb$num_reactions, sum,.desc=T)


## ----------------------------------------------------------------------------------------------------
#party_f2 néven hozzunk létre egy olyan változót, ami csak a 3 legtöbb posztot közzétevő pártot tartalmazza, a többi pártot pedig Egyéb néven vonjuk össze

fb$party_f2<-fb$party_f %>% 
  fct_lump(3, other_level="Egyéb")


## ----------------------------------------------------------------------------------------------------
# hozzunk létre egy party_f2 változót, ahol a party_f változó kategóriái közül csak a legalább 500 posztot tartalmazó kategóriák maradjanak meg, a többi kerüljön összevonásra

fb$party_f2<-fb$party_f %>% 
  fct_lump_min(500, other_level="Egyéb")

# hozzunk létre egy party_f2 változót, ahol a party_f változó kategóriái közül csak a posztok legalább 5%-át tartalmazó kategóriák maradjanak meg, a többi kerüljön összevonásra

fb$party_f2<-fb$party_f %>% 
  fct_lump_prop(0.05, other_level="Egyéb")


## ----------------------------------------------------------------------------------------------------
#party_f2 néven mentsük el a party_f változónak azt a változatát, ahol a Fidesz-KDNP és az MSZP-Párbeszéd kategóriája a rövidebb Fidesz, illetve MSZP néven szerepel

fb$party_f2<-fb$party_f %>% 
  fct_recode(Fidesz="Fidesz-KDNP",
             MSZP="MSZP-Párbeszéd")


## ----------------------------------------------------------------------------------------------------
# alakítsuk át a polintr változót faktorváltozóvá és mentsük el azt polintr_f néven

ess$polintr_f<- ess$polintr %>% 
  factor(labels = c("Nagyon érdekli", 
                    "Eléggé érdekli", 
                    "Alig érdekli", 
                    "Egyáltalán nem érdekli",
                    "Nem tudja",
                    "Nem válaszol", 
                    "Nem alkalmazható"))


## ----------------------------------------------------------------------------------------------------
# alakítsuk vissza a polintr_f változót numerikussá és mentsük polintr_n néven.

ess$polintr_n<-as.numeric(ess$polintr_f)


## ----------------------------------------------------------------------------------------------------
# jelenítsük meg indexálás segítségével a trstep változó 123. értékét

ess$trstep[123] 


## ----------------------------------------------------------------------------------------------------
# változtassuk a trstep változó értékét 4-es értékre

ess$trstep[123]<-4 


## ----------------------------------------------------------------------------------------------------
# változtassuk a "VL" kategória címkéjét "visszalépett"-re

fb$EVK_megye<-fb$EVK_megye %>% 
  replace(fb$EVK_megye=="VL", "visszalépett")


## ----------------------------------------------------------------------------------------------------
# missingeljük a polintr változó 7, 8 és 9 értékeit

ess$polintr<-ess$polintr %>% 
  replace(ess$polintr%in%c(7,8,9), NA)


## ----------------------------------------------------------------------------------------------------
# missingeljük a netustm változó 960-nál magasabb értékeit

ess$netustm<-ess$netustm %>% 
  replace(ess$netustm>960, NA)


## ----------------------------------------------------------------------------------------------------
# hozzuk létre a kor változót és helyezzük közvetlenül a year változó mögé

fb<-fb %>% 
  mutate(kor=2018-year, .after=year)


## ----------------------------------------------------------------------------------------------------
# hozzuk létre az eng_rate változót a num_reactions és a FB_follow_end változók hányadosából és helyezzük közvetlenül a num_reactions változó mögé

fb<-fb %>% 
 mutate(eng_rate=num_reactions/FB_follow_end,
        .after=num_reactions)


## ----------------------------------------------------------------------------------------------------
# hozzuk létre a kor és az eng_rate változót egy parancson belül, és mindkettőt helyezzük közvetlenül a year változó mögé

fb<-fb %>% 
  mutate(kor=2018-year, 
         eng_rate=num_reactions/FB_follow_end, .after=year)


## ----------------------------------------------------------------------------------------------------
# hozzuk létre a num_reactions változó egyes politikusokhoz tartozó átlagát mérő react_mean, a szórását mérő react_sd és a politikus posztjainak számát mérő post_num változókat 

fb<-fb %>% group_by(name) %>% 
  mutate(react_mean=mean(num_reactions),
         react_sd=sd(num_reactions),
         post_num=n())


## ----------------------------------------------------------------------------------------------------
# hozzuk létre a react_diff változót, ami azt mutatja, hogy a poszt reakciószáma hány szórásnyival tér el a közzétevő politikus átlagos reakciószámától

fb<-fb %>% 
  mutate(react_diff=
           (num_reactions-react_mean)/react_sd)


## ----------------------------------------------------------------------------------------------------
# hozzuk létre az eng_rate változó 4-kategóriás változatát és helyezzük az eng_rate változó mögé

fb<-fb %>% 
  mutate(eng_rate_cat=
           case_when(
             eng_rate<0.01 ~ "kevesebb mint 1%",
             eng_rate>=0.01 & eng_rate<0.05 ~"1% és 4,99% között",
             eng_rate>=0.05 & eng_rate<=1 ~ "5%-100% között",
             eng_rate>1 ~ "100% felett"), .after=eng_rate)


## ----------------------------------------------------------------------------------------------------
# ellenőrizzük a table () funkcióval az eng_rate_cat változót

table (fb$eng_rate_cat)


## ----------------------------------------------------------------------------------------------------
# hozzuk létre a success változót, ami a kormánypárti és ellenzéki sikeres és sikertelen posztokat különbözteti meg egymástól 

fb<-fb %>% 
  mutate(success=
           case_when(
             (eng_rate_cat=="5%-100% között" | 
                eng_rate_cat=="100% felett") & 
                party=="Fidesz-KDNP" ~ "kormánypárti sikeres",
              (eng_rate_cat=="5%-100% között" |
                eng_rate_cat=="100% felett") & 
                party!="Fidesz-KDNP" ~ "ellenzéki sikeres", 
              (eng_rate_cat=="kevesebb mint 1%" |
                eng_rate_cat=="1% és 4.99% között") & 
                party=="Fidesz-KDNP" ~ "kormánypárti sikertelen",
              (eng_rate_cat=="kevesebb mint 1%" |
                eng_rate_cat=="1% és 4.99% között") & 
                party!="Fidesz-KDNP" ~ "ellenzéki sikertelen"))


## ----------------------------------------------------------------------------------------------------
# kódoljuk át a polintr változót úgy, hogy a magasabb érték magasabb politikai érdeklődési szintet jelöljön

ess<-ess %>% 
  mutate(polintr=
           case_when(
             polintr==1 ~ 4,
             polintr==2 ~ 3,
             polintr==3 ~ 2,
             polintr==4 ~ 1))


## ----------------------------------------------------------------------------------------------------
# hozzuk létre a fb adatbázis jelöltek szintjén aggregált változatát, ami a jelöltek szintjén a reakciók összegét, átlagát, szórását, közzétett posztok számát és a pártot tartalmazza változóként

fb_agg<-fb %>% group_by(name) %>% 
  summarise(react_sum=sum(num_reactions),
            react_mean=mean(num_reactions),
            react_sd=sd(num_reactions),
            num_posts=n(),
            party=first(party))


## -------------------------------------------------------------------------------------------
# fb_agg2 néven a pártok és a megyék szintjén aggregáljuk az előbbi változók mentén a fb adattáblát

fb_agg2<-fb %>% group_by(party, EVK_megye) %>% 
  summarise(react_sum=sum(num_reactions),
            react_mean=mean(num_reactions),
            react_sd=sd(num_reactions),
            num_posts=n())


## ----------------------------------------------------------------------------------------------------
# ess_trust néven mentsük el a politikai bizalmi változók országonkénti átlagait mutató, országok szintjén aggregált adattáblát 

ess_trust<-ess %>% 
  group_by(cntry) %>% 
  summarise_at(vars(trstprl:trstun), mean)


## -----------------------------------------------------------------------------------------
# az előbb létrehozott ess_trust aggregált adatbázisban a politikai bizalom egyes dimenzióinak országonkénti átlaga mellett azok szórása is szerepeljen

ess_trust<-ess %>% 
  group_by(cntry) %>% 
  summarise_at(vars(trstprl:trstun), funs(mean=mean, sd=sd))


## ----------------------------------------------------------------------------------------------------
# bontsuk ketté a slice () funkció segítségével a fb adattáblát úgy, hogy a fb_a adatbázis az első 15000 esetet, míg a fb_b adattábla a további 23030 esetet tartalmazza

fb_a<-fb %>% slice(1:15000)
fb_b<-fb %>% slice(15001:38030)


## ----------------------------------------------------------------------------------------------------
# fb_all néven mentsük el a fb_a és fb_b adatbázisok összekapcsolásának eredményét

fb_all<-bind_rows(fb_a, fb_b)


## ----------------------------------------------------------------------------------------------------
# az összefűzött fb_all adatbázisban a coder változó rögzítse az adatok forrását: a fb_a adattáblát a „fd”, míg a fb_b adattáblát a „ts” monogrammal rendelkező kódoló rögzítette, és a változóban az ő monogramjuk jelenjen meg

fb_all<-bind_rows(fd=fb_a, ts=fb_b, .id="coder")


## ----------------------------------------------------------------------------------------------------
# a fb_a adattábla pageid változóját nevezzük át pageeid-re

fb_a <- fb_a %>%  rename(pageeid=pageid)


## ----------------------------------------------------------------------------------------------------
# kapcsoljuk össze a két adattáblát, oly módon, hogy az adatok forrását a coder elnevezésű változóban rögzítjük sorszámmal

fb_all<-bind_rows(fb_a, fb_b, .id="coder")


## -------------------------------------------------------------------------------------------
# a fb adattáblát az oszlopok mentén bontsuk ketté a már ismert select () funkció segítségével, úgy hogy a `fb_c` adattábla az első 30 változót, a fb_d adattábla pedig a maradék 47 változót tartalmazza

fb_c<-fb %>% select(1:30)
fb_d<-fb %>% select(31:77)


## -------------------------------------------------------------------------------------------
# kapcsoljuk össze a fb_c és fb_d adattáblákat, és mentsük el fb_all néven

fb_all<-bind_cols(fb_c, fb_d)


## ----------------------------------------------------------------------------------------------
## # töltsük be a country_eco.csv adattáblát az R-be és mentsük el country_eco néven

country_eco<-read_csv2("datasets/country_eco.csv")


## ----------------------------------------------------------------------------------------------------
# a table () funkció segítségével megnézhetjük, hogy milyen országok szerepelnek az ess, cntry és a country_eco country változóinak értékszettjében

table(ess$cntry)
table(country_eco$country)


## ----------------------------------------------------------------------------------------------------
# ess2 néven mentsük el az ess és a country_eco adattáblák összekapcsolt változatát

ess2<-left_join(ess, country_eco, by=c("cntry" = "country"))

