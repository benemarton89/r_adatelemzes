# munkakönyvtár megadása
setwd("SAJÁT MAPPA")

# ess-fájl behívása
load("ess.RData")

# tidyverse package behívása
library(tidyverse)

# csak magyar adatok az ess-ből
hun<-ess %>% filter(cntry=="HU")

# meglévő változók átkódolása, kategóriák hozzáadása
hun<-hun %>% mutate(dom=
                      case_when(domicil == 1 ~ "nagyváros",
                                domicil == 2 ~ "nagyváros",
                                domicil == 3 ~ "város",
                                domicil == 4 ~ "falu",
                                domicil == 5 ~ "falu"),
                    polintr_f=case_when(polintr==1 ~ "nagyon",
                                        polintr==2 ~ "eléggé",
                                        polintr==3 ~ "alig",
                                        polintr==4 ~ "egyáltalán nem"),
                    gndr_f=case_when(gndr==1 ~ "férfi",
                                     gndr==2 ~ "nő"),
                    health_f=case_when(health==1 ~ "nagyon jó",
                                       health==2 ~ "jó",
                                       health==3 ~ "kielégítő",
                                       health==4 ~ "rossz", 
                                       health==5 ~ "nagyon rossz"),
                    party_f=case_when(prtvtfhu==3|prtvtfhu==5~ "kormánypárti",
                                      prtvtfhu%in%c(1:2,4,6:11, 55)~ "ellenzéki",
                                      prtvtfhu==66 ~ "nem szavazó"))

# a létrehozott változók faktorváltozóvá alakítása
hun$dom<-factor(hun$dom, levels=c("falu", "város", "nagyváros"))
hun$polintr_f<-factor(hun$polintr_f, levels=c("egyáltalán nem", "alig", 
                                              "eléggé", "nagyon"))
hun$gndr_f<-factor(hun$gndr_f)
hun$health_f<-factor(hun$health_f, levels=c("nagyon rossz", "rossz", "kielégítő", 
                                            "jó", "nagyon jó"))
hun$party_f<-factor(hun$party_f, levels=c("kormánypárti", "ellenzéki", 
                                          "nem szavazó"))


# egégszségüggyel való elégedettség változó numerikus változatának elkészítése
hun$health_n<-as.numeric(hun$health_f)

# használt változók missingelése
hun$stfeco<-hun$stfeco %>% 
  replace(hun$stfeco%in%c(77,88), NA) 
hun$stfgov<-hun$stfgov %>% 
  replace(hun$stfgov%in%c(77,88), NA)
hun$stfedu<-hun$stfedu %>% 
  replace(hun$stfedu%in%c(77,88), NA) 
hun$stfhlth<-hun$stfhlth %>% 
  replace(hun$stfhlth%in%c(77,88), NA)
hun$happy<-hun$happy %>% 
  replace(hun$happy%in%c(77,88), NA)
hun$hinctnta<-hun$hinctnta %>% 
  replace(hun$hinctnta%in%c(77,88), NA)
hun$ppltrst<-hun$ppltrst %>% 
  replace(hun$ppltrst%in%c(77,88), NA) 
hun$trstprl<-hun$trstprl %>% 
  replace(hun$trstprl%in%c(77,88), NA) 
hun$trstlgl<-hun$trstlgl %>% 
  replace(hun$trstlgl%in%c(77,88), NA) 
hun$trstplc<-hun$trstplc %>% 
  replace(hun$trstplc%in%c(77,88), NA) 
hun$trstplt<-hun$trstplt %>% 
  replace(hun$trstplt%in%c(77,88), NA)
hun$trstprt<-hun$trstprt %>% 
  replace(hun$trstprt%in%c(77,88), NA)
hun$trstep<-hun$trstep %>% 
  replace(hun$trstep%in%c(77,88), NA)
hun$trstun<-hun$trstun %>% 
  replace(hun$trstun%in%c(77,88), NA) 
hun$netinum<-hun$netinum %>% 
  replace(hun$netinum>1500000, NA)
hun$eduyrs<-hun$eduyrs %>% 
  replace(hun$eduyrs%in%c(77,88), NA)
hun$nwspol<-hun$nwspol %>% 
  replace(hun$nwspol%in%c(7777,8888), NA)
hun$wkhtot<-hun$wkhtot %>% 
  replace(hun$wkhtot%in%c(666,777,888), NA)

# pártosság faktorváltozó létrehozása
hun$clsprty<-hun$clsprty %>% 
  replace(hun$clsprty%in%c(7,8), NA)
hun$clsprty<-hun$clsprty %>% as_factor() %>% fct_recode("nem pártos"='1', 
                                                        pártos="2")

## politikai részvétel faktorváltozók
hun$contplt<-hun$contplt %>% 
  replace(hun$contplt%in%c(7,8,9), NA)
hun$contplt<-hun$contplt %>% as_factor() %>% 
  fct_recode(igen='1', nem="2") %>% 
  fct_rev()
hun$wrkprty<-hun$wrkprty %>% 
  replace(hun$wrkprty%in%c(7,8,9), NA)
hun$wrkprty<-hun$wrkprty %>% as_factor() %>% 
  fct_recode(igen='1', nem="2") %>% 
  fct_rev()
hun$wrkorg<-hun$wrkorg %>% 
  replace(hun$wrkorg%in%c(7,8,9), NA)
hun$wrkorg<-hun$wrkorg %>% as_factor() %>% 
  fct_recode(igen='1', nem="2") %>% 
  fct_rev()
hun$badge<-hun$badge %>% 
  replace(hun$badge%in%c(7,8,9), NA)
hun$badge<-hun$badge %>% as_factor() %>% 
  fct_recode(igen='1', nem="2") %>% 
  fct_rev()
hun$sgnptit<-hun$sgnptit %>% 
  replace(hun$sgnptit%in%c(7,8,9), NA)
hun$sgnptit<-hun$sgnptit %>% as_factor() %>% 
  fct_recode(igen='1', nem="2") %>% 
  fct_rev()
hun$pbldmn<-hun$pbldmn %>% 
  replace(hun$pbldmn%in%c(7,8,9), NA)
hun$pbldmn<-hun$pbldmn %>% as_factor() %>% 
  fct_recode(igen='1', nem="2") %>% 
  fct_rev()
hun$bctprd<-hun$bctprd %>% 
  replace(hun$bctprd%in%c(7,8,9), NA)
hun$bctprd<-hun$bctprd %>% as_factor() %>% 
  fct_recode(igen='1', nem="2") %>% 
  fct_rev()
hun$pstplonl<-hun$pstplonl %>% 
  replace(hun$pstplonl%in%c(7,8,9), NA)
hun$pstplonl<-hun$pstplonl %>% as_factor() %>% 
  fct_recode(igen='1', nem="2") %>% 
  fct_rev()
hun$vote<-hun$vote %>% 
  replace(hun$vote%in%c(3,7,8), NA)
hun$vote<-hun$vote %>% as_factor() %>% 
  fct_recode(igen='1', nem="2") %>% 
  fct_rev()

# politikai részvétel kompozít változó (hány féle részvételi formában vett részt)
partic<-hun %>% select(vote, contplt:pstplonl)
hun$polpart<-rowSums(partic== "igen", na.rm =T)


# súlyok karakterváltozóként jelennek meg, ezért numerikussá alakítjuk
hun$anweight<-as.numeric(hun$anweight)
hun$pspwght<-as.numeric(hun$pspwght)

# jövedelem változó átnevezése
hun<-hun %>% rename (income = hinctnta)

# a hun adatbázis mentése a munkakönyvtárba
save(hun, file="hun.RData")
