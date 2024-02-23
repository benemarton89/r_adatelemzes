### AZ R ÉS AZ RSTUDIO PROGRAMFELÜLETE ###

## ----------------------------------------------------------------------------------------------------
1+1


## ----------------------------------------------------------------------------------------------------
mean(c(1,2,3,4,5))


## -------------------------------------------------------------------------------------------
men(c(1,2,3,4,5))


## -------------------------------------------------------------------------------------------
# egyszerű összeadási művelet

1+1

# átlag lekérése az 1-5 számsorra

mean(c(1,2,3,4,5))

# hibás átlag

men(c(1,2,3,4,5))



## ----------------------------------------------------------------------------------------------------
# numbers objektumnéven mentsük el az 1,3,4,6,7 számsort

numbers<-c(1,3,4,6,7)


## ----------------------------------------------------------------------------------------------------
# írjuk be a `numbers` nevet az R konzolba, és nyomjuk meg az ENTER-t

numbers


## ----------------------------------------------------------------------------------------------------
# adjuk össze a numbers objektumot és az 1-es értéket

 numbers+1


## ----------------------------------------------------------------------------------------------------
# mentsük el a numbers objektum és az 1-es érték összegét numbers2 néven 

numbers2<-numbers+1 


## ----------------------------------------------------------------------------------------------------
# töröljük a `fb` adatbázist és a `numbers2` objektumot

rm (fb, numbers2)


## ----------------------------------------------------------------------------------------------------
# ellenőrizzük le, hogy mi az aktuálisan érvényes munkakönyvtár

getwd ()


## ----------------------------------------------------------------------------------------------
## # állítsuk be a munkakönyvtárat
## 
## setwd(C:\\User\\R-Munka)


## ----------------------------------------------------------------------------------------------------
# ábrázoljuk a numbers objektumot egy boxplot ábrával

boxplot(numbers)


## ----------------------------------------------------------------------------------------------------
# ábrázoljuk a numbers objektumot oszlopdiagrammal
barplot(numbers)


## ---------------------------------------------------------------------------------------------
## # telepítsük a summarytools package-t az install.packages () funkcióval
## 
install.packages (summarytools)


## --------------------------------------------------------------------------------------------
## # aktiváljuk a summarytools package-t
## 
library(summarytools)


## ---------------------------------------------------------------------------------------------
?mean


## A KÖNYVHÖZ HASZNÁLT ADATBÁZISOK LÉTREHOZÁSA ##

# töröljük a workspace tartalmát
rm(list = ls())

# a munkakönyvtár megadása
setwd("C:\\User\\R-munka") # saját elérési út megadása

# hívjuk be az ESS kilencedik hullámának adatbázisát
ess<-read.csv("ESS9e03_2.csv")

# mentsük el RData formátumban az ESS adatbázisát
save(ess, file="ess.RData")

# töltsük be a fb.RData adatbázist
load("fb.RData")

# mentsük el a workspace-en szereplő két adatbázist datasets néven
save.image("datasets.RData")