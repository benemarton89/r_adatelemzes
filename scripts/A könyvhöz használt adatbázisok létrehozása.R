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