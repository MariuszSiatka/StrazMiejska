# Deklaracja sciezki do bilbiotek
options(stringsAsFactors = FALSE)

##########################################################################
#### upewnienie siê ¿e nie ma ¿adnych pakietów za³adowanych ####
gc(reset = TRUE)
#od³¹czeni wszytkich pakietów - stowrzebnuie funkcji
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}

detachAllPackages() #wywo³anie funkcji 


##########################################################################
#### za³adowanie pakietów ####

library(dplyr)
library(ggplot2)
library(readxl) #do czytania excela
#library(xlsx) #do czytania excela

#library(reshape2)
#library(gridExtra)
#library(grid)

library(ggmap) #do zbierania wspó³rzednych
library(sp)  #do zbierania wspó³rzednych

##########################################################################
#### wczytanie danych ####
setwd("D:\\KK\\OneDrive\\Wroclaw w Liczbach\\Gotowe projekty\\stra¿ miejska")
load(file = "1 Dane.RData")

##########################################################################
#### Wybranie Adresów ####

Adresy <- Mandaty %>%
  select(Adres) %>% distinct() %>%
  arrange(Adres)
Adresy$ID <- 1:nrow(Adresy) 

Adresy2 <- merge(Adresy, Adr_wspl, by = "Adres", all.x = T)
save(Adresy2, file = "Adresy2.RData")

##########################################################################
#### Szukanie wspólrzêdnych ####
#geocode(Adresy$Adres[3])
tmp <- Adresy [4001:nrow(Adresy),]
wspolzendne <- geocode(tmp$Adres)

Adresy_i_wspolrzedne <- cbind(tmp, wspolzendne)


##########################################################################
#### zapis ####
save(Adresy_i_wspolrzedne, file = "Adresy_i_wspolrzedne III.RData")

##########################################################################
#### Dodanie tych co siê nie uda³o ####

load(file = ".\\Dane RData\\Adresy_i_wspolrzedne I.RData")
Adresy_i_wspolrzedne_I <- Adresy_i_wspolrzedne
load(file = ".\\Dane RData\\Adresy_i_wspolrzedne II.RData")
Adresy_i_wspolrzedne_II <- Adresy_i_wspolrzedne
load(file = ".\\Dane RData\\Adresy_i_wspolrzedne III.RData")
Adresy_i_wspolrzedne_III <- Adresy_i_wspolrzedne
load(file = ".\\Dane RData\\Adresy_i_wspolrzedne IIII.RData")
Adresy_i_wspolrzedne_IIII <- Adresy_i_wspolrzedne

##########################################################################
#### Przygotowanie danych ####

Adresy_i_wspolrzedne <- rbind(Adresy_i_wspolrzedne_I,   Adresy_i_wspolrzedne_II, 
                              Adresy_i_wspolrzedne_III, Adresy_i_wspolrzedne_IIII)
NA_Adresy_i_wspolrzedne <- Adresy_i_wspolrzedne %>%
  filter(is.na(lon)) %>%
  select(Adres, ID)

save(NA_Adresy_i_wspolrzedne, file = "NA_Adresy_i_wspolrzedne.RData")
##########################################################################
#### Szukanie wspólrzêdnych ####
wspolzendne <- geocode(Adresy_i_wspolrzedne_NA$Adres)

Adresy_i_wspolrzedne <- cbind(Adresy_i_wspolrzedne_NA, wspolzendne)


##########################################################################
#### zapis ####
save(Adresy_i_wspolrzedne, file = "Adresy_i_wspolrzedne IIII.RData")
