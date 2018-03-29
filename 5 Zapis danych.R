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
library(tidyr) #do geather
library(xlsx) #do zapisu excela
library(rgdal) # do wczytania shapefile do granic osiedli

#library(reshape2)
#library(gridExtra)
#library(grid)
library(lubridate) # do numeru tygodnia
library(stringr) # do funkcji pad

library(ggmap) #do zbierania wspó³rzednych
#library(sp)  #do zbierania wspó³rzednych
library(extrafont) #do czcionek
library(scales) #do osi Y jako %
##########################################################################
#### wczytanie danych ####
setwd("D:\\KK\\OneDrive\\Wroclaw w Liczbach\\Gotowe projekty\\stra¿ miejska")
#dane nt Mandatów
load(file = ".\\Dane RData\\1 Dane nt mandatów.RData")

# granice osiedli
GR_os<- readOGR("D:/KK/OneDrive/Wroclaw w Liczbach/Gotowe projekty/Pozwolenia na budowe/GraniceOsiedli", layer = "GraniceOsiedli") #katalog pierwszy argument. Drugi - nazwa plików shp
GR_os <- spTransform(GR_os, CRS("+proj=longlat +datum=WGS84")) 
area.points <- fortify(GR_os)

#dane nt wspolrzednych
load(file = ".\\Dane RData\\Adresy_ALL.RData") 
##########################################################################
#### Przygotowanie danych ####

names(Mandaty)
Mandaty_i_wspolrzedne <- Mandaty %>%
  merge(Adr_wspl, by = "Adres") %>%
  filter(!is.na(Typ)) %>%
  mutate(Typ2 = as.character(Typ),
         Typ2 =  case_when(Typ2 == "Nieprawid³owe parkowanie" ~ "Nieprawid³owe parkowanie" ,
                           Typ2 == "Ochrona œrodowiska" ~ "Ochrona œrodowiska" ,
                           T ~ "Inne")         ,
         Typ2 = factor(Typ2, levels = c(
           "Inne",
           "Ochrona œrodowiska", 
           "Nieprawid³owe parkowanie"
         ))) %>%
  filter(lat < 51.21)  %>%
  filter(lon < 17.164) 



lon <- c(min(Mandaty_i_wspolrzedne$lon), max(Mandaty_i_wspolrzedne$lon))
lat <- c(min(Mandaty_i_wspolrzedne$lat), max(Mandaty_i_wspolrzedne$lat))



#write.xlsx(Mandaty_i_wspolrzedne, file = ".\\Dane Obrobione\\Interwencje SM Obrobione.xlsx", 
#           sheetName = "Dane", 
#           col.names = TRUE, row.names = TRUE, append = FALSE)

write.csv2(Mandaty_i_wspolrzedne, 
           file = ".\\Dane Obrobione\\Interwencje SM Obrobione - mandaty.csv")




Ilosc_tygodnie_mandaty_typ <- Mandaty %>%
  mutate(Dzien = as.Date(Dzien, format = "%d %m %y")) %>%
  mutate(Typ = as.character(Typ),
         Typ =  case_when(Typ == "Nieprawid³owe parkowanie" ~ "Nieprawid³owe parkowanie" ,
                          Typ == "Ochrona œrodowiska" ~ "Ochrona œrodowiska" ,
                          T ~ "Inne")         ,
         Typ = factor(Typ, levels = c(
           "Inne",
           "Ochrona œrodowiska", 
           "Nieprawid³owe parkowanie"
         )))  %>%
  filter(Tydzien_rok != "53-2016") %>% #odrzucamy, bo to tak na prawde pierwszyt tydzien i tylko psuje wykres
  group_by(Typ, grp, Tydzien_rok) %>%
  summarise(Dzien_Min = min(Dzien), 
            Dzien_Max = max(Dzien), 
            Ilosc_dni = Dzien_Max - Dzien_Min,
            Ilosc_interwencji = n(),
            Suma_mandatow = sum(Kwota_mandatu, na.rm = T),
            Ilosc_mandatow = sum(Czy_mandat_wystawiony, na.rm = T),
            Odsetek_interwencji_z_mandatami = Ilosc_mandatow / Ilosc_interwencji,
            Sredni_Mandat = mean(Kwota_mandatu, na.rm = T)
  ) %>%
  mutate(Dzien_Min = as.Date(Dzien_Min)) %>%
  ungroup() %>%
  filter((Ilosc_dni > 4 & Typ == "Nieprawid³owe parkowanie") |
         (Ilosc_dni > 4 & Typ == "Inne") |
          Typ == "Ochrona œrodowiska")   #wyfiltrwanie nie pe³ych tygodni 


#write.xlsx(Ilosc_tygodnie_mandaty_typ, file = ".\\Dane Obrobione\\Interwencje SM Obrobione.xlsx", 
#           sheetName = "Dane po tygodniach", 
#           col.names = TRUE, row.names = TRUE, append = FALSE)

write.csv2(Ilosc_tygodnie_mandaty_typ, 
           file = ".\\Dane Obrobione\\Interwencje SM Obrobione - mandaty tygodniami.csv")