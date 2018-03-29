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
#library(xlsx) #do czytania excela
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
  mutate(Typ = as.character(Typ),
         Typ =  case_when(Typ == "Nieprawid³owe parkowanie" ~ "Nieprawid³owe parkowanie" ,
                          Typ == "Ochrona œrodowiska" ~ "Ochrona œrodowiska" ,
                          T ~ "Inne")         ,
         Typ = factor(Typ, levels = c(
           "Inne",
           "Ochrona œrodowiska", 
           "Nieprawid³owe parkowanie"
         ))) %>%
  filter(lat < 51.21)  %>%
  filter(lon < 17.164) 

lon <- c(min(Mandaty_i_wspolrzedne$lon), max(Mandaty_i_wspolrzedne$lon))
lat <- c(min(Mandaty_i_wspolrzedne$lat), max(Mandaty_i_wspolrzedne$lat))






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

#gather(Metryka, Wartosc, -grp, -Tydzien_rok, -Dzien_Min, -Dzien_Max) 

Breakes <- as.Date(c("2016-01-01", "2017-01-01", "2018-01-01"))

# wielkoœæ obrazka
a <- 9


##########################################################################
#### Ustalenie spójnego stylu dla wykresów ####
Theme <-  theme(legend.position="bottom",
                legend.key.width = unit(1,"cm"),
                legend.title = element_blank(),
                axis.text    = element_text(family = "Ubuntu", size = 12, color = "#22211d"),
                axis.title   = element_text(family = "Ubuntu", size = 14, color = "#22211d"),
                text = element_text(family = "Ubuntu", color = "#22211d"),
                panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
                plot.background  = element_rect(fill = "#f5f5f2",  color = NA), 
                panel.background = element_rect(fill = "#f5f5f2",  color = NA), 
                legend.background = element_rect(fill = "#f5f5f2", color = NA),
                legend.text       = element_text(family = "Ubuntu", size = 13, hjust = 0, color = "#22211d"),
                plot.title    = element_text(family = "Ubuntu", size = 21,  hjust = 0.5,  color = "#4e4d47"),
                plot.subtitle = element_text(family = "Ubuntu", size = 13,  hjust = 0.01,  face = "italic", color = "#4e4d47"),
                plot.caption  = element_text(family = "Ubuntu", size = 11,  hjust = 0.99, color = "#4e4d47"),
                panel.border = element_blank()
)  

##########################################################################
#### Wykres 1 - ilosæ zg³oszeñ w czasie ####
w1 <- ggplot(Ilosc_tygodnie_mandaty_typ, aes(x = Dzien_Min, y = Ilosc_interwencji,  col = Typ, group = interaction(Typ, grp))) +
  geom_line( ) +
  geom_smooth(method = "lm", se = F, linetype = 2, size = 0.5, alpha = 0.1, show_guide = FALSE) + 
  geom_point(aes(shape = Typ), size = 2) +
  scale_colour_manual(values = c("#800000", "#ffa400", "#08519C")) +  
  scale_shape_manual(values = c(16, 15, 18)) + 
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y", 
               date_minor_breaks = "1 month") + 
  labs(title = "Interwencje Stra¿y Miejskiej we Wroc³awiu",
       subtitle = "Suma interwencji w tygodniu",
       x = "",
       y = "",
       caption = "Autor: WroData | ród³o: Stra¿ Miejska udostêpnione dla Publiczny Rejestr Zg³oszeñ Stra¿y Miejskiej Wroc³aw" ) 



w1 <- w1 + Theme


png(filename = paste("wykresy\\", Sys.Date()," w1" , ".png", sep=""),
    bg="white", width = a * 1.161803, height = a, units = 'in', res = 150)
      plot(w1)
dev.off()

save(w1, file = paste("wykresy\\", Sys.Date()," w1" , ".RData", sep=""))

##########################################################################
#### Wykres 5 - udzia³ kategorii w czasie ####
Brakujace_daty <- data.frame(Typ = "Brak danych", 
                             Dzien_Min = c(#seq(as.Date("2016-05-30"), as.Date("2016-07-31"), "weeks"),
                                           seq(as.Date("2017-04-24"), as.Date("2017-07-02"), "weeks"))) %>%
  mutate(Dzien = as.Date(Dzien_Min, format = "%Y-%m-%d"),
         Rok     = format(Dzien,"%Y") , 
         Tydzien = str_pad(lubridate::isoweek(Dzien), width = 2, side = "left", pad = "0"), 
         Tydzien_rok = paste0(Tydzien, "-", Rok)) %>%
  select(Tydzien_rok, Typ)

names(Mandaty)
W5 <- Mandaty %>%
  mutate(Dzien = as.Date(Dzien, format = "%d %m %y")) %>%
  mutate(Typ = as.character(Typ),
         Typ = ifelse(is.na(Typ), "Inne", Typ),
         Typ = ifelse(Typ == "Wykorczenia drogowe", "Inne", Typ),
         Typ = ifelse(Typ == "Kontrola czystosci", "Inne", Typ),
         Typ = ifelse(Typ == "Wandalizm", "Inne", Typ),
         Typ = ifelse(Typ == "Handel w miejscu zabronionym", "Inne", Typ),
         Typ = ifelse(Typ == "Wykroczenia drogowe", "Inne", Typ)
  ) %>%
  group_by(Tydzien_rok) %>%
  mutate(Dzien_Min = min(Dzien), 
         Dzien_Max = max(Dzien), 
         Ilosc_dni = Dzien_Max - Dzien_Min + 1) %>%
  filter(Ilosc_dni > 5) %>%   #wyfiltrwanie nie pe³ych tygodni 
  ungroup()  %>%
  select(Tydzien_rok, Typ) %>%
  rbind(Brakujace_daty) %>%
  mutate(Typ = factor(Typ, levels = c(
                                      "Brak danych",
                                      "Inne",
                                      "Ochrona œrodowiska", 
                                      "Nieprawid³owe parkowanie"
                                      )))



w5 <- ggplot(W5, aes(x = Tydzien_rok, fill = Typ )) +
  geom_bar(position = "fill") +
  #scale_fill_brewer() +
  scale_fill_manual(values = c("gray75", "#800000", "#ffa400",  "#08519C")) +   
  scale_x_discrete(breaks = c("01-2016", "01-2017", "01-2018")) + 
  scale_y_continuous(labels = percent, expand = c(0, 0)) +
  labs(title = "Interwencje Stra¿y Miejskiej we Wroc³awiu",
       subtitle = "Udzia³ procentowy kategorii podjêtych interwencji",
       x = "",
       y = "",
       caption = "Autor: WroData | ród³o: Stra¿ Miejska udostêpnione dla Publiczny Rejestr Zg³oszeñ Stra¿y Miejskiej Wroc³aw" ) 

w5 + Theme



png(filename = paste("wykresy\\", Sys.Date()," w5" , ".png", sep=""),
    bg="white", width = a * 1.161803, height = a, units = 'in', res = 150)
plot(w5 + Theme)
dev.off()

save(w5 + Theme, file = paste("wykresy\\", Sys.Date()," w5" , ".RData", sep=""))




##########################################################################
#### Wykres 7 - mapa zdazeñ ####
names(Adresy_i_wspolrzedne)

map <- ggmap(get_map(location = c(lon = mean(lon), lat = mean(lat)),
                     zoom = 11, maptype='roadmap'), extent = "device")  

w7 <- map +
#  scale_x_continuous(limits = lon, expand = c(0, 0)) +
#  scale_y_continuous(limits = lat, expand = c(0, 0)) +
  coord_map(xlim = lon, ylim = lat) +
  geom_polygon(aes(x = long, y = lat , group = group), data = area.points,  
               color = "black", fill = NA, alpha = 1) +
  geom_point(aes(x = lon, y = lat , col = Typ, shape = Typ), 
                   data = Mandaty_i_wspolrzedne,  size = 1, alpha = 0.7 ) +
  scale_colour_manual(values = c("#800000", "#ffa400", "#08519C")) +  
  scale_shape_manual(values = c(16, 15, 18)) +   
  labs(title = "Interwencje Stra¿y Miejskiej we Wroc³awiu",
         subtitle = "Lata 2016-2018",
         x = "",
         y = "",
         caption = "Autor: WroData | ród³o: Stra¿ Miejska udostêpnione dla Publiczny Rejestr Zg³oszeñ Stra¿y Miejskiej Wroc³aw" ) 

w7 <- w7 + 
  Theme +
  guides(shape = guide_legend(override.aes = list(size = 3))) +
  theme(axis.text=element_blank()) 



png(filename = paste("wykresy\\", Sys.Date()," w7" , ".png", sep=""),
    bg="white", width = a * 1.161803, height = a, units = 'in', res = 150)
plot(w7)
dev.off()

save(w7, file = paste("wykresy\\", Sys.Date()," w7" , ".RData", sep=""))
