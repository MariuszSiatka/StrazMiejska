# Deklaracja sciezki do bilbiotek
options(stringsAsFactors = FALSE)

##########################################################################
#### upewnienie siÍ øe nie ma øadnych pakietÛw za≥adowanych ####
gc(reset = TRUE)
#od≥πczeni wszytkich pakietÛw - stowrzebnuie funkcji
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}

detachAllPackages() #wywo≥anie funkcji 


##########################################################################
#### za≥adowanie pakietÛw ####

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

library(ggmap) #do zbierania wspÛ≥rzednych
#library(sp)  #do zbierania wspÛ≥rzednych
library(extrafont) #do czcionek
##########################################################################
#### wczytanie danych ####
setwd("D:\\KK\\OneDrive\\Wroclaw w Liczbach\\Gotowe projekty\\straø miejska")
#dane nt MandatÛw
load(file = ".\\Dane RData\\1 Dane nt mandatÛw.RData")

# granice osiedli
GR_os<- readOGR("D:/KK/OneDrive/Wroclaw w Liczbach/Gotowe projekty/Pozwolenia na budowe/GraniceOsiedli", layer = "GraniceOsiedli") #katalog pierwszy argument. Drugi - nazwa plikÛw shp
GR_os <- spTransform(GR_os, CRS("+proj=longlat +datum=WGS84")) 
area.points <- fortify(GR_os)

#dane nt wspolrzednych
load(file = ".\\Dane RData\\Adresy_i_wspolrzedne I.RData")
Adresy_i_wspolrzedne_I <- Adresy_i_wspolrzedne
load(file = ".\\Dane RData\\Adresy_i_wspolrzedne II.RData")
Adresy_i_wspolrzedne_II <- Adresy_i_wspolrzedne
load(file = ".\\Dane RData\\Adresy_i_wspolrzedne III.RData")
Adresy_i_wspolrzedne_III <- Adresy_i_wspolrzedne
load(file = ".\\Dane RData\\Adresy_i_wspolrzedne IIII.RData")
Adresy_i_wspolrzedne_IIII <- Adresy_i_wspolrzedne
load(file = ".\\Dane RData\\Adresy_i_wspolrzedne-NA.RData")
Adresy_i_wspolrzedne_NA <- Adresy_i_wspolrzedne
load(file = ".\\Dane RData\\Adresy_i_wspolrzedne NA II.RData")
Adresy_i_wspolrzedne_NAII <- Adresy_i_wspolrzedne

##########################################################################
#### Przygotowanie danych ####

Adr_wspl <- rbind(Adresy_i_wspolrzedne_I, Adresy_i_wspolrzedne_II,
                              Adresy_i_wspolrzedne_III, Adresy_i_wspolrzedne_IIII,
                              Adresy_i_wspolrzedne_NA, Adresy_i_wspolrzedne_NAII) %>%
  filter(!is.na(lon)) %>%
  group_by(Adres) %>%
  summarise(lon = mean(lon, na.rm = T), 
         lat = mean(lat, na.rm = T)) 



names(Mandaty)
Mandaty_i_wspolrzedne <- Mandaty %>%
  merge(Adr_wspl, by = "Adres")



Ilosi_Dzien_mandaty <- Mandaty %>%
  group_by(grp, Dzien) %>%
  summarise(Ilosc_interwencji = n(),
            Suma_mandatow = sum(Kwota_mandatu, na.rm = T),
            Ilosc_mandatow = sum(Czy_mandat_wystawiony, na.rm = T),
            Odsetek_interwencji_z_mandatami = Ilosc_mandatow / Ilosc_interwencji) %>%
  mutate(Dzien = as.Date(Dzien, format = "%d %m %y"))

mean(Ilosi_Dzien_mandaty$Ilosc_interwencji, na.rm = T)

Ilosc_tygodnie_mandaty <- Mandaty %>%
  mutate(Dzien = as.Date(Dzien, format = "%d %m %y")) %>%
  filter(Tydzien_rok != "53-2016") %>% #odrzucamy, bo to tak na prawde pierwszyt tydzien i tylko psuje wykres
  group_by(grp, Tydzien_rok) %>%
  summarise(Dzien_Min = min(Dzien), 
            Dzien_Max = max(Dzien), 
            Ilosc_dni = Dzien_Max - Dzien_Min,
            Ilosc_interwencji = n(),
            Suma_mandatow = sum(Kwota_mandatu, na.rm = T),
            Ilosc_mandatow = sum(Czy_mandat_wystawiony, na.rm = T),
            Odsetek_interwencji_z_mandatami = Ilosc_mandatow / Ilosc_interwencji,
            Sredni_Mandat = mean(Kwota_mandatu, na.rm = T)
  ) %>%
  filter(Ilosc_dni > 4)   #wyfiltrwanie nie pe≥ych tygodni 
#gather(Metryka, Wartosc, -grp, -Tydzien_rok, -Dzien_Min, -Dzien_Max) 



Ilosc_tygodnie_mandaty_typ <- Mandaty %>%
  mutate(Dzien = as.Date(Dzien, format = "%d %m %y")) %>%
  mutate(Typ = as.character(Typ),
         Typ =  case_when(Typ == "Nieprawid≥owe parkowanie" ~ "Nieprawid≥owe parkowanie" ,
                          Typ == "Ochrona úrodowiska" ~ "Ochrona úrodowiska" ,
                          T ~ "Inne")         ,
         Typ = factor(Typ, levels = c(
           "Inne",
           "Ochrona úrodowiska", 
           "Nieprawid≥owe parkowanie"
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
  filter((Ilosc_dni > 4 & Typ == "Nieprawid≥owe parkowanie") |
         (Ilosc_dni > 4 & Typ == "Inne") |
          Typ == "Ochrona úrodowiska")   #wyfiltrwanie nie pe≥ych tygodni 

#gather(Metryka, Wartosc, -grp, -Tydzien_rok, -Dzien_Min, -Dzien_Max) 

Breakes <- as.Date(c("2016-01-01", "2017-01-01", "2018-01-01"))

##########################################################################
#### Wykres 1 - ilosÊ zg≥oszeÒ w czasie ####

# tmp <- Ilosc_tygodnie_mandaty %>%
#   filter(Metryka == "Ilosc_interwencji" | Metryka ==  "Ilosc_mandatow")
# 
# ggplot(Ilosc_tygodnie_mandaty, aes(x = Dzien_Min, y = Ilosc_interwencji, group = grp)) +
#   geom_line() +
#   geom_smooth(method = "lm", se = F, linetype = 2) + 
#   labs(title = "IloúÊ interwencji Straøy Miejskiej we Wroc≥awiu",
#        x = "",
#        y = "IloúÊ interwencji w ciπgu tygodnia",
#        caption = "Autor: WroData | èrÛd≥o: Straø Miejska udostÍpnione dla Publiczny Rejestr Zg≥oszeÒ Straøy Miejskiej Wroc≥aw" )



w1 <- ggplot(Ilosc_tygodnie_mandaty_typ, aes(x = Dzien_Min, y = Ilosc_interwencji,  col = Typ, group = interaction(Typ, grp))) +
  geom_line( ) +
  geom_smooth(method = "lm", se = F, linetype = 2, size = 0.5, alpha = 0.1, show_guide = FALSE) + 
  geom_point(aes(shape = Typ), size = 2) +
  scale_colour_manual(values = c("#800000", "#ffa400", "#08519C")) +  
  scale_shape_manual(values = c(16, 15, 18)) + 
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y", 
               date_minor_breaks = "1 month") + 
  labs(title = "Interwencje Straøy Miejskiej we Wroc≥awiu",
       subtitle = "Suma interwencji w tygodniu",
       x = "",
       y = "",
       caption = "Autor: WroData | èrÛd≥o: Straø Miejska udostÍpnione dla Publiczny Rejestr Zg≥oszeÒ Straøy Miejskiej Wroc≥aw" ) 



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

w1 + Theme

a <- 9
png(filename = paste("wykresy\\", Sys.Date()," w1" , ".png", sep=""),
    bg="white", width = a * 1.161803, height = a, units = 'in', res = 150)
      plot(w1)
dev.off()


##########################################################################
#### Wykres 2 - årednia wartosÊ mandatu ####
ggplot(Ilosc_tygodnie_mandaty, aes(x = Dzien_Min, y = Sredni_Mandat, group = grp)) +
  geom_line() +
  geom_smooth(method = "lm", se = F, linetype = 2) + 
  labs(title = "årednia wartosÊ mandatu Straøy Miejskiej we Wroc≥awiu",
       x = "",
       y = "årednia wartosÊ mandatu",
       caption = "Autor: WroData | èrÛd≥o: Straø Miejska udostÍpnione dla Publiczny Rejestr Zg≥oszeÒ Straøy Miejskiej Wroc≥aw" )

##########################################################################
#### Wykres 2 - Suma wartosÊ mandatu ####
ggplot(Ilosc_tygodnie_mandaty, aes(x = Dzien_Min, y = Suma_mandatow, group = grp)) +
  geom_line() +
  geom_smooth(method = "lm", se = F, linetype = 2) + 
  labs(title = "Sumaryczna wartosÊ mandatu Straøy Miejskiej we Wroc≥awiu",
       x = "",
       y = "Sumaryczna wartosÊ mandatÛw wystawionych w tygodniu",
       caption = "Autor: WroData | èrÛd≥o: Straø Miejska udostÍpnione dla Publiczny Rejestr Zg≥oszeÒ Straøy Miejskiej Wroc≥aw" )

##########################################################################
#### Wykres 3 - dni tygodnia mandatÛw ####

ggplot(Mandaty, aes(x = Dzien_tyg)) +
  geom_bar()  

##########################################################################
#### Wykres 4 - godziny mandatÛw ####

ggplot(Mandaty, aes(x = Godzina)) +
  geom_bar() 

##########################################################################
#### Wykres 5 - udzia≥ kategorii w czasie ####
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
  filter(Ilosc_dni > 5) %>%   #wyfiltrwanie nie pe≥ych tygodni 
  ungroup()  %>%
  select(Tydzien_rok, Typ) %>%
  rbind(Brakujace_daty) %>%
  mutate(Typ = factor(Typ, levels = c(
                                      "Brak danych",
                                      "Inne",
                                      "Ochrona úrodowiska", 
                                      "Nieprawid≥owe parkowanie"
                                      )))
library(scales)


w5 <- ggplot(W5, aes(x = Tydzien_rok, fill = Typ )) +
  geom_bar(position = "fill") +
  #scale_fill_brewer() +
  scale_fill_manual(values = c("gray75", "#800000", "#ffa400",  "#08519C")) +   
  scale_x_discrete(breaks = c("01-2016", "01-2017", "01-2018")) + 
  scale_y_continuous(labels = percent, expand = c(0, 0)) +
  labs(title = "Interwencje Straøy Miejskiej we Wroc≥awiu",
       subtitle = "Udzia≥ procentowy kategorii podjÍtych interwencji",
       x = "",
       y = "",
       caption = "Autor: WroData | èrÛd≥o: Straø Miejska udostÍpnione dla Publiczny Rejestr Zg≥oszeÒ Straøy Miejskiej Wroc≥aw" ) 

w5 + Theme



png(filename = paste("wykresy\\", Sys.Date()," w5" , ".png", sep=""),
    bg="white", width = a * 1.161803, height = a, units = 'in', res = 150)
plot(w5 + Theme)
dev.off()







ggplot(Ilosc_tygodnie_mandaty_typ, aes(x = Tydzien_rok, y = Ilosc_mandatow, fill = Typ )) +
  geom_bar(position = "fill") +
  scale_fill_brewer() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##########################################################################
#### Wykres 6 - histogram wysokosci mandatÛw ####

names(Mandaty)
t <- Mandaty %>%
  mutate(Mandat = round(Kwota_mandatu / 50, 0) * 50) %>%
  group_by(Typ, Mandat) %>%
  summarise(n_typ_Man = n()) %>%
  ungroup(Typ, Mandat) %>%
  group_by(Typ) %>%
  mutate(n_typ = sum(n_typ_Man),
         udzial = n_typ_Man / n_typ) %>%
  ungroup() %>%
  filter(!is.na(Typ))

ggplot(t, aes(x = Mandat, y = udzial, fill = Typ ), alpha = 0.1) +
  geom_bar()

ggplot(Mandaty, aes(Kwota_mandatu, col = Typ)) +
  geom_freqpoly(aes(y=..count../sum(..count..)), binwidth = 50, position = "identity")

ggplot(Mandaty, aes(Kwota_mandatu, col = Typ)) +
  geom_freqpoly(aes(y=..density..), binwidth = 100, position = "identity") +
  coord_cartesian(xlim =c(0,500))

+
  scale_fill_brewer() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Definition of a set of blue colors
blues <- brewer.pal(9, "Blues") # from the RColorBrewer package

# 1 - Make a color range using colorRampPalette() and the set of blues
blue_range <- colorRampPalette(blues)
blue_range(4) # the newly extrapolated colours
munsell::plot_hex(blue_range(12))
# 2 - Use blue_range to adjust the color of the bars, use scale_fill_manual()
ggplot(Vocab, aes(x = education, fill = vocabulary)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = blue_range(11) )



##########################################################################
#### Wykres 7 - mapa zdazeÒ ####
names(Adresy_i_wspolrzedne)
Mandaty_i_wspolrzedne <- Mandaty_i_wspolrzedne %>%
  filter(!is.na(Typ)) %>%
  mutate(Typ = as.character(Typ),
         Typ =  case_when(Typ == "Nieprawid≥owe parkowanie" ~ "Nieprawid≥owe parkowanie" ,
                          Typ == "Ochrona úrodowiska" ~ "Ochrona úrodowiska" ,
                          T ~ "Inne")         ,
         Typ = factor(Typ, levels = c(
           "Inne",
           "Ochrona úrodowiska", 
           "Nieprawid≥owe parkowanie"
         ))) %>%
  filter(lat < 51.21)  %>%
  filter(lon < 17.164) 
# 
# Pozwolenia<-Pozwolenia[Pozwolenia$lat<51.21,]
# Pozwolenia<-Pozwolenia[Pozwolenia$lon<17.164,]
# Pozwolenia<-Pozwolenia[Pozwolenia$lon>16.9 | Pozwolenia$lat>51.1,]
#%>%
#  filter(Typ != "Wandalizm")

#zmiana wspÛ≥rzÍdnych
Skalar <- 1  /   ((18.27/13.86)*((max(area.points$lat)-min(area.points$lat))/(max(area.points$long)-min(area.points$long))))
Skalar <- 1


lon <- c(min(Mandaty_i_wspolrzedne$lon), max(Mandaty_i_wspolrzedne$lon))
lat <- c(min(Mandaty_i_wspolrzedne$lat), max(Mandaty_i_wspolrzedne$lat))
 
map <- ggmap(get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 11, maptype='roadmap'), extent = "device")  

w7 <- map +
#  scale_x_continuous(limits = lon, expand = c(0, 0)) +
#  scale_y_continuous(limits = lat, expand = c(0, 0)) +
  coord_map(xlim = lon, ylim = lat) +
  geom_polygon(aes(x = long, y = lat * Skalar, group = group), data = area.points,  
               color = "black", fill = NA, alpha = 1) +
  geom_point(aes(x = lon, y = lat * Skalar, col = Typ, shape = Typ), 
                   data = Mandaty_i_wspolrzedne,  size = 1, alpha = 0.7 ) +
  scale_colour_manual(values = c("#800000", "#ffa400", "#08519C")) +  
  scale_shape_manual(values = c(16, 15, 18)) +   
  labs(title = "Interwencje Straøy Miejskiej we Wroc≥awiu",
         subtitle = "Lata 2016-2018",
         x = "",
         y = "",
         caption = "Autor: WroData | èrÛd≥o: Straø Miejska udostÍpnione dla Publiczny Rejestr Zg≥oszeÒ Straøy Miejskiej Wroc≥aw" ) + 
    Theme



png(filename = paste("wykresy\\", Sys.Date()," w7" , ".png", sep=""),
    bg="white", width = a * 1.161803, height = a, units = 'in', res = 150)
plot(w7)
dev.off()


#########################

map2 <- get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 14,
               maptype = "roadmap", source = "google")

### When you draw a figure, you limit lon and lat.      
foo <- ggmap(map) +
  scale_x_continuous(limits = lon, expand = c(0, 0)) +
  scale_y_continuous(limits = lat, expand = c(0, 0))
###########################


install.packages("OpenStreetMap")
library(OpenStreetMap)

# mapa <- qmap(location = "Wroclaw", zoom = 12, maptype='roadmap')
# mapa +
# map <- openmap(c(min(Mandaty_i_wspolrzedne$lat), max(Mandaty_i_wspolrzedne$lat)),
#                c(min(Mandaty_i_wspolrzedne$lon), max(Mandaty_i_wspolrzedne$lon)),
#                type="osm")
# map    <- get_googlemap('Wroclaw', zoom =12) 
# plot(map)
map <- ggmap(get_map(location = "Wroclaw", zoom = 11, maptype='roadmap'), extent = "device") 

map <- openmap(c(min(Mandaty_i_wspolrzedne$lat), min(Mandaty_i_wspolrzedne$lon)),
               c(max(Mandaty_i_wspolrzedne$lat),max(Mandaty_i_wspolrzedne$lon)),
               type="osm")

tmp_mandaty <- cbind(Mandaty_i_wspolrzedne,
                     projectMercator(Mandaty_i_wspolrzedne$lat, Mandaty_i_wspolrzedne$lon))
tmp_osiedla <- cbind(area.points , 
                     projectMercator(area.points$lat, area.points$long))


autoplot(map)  +   
  geom_point(aes(x = x, y = y * Skalar, col = Typ, shape = Typ), data = tmp_mandaty,  
             size = 1, alpha = 0.7 ) +
  scale_colour_manual(values = c("#800000", "#ffa400", "#08519C")) +  
  scale_shape_manual(values = c(16, 15, 18)) + 
  geom_polygon(aes(x = x, y = y * Skalar, group = group), data = tmp_osiedla,  
               color = "black", fill = NA, alpha = 1) 

  

#ggplot() + 
autoplot(map)  +  
  coord_fixed(xlim = c(min(Mandaty_i_wspolrzedne$lon), max(Mandaty_i_wspolrzedne$lon)),
                    ylim = c(min(Mandaty_i_wspolrzedne$lat), max(Mandaty_i_wspolrzedne$lat)),
                    ratio = 1) +
  geom_polygon(aes(x = long, y = lat * Skalar, group = group), data = area.points,  
               color = "black", fill = NA, alpha = 1) +
  geom_point(aes(x = lon, y = lat * Skalar, col = Typ), data = Mandaty_i_wspolrzedne,  
                   size = 1, alpha = 0.7, shape = 1 ) 

+
  scale_colour_brewer()

shape = 1,

map
  stat_density2d(data = Adresy_i_wspolrzedne, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 1, bins = 50, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE) + geom_point(data = Mandaty_Handel, aes(x = lon, y = lat, col = Typ),
                                                             size=1, alpha=1)+
  theme(legend.position="none")

