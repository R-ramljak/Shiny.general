library(leaflet)
library(maptools)
library(rgdal)
library(broom)
library(rgeos)
library(shiny)
library(sp)
library(tidyverse)
library(sf)
library(rmapshaper)
library(readxl)

setwd("~/Projekte/BMFSFJ/Kitastudie/Shinyapp 4.0/")


# Kita Daten werden mit Geodaten jeweils zusammengebracht und dann als geeinte sf File in
# die App geladen. Somit gibt es für die drei Ansichten jeweils eine eigene sf File.


#Polygondaten Deutschlands
Germany <- readRDS("gadm36_DEU_1_sf.rds")
#Germany <- readOGR("Data/DEU_adm/DEU_adm1.shp")
Germany <- ms_simplify(Germany, keep_shapes = T, keep = 0.002)
#MATCHED_BL <- spTransform(MATCHED, CRS("+init=epsg:4326"))

#Kinderbetreungsrate 0-2 & 3-5 Jahre im Zeitraum von 2012-2017 auf Bundesebene
kita_bund <- read_xlsx("Daten_Kita_App.xlsx", sheet = "Factsheet.Gesamt")
#Kinderbetreungsrate 0-2 & 3-5 Jahre im Zeitraum von 2012-2017 auf Ost/West Ebene
kita_ow <- read_xlsx("Daten_Kita_App.xlsx", sheet = "Factsheet.Ost-West")
#Kinderbetreungsrate 0-2 & 3-5 Jahre im Zeitraum von 2012-2017 auf Bundeslandebene
kita_bl <- read_xlsx("Daten_Kita_App.xlsx", sheet = "Factsheet.BL")

#Dummy Kreation
dummy_bund <- c(50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
               50, 50, 50, 50, 50)
dummy_ow <- c(30, 30, 20, 20, 30, 30, 30, 20, 30, 30, 30,
              30, 20, 20, 30, 20)

#Merging der Objekte
#Bundesebene
Germany$dummy_bund <- dummy_bund
kita_bund_sf <- Germany %>%
  group_by(dummy_bund) %>%
  summarize(first(dummy_bund)) %>%
  rename(bereichsnummer_1 = `first(dummy_bund)`) %>%
  left_join(kita_bund, by = c("dummy_bund" = "bereichsnummer_1")) %>%
  mutate_at(c("dummy_bund", "bereichsnummer_1"), ~as.character(.)) %>%
  mutate_if(is.numeric, ~round(., digits = 1))
kita_bund_sf$`first(NAME_1)` <- NULL

#Ost West
Germany$dummy_ow <- dummy_ow
kita_ow_sf <- Germany %>%
  group_by(dummy_ow) %>%
  summarize(first(dummy_ow)) %>%
  rename(bereichsnummer_1 = `first(dummy_ow)`) %>%
  left_join(kita_ow, by = c("dummy_ow"= "bereichsnummer_1")) %>%
  mutate_at(c("dummy_ow", "bereichsnummer_1"), ~as.character(.)) %>%
  mutate_if(is.numeric, ~round(., digits = 1))
kita_ow_sf$`first(NAME_1)` <- NULL

#Bundesland
kita_bl_sf <- Germany %>%
  group_by(NAME_1) %>%
  summarize(first(NAME_1)) %>%
  left_join(kita_bl, by = c("NAME_1" = "Einheit")) %>%
  rename(Einheit = NAME_1) %>%
  mutate_at(c("bereichsnummer_1"), ~as.character(.)) %>%
  mutate_if(is.numeric, ~round(., digits = 1))
kita_bl_sf$`first(NAME_1)` <- NULL
plot(kita_ow_sf)


#Abspeichern der finalen Datei für die weitere Leaflet Berabeitung
saveRDS(kita_bl_sf, file = "KITA_Spatial_BL.rds")
saveRDS(kita_bund_sf, file = "KITA_Spatial_ALL.rds")
saveRDS(kita_ow_sf, file = "KITA_Spatial_OST.rds")

#############################

