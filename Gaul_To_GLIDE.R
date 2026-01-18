library(readxl)
library(writexl)
library(dplyr)
library(lubridate)
library(tidyr)
library(slider)

shp_1 <- st_read("/Users/zhangxiangyong/Library/CloudStorage/Box-Box/SevereWeather_RefugeeOutflows/GAUL/gaul_1_2015_fixed/gaul_1_2015_fixed.shp")

shp_1$ISO3C <- countrycode(shp_1$ADM0_NAME, "country.name", "iso3c")

shp_1 <- shp_1 %>%
  select(ISO3C, ADM1_NAME, ADM0_NAME) %>%
  rename("mtchd_1" = "ADM1_NAME")

shp_1_mmr <- shp_1 %>%
  filter(mtchd_1 %in% c("Bago (E)","Bago (W)","Shan (E)", "Shan (N)","Shan (S)"))

shp_union_bago <- shp_1_mmr %>%
  filter(mtchd_1 %in% c("Bago (E)","Bago (W)")) %>%
  summarize(geometry = st_union(geometry), .groups = "drop") %>%
  mutate(ISO3C = "MMR", mtchd_1 = "Bago", ADM0_NAME = "Myanmar") %>%
  select(ISO3C, mtchd_1, ADM0_NAME, geometry)


shp_union_shan <- shp_1_mmr %>%
  filter(mtchd_1 %in% c("Shan (E)", "Shan (N)","Shan (S)")) %>%
  summarize(geometry = st_union(geometry), .groups = "drop") %>%
  mutate(ISO3C = "MMR", mtchd_1 = "Shan", ADM0_NAME = "Myanmar") %>%
  select(ISO3C, mtchd_1, ADM0_NAME, geometry)

shp_1 <- shp_1 %>%
  filter(!mtchd_1 %in% shp_1_mmr$mtchd_1)

shp_1_clean <- rbind(shp_1, shp_union_bago, shp_union_shan)

################################
##     Work on the Macbook    ##
################################

#Set work directory
setwd('/Users/zhangxiangyong/Desktop/Severe Weather and Refugee Flows')

#Load the Dartmouth Flood Observatory Data
DFO <- read_xlsx("Global Disaster Dataset/FloodArchive.xlsx")

#Load the GLIDE Natural Disaster Data
GLIDE <- read_xlsx("Global Disaster Dataset/GLIDE.xlsx")

#Load the EM-DAT Natural Disaster Data
EMDAT <- read_xlsx("Global Disaster Dataset/public_emdat_2024-06-24.xlsx")

#Load the GFLD Land Slide Data
gfld_sf <- st_read('/Users/zhangxiangyong/Desktop/Severe Weather and Refugee Flows/Global Disaster Dataset/Landslidepoints_04to17/Landslidepoints_04to17.shp')

#Replace the GFLD Geometry with POINT(Long, Lat) Format
gfld_df <- data.frame(gfld_sf)

gfld_df <- gfld_df %>%
  select(-geometry)

gfld_sf <- st_as_sf(gfld_df, coords = c("Longitude", "Latitude"), crs = 4326)

#Convert the DFO Longitude and Latitude to sf object
DFO_sf <- st_as_sf(DFO, coords = c("long", "lat"), crs = 4326)

#Clean the Latitude and Longitude variables in the GLIDE Data
GLIDE$Latitude <- str_trim(GLIDE$Latitude)
GLIDE$Latitude <- as.numeric(GLIDE$Latitude)

GLIDE$Longitude <- str_trim(GLIDE$Longitude)
GLIDE$Longitude <- as.numeric(GLIDE$Longitude)

#Clean the records with unavailable coordinates
GLIDE <- GLIDE %>%
  filter(Longitude != 0 | Latitude != 0)

#Convert the GLIDE Longitude  and Latitude to sf Object
GLIDE_sf <- st_as_sf(GLIDE, coords = c("Longitude", "Latitude"), crs = 4326)

#####################################
##  Apply GAUL Layer on Datasets   ##
#####################################

#st_join GAUL with Dartmouth Flood Observatory Dataset
DFO_sf_gaul <- DFO_sf %>%
  st_join(shp_1_clean, left = TRUE)

#st_join GAUL with Global Fatal Land Slide Dataset
gfld_sf_gaul <- gfld_sf %>%
  st_join(shp_1_clean, left = TRUE)

#st_join GAUL with the GLIDE Dataset
GLIDE_sf_gaul <- GLIDE_sf %>%
  st_join(shp_1_clean, left = TRUE)

#DFO
DFO_sf_gaul_miss <- DFO_sf_gaul[is.na(DFO_sf_gaul$mtchd_1),]

DFO_sf_gaul_miss$id <- st_nearest_feature(DFO_sf_gaul_miss, shp_1_clean)

DFO_sf_gaul_miss$mtchd_1 <- shp_1_clean$mtchd_1[DFO_sf_gaul_miss$id]

DFO_sf_gaul_miss$ADM0_NAME <- shp_1_clean$ADM0_NAME[DFO_sf_gaul_miss$id]

DFO_sf_gaul_miss$ISO3C <- countrycode(DFO_sf_gaul_miss$ADM0_NAME, "country.name", "iso3c")

DFO_sf_gaul <- DFO_sf_gaul %>% filter(!ID %in% DFO_sf_gaul_miss$ID)
DFO_sf_gaul <- DFO_sf_gaul_miss %>% 
  select(-id) %>%
  rbind(DFO_sf_gaul)

DFO_sf_gaul$ISO3C[is.na(DFO_sf_gaul$ISO3C)] <- countrycode(DFO_sf_gaul$Country[is.na(DFO_sf_gaul$ISO3C)], "country.name", "iso3c")
DFO_sf_gaul$ISO3C[is.na(DFO_sf_gaul$ISO3C)] <- "PRT"

#GFLD
gfld_sf_gaul_miss <- gfld_sf_gaul[is.na(gfld_sf_gaul$mtchd_1),]

gfld_sf_gaul_miss$id <- st_nearest_feature(gfld_sf_gaul_miss, shp_1_clean)

gfld_sf_gaul_miss$mtchd_1 <- shp_1_clean$mtchd_1[gfld_sf_gaul_miss$id]

gfld_sf_gaul_miss$ADM0_NAME <- shp_1_clean$ADM0_NAME[gfld_sf_gaul_miss$id]

gfld_sf_gaul_miss$ISO3C <- countrycode(gfld_sf_gaul_miss$Country, "country.name", "iso3c")

gfld_sf_gaul <- gfld_sf_gaul %>% filter(!LandslideN %in% gfld_sf_gaul_miss$LandslideN)
gfld_sf_gaul <- gfld_sf_gaul_miss %>% 
  select(-id) %>%
  rbind(gfld_sf_gaul)

gfld_sf_gaul$ISO3C[is.na(gfld_sf_gaul$ISO3C)] <- countrycode(gfld_sf_gaul$Country[is.na(gfld_sf_gaul$ISO3C)], "country.name", "iso3c")

#GLIDE
GLIDE_sf_gaul_miss <- GLIDE_sf_gaul[is.na(GLIDE_sf_gaul$mtchd_1),]

GLIDE_sf_gaul_miss$id <- st_nearest_feature(GLIDE_sf_gaul_miss, shp_1_clean)

GLIDE_sf_gaul_miss$mtchd_1 <- shp_1_clean$mtchd_1[GLIDE_sf_gaul_miss$id]

GLIDE_sf_gaul_miss$ADM0_NAME <- shp_1_clean$ADM0_NAME[GLIDE_sf_gaul_miss$id]

GLIDE_sf_gaul_miss$ISO3C <- countrycode(GLIDE_sf_gaul_miss$Country, "country.name", "iso3c")

GLIDE_sf_gaul_miss$ISO3C[GLIDE_sf_gaul_miss$Country == "Canary Islands"] <- "ESP"
GLIDE_sf_gaul_miss$ISO3C[GLIDE_sf_gaul_miss$Country == "Netherlands Antilles"] <- "NLD"

GLIDE_sf_gaul <- GLIDE_sf_gaul %>% filter(!GLIDE_number %in% GLIDE_sf_gaul_miss$GLIDE_number)
GLIDE_sf_gaul <- GLIDE_sf_gaul_miss %>% 
  select(-id) %>%
  rbind(GLIDE_sf_gaul)
