install.packages("tidyverse")
install.packages("readxl")
install.packages("sf")
install.packages("terra")
install.packages("mapview")
install.packages("bcdata")
install.packages("bcmaps")

library(readxl)
library(sf)
library(tidyverse)
library(mapview)
library(terra)
library(bcmaps)
library(dplyr)


read.csv("H_FIRE_PNT.csv")
fire_points <- read.csv("H_FIRE_PNT.csv")

fire_sf <- st_as_sf(fire_points, coords = c("X_COORDINATE", "Y_COORDINATE"), crs = 3005)

park <- st_read("TA_PEP_SVW_polygon.shp")


fire_sf <- st_transform(fire_sf, st_crs(park))

LDB <- park %>%
  filter(PROT_NAME == "LAC DU BOIS GRASSLANDS PROTECTED AREA")

mapview(LDB)

fires_in_LDB <- fire_sf[st_intersects(fire_sf, LDB, sparse = FALSE), ]

mapview(LDB) + mapview(fires_in_LDB)

fires_per_year <- fires_in_LDB %>%
  group_by(FIRE_YEAR) %>%
  summarise(number_of_fires = n())

fire_cause <- fires_in_LDB %>%
  group_by(FIRE_CAUSE) %>%
  summarise(number_of_fires = n())

mapview(fires_in_LDB, zcol = "FIRE_CAUSE", legend = TRUE)

fires_summary <- fires_in_LDB %>%
  group_by(FIRE_YEAR, FIRE_CAUSE) %>%
  summarise(number_of_fires = n(), .groups = "drop")

library(ggplot2)

ggplot(fires_summary, aes(x = FIRE_CAUSE, y = number_of_fires, fill = FIRE_CAUSE)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Fires Per Year by Cause",
       x = "Fire Cause",
       y = "Number of Fires")
