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
library(bcdata)



park_search_results <- bcdc_search("BC Parks Ecological Reserves Protected Areas")
View(park_search_results)

protected_areas <- bcdc_get_data(record = "1130248f-f1a3-4956-8b2e-38d29d3e4af7")
view(protected_areas)

LDB <- protected_areas %>%
  filter(PROTECTED_LANDS_NAME == "LAC DU BOIS GRASSLANDS PROTECTED AREA")
mapview(LDB)

fire_search_results <- bcdc_search("Fire")
View(fire_search_results)

fire_points <- bcdc_get_data(record = "e2dadc60-292f-4d98-b42b-56ca9e4fe694")
view(fire_points)

fires_in_LDB <- fire_points[st_intersects(fire_points, LDB, sparse = FALSE), ]

## What you did above works, however it's a bit more concise to use the 
## bcdc_query_geodata() function and then provide filters before downloading data; 
## this prevents you needing to download the whole dataset to your PC as it will
## do some filtering on their servers before it sends you the filtered dataset.

#########
## 4/4 ##
#########

mapview(fires_in_LDB) + mapview(LDB)


fires_per_year <- fires_in_LDB %>%
  group_by(FIRE_YEAR) %>%
  summarise(number_of_fires = n())

#########
## 2/2 ##
#########

fire_cause <- fires_in_LDB %>%
  group_by(FIRE_CAUSE) %>%
  summarise(number_of_fires = n())

#########
## 2/2 ##
#########

mapview(fires_in_LDB, zcol = "FIRE_CAUSE", legend = TRUE) + mapview(LDB)

#########
## 2/2 ##
#########

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

#########
## 5/5 ##
#########


## Total:

###########
## 15/15 ##
###########