
library(readxl)
library(sf)
library(tidyverse)
library(mapview)
library(bcmaps)
library(tidyverse)
library(terra)
library(bcdata)
library(dplyr)
library(units)

View(available_layers())

BEC_MAP<-bec()
# mapview(BEC_MAP)


park_search_results <- bcdc_search("BC Parks Ecological Reserves Protected Areas")
View(park_search_results)

protected_areas <- bcdc_get_data(record = "1130248f-f1a3-4956-8b2e-38d29d3e4af7")
view(protected_areas)

LacDuBois <- protected_areas %>%
  filter(PROTECTED_LANDS_NAME == "LAC DU BOIS GRASSLANDS PROTECTED AREA")
mapview(LacDuBois)

BEC_Inside_LacDuBois<- st_intersection(BEC_MAP, LacDuBois)

mapview(BEC_Inside_LacDuBois)
view(BEC_Inside_LacDuBois)


## What you did above works, however it's a bit more concise to use the 
## bcdc_query_geodata() function and then provide filters before downloading data; 
## this prevents you needing to download the whole dataset to your PC as it will
## do some filtering on their servers before it sends you the filtered dataset.

#########
## 4/4 ##
#########

#####Calculate the total area in hectares for all 9 BEC zones within the Lac Du Bois Grasslands#####

BEC_Inside_LacDuBois$area_sqm <- st_area(BEC_Inside_LacDuBois)

BEC_Inside_LacDuBois$area_ha <- BEC_Inside_LacDuBois$area_sqm / 10000
view(BEC_Inside_LacDuBois)

BEC_Inside_LacDuBois$area_ha<- as.numeric(BEC_Inside_LacDuBois[["area_ha"]])
view(BEC_Inside_LacDuBois)

BEC.Hectares<- BEC_Inside_LacDuBois[, 40]
view(BEC.Hectares)

BEC_Zone_Area_Summary <- BEC_Inside_LacDuBois %>%
  summarise(Total_Area_Ha = sum(area_ha))
view(BEC_Zone_Area_Summary)

#########
## 2/2 ##
#########

#####BEC Zones by colour in Lac Du Bois#####

colnames(BEC_Inside_LacDuBois)[8] <- 'BEC_Zones'

Map_Colour_BEC<-mapview(BEC_Inside_LacDuBois,zcol = "BEC_Zones", legend = TRUE)
print(Map_Colour_BEC)


#####Bar plot of total area covered by each BEC zone#####
ggplot(BEC_Inside_LacDuBois, aes(x = BEC_Zones, y = area_ha, fill = BEC_Zones)) +
  geom_bar(stat = "identity")+
  labs(title = "BEC Cover Across the Lac du Bois Grasslands", x = "BEC Zones", y = "Area (ha)")+
  theme(plot.title = element_text(size = 10, face = "bold"))+
  guides(fill = guide_legend(title = "BEC Zones"))

#########
## 5/5 ##
#########

#####Creating a dem of the Lac Du Bois Grasslands and creating a map coloured by BEC zone#####
LacDuBois_dem <- cded_terra(BEC_Inside_LacDuBois)
LacDuBois_dem
plot(LacDuBois_dem)

LacDuBois_dem_albers <- project(LacDuBois_dem, "epsg:3005")
res(LacDuBois_dem_albers)

resamp_grid <- rast(ext(LacDuBois_dem_albers), res = 20, crs = "epsg:3005")
LacDuBois_dem_albers <- resample(LacDuBois_dem_albers, resamp_grid)
plot(LacDuBois_dem_albers)

LacDuBois_dem_mask <- mask(LacDuBois_dem_albers, vect(BEC_Inside_LacDuBois))
plot(LacDuBois_dem_mask, colNA = "grey")

FinalMap<-mapview(BEC_Inside_LacDuBois)+ mapview(LacDuBois_dem_mask)

BEC_Inside_LacDuBois_vect <- vect(BEC_Inside_LacDuBois)

elev_values <- extract(LacDuBois_dem_mask, BEC_Inside_LacDuBois_vect, fun = mean, na.rm = TRUE)

#########
## 4/4 ##
#########


if (any(is.na(elev_values))) {elev_values <- elev_values[!is.na(elev_values)]}


elev_values[is.na(elev_values)] <- -9999

BEC_Inside_LacDuBois$mean_elevation <- elev_values
view(BEC_Inside_LacDuBois)

BEC.Mean.Elevation<- BEC_Inside_LacDuBois[, 41]
view(BEC.Mean.Elevation)


ggplot(BEC_Inside_LacDuBois, aes(fill = mean_elevation$elevation)) +
  geom_sf() +
  scale_fill_gradient(low = "blue", high = "orange") +
  theme_minimal()


#####Colour Lac Du Bois by BEC subzone#####
Map_Colour_Subzone<-mapview(BEC_Inside_LacDuBois,zcol = "SUBZONE", legend = TRUE)
print(Map_Colour_Subzone)

#########
## 2/2 ##
#########


## Total:

###########
## 17/17 ##
###########

