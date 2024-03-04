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
library(bcmaps)
library(tidyverse)
library(terra)
library(bcdata)
library(dplyr)
View(available_layers())

BEC_MAP<-bec()
mapview(BEC_MAP)


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


BEC_Inside_LacDuBois$area_sqm <- st_area(BEC_Inside_LacDuBois)

BEC_Inside_LacDuBois$area_ha <- BEC_Inside_LacDuBois$area_sqm / 10000
view(BEC_Inside_LacDuBois)

BEC_Zone_Area_Summary <- BEC_Inside_LacDuBois %>%
  summarise(Total_Area_Ha = sum(area_ha))
view(BEC_Zone_Area_Summary)

library(units)


BEC_Inside_LacDuBois$area_ha<- as.numeric(BEC_Inside_LacDuBois[["area_ha"]])
view(BEC_Inside_LacDuBois)

colnames(BEC_Inside_LacDuBois)[8] <- 'BEC_Zones'


ggplot(BEC_Inside_LacDuBois, aes(x = BEC_Zones, y = area_ha, fill = BEC_Zones)) +
  geom_bar(stat = "identity")+
  labs(title = "BEC Cover Across the Lac du Bois Grasslands", x = "BEC Zones", y = "Area (ha)")+
  theme(plot.title = element_text(size = 10, face = "bold"))+
  guides(fill = guide_legend(title = "BEC Zones"))


####Question 2####
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
view(LacDuBois_dem_mask)

FinalMap<-mapview(BEC_Inside_LacDuBois)+ mapview(LacDuBois_dem_mask)

BEC_Inside_LacDuBois_vect <- vect(BEC_Inside_LacDuBois)

elev_values <- extract(LacDuBois_dem_mask, BEC_Inside_LacDuBois_vect, fun = mean, na.rm = TRUE)


if (any(is.na(elev_values))) {elev_values <- elev_values[!is.na(elev_values)]}


elev_values[is.na(elev_values)] <- -9999

BEC_Inside_LacDuBois$mean_elevation <- elev_values
view(BEC_Inside_LacDuBois)


Map_Colour_Subzone<-mapview(BEC_Inside_LacDuBois,zcol = "SUBZONE", legend = TRUE)
print(Map_Colour_Subzone)
