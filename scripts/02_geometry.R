# joining to city boundaries
# open questions: what units are cities divided into? what data points are available for these boundaries?

library(tigris)
library(tidycensus)
library(tidyverse)
library(magrittr)
library(sf)
library(leaflet)
library(mapview)
library(htmltools)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
census_api_key("0ce53e1ad57e9d3f9f276464e121cc40db4312ac")

# loading CA census tracts
cal_tracts <- tracts("CA", cb = TRUE)

# read in Santa Clarita shapefile
sc_bounds <- st_read("data/raw/city_boundary_scag_2010/city_boundary_scag_2010.shp") %>% 
  filter(CITY == "Santa Clarita")

# # loading boundary for LA CBSA
# cb <- core_based_statistical_areas(cb = TRUE)
# LA_cbsa <- cb %>% filter(grepl("Los Angeles", NAME))
# 
# # clipping CA tracts to LA CBSA
# LA_tracts <- st_join(cal_tracts, LA_cbsa, join = st_within)
# 
# join_matrix <- st_within(cal_tracts, LA_cbsa, sparse = FALSE)

# reprojecting
sc_bounds %<>% st_transform(st_crs(cal_tracts))

## this join here shows the greater tract polygons too 
#sc_tracts <- cal_tracts[sc_bounds, ]

# clipping cal_tracts object into Santa Clarita city boundaries 
sc_tracts <- st_intersection(cal_tracts, sc_bounds)

# ggplot() +
#   geom_sf(data = sc_tracts, color = "black") +
#   geom_sf(data = sc_bounds, color = "blue") 

# retrieving census data for LA county
# first, loading variable list
v18 <- load_variables(2018, "acs5", cache = TRUE)
# write.csv(v18, "data/processed/2018acs_vars.csv")

# total white, total pop, median household income, aggregate # of vehicles used for commuting, no. of households, median age, owner-occupied, bachelor's, graduate
la_acs <- get_acs(geography = "tract", variables =c("B02001_002", 	"B01003_001", "B19019_001", "B08015_001", "B08201_001", "B01002_001", "B25106_002", "B06009_005", "B06009_006"), state = "CA", geometry = TRUE, output = "wide")

# clipping to Santa Clarita & selecting only ACS columns
sc_acs <- st_intersection(la_acs, sc_bounds) %>%
  select(1:20) 

names(sc_acs) <- c("GEOID", "NAME", "total_white", "tw_moe", "total_pop", "tp_moe", "mhi", "mhi_moe", "vehicles", "veh_moe", "no_house", "nh_moe", "med_age", "ma_moe", "owner_occ", "oo_moe", "bach", "bach_moe", "grad", "grad_moe", "geometry")



## WATER DATA
# read in water data
scv <- read.csv("data/processed/scv_gc.csv") 
  # filter(is.na(long) == FALSE) %>%
  # st_as_sf(coords = c("long", "lat"))
turf <- read.csv("data/processed/turf_gc.csv") 
  # filter(is.na(long) == FALSE) %>%
  # st_as_sf(coords = c("long", "lat"))

# setting CRS for water data
# st_crs(scv) <- st_crs(sc_acs)
# st_crs(turf) <- st_crs(sc_acs)

sc_acs %<>% filter(GEOID != "06037920200")
scv %<>% filter(X != 143)

# ggplot() +
#   geom_sf(data = sc_acs, aes(fill = total_white / total_pop), color = "white", size = 0.2) +
#   geom_sf(data = sc_bounds, color = "black", fill = NA) +
#   geom_sf(data = scv, size = 0.1)

# standardize data 
sc_acs$white_norm <- (sc_acs$total_white / sc_acs$total_pop) * 100
sc_acs$veh_norm <- sc_acs$vehicles / sc_acs$no_house
sc_acs$oo_norm <- (sc_acs$owner_occ / sc_acs$no_house)*100
sc_acs$bach_norm <- (sc_acs$bach / sc_acs$total_pop) * 100
sc_acs$grad_norm <- (sc_acs$grad / sc_acs$total_pop) * 100
sc_acs$house_size <- sc_acs$total_pop / sc_acs$no_house

pal <- colorNumeric("Greys", st_zm(sc_acs)$white_norm)
pal2 <- colorNumeric("Greys", st_zm(sc_acs)$mhi)
pal3 <- colorNumeric("Greys", st_zm(sc_acs)$veh_norm)
pal4 <- colorNumeric("Greys", st_zm(sc_acs)$oo_norm)
pal5 <- colorNumeric("Greys", st_zm(sc_acs)$med_age)
pal6 <- colorNumeric("Greys", st_zm(sc_acs)$house_size)

#3969AC,#F2B701,#E73F74,#80BA5A
ieua_grp <-  "<span style='color: #3969AC;'><b>IEUA Turf Replacement</b></span>" %>%
  lapply(htmltools::HTML) 
scv_smart <- "<span style='color: #F2B701;'><b>SCV Smart Controller</b></span>" %>%
  lapply(htmltools::HTML) 
scv_poolcover <- "<span style='color: #E73F74;'><b>SCV Pool Cover</b></span>" %>%
  lapply(htmltools::HTML) 
scv_turf <- "<span style='color: #80BA5A;'><b>SCV Turf Conversion</b></span>" %>%
  lapply(htmltools::HTML) 


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  ### POLYGONS ###
  addPolygons(data = st_zm(sc_acs), fillColor = ~colorQuantile("Greys", white_norm)(white_norm), color = "black", fillOpacity = 0.5, weight = 1, group = "White population (%)", label = lapply(paste0("<b>White population:</b> ", st_zm(sc_acs)$white_norm, "%"), htmltools::HTML)) %>%
  addPolygons(data = st_zm(sc_acs), fillColor = ~colorNumeric("Greys", mhi)(mhi), color = "black", fillOpacity = 0.5, weight = 1, group = "Median Household Income ($)", label = lapply(paste0("<b>Median Household Income:</b> $", st_zm(sc_acs)$mhi), htmltools::HTML)) %>%
  addPolygons(data = st_zm(sc_acs), fillColor = ~colorNumeric("Greys", veh_norm)(veh_norm), color = "black", fillOpacity = 0.5, weight = 1, group = "Average no. of vehicles per household", label = lapply(paste0("<b>Avg. no. of vehicles per household:</b><br>", st_zm(sc_acs)$veh_norm), htmltools::HTML)) %>%
  addPolygons(data = st_zm(sc_acs), fillColor = ~colorNumeric("Greys", oo_norm)(oo_norm), color = "black", fillOpacity = 0.5, weight = 1, group = "Owner-occupied households (%)", label = lapply(paste0("<b>% of Owner-occupied housing:</b> ", st_zm(sc_acs)$oo_norm, "%"), htmltools::HTML)) %>%
  addPolygons(data = st_zm(sc_acs), fillColor = ~colorNumeric("Greys", med_age)(med_age), color = "black", fillOpacity = 0.5, weight = 1, group = "Median Age", label = lapply(paste0("<b>Median Age:</b> ", st_zm(sc_acs)$med_age), htmltools::HTML)) %>%
  addPolygons(data = st_zm(sc_acs), fillColor = ~colorNumeric("Greys", house_size)(house_size), color = "black", fillOpacity = 0.5, weight = 1, group = "Household Size", label = lapply(paste0("<b>Avg. household size:</b> ", st_zm(sc_acs)$house_size), htmltools::HTML)) %>%
  ### LEGENDS ###
  addLegend(data = st_zm(sc_acs), values = ~white_norm, group = "White population (%)", position = "bottomleft", pal = pal, title = "White population (%)") %>%
  addLegend(data = st_zm(sc_acs), values = ~mhi, group = "Median Household Income ($)", position = "bottomleft", pal = pal2, title = "Median Household Income ($)") %>%
  addLegend(data = st_zm(sc_acs), values = ~veh_norm, group = "Average no. of vehicles per household", position = "bottomleft", pal = pal3, title = "Average no. of vehicles per household") %>%
  addLegend(data = st_zm(sc_acs), values = ~oo_norm, group = "Owner-occupied households (%)", position = "bottomleft", pal = pal4, title ="Owner-occupied households (%)") %>%
  addLegend(data = st_zm(sc_acs), values = ~med_age, group = "Median Age", position = "bottomleft", pal = pal5, title = "Median Age") %>%
  addLegend(data = st_zm(sc_acs), values = ~house_size, group = "Household Size", position = "bottomleft", pal = pal6, title = "Household Size") %>%
  ### WATER DATA ###
  addCircles(data = turf, lng = turf$long, lat = turf$lat, stroke = FALSE, fillOpacity = 0.8, radius = 50, color = "#3969AC", group = ieua_grp) %>%
  addCircles(data = scv[scv$program == "Smart Controller", ], stroke = FALSE, fillOpacity = 0.8, radius = 50, color = "#F2B701", group = scv_smart) %>%
  addCircles(data = scv[scv$program == "Pool Cover", ], stroke = FALSE, fillOpacity = 0.8, radius = 50, color = "#E73F74", group = scv_poolcover) %>%
  addCircles(data = scv[scv$program == "Turf Conversion", ], stroke = FALSE, fillOpacity = 0.8, color = "#80BA5A", radius = 50, group = scv_turf) %>%
  addLayersControl(baseGroups = c(), overlayGroups = c(ieua_grp, scv_smart, scv_poolcover, scv_turf, "Median Household Income ($)", "White population (%)", "Average no. of vehicles per household", "Owner-occupied households (%)", "Median Age", "Household Size")) %>%
  hideGroup(c("White population (%)", "Average no. of vehicles per household", "Owner-occupied households (%)", "Median Age", "Household Size", scv_smart, scv_poolcover, scv_turf))




?addLegend
sc_acs <- st_transform(sc_acs, "+proj=longlat +datum=WGS84")

?addPolygons
