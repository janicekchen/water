## FINAL DATA QUERY FOR CENSUS 

library(tigris)
library(tidycensus)
library(tidyverse)
library(magrittr)
library(sf)
library(leaflet)
library(geojsonio)
library(lubridate)
library(gganimate)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)


# reading in shapefiles for cities
scv <- geojson_read("data/processed/scv_boundary.geojson", parse = TRUE, what = "sp") %>%
  st_as_sf() %>% 
  st_transform(crs = 3857)

ieua <- geojson_read("data/processed/ieua_servicearea.geojson", parse = TRUE, what = "sp") %>%
  st_as_sf() %>%
  st_transform(crs = 3857)

ieua_cities <- geojson_read("data/processed/ieua_cities.geojson", parse = TRUE, what = "sp") %>%
  st_as_sf() %>%
  st_transform(crs = 3857)

# retrieving census data, with geometry
# total white, total pop, total above bach above 25, totla above 25, housing units
ca_acs <- get_acs(geography = "block group", variables = c(total_white = "B02001_002", 	total_pop = "B01003_001",  mhi = "B19013_001", bach_ge25 =	"B15012_001", no_hu = "B25001_001", avg_hs = "B25010_001", med_age = "B01002_001", ge25_1 = "B29001_002", ge25_2 = "B29001_003", ge25_3 = "B29001_004", ge25_4 = "B29001_005"), state = "CA", geometry = TRUE, output = "wide") %>%
  st_transform(crs = 3857)


# summing ge25 rows to get sum of total population over 30 
ca_acs %<>% mutate(ge25E = rowSums(st_set_geometry(select(ca_acs, c(ge25_2E, ge25_3E, ge25_4E)), NULL)), ge25M= rowSums(st_set_geometry(select(ca_acs, c(ge25_2M, ge25_3M, ge25_4M)), NULL))) %>%
  select(c(1:16, 26))


# spatial subsetting CA block groups to repsective shape files 
scv_bg <- ca_acs[scv, , op = st_intersects]
ieua_bg <- ca_acs[ieua, , op = st_intersects]


# WATER DATA WRANGLING

scv_water <- read.csv("data/processed/scv_gc.csv") %>%
  st_as_sf(coords = c(lon = "long", lat = "lat"), remove = FALSE) %>%
  # filtering out Murrieta address because geographic outlier
  filter(!grepl("Murrieta", customer_address)) %>%
  # setting to WGS84 crs
  st_set_crs(4326) %>%
  # transforming to Web Mercator to match other sf objects
  st_transform(3857)

ieua_water <- read.csv("data/processed/turf_gc.csv") %>%
  st_as_sf(coords = c(lon = "long", lat = "lat"), remove = FALSE) %>%
  st_set_crs(4326) %>%
  st_transform(3857)


# joining to block group data
scv_in_bg <- st_join(scv_water, scv_bg, join = st_within)
ieua_in_bg <- st_join(ieua_water, ieua_bg, join = st_within)

# VISUALIZING TO IDENTIFY PATTERNS
ggplot() +
  geom_sf(data = scv_bg, aes(fill = total_whiteE), color = "white", size = 0.1) + 
  geom_sf(data = scv_water, size = 0.05, color = "white")

ggplot() + 
  geom_sf(data = ieua_bg, aes(fill = total_whiteE), color = "white", size = 0.1) +
  geom_sf(data = ieua_water, size = 0.05, color = "white")


# clipping CA block groups to just tracts with program data because not all tracts have enrolled households

scv_bg_clip <- ca_acs[scv_water, , op = st_intersects]
ieua_bg_clip <- ca_acs[ieua_water, , op = st_intersects]

# joining census block group data to scv_water data
scv_in_bgclip <- st_join(scv_water, scv_bg_clip, join = st_within)
ieua_in_bgclip <- st_join(ieua_water, ieua_bg_clip, join = st_within)


# AGGREGATING WATER DATA 
# I will just analyze based on block groups that contain program data. 
# tallying participation in each program 

scv_waterbg <- scv_in_bgclip %>%
  group_by(program, GEOID) %>%
  summarise(program_count = n()) %>%
  st_set_geometry(NULL) %>%
  pivot_wider(names_from = program, values_from = program_count)

# replacing all NAs with 0 
scv_waterbg[is.na(scv_waterbg)] <- 0

# calculating program totals
scv_waterbg %<>% mutate(program_total = rowSums(select(., 2:4))) %>%
  left_join(scv_bg_clip) %>%
  st_as_sf()

ieua_waterbg <- ieua_in_bgclip %>%
  group_by(program, GEOID) %>%
  summarise(program_count = n()) %>%
  st_set_geometry(NULL) %>%
  pivot_wider(names_from = program, values_from = program_count) %>%
  left_join(ieua_bg_clip) %>%
  st_as_sf()

# REGRESSION TIME!
# first, normalizing variables
names(scv_waterbg)[2:4] <- c("pool", "smart", "turf")

scv_waterbg %<>% mutate(poolN = pool / no_huE, smartN = smart / no_huE, turfN = smart / no_huE, progTotalN = program_total / no_huE, whiteN = total_whiteE / total_popE, bachN = bach_ge25E / ge25)

r0 <- progTotalN ~ whiteN + bachN + mhiE + avg_hsE + med_ageE
r1 <- progTotalN ~ avg_hsE
r2 <- progTotalN ~ whiteN
r3 <- progTotalN ~ bachN
r4 <- progTotalN ~ mhiE
r5 <- progTotalN ~ med_ageE

r2 <- program_total ~ no_huE
r3 <- poolN ~ whiteN + bachN + mhiE + avg_hsE + med_ageE
lm0 <- lm(r0, data = scv_waterbg)
lm1 <- lm(r1, data = scv_waterbg)
lm2 <- lm(r2, data = scv_waterbg)
lm3 <- lm(r3, data = scv_waterbg)
lm4 <- lm(r4, data = scv_waterbg)
lm5 <- lm(r5, data = scv_waterbg)
summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)
summary(lm5)
lm1
colnames(scv_waterbg)

# run single variable linear regression for all
# and then multipel lva

ggplot(data = scv_waterbg) + 
  geom_point(aes(x = mhiE, y = progTotalN)) +
  geom_smooth(aes(x = mhiE, y = progTotalN), method = "lm", formula = y~x)

names(ieua_waterbg)[2] <- "turf"
ieua_waterbg %<>% mutate(progTotalN = turf / no_huE, whiteN = total_whiteE / total_popE, bachN = bach_ge25E / ge25)

lm1_ieua <- lm(r0, data = ieua_waterbg)
summary(lm1_ieua)

scv_waterbg_export <- scv_waterbg %>% 
  st_transform(4326)
  
ieua_waterbg_export <- ieua_waterbg %>%
  st_transform(4326)

st_write(scv_waterbg_export, "data/processed/scv_bg.geojson")
st_write(ieua_waterbg_export, "data/processed/ieua_bg2.geojson")

st_bbox(ieua_waterbg_export)[4] - st_bbox(ieua_waterbg_export)[2] + st_bbox(ieua_waterbg_export)[2]
st_bbox(ieua_waterbg_export)[3] - st_bbox(ieua_waterbg_export)[1] + st_bbox(ieua_waterbg_export)[1]


ggplot() +
  geom_sf(data = scv_waterbg_export, aes(color = program_total, fill = program_total))


