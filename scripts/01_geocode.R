# geocode Santa Clarita water data

library(tidygeocoder)
library(lubridate)
library(dplyr)
library(magrittr)

# loading CSVs
scv_part <- read.csv('data/raw/scv_participation.csv')
turf_rep <- read.csv('data/raw/turf_replacement.csv')

# converting column containing dates of completion to date class
scv_part$date_of_completion <- ymd(scv_part$date_of_completion)
turf_rep$date_of_completion <- mdy(turf_rep$date_of_completion)


# concatenating address for turf replacement
turf_rep$customer_address <- paste(turf_rep$installation_address, turf_rep$installation_city, turf_rep$installation_state, turf_rep$installation_zip)

# geocoding
scv_part %<>% geocode(customer_address, method = 'census')
turf_rep %<>% geocode(customer_address, method = 'census')

# number of rows unsuccessfully geocoded?
nrow(scv_part[is.na(scv_part$lat), ])
nrow(turf_rep[is.na(turf_rep$lat), ])

# trying to geocode with OSM
scvNA <- scv_part[is.na(scv_part$lat), ]
turfNA <- turf_rep[is.na(turf_rep$lat), ]

scvNA %<>% geocode(customer_address, method = 'osm')

# write CSVs

write.csv(scv_part, 'data/processed/scv_gc.csv')
write.csv(turf_rep, 'data/processed/turf_gc.csv')

# NOTE: geocode unsuccessful entries later (try cascade method before going to manual geocode)

