# QUERYING ESTATED DATA FOR IEUA DATASET
library(tidyverse); library(httr); library(jsonlite); library(gtools)

# defining path and token
path <- "https://apis.estated.com/v4/property"
token <- "6dEsc7UMjstJ1PWq6iYXscIcBmIpJn"

# creating function to convert NULL values to NA values
simple_rapply <- function(x, fn)
{
  if(is.list(x))
  {
    lapply(x, simple_rapply, fn)
  } else
  {
    fn(x)
  }
}

ieua <- read.csv("data/processed/turf_gc.csv")

df_list <- lapply(1:5, function(i) {
  
  request <- GET(url = path,
                 query = list(
                   token = token,
                   street_address = ieua$installation_address[i],
                   city = ieua$installation_city[i],
                   state = ieua$installation_state[i],
                   zip_code = ieua$installation_zip[i]
                 )) %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(flatten = FALSE) %>%
    simple_rapply(function(x) if (is.null(x)) NA else x)
  
  if (length(request) < 3) {
    
    df <- data.frame(matrix(data = "error", nrow = 1, ncol = 10,  dimnames = list(c("row"), c("address", "year_built", "bed_count", "bath_count", "heating_type", "heatfuel_type", "ac_type", "area_sqft", "prop_value", "own_occupied"))))
    
  } else if (is.na(request$data)) {
    
    df <- data.frame(matrix(data = request$warnings$code, nrow = 1, ncol = 10, dimnames = list(c("row"), c("address", "year_built", "bed_count", "bath_count", "heating_type", "heatfuel_type", "ac_type", "area_sqft", "prop_value", "own_occupied"))))
    
  } else {
    
    df <- request$data[c(2, 4, 5, 9)] %>%
      unlist() %>%
      t() %>%
      data.frame() %>%
      mutate(address = paste(address.formatted_street_address, address.city, address.state, address.zip_code, sep = ", "))  
    
    print(df$address)
    
    if (is.na(request$data$structure)) {
      df <- df %>%
        mutate(structure.year_built = NA, structure.beds_count = NA, structure.baths = NA, structure.heating_type = NA, structure.heating_fuel_type = NA, structure.air_conditioning_type = NA, structure.total_area_sq_ft = NA)
    }
    
    if (is.na(request$data$valuation)) {
      df <- df %>%
        mutate(valuation.value = NA)
    }
    
    if (is.na(request$data$owner)) {
      df <- df %>%
        mutate(owner.owner_occupied = NA)
    }
    
    
    df <- df %>% select(c(address, 
                          year_built = structure.year_built, 
                          bed_count = structure.beds_count, 
                          bath_count = structure.baths, 
                          heating_type = structure.heating_type, 
                          heatfuel_type = structure.heating_fuel_type, 
                          ac_type = structure.air_conditioning_type, 
                          area_sqft = structure.total_area_sq_ft, 
                          prop_value = valuation.value, 
                          own_occupied = owner.owner_occupied))
    
    
  }
  
  return(df)
  
})

estated_ieua_1to5 <- do.call(smartbind, df_list)

df_list <- lapply(6:nrow(ieua), function(i) {
  
  request <- GET(url = path,
                 query = list(
                   token = token,
                   street_address = ieua$installation_address[i],
                   city = ieua$installation_city[i],
                   state = ieua$installation_state[i],
                   zip_code = ieua$installation_zip[i]
                 )) %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(flatten = FALSE) %>%
    simple_rapply(function(x) if (is.null(x)) NA else x)
  
  if (length(request) < 3) {
    
    df <- data.frame(matrix(data = "error", nrow = 1, ncol = 10,  dimnames = list(c("row"), c("address", "year_built", "bed_count", "bath_count", "heating_type", "heatfuel_type", "ac_type", "area_sqft", "prop_value", "own_occupied"))))
    
  } else if (is.na(request$data)) {
    
    df <- data.frame(matrix(data = request$warnings$code, nrow = 1, ncol = 10, dimnames = list(c("row"), c("address", "year_built", "bed_count", "bath_count", "heating_type", "heatfuel_type", "ac_type", "area_sqft", "prop_value", "own_occupied"))))
    
  } else {
    
    df <- request$data[c(2, 4, 5, 9)] %>%
      unlist() %>%
      t() %>%
      data.frame() %>%
      mutate(address = paste(address.formatted_street_address, address.city, address.state, address.zip_code, sep = ", "))  
    
    print(df$address)
    
    if (is.na(request$data$structure)) {
      df <- df %>%
        mutate(structure.year_built = NA, structure.beds_count = NA, structure.baths = NA, structure.heating_type = NA, structure.heating_fuel_type = NA, structure.air_conditioning_type = NA, structure.total_area_sq_ft = NA)
    }
    
    if (is.na(request$data$valuation)) {
      df <- df %>%
        mutate(valuation.value = NA)
    }
    
    if (is.na(request$data$owner)) {
      df <- df %>%
        mutate(owner.owner_occupied = NA)
    }
    
    
    df <- df %>% select(c(address, 
                          year_built = structure.year_built, 
                          bed_count = structure.beds_count, 
                          bath_count = structure.baths, 
                          heating_type = structure.heating_type, 
                          heatfuel_type = structure.heating_fuel_type, 
                          ac_type = structure.air_conditioning_type, 
                          area_sqft = structure.total_area_sq_ft, 
                          prop_value = valuation.value, 
                          own_occupied = owner.owner_occupied))
    
    
  }
  
  return(df)
  
})

estated_ieua_therest <- do.call(smartbind, df_list)

estated_ieua_all <- rbind(estated_ieua_1to5, estated_ieua_therest)
write.csv(estated_ieua_all, "data/processed/estated_ieua.csv")
