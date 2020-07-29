# FINAL QUERY FOR ESTATED DATA (SCV)

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

scv <- read.csv("data/processed/scv_gc.csv")

# query for scv data
df_list <- lapply(51:100, function(i) {
  
  request <- GET(url = path,
                 query = list(
                   token = token,
                   combined_address = scv$customer_address[i]
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


estated_scv_51to100 <- do.call(smartbind, df_list)

df_list <- lapply(101:nrow(scv), function(i) {
  
  request <- GET(url = path,
                 query = list(
                   token = token,
                   combined_address = scv$customer_address[i]
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

estated_scv_therest <- do.call(smartbind, df_list)
estated_scv_51toall <- rbind(estated_scv_51to100, estated_scv_therest)

write.csv(estated_scv_51toall, "data/processed/estated_51rest.csv")

# test call in 03_estated.R missed rows 4, 34, and 50; calling them now:
df_list <- lapply(c(4, 34, 50), function(i) {
  
  request <- GET(url = path,
                 query = list(
                   token = token,
                   combined_address = scv$customer_address[i]
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

estated_scv_select <- do.call(smartbind, df_list)

# writing to csv and then adding them manual to final estated_scv data
write.csv(estated_scv_select, "data/processed/estated_scv_select.csv")
