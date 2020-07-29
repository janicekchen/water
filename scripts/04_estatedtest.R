# TESTING LOOP WITH ESTATED SANDBOX API

library(tidyverse); library(httr); library(jsonlite); library(gtools)

# defining path and token
path <- "https://sandbox.estated.com/v4/property"
token <- "g8Cz2ZHWVbdL4bQOePZdYXoQ2CAA6a"

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

sample <- read.csv("data/processed/estated_test.csv") %>%
  mutate(address_cc = paste(paste(Address, City, State, sep = ", "), Zip.Code, sep = " " ))


# function to query API and wrangle into list of dataframes
df_list <- lapply(1:nrow(sample), function(i) {
  
  request <- GET(url = path,
                 query = list(
                   token = token,
                   combined_address = sample$address_cc[i]
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


sample_dfs <- do.call(smartbind, df_list)


