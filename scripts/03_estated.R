### PREPARING SAMPLE FOR MO ####


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

# read in data:
scv <- read.csv('data/processed/scv_gc.csv')
scv_noNA <- scv[complete.cases(scv), ]

# parseEstated <- function(rows, data) {
#   df_list <- lapply(rows, function(i) {
#     
#     request <- GET(url = path,
#                    query = list(
#                      token = token,
#                      combined_address = data$customer_address[i]
#                    )) %>%
#       content(as = "text", encoding = "UTF-8") %>%
#       fromJSON(flatten = FALSE) %>%
#       simple_rapply(function(x) if (is.null(x)) NA else x)
#     
#     df <- request$data[c(2, 4, 5, 9)] %>%
#       unlist() %>%
#       t() %>%
#       data.frame() %>%
#       mutate(address = paste(address.formatted_street_address, address.city, address.state, address.zip_code, sep = ", ")) %>%
#       select(c(address, year_built = structure.year_built, bed_count = structure.beds_count, bath_count = structure.baths, heating_type = structure.heating_type, heatfuel_type = structure.heating_fuel_type, ac_type = structure.air_conditioning_type, area_sqft = structure.total_area_sq_ft, prop_value = valuation.value, own_occupied = owner.owner_occupied)) 
#     
#     return(df)
#   })
# }

df_list <- lapply(6:50, function(i) {
  
  request <- GET(url = path,
                 query = list(
                   token = token,
                   combined_address = scv_noNA$customer_address[i]
                 )) %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON(flatten = FALSE) %>%
    simple_rapply(function(x) if (is.null(x)) NA else x)
  
  if (is.na(request$data)) {
    
    df <- data.frame(matrix(data = NA, nrow = 1, ncol = 10))
    
  } else {
    
    df <- request$data[c(2, 4, 5, 9)] %>%
      unlist() %>%
      t() %>%
      data.frame() %>%
      mutate(address = paste(address.formatted_street_address, address.city, address.state, address.zip_code, sep = ", ")) %>%
      select(c(address, year_built = structure.year_built, bed_count = structure.beds_count, bath_count = structure.baths, heating_type = structure.heating_type, heatfuel_type = structure.heating_fuel_type, ac_type = structure.air_conditioning_type, area_sqft = structure.total_area_sq_ft, prop_value = valuation.value, own_occupied = owner.owner_occupied)) 
    
  }
  
  return(df)
  
})

# storing first 5 queries here
temp_dflist <- df_list

# combining first 5 queries with next 45 queries
df_list_combine <- c(df_list, temp_dflist)

all_estated <- do.call(smartbind, df_list_combine) %>%
  select(1:10) %>%
  slice(-43)

write.csv(all_estated, "data/processed/estated_sample.csv")


