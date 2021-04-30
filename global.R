
library(shiny)
library(leaflet)
library(reactable)
library(here)
library(dplyr)
library(purrr)
library(readr)
library(sf)
library(shinyBS)
library(shinythemes)
library(shinyalert)
library(here)





barrios_bcn <- read_rds(paste0(here(), "/data/2019-combined.RData"))


barrios_bcn <- barrios_bcn %>% 
  mutate(sale_price_m2 = round(sale_price_m2, 0),
         price_to_rent = round(price_to_rent, 1),
         rented_estimate = round(rented_estimate, 0),
         percent_sold = round(percent_sold, 2),
         percent_rented = round(percent_rented, 2),
         sale_price_flat = sale_price_flat * 1000)


# bounding box for BCN
bbox_bcn <- c(2.05, 41.32, 2.23, 41.47)



