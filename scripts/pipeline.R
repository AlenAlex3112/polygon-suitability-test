library(skimmr)
library(sf)
library(h3jsr)
library(dplyr)
library(purrr)
library(tidyverse)

map <- st_read("data/map.geojson", quiet = TRUE) %>%
  filter(str_detect(subregion, regex("Kerala", ignore_case = TRUE))) %>% 
  #Change State Name if required
  filter(!str_detect(subregion, regex("Pelagic|Offshore", ignore_case = TRUE))) %>%
  #Removing Offshore/Pelagic Regions because they are not relevant in this analysis
  st_transform(4326)

h3_ids_master <- map %>%
  polygon_to_cells(res = 7, simple = FALSE) %>%
  unnest(h3_addresses) %>%
  pull(h3_addresses) %>%
  unique()

if (file.exists("data/data.RData") == FALSE) { 
  source("ebd_prep.R")
} else {
  load("data/data.RData") 
}

source("data_prep.R")

source("functions.R")

filename <- "map"
source("leaflet.R")

