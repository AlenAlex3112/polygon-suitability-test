library(skimmr)
library(lubridate)
library(sf)
library(h3jsr)
library(dplyr)
library(purrr)
library(glue)
library(tidyverse)
library(leaflet)
library(htmlwidgets)

# SECTION 1: LOAD & PREPARE SPATIAL DATA ----------------------------------

# Use the Kerala map as the primary region
map <- st_read("data/map.geojson", quiet = TRUE) %>%
  filter(str_detect(subregion, regex("Kerala", ignore_case = TRUE))) %>%
  filter(!subregion %in% c("India--Kerala--Offshore", "India--Kerala--Pelagic")) %>%
  st_transform(4326)

# Generate the master list of H3 cells covering the region
h3_ids <- map %>%
  polygon_to_cells(res = 7, simple = FALSE) %>%
  unnest(h3_addresses) %>%
  pull(h3_addresses) %>%
  unique()

# SECTION 2: LOAD & PREPARE BIRD DATA -------------------------------------

load("data/KL_data.RData") 

# Clean bird data and assign H3 cells
data_req <- data_req %>% 
  filter(ALL.SPECIES.REPORTED == 1) %>%
  select(COMMON.NAME, LATITUDE, LONGITUDE, GROUP.ID) %>%
  mutate(h3_id = point_to_cell(data.frame(LONGITUDE, LATITUDE), res = 7, simple = TRUE))

# Spatial Join to get polygon_id 
unique_lists <- data_req %>%
  distinct(GROUP.ID, LATITUDE, LONGITUDE) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

lists_within_polygon <- st_join(unique_lists, map %>% select(id), join = st_intersects) %>%
  st_drop_geometry() %>%
  select(GROUP.ID, polygon_id = id) 

# Merge polygon IDs back and filter points outside the boundary
data_req <- data_req %>%
  left_join(lists_within_polygon, by = "GROUP.ID") %>% 
  filter(!is.na(polygon_id))

rm(unique_lists, lists_within_polygon)

# SECTION 3: AGGREGATE STATS ----------------------------------------------

# 1. Species counts per cell
grid_counts <- data_req %>%
  group_by(h3_id) %>%  
  reframe(unique_species = n_distinct(COMMON.NAME))

# 2. Checklist counts per cell (Effort)
grid_effort <- data_req %>%
  group_by(h3_id) %>%  
  reframe(checklist_count = n_distinct(GROUP.ID))

# 3. Rank statistics per species per cell (Local)
grid_stats <- data_req %>%
  group_by(h3_id) %>%
  mutate(total_lists = n_distinct(GROUP.ID)) %>%
  ungroup() %>%
  group_by(h3_id, total_lists, COMMON.NAME, polygon_id) %>%
  summarise(detections = n_distinct(GROUP.ID), .groups = "drop") %>%
  mutate(frequency = detections / total_lists) %>%
  group_by(h3_id) %>%
  arrange(desc(frequency)) %>%
  filter(percent_rank(frequency) > 0.05) %>% # Remove bottom 5%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  as.data.frame()

# 4. Rank statistics per species per polygon (Regional)
polygon_stats <- data_req %>%
  group_by(polygon_id) %>%
  mutate(total_lists = n_distinct(GROUP.ID)) %>%
  ungroup() %>%
  group_by(polygon_id, total_lists, COMMON.NAME) %>%
  summarise(detections = n_distinct(GROUP.ID), .groups = "drop") %>%
  mutate(frequency = detections / total_lists) %>%
  group_by(polygon_id) %>%
  arrange(desc(frequency)) %>%
  filter(percent_rank(frequency) > 0.1) %>% # Remove bottom 10%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  as.data.frame()

# SECTION 4: SPECIES SIMILARITY MATH --------------------------------------

calc <- function(target_h3, target_poly, grid_df, poly_df) {

    local_sp <- grid_df[grid_df$h3_id == target_h3, c("COMMON.NAME", "frequency")]
 
   poly_master <- poly_df %>%
    filter(polygon_id == target_poly) %>%
    select(COMMON.NAME, poly_freq = frequency)
 
   merged <- inner_join(local_sp, poly_master, by = "COMMON.NAME")
  if (nrow(merged) < 5) return(NA)
  
  merged <- merged %>%
    mutate(
      local_rank_new = min_rank(desc(frequency)),
      poly_rank_new  = min_rank(desc(poly_freq))
    )
  
  return(cor(merged$local_rank_new, merged$poly_rank_new, method = "spearman"))
}

comparison_tasks <- grid_stats %>% 
  distinct(h3_id, polygon_id)

results <- comparison_tasks %>%
  mutate(regional_corr = map2_dbl(
    .x = h3_id, 
    .y = polygon_id, 
    .f = ~calc(.x, .y, grid_stats, polygon_stats)
  )) %>%
  filter(!is.na(regional_corr))


calc_outliers <- function(target_h3, target_poly, grid_df, poly_df) {
  local_species <- grid_df %>% filter(h3_id == target_h3) %>% pull(COMMON.NAME)
  poly_typical  <- poly_df %>% filter(polygon_id == target_poly) %>% pull(COMMON.NAME)
  
  outliers <- setdiff(local_species, poly_typical)
  
  return(tibble(
    outlier_count = length(outliers),
    # Capture the FULL list here
    outlier_list = if(length(outliers) > 0) paste(sort(outliers), collapse = ", ") else "None"
  ))
}

outlier_results <- comparison_tasks %>%
  group_by(h3_id, polygon_id) %>%
  reframe(calc_outliers(h3_id, polygon_id, grid_stats, polygon_stats))

results <- results %>%
  left_join(outlier_results, by = c("h3_id", "polygon_id"))

# SECTION 5: SPATIAL CONVERSION FOR PLOTTING ------------------------------
# 1. Create a lookup table for Polygon Names
polygon_lookup <- map %>%
  st_drop_geometry() %>%
  select(polygon_id = id, subregion) %>%
  distinct()

# 2. Build the unified_stats using joins (now including outlier data from 'results')
unified_stats <- grid_counts %>%
  left_join(grid_effort, by = "h3_id") %>%
  left_join(results %>% select(h3_id, regional_corr, polygon_id, outlier_count, outlier_list), by = "h3_id") %>%
  left_join(polygon_lookup, by = "polygon_id") %>%
  mutate(
    subregion_name = ifelse(is.na(subregion), "Unknown", subregion),
    
    # Create the HTML label
    hover_label = glue(
      "<div style='width: 250px; font-family: sans-serif;'>",
      "<h4 style='margin:0; color:#2c3e50;'>{subregion_name}</h4>",
      "<hr style='margin:5px 0;'>",
      "<b>Species:</b> {unique_species}<br/>",
      "<b>Checklists:</b> {checklist_count}<br/>",
      "<b>Similarity Index:</b> {ifelse(is.na(regional_corr), 'N/A', round(regional_corr, 2))}<br/>",
      "<p style='margin: 5px 0 2px 0; color: #d9534f;'><b>Regional Rarities ({outlier_count}):</b></p>",
      "<div style='font-size: 0.85em; color: #666; font-style: italic; line-height: 1.2;'>",
      "{outlier_list}",
      "</div>",
      "</div>"
    ) %>% map(htmltools::HTML)
  )

# 3. Re-create spatial objects with the unified labels
map_corr <- cell_to_polygon(results$h3_id, simple = FALSE) %>%
  rename(h3_id = h3_address) %>%
  left_join(unified_stats, by = "h3_id")

map_density_h3 <- cell_to_polygon(grid_counts$h3_id, simple = FALSE) %>%
  rename(h3_id = h3_address) %>%
  left_join(unified_stats, by = "h3_id")

map_effort_h3 <- cell_to_polygon(grid_effort$h3_id, simple = FALSE) %>%
  rename(h3_id = h3_address) %>%
  left_join(unified_stats, by = "h3_id")

empty_h3_ids <- setdiff(h3_ids, grid_counts$h3_id)
empty_grids_h3 <- cell_to_polygon(empty_h3_ids, simple = FALSE)

# SECTION 6: GENERATE LEAFLET MAP -----------------------------------------
pal_density <- colorNumeric(palette = "YlOrRd", domain = map_density_h3$unique_species)
pal_corr    <- colorNumeric(palette = "RdYlBu", domain = c(-1, 1), na.color = "transparent")
effort_bins <- c(0, 10, 25, 50, 100, 500, 1000, Inf)
pal_effort  <- colorBin(palette = "Purples", domain = map_effort_h3$checklist_count, bins = effort_bins)
# --- SECTION 5: Add Rarities Spatial Object ---
# Use a distinct palette (Oranges/Reds) for rarities
pal_rarities <- colorBin(palette = "YlOrBr", domain = map_corr$outlier_count, bins = c(0, 1, 3, 5, 10, 20, Inf))

# --- SECTION 6: Update Leaflet Map ---
m <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Boundaries and No Data
  addPolygons(data = map, fill = FALSE, color = "black", weight = 2, group = "Boundaries") %>%
  addPolygons(data = empty_grids_h3, fillColor = "#333333", fillOpacity = 0.4, weight = 0.5, color = "#444444", group = "No Data Zones") %>%
  
  # NEW Layer: Regional Rarities
  addPolygons(
    data = map_corr, 
    fillColor = ~pal_rarities(outlier_count), 
    fillOpacity = 0.8,
    weight = 1, color = "white", 
    group = "Regional Rarities",
    label = ~hover_label,
    highlightOptions = highlightOptions(weight = 3, color = "orange", bringToFront = TRUE)
  ) %>%
  
  # Layer: Regional Similarity
  addPolygons(
    data = map_corr, fillColor = ~pal_corr(regional_corr), fillOpacity = 0.8,
    weight = 1, color = "white", group = "Regional Similarity",
    label = ~hover_label,
    highlightOptions = highlightOptions(weight = 3, color = "cyan", bringToFront = TRUE)
  ) %>%
  
  # Layer: Species Density
  addPolygons(
    data = map_density_h3, fillColor = ~pal_density(unique_species), fillOpacity = 0.7,
    weight = 1, color = "white", group = "Species Density", 
    label = ~hover_label
  ) %>%
  
  # Layer: Checklist Density
  addPolygons(
    data = map_effort_h3, fillColor = ~pal_effort(checklist_count), fillOpacity = 0.7,
    weight = 1, color = "white", group = "Checklist Density", 
    label = ~hover_label
  ) %>%
  
  # Legends
  addLegend(pal = pal_corr, values = c(-1, 1), title = "Similarity Index", position = "bottomright") %>%
  addLegend(pal = pal_rarities, values = map_corr$outlier_count, title = "Rare Bird Count", position = "topright") %>%
  addLegend(pal = pal_density, values = map_density_h3$unique_species, title = "Species Count", position = "bottomleft") %>%
  
  # Controls
  addLayersControl(
    overlayGroups = c("Regional Rarities", "Regional Similarity", "Species Density", "Checklist Density", "No Data Zones", "Boundaries"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(c("Regional Rarities"))

saveWidget(m, file = "polygon-suitability-test.html", selfcontained = TRUE)

