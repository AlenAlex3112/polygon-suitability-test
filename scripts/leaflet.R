library(leaflet)
library(htmlwidgets)

pal_corr <- colorNumeric(palette = "RdYlBu", domain = c(-1, 1), na.color = "transparent")
pal_density <- colorNumeric(palette = "YlOrRd", domain = map_density_h3$unique_species)
pal_effort <- colorBin(palette = "Purples", domain = map_effort_h3$checklist_count, bins = c(0, 10, 25, 50, 100, 500, 1000, Inf))
pal_rarities <- colorBin(palette = "YlOrBr", domain = map_corr$outlier_count, bins = c(0, 1, 3, 5, 10, 20, Inf))

m <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMapPane("boundary_pane", zIndex = 450) %>% # Forces boundaries to top
  
  addPolygons(data = map, fill = FALSE, color = "black", weight = 3, group = "Boundaries", options = pathOptions(pane = "boundary_pane")) %>%
  addPolygons(data = empty_grids_h3, fillColor = "#333333", fillOpacity = 0.4, weight = 0.5, color = "#444444", group = "No Data Zones") %>%
  
  addPolygons(data = map_corr, fillColor = ~pal_rarities(outlier_count), fillOpacity = 0.8, weight = 1, color = "white", 
              group = "Regional Rarities", label = ~hover_label, highlightOptions = highlightOptions(weight = 3, color = "orange", bringToFront = TRUE)) %>%
  
  addPolygons(data = map_corr, fillColor = ~pal_corr(regional_corr), fillOpacity = 0.8, weight = 1, color = "white", 
              group = "Regional Similarity", label = ~hover_label, highlightOptions = highlightOptions(weight = 3, color = "cyan", bringToFront = TRUE)) %>%
  
  addPolygons(data = map_density_h3, fillColor = ~pal_density(unique_species), fillOpacity = 0.7, weight = 1, color = "white", 
              group = "Species Density", label = ~hover_label) %>%
  
  addPolygons(data = map_effort_h3, fillColor = ~pal_effort(checklist_count), fillOpacity = 0.7, weight = 1, color = "white", 
              group = "Checklist Density", label = ~hover_label) %>%
  
  addLegend(pal = pal_corr, values = c(-1, 1), title = "Similarity Index", position = "bottomright") %>%
  addLegend(pal = pal_rarities, values = map_corr$outlier_count, title = "Rare Bird Count", position = "topright") %>%
  addLegend(pal = pal_density, values = map_density_h3$unique_species, title = "Species Count", position = "bottomleft") %>%
  addLegend(pal = pal_effort, values = map_effort_h3$checklist_count, title = "Checklists", position = "topleft") %>%
  
  addLayersControl(overlayGroups = c("Regional Rarities", "Regional Similarity", "Species Density", "Checklist Density", "No Data Zones", "Boundaries"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("Regional Similarity", "Species Density", "Checklist Density", "No Data Zones"))

saveWidget(m, file = filename,".html", selfcontained = TRUE)