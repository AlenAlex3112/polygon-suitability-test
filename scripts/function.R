
calc_similarity <- function(target_h3, target_poly, grid_df, poly_df) {
  local_sp <- grid_df[grid_df$h3_id == target_h3, c("COMMON.NAME", "frequency")]
  poly_master <- poly_df[poly_df$polygon_id == target_poly, c("COMMON.NAME", "frequency")] %>%
    rename(poly_freq = frequency)
  
  merged <- inner_join(local_sp, poly_master, by = "COMMON.NAME")
  if (nrow(merged) < 5) return(NA)
  
  merged <- merged %>%
    mutate(local_rank_new = min_rank(desc(frequency)),
           poly_rank_new = min_rank(desc(poly_freq)))
  
  return(cor(merged$local_rank_new, merged$poly_rank_new, method = "spearman"))
}

calc_outliers <- function(target_h3, target_poly, grid_df, poly_df) {
  local_species <- grid_df %>% filter(h3_id == target_h3) %>% pull(COMMON.NAME)
  poly_typical  <- poly_df %>% filter(polygon_id == target_poly) %>% pull(COMMON.NAME)
  outliers <- setdiff(local_species, poly_typical)
  return(tibble(outlier_count = length(outliers),
                outlier_list = if(length(outliers) > 0) paste(sort(outliers), collapse = ", ") else "None"))
}

comparison_tasks <- grid_stats %>% distinct(h3_id, polygon_id)

results <- comparison_tasks %>%
  mutate(regional_corr = map2_dbl(h3_id, polygon_id, ~calc_similarity(.x, .y, grid_stats, polygon_stats))) %>%
  filter(!is.na(regional_corr)) %>%
  left_join(
    comparison_tasks %>% group_by(h3_id, polygon_id) %>% 
      reframe(calc_outliers(h3_id, polygon_id, grid_stats, polygon_stats)),
    by = c("h3_id", "polygon_id")
  )

polygon_lookup <- map %>% st_drop_geometry() %>% select(polygon_id = id, subregion) %>% distinct()

word_safe_wrap <- function(x, width = 50) {
  if (is.na(x) || x == "None" || x == "") return("None")
  paste(strwrap(x, width = width), collapse = "<br>")
}

unified_stats <- grid_counts %>%
  left_join(grid_effort, by = "h3_id") %>%
  left_join(results %>% select(h3_id, regional_corr, polygon_id, outlier_count, outlier_list), by = "h3_id") %>%
  left_join(polygon_lookup, by = "polygon_id") %>%
  mutate(
    subregion_name = ifelse(is.na(subregion), "Unknown", subregion),
    wrapped_rarities = sapply(outlier_list, word_safe_wrap, width = 50),
    hover_label = glue(
      "<b>{subregion_name}</b><br><hr style='margin:3px 0;'>",
      "<b>Species:</b> {unique_species}<br><b>Checklists:</b> {checklist_count}<br>",
      "<b>Similarity Index:</b> {ifelse(is.na(regional_corr), 'N/A', round(regional_corr, 2))}<br>",
      "<p style='color: #d9534f; margin: 5px 0 0 0;'><b>Regional Rarities ({outlier_count}):</b></p>",
      "<i style='font-size: 0.85em; color: #555;'>{wrapped_rarities}</i>"
    ) %>% map(htmltools::HTML)
  )

map_corr <- cell_to_polygon(results$h3_id, simple = FALSE) %>%
  rename(h3_id = h3_address) %>% left_join(unified_stats, by = "h3_id")

map_density_h3 <- cell_to_polygon(grid_counts$h3_id, simple = FALSE) %>%
  rename(h3_id = h3_address) %>% left_join(unified_stats, by = "h3_id")

map_effort_h3 <- cell_to_polygon(grid_effort$h3_id, simple = FALSE) %>%
  rename(h3_id = h3_address) %>% left_join(unified_stats, by = "h3_id")

empty_grids_h3 <- cell_to_polygon(setdiff(h3_ids_master, grid_counts$h3_id), simple = FALSE)

