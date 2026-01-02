
grid_counts <- data_req %>%
  group_by(h3_id) %>%  
  reframe(unique_species = n_distinct(COMMON.NAME))

grid_effort <- data_req %>%
  group_by(h3_id) %>%  
  reframe(checklist_count = n_distinct(GROUP.ID))

grid_stats <- data_req %>%
  group_by(h3_id) %>%
  mutate(total_lists = n_distinct(GROUP.ID)) %>%
  ungroup() %>%
  group_by(h3_id, total_lists, COMMON.NAME, polygon_id) %>%
  summarise(detections = n_distinct(GROUP.ID), .groups = "drop") %>%
  mutate(frequency = detections / total_lists) %>%
  group_by(h3_id) %>%
  arrange(desc(frequency)) %>%
  filter(frequency >= 0.1) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  as.data.frame()

polygon_stats <- data_req %>%
  group_by(polygon_id) %>%
  mutate(total_lists = n_distinct(GROUP.ID)) %>%
  ungroup() %>%
  group_by(polygon_id, total_lists, COMMON.NAME) %>%
  summarise(detections = n_distinct(GROUP.ID), .groups = "drop") %>%
  mutate(frequency = detections / total_lists) %>%
  group_by(polygon_id) %>%
  arrange(desc(frequency)) %>%
  filter(frequency >= 0.01) %>% 
  mutate(rank = row_number()) %>%
  ungroup() %>%
  as.data.frame()