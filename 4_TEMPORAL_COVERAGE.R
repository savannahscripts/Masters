#4.temporal coverage 
#-----temporal sampling intensity
#-----temporal dist coloured by data source stacked bar plot

#-------------------------------------------------------------------------------
# TEMPORAL SAMPLING INTENSITY (per grid cell)
#-------------------------------------------------------------------------------
grid_coast_lookup <- grid_sa %>%
  mutate(
    lon = st_coordinates(st_centroid(geometry))[,1],
    coast = case_when(
      lon < 20 ~ "West",
      lon >= 20 & lon < 27 ~ "South",
      lon >= 27 ~ "East"
    )
  ) %>%
  st_drop_geometry() %>%
  select(grid_id, coast)


grid_time <- final_dat %>%
  filter(!is.na(species), !is.na(year)) %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(
    n_years_sampled = n_distinct(year),
    first_year = min(year),
    last_year  = max(year),
    .groups = "drop"
  )

names(final_dat)

grid_time_map <- grid_sa %>%
  left_join(grid_time, by = "grid_id") %>%
  mutate(
    n_years_sampled = replace_na(n_years_sampled, 0)
  )

grid_time_map <- grid_time_map %>%
  left_join(
    grid_cov %>%
      st_drop_geometry() %>%
      select(grid_id, depth_zone),
    by = "grid_id"
  ) %>%
  left_join(
    grid_coast_lookup,
    by = "grid_id"
  )

final_dat %>%
  st_drop_geometry() %>%
  distinct(grid_id, coast) %>%
  count(grid_id) %>%
  filter(n > 1)


grid_coast_cov <- grid_coast_cov %>%
  left_join(grid_time, by = "grid_id")

time_summary <- grid_coast_cov %>%
  st_drop_geometry() %>%
  group_by(depth_zone) %>%
  summarise(
    mean_years_sampled   = mean(n_years_sampled, na.rm = TRUE),
    median_years_sampled = median(n_years_sampled, na.rm = TRUE),
    .groups = "drop"
  )

#depth_zone mean_years_sampled median_years_sampled
#Inshore                 29.0                    30
# Offshore                 9.07                    3
# NA                      33                      33


grid_time_map <- grid_time_map %>%
  mutate(
    years_bin = case_when(
      n_years_sampled == 0  ~ "0",
      n_years_sampled == 1  ~ "1",
      n_years_sampled <= 3  ~ "2–3",
      n_years_sampled <= 5  ~ "4–5",
      n_years_sampled <= 10 ~ "6–10",
      n_years_sampled > 10  ~ ">10"
    )
  )


p_time <- ggplot() +
  
  geom_sf(data = sa, fill = "grey90", color = "black", linewidth = 0.2) +
  geom_sf(data = eez_sa, fill = NA, color = "red", linewidth = 0.4) +
  
  geom_sf(data = grid_time_map, aes(fill = years_bin), color = NA) +
  
  scale_fill_manual(
    values = c(
      "0"    = "grey90",
      "1"    = "#c6dbef",
      "2–3"  = "#9ecae1",
      "4–5"  = "#6baed6",
      "6–10" = "#3182bd",
      ">10"  = "#08519c"
    ),
    name = "Years sampled"
  ) +
  
  coord_sf(
    crs = 4326,
    xlim = c(10, 40),
    ylim = c(-40, -25),
    expand = FALSE
  ) +
  
  theme_classic(base_family = "serif") +
  theme(
    legend.position = "right",
    axis.title = element_blank()
  ) 

p_time

ggsave("FIG_temporal_intensity.png", p_time,
       width = 8, height = 6, dpi = 600)


source_time <- final_dat %>%
  filter(!is.na(year)) %>%
  count(year, source)

p_source_time <- ggplot(source_time, aes(x = year, y = n, fill = source)) +
  geom_col() +
  
  scale_fill_viridis_d(option = "C") +
  
  theme_classic(base_family = "serif") +
  theme(
    legend.position = "right"
  ) +
  
  labs(
    title = "Temporal distribution of records by data source",
    x = "Year",
    y = "Number of records"
  )

p_source_time #change scale

time_intensity <- final_dat %>%
  filter(!is.na(year)) %>%
  count(year)

p_time_int <- ggplot(time_intensity, aes(x = year, y = n)) +
  geom_line(linewidth = 0.8) +
  
  theme_classic(base_family = "serif") +
  
  labs(
    title = "Sampling intensity over time",
    x = "Year",
    y = "Number of records"
  )

p_time_int

