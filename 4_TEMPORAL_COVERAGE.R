#-------------------------------------------------------------------------------
#TIME
#-------------------------------------------------------------------------------
#temporal sampling intensity
#temporal dist coloured by data source stacked bar plot
library(dplyr)
library(ggplot2)
library(ggpattern)
library(scales)
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

grid_coast_cov <- grid_coast_cov %>%
  left_join(
    grid_cov %>%
      st_drop_geometry() %>%
      select(grid_id, depth_zone),
    by = "grid_id"
  )

time_summary <- grid_coast_cov %>%
  st_drop_geometry() %>%
  group_by(depth_zone) %>%
  summarise(
    mean_years_sampled   = mean(n_years_sampled, na.rm = TRUE),
    median_years_sampled = median(n_years_sampled, na.rm = TRUE),
    .groups = "drop"
  )
#-------------------------------------------------------------------------------
time_summary 
#depth_zone.    mean_years_sampled median_years_sampled
#Inshore                 29.0                    30
# Offshore                 9.09                    3
# NA                      33                      33
#-------------------------------------------------------------------------------
time_summary_coast <- grid_coast_cov %>%
  st_drop_geometry() %>%
  filter(!is.na(coast)) %>%   
  group_by(coast) %>%
  summarise(
    mean_years_sampled   = mean(n_years_sampled, na.rm = TRUE),
    median_years_sampled = median(n_years_sampled, na.rm = TRUE),
    .groups = "drop"
  )

time_summary_coast
#-------------------------------------------------------------------------------
#  coast mean_years_sampled median_years_sampled
#east               8.31                    3
#south              24.5                    28
# west               14.6                     5
#-------------------------------------------------------------------------------
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

p_time_sampling_intensity <- ggplot() +
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

p_time_sampling_intensity
#-------------------------------------------------------------------------------
ggsave("figure10_temporal_intensity.png", p_time_sampling_intensity,
       width = 8, height = 6, dpi = 600)
#-------------------------------------------------------------------------------
# time coverage per source: 
year_source <- final_dat %>%
  filter(!is.na(year), !is.na(source)) %>%
  mutate(year = as.integer(year)) %>%
  count(year, source, name = "n")

year_source <- year_source %>%
  mutate(source = factor(source, levels = c(
    "LINEFISH","DEM_TRAWL","CAPFISH","MW_TRAWL","BRUV","MUSEUM","LITERATURE","INAT"
  )))

p_time_source <- ggplot(year_source, aes(x = year, y = n, fill = source)) +
  geom_col(width = 0.9, color = "black", linewidth = 0.15) +
  scale_fill_manual(
    values = c(
      "LINEFISH"   = "lightblue",
      "DEM_TRAWL"  = "steelblue",
      "CAPFISH"    = "navy",
      "MW_TRAWL"   = "grey20",
      "BRUV"       = "#1b9e77",
      "MUSEUM"     = "#666666",
      "LITERATURE" = "grey80",
      "INAT"       = "white"
    ),
    labels = c(
      "BRUV"        = "BRUV (S)",
      "CAPFISH"     = "CapMarine (O)",
      "DEM_TRAWL"   = "Demersal trawl (S)",
      "INAT"        = "iNaturalist (S)",
      "LINEFISH"    = "Angling (C,O)",
      "LITERATURE"  = "Literature (S,C,O)",
      "MUSEUM"      = "Museum (S)",
      "MW_TRAWL"    = "MW Trawl (O)"
    )
  ) +
  scale_y_continuous(
    trans = "sqrt",
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(
    breaks = seq(1800, 2030, by = 20)
  ) +
  labs(
    x = "Year",
    y = "Number of Records (square-root scale)",
    fill = "Source"
  ) +
  theme_classic(base_family = "serif") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

p_time_source

ggsave("figure11_temporal_dist_source.png", p_time_source,
       width = 8, height = 6, dpi = 600)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------