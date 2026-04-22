#3. spatial coverage 
#-----source specific coverage (faceted plot) and numbers
#-----total sampling coverage of eez grid (percentage) including % offshore (source specfifc for table 4)
#-----MPA coverage in % and source specific MPA coverage (Table 5)
#-----MPA, decleration year, species and % records recorded post declaration
#-----sampling coverage plot
#-----bioregions coverage
#-----sampling intensity



# Species coverage per source
#Species coverage per gear/method
#Coverage relative to the total integrated dataset
#Statistical tests
#Plots
#Thesis-ready text

species_by_source <- master_sa |> 
  st_drop_geometry() |> 
  group_by(source) |> 
  summarise(
    n_records = n(),
    n_species = n_distinct(species),
    n_genera  = n_distinct(genus),
    n_families = n_distinct(family)
  ) |> 
  arrange(desc(n_species))

species_by_source


species_by_gear <- master_sa |> 
  st_drop_geometry() |> 
  group_by(gear_grouped) |> 
  summarise(
    n_records = n(),
    n_species = n_distinct(species),
    n_genera  = n_distinct(genus),
    n_families = n_distinct(family)
  ) |> 
  arrange(desc(n_species))

species_by_gear


total_species <- master_sa |> 
  st_drop_geometry() |> 
  summarise(total_species = n_distinct(species)) |> 
  pull(total_species)

total_species

species_by_source <- species_by_source |> 
  mutate(
    perc_of_total = (n_species / total_species) * 100
  )


# ______________________________________________________________________
#species contribution per datasource 
# ______________________________________________________________________
library(dplyr)
library(ggplot2)
library(forcats)

# How many "top" species per source?
n_top <- 10

top_species_by_source <- master_dat %>%
  # change Species to your species column if different
  group_by(source, species) %>%
  summarise(n_records = n(), .groups = "drop") %>%
  group_by(source) %>%
  # keep the top n species by number of records per source
  slice_max(order_by = n_records, n = n_top, with_ties = FALSE) %>%
  ungroup() %>%
  group_by(source) %>%
  # order species within each source by their abundance
  mutate(species = fct_reorder(species, n_records)) %>%
  ungroup()

top_species_by_source_relative <- top_species_by_source %>%
  group_by(source) %>%
  mutate(prop_records = n_records / sum(n_records)) %>%
  ungroup()


#plot
ggplot(top_species_by_source,
       aes(x = n_records, y = species)) +
  geom_col() +
  facet_wrap(~ source, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Top 10 species per data source",
    x = "Number of records",
    y = NULL
  ) +
  theme_classic(base_family = "Times") +
  theme(
    strip.background = element_rect(color = "black", fill = "grey90"),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(face = "italic", size = 8),
    axis.title.x = element_text(margin = margin(t = 8)),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  )

top_n_species <- 30 

top_species <- master_dat %>%
  group_by(species) %>%
  summarise(total_records = n()) %>%
  slice_max(order_by = total_records, n = top_n_species) %>%
  pull(species)


species_source_matrix <- master_dat %>%
  filter(species %in% top_species) %>%
  group_by(species, source) %>%
  summarise(n_records = n(), .groups = "drop")


species_source_matrix <- species_source_matrix %>%
  group_by(species) %>%
  mutate(prop = n_records / sum(n_records)) %>%
  ungroup()


#bubble plot 
ggplot(species_source_matrix,
       aes(x = source,
           y = fct_reorder(species, -n_records),
           size = n_records)) +
  geom_point(alpha = 0.5) +
  scale_size_continuous(range = c(1, 08)) +
  labs(title = "Contribution of data sources to top species",
       x = "Data source",
       y = "Species",
       size = "Record count") +
  theme_classic(base_family = "Times") +
  theme(
    axis.text.y = element_text(face = "italic", size = 6),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(species_source_matrix, n = Inf)
ggplot(species_source_matrix,
       aes(x = source, y = fct_reorder(species, -n_records), fill = n_records)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C", trans = "log10") +
  labs(title = "Data source contribution to top species",
       x = "Data source",
       y = "Species",
       fill = "Record count") +
  theme_minimal(base_family = "Times") +
  theme(
    axis.text.y = element_text(face = "italic", size = 6),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )


species_source_matrix2 <- species_source_matrix %>%
  group_by(species) %>%
  mutate(total_records = sum(n_records)) %>%     # total across all sources
  ungroup() %>%
  mutate(
    species = fct_reorder(species, total_records)  # order by total abundance
  )

ggplot(species_source_matrix2,
       aes(x = source,
           y = species,
           size = prop)) +   # proportion within species
  geom_point(alpha = 0.5) +
  scale_size_continuous(
    range = c(1, 7),
    breaks = c(0.1, 0.5, 0.9),
    labels = c("10%", "50%", "90%")
  ) +
  labs(
    title = "Contribution of data sources to top species",
    x = "Data source",
    y = "Species",
    size = "Share of\nrecords for species"
  ) +
  theme_classic(base_family = "Times") +
  theme(
    axis.text.y = element_text(face = "italic", size = 6),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

#####number of spp and fam per source and per gear
species_per_source <- master_dat %>%
  filter(!is.na(species)) %>%     
  group_by(source) %>%
  summarise(n_species = n_distinct(species)) %>%
  arrange(desc(n_species))

species_per_source

families_per_source <- master_dat %>%
  filter(!is.na(family)) %>%
  group_by(source) %>%
  summarise(n_families = n_distinct(family)) %>%
  arrange(desc(n_families))

families_per_source

#per gear
species_per_gear <- master_dat %>%
  filter(!is.na(species)) %>%
  group_by(gear_grouped) %>%
  summarise(n_species = n_distinct(species)) %>%
  arrange(desc(n_species))

species_per_gear

families_per_gear <- master_dat %>%
  filter(!is.na(family)) %>%
  group_by(gear_grouped) %>%
  summarise(n_families = n_distinct(family)) %>%
  arrange(desc(n_families))

families_per_gear



# ______________________________________________________________________
#% of MPA covered
# ______________________________________________________________________
library(sf)
library(dplyr)
library(tidyr)

# Read layers (WGS84)
#load shp files
mpas <- st_read("MAPPING/SANBI_PA", layer = "SANBI_PA_2023Q4_July2024", options = "PROMOTE_TO_MULTI=YES") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_make_valid() %>%
  st_transform(4326)

eez <- st_read("MAPPING/EEZ/eez_v12.shp", quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(4326)

eez_sa <- eez %>% filter(TERRITORY1 == "South Africa")

# Clip MPAs to EEZ (important if MPAs have land parts etc.)
mpas_sa <- st_intersection(mpas, eez_sa) %>% st_make_valid()



target_crs <- 32735  # UTM 35S (recommended)
eez_proj   <- st_transform(eez_sa, target_crs)
mpas_proj  <- st_transform(mpas_sa, target_crs)

cellsize_m <- 20 * 1852  # 37,040 m

grid_sfc <- st_make_grid(
  eez_proj,
  cellsize = cellsize_m,
  what = "polygons",
  square = TRUE
)

grid_sa <- st_sf(
  grid_id = seq_along(grid_sfc),
  geometry = grid_sfc
)

# keep only cells that intersect the EEZ polygon
hits <- st_intersects(grid_sa, eez_proj)
grid_eez <- grid_sa[lengths(hits) > 0, ]

master_dat_xy <- master_dat %>%
  filter(!is.na(latitude), !is.na(longitude))

master_pts <- st_as_sf(
  master_dat_xy,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

master_sa <- master_pts[eez_sa, ] %>% st_transform(target_crs)

master_sa_small <- master_sa %>%
  select(species, source, geometry)

# Join points to grid cells
grid_hits <- st_join(grid_eez, master_sa_small, join = st_intersects, left = TRUE)

# Overall sampled cells (EEZ coverage)
coverage <- grid_hits %>%
  mutate(sampled = !is.na(species)) %>%
  group_by(grid_id) %>%
  summarise(sampled = any(sampled), .groups = "drop")

mpa_union <- st_union(mpas_proj)  # one geometry

grid_mpa_flag <- grid_eez %>%
  mutate(in_mpa = lengths(st_intersects(geometry, mpa_union)) > 0) %>%
  st_drop_geometry() %>%
  select(grid_id, in_mpa)

coverage2 <- coverage %>%
  st_drop_geometry() %>%
  left_join(grid_mpa_flag, by = "grid_id") %>%
  mutate(in_mpa = if_else(is.na(in_mpa), FALSE, in_mpa))



overall_mpa_summary <- coverage2 %>%
  summarise(
    total_cells_eez        = n(),
    cells_in_mpa           = sum(in_mpa),
    pct_eez_cells_in_mpa   = 100 * cells_in_mpa / total_cells_eez,
    
    sampled_cells_total    = sum(sampled),
    sampled_cells_in_mpa   = sum(sampled & in_mpa),
    pct_sampled_in_mpa     = 100 * sampled_cells_in_mpa / sampled_cells_total,
    
    pct_eez_covered_in_mpa = 100 * sampled_cells_in_mpa / cells_in_mpa
  )

overall_mpa_summary


# Per (grid_id, source): any records?
grid_source_hits <- grid_hits %>%
  filter(!is.na(source)) %>%
  mutate(sampled = !is.na(species)) %>%
  group_by(grid_id, source) %>%
  summarise(sampled = any(sampled), .groups = "drop") %>%
  left_join(grid_mpa_flag, by = "grid_id") %>%
  mutate(in_mpa = if_else(is.na(in_mpa), FALSE, in_mpa))

# All sources present
sources_vec <- master_sa %>%
  st_drop_geometry() %>%
  distinct(source) %>%
  filter(!is.na(source)) %>%
  pull(source)

# Full grid_id × source for denominators
all_grid_source <- tidyr::crossing(
  grid_id = grid_eez$grid_id,
  source  = sources_vec
) %>% left_join(grid_mpa_flag, by = "grid_id") %>%
  mutate(in_mpa = if_else(is.na(in_mpa), FALSE, in_mpa))

coverage_source_mpa <- all_grid_source %>%
  left_join(grid_source_hits %>% select(grid_id, source, sampled),
            by = c("grid_id", "source")) %>%
  mutate(sampled = if_else(is.na(sampled), FALSE, sampled)) %>%
  group_by(source) %>%
  summarise(
    total_cells_eez      = n(),
    sampled_cells_total  = sum(sampled),
    
    cells_in_mpa         = sum(in_mpa),
    sampled_cells_in_mpa = sum(sampled & in_mpa),
    
    pct_eez_cells_in_mpa = 100 * cells_in_mpa / total_cells_eez,
    pct_source_in_mpa    = dplyr::if_else(sampled_cells_total > 0,
                                          100 * sampled_cells_in_mpa / sampled_cells_total,
                                          NA_real_),
    pct_mpa_sampled      = dplyr::if_else(cells_in_mpa > 0,
                                          100 * sampled_cells_in_mpa / cells_in_mpa,
                                          NA_real_),
    .groups = "drop"
  ) %>%
  arrange(desc(sampled_cells_in_mpa))

coverage_source_mpa

coverage_source_mpa <- coverage_source_mpa %>%
  rename(
    pct_mpa_cells_sampled_by_source = pct_mpa_sampled,
    pct_of_source_sampling_in_mpas  = pct_source_in_mpa,
    pct_eez_cells_that_are_mpas     = pct_eez_cells_in_mpa
  )



all.equal(
  coverage_source %>% arrange(source) %>% pull(sampled_cells),
  coverage_source_mpa %>% arrange(source) %>% pull(sampled_cells_total)
)

names(mpas)
################################################################################
################################################################################

mpas_marine <- mpas %>%
  filter(TYPE == "Marine Protected Area")

mpas_proj <- mpas_marine %>%
  st_transform(st_crs(grid_eez)) %>%
  st_make_valid()

grid_mpa <- st_join(
  grid_eez,
  mpas_proj,
  join = st_intersects,
  left = FALSE   # only grid cells inside MPAs
)

mpa_grid_coverage <- grid_mpa %>%
  st_drop_geometry() %>%
  distinct(grid_id, MPA_NAME = CUR_NME) %>%  # adjust NAME field
  count(MPA_NAME, name = "n_cells") %>%
  arrange(desc(n_cells))

mpa_total_cells <- grid_mpa %>%
  st_drop_geometry() %>%
  distinct(MPA_NAME = CUR_NME, grid_id) %>%
  count(MPA_NAME, name = "total_cells")

mpa_coverage_pct <- mpa_grid_coverage %>%
  left_join(mpa_total_cells, by = "MPA_NAME") %>%
  mutate(pct_cells_sampled = 100 * n_cells / total_cells)

grid_hits_mpa <- st_join(
  grid_mpa,
  master_sa_proj %>% select(source, species),
  join = st_intersects,
  left = TRUE
)

# keep only what you need (massive speedup)
pts_small <- master_sa_proj %>%
  select(source, species, year)  # add year if you have it; otherwise drop

pts_grid <- st_join(
  pts_small,
  grid_eez %>% select(grid_id),
  join = st_within,     # points within grid cell
  left = FALSE
)

grid_source_presence <- pts_grid %>%
  st_drop_geometry() %>%
  distinct(grid_id, source)     # presence/absence per source per cell

grid_mpa <- st_join(
  grid_eez %>% select(grid_id),
  mpas_proj %>% select(CUR_NME),   # your MPA name field
  join = st_intersects,
  left = FALSE
) %>%
  st_drop_geometry() %>%
  distinct(grid_id, MPA_NAME = CUR_NME)


mpa_source_coverage <- grid_mpa %>%
  left_join(grid_source_presence, by = "grid_id") %>%
  filter(!is.na(source)) %>%
  distinct(MPA_NAME, source, grid_id) %>%
  count(MPA_NAME, source, name = "sampled_cells") %>%
  arrange(MPA_NAME, source)


mpa_cell_totals <- grid_mpa %>%
  count(MPA_NAME, name = "total_cells_mpa")

mpa_source_coverage <- mpa_source_coverage %>%
  left_join(mpa_cell_totals, by = "MPA_NAME") %>%
  mutate(pct_mpa_covered = 100 * sampled_cells / total_cells_mpa)


top_mpas <- mpa_cell_totals %>%
  arrange(desc(total_cells_mpa)) %>%
  slice_head(n = 10)

best_sampled_mpas <- mpa_source_coverage %>%
  group_by(MPA_NAME) %>%
  summarise(
    pct_mpa_any_source = max(pct_mpa_covered),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_mpa_any_source))

master_sa_proj <- master_sa_proj %>%
  mutate(year = lubridate::year(date))   # adjust date column name

records_mpa <- st_join(
  master_sa_proj,
  mpas_proj,
  join = st_intersects,
  left = FALSE
)

records_mpa <- records_mpa %>%
  mutate(
    years_since_declaration = year - DECL_YEAR,
    period = case_when(
      years_since_declaration < 0 ~ "Pre-declaration",
      years_since_declaration >= 0 ~ "Post-declaration"
    )
  )

mpa_temporal_summary <- records_mpa %>%
  group_by(MPA_NAME = CUR_NME, source, period) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species),
    .groups = "drop"
  )

mpa_temporal_bias <- records_mpa %>%
  filter(!is.na(period)) %>%
  group_by(MPA_NAME = CUR_NME) %>%
  summarise(
    pct_pre_decl = 100 * mean(period == "Pre-declaration"),
    .groups = "drop"
  ) %>%
  filter(pct_pre_decl > 70)

mpa_source_plot <- mpa_source_coverage %>%
  filter(total_cells_mpa >= 3) %>%
  mutate(source = factor(source))

mpa_order <- mpa_source_plot %>%
  group_by(MPA_NAME) %>%
  summarise(total_pct = sum(pct_mpa_covered)) %>%
  arrange(desc(total_pct)) %>%
  pull(MPA_NAME)

mpa_source_plot$MPA_NAME <- factor(
  mpa_source_plot$MPA_NAME,
  levels = mpa_order
)

ggplot(mpa_source_plot,
       aes(x = MPA_NAME, y = pct_mpa_covered, fill = source)) +
  geom_col(width = 0.85, color = "black", linewidth = 0.2) +
  scale_y_continuous(
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Marine Protected Area",
    y = "MPA grid cells sampled (%)",
    fill = "Data source"
  ) +
  theme_classic(base_family = "Times") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )


names(mpas)
unique(mpas$TYPE)

mpa_any_coverage <- grid_mpa %>%
  inner_join(grid_source_presence, by = "grid_id") %>%  # grid has ≥1 record from some source
  distinct(MPA_NAME, grid_id) %>%
  count(MPA_NAME, name = "sampled_cells_any") %>%
  left_join(mpa_cell_totals, by = "MPA_NAME") %>%
  mutate(pct_mpa_any_source = 100 * sampled_cells_any / total_cells_mpa)

mpa_exclusion_coverage <- mpa_any_coverage %>%
  filter(pct_mpa_any_source < 20) %>%
  select(MPA_NAME, pct_mpa_any_source)

mpa_source_dominance <- mpa_source_plot %>%
  group_by(MPA_NAME) %>%
  summarise(max_source_pct = max(pct_mpa_covered), .groups = "drop") %>%
  filter(max_source_pct > 80)

eligible_mpas <- mpa_cell_totals %>%
  filter(total_cells_mpa >= 3) %>%
  anti_join(mpa_exclusion_coverage, by = "MPA_NAME") %>%
  anti_join(mpa_source_dominance, by = "MPA_NAME") %>%
  anti_join(mpa_temporal_bias, by = "MPA_NAME")

eligible_mpas


################################################################################



mpas_marine <- mpas %>%
  filter(TYPE == "Marine Protected Area") %>%
  st_make_valid() %>%
  st_transform(st_crs(grid_eez))

grid_mpa <- st_join(
  grid_eez %>% select(grid_id),
  mpas_marine %>% select(MPA_NAME = CUR_NME, DECL_YEAR),
  join = st_intersects,
  left = FALSE
) %>%
  st_drop_geometry() %>%
  distinct(grid_id, MPA_NAME, DECL_YEAR)

grid_source_presence <- pts_grid %>%
  st_drop_geometry() %>%
  distinct(grid_id, source)

mpa_source_coverage <- grid_mpa %>%
  left_join(grid_source_presence, by = "grid_id") %>%
  filter(!is.na(source)) %>%
  distinct(MPA_NAME, source, grid_id) %>%
  count(MPA_NAME, source, name = "sampled_cells")

mpa_any_coverage <- grid_mpa %>%
  inner_join(grid_source_presence, by = "grid_id") %>%
  distinct(MPA_NAME, grid_id) %>%
  count(MPA_NAME, name = "sampled_cells_any")


mpa_source_plot2 <- mpa_source_coverage %>%
  left_join(mpa_any_coverage, by = "MPA_NAME") %>%
  mutate(pct_of_sampled = 100 * sampled_cells / sampled_cells_any)


ggplot(mpa_source_plot2,
       aes(x = MPA_NAME, y = pct_of_sampled, fill = source)) +
  geom_col(width = 0.85, color = "black", linewidth = 0.2) +
  labs(
    x = "Marine Protected Area",
    y = "MPA grid cells containing records from source (%)",
    fill = "Data source"
  ) +
  theme_classic(base_family = "Times") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid_mpa %>%
  distinct(MPA_NAME) %>%
  arrange(MPA_NAME)

mpa_diag <- mpa_cell_totals %>%
  left_join(mpa_any_coverage %>% select(MPA_NAME, pct_mpa_any_source, sampled_cells_any),
            by = "MPA_NAME") %>%
  left_join(mpa_source_plot %>%
              group_by(MPA_NAME) %>%
              summarise(max_source_pct = max(pct_mpa_covered), .groups = "drop"),
            by = "MPA_NAME") %>%
  left_join(mpa_temporal_bias %>% select(MPA_NAME, pct_pre_decl),
            by = "MPA_NAME") %>%
  mutate(
    pct_pre_decl = if_else(is.na(pct_pre_decl), 0, pct_pre_decl) # treat missing as 0 if no records classified
  )

mpa_diag <- mpa_diag %>%
  mutate(
    tier = case_when(
      total_cells_mpa < 3 ~ "Tier C: too small",
      pct_mpa_any_source < 20 ~ "Tier C: low coverage",
      max_source_pct > 90 ~ "Tier C: extreme dominance",
      pct_pre_decl > 85 ~ "Tier C: extreme pre-decl bias",
      # “good enough for cautious inference”
      pct_mpa_any_source >= 40 & max_source_pct <= 80 & pct_pre_decl <= 70 ~ "Tier A: strongest inference",
      TRUE ~ "Tier B: descriptive only"
    )
  )

mpa_diag %>% count(tier, sort = TRUE)


best_sampled_mpas <- mpa_any_coverage %>%
  arrange(desc(pct_mpa_any_source))

################################################################################
################################################################################
mpas_marine <- mpas %>%
  filter(TYPE == "Marine Protected Area") %>%   # or MAJ_TYPE == "Marine Protected Area" if cleaner
  st_make_valid()

mpas_proj <- mpas_marine %>%
  st_transform(st_crs(master_sa_proj)) %>%      # match points CRS
  st_make_valid()


records_mpa <- st_join(
  master_sa_proj %>% select(source, species, year),          # include year if you have it
  mpas_proj %>% select(MPA_NAME, DECL_YEAR),
  join = st_intersects,
  left = FALSE
) %>%
  st_drop_geometry()


records_mpa %>%
  filter(MPA_NAME == "Tsitsikamma Marine Protected Area") %>%
  summarise(
    decl = first(DECL_YEAR),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    n_pre  = sum(year < DECL_YEAR, na.rm = TRUE),
    n_post = sum(year >= DECL_YEAR, na.rm = TRUE),
    pct_post = 100 * mean(year >= DECL_YEAR, na.rm = TRUE)
  )

records_mpa <- records_mpa %>%
  mutate(
    year = as.integer(year),
    DECL_YEAR = as.integer(DECL_YEAR),
    post_decl = !is.na(year) & !is.na(DECL_YEAR) & year >= DECL_YEAR
  )
mpa_summary_tbl <- records_mpa %>%
  filter(!is.na(MPA_NAME)) %>%
  group_by(MPA_NAME, DECL_YEAR) %>%
  summarise(
    n_records = n(),
    n_sources = n_distinct(source),
    sources_contributed = paste(sort(unique(source)), collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(desc(n_records))


mpa_summary_tbl <- records_mpa %>%
  group_by(MPA_NAME, DECL_YEAR) %>%
  summarise(
    n_records = n(),
    n_sources = n_distinct(source),
    sources_contributed = paste(sort(unique(source)), collapse = "; "),
    first_year = min(year, na.rm = TRUE),
    last_year  = max(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_records))

mpa_summary_tbl <- records_mpa %>%
  filter(!is.na(year), !is.na(DECL_YEAR)) %>%
  mutate(post_decl = year >= DECL_YEAR) %>%
  group_by(MPA_NAME, DECL_YEAR) %>%
  summarise(
    n_records = n(),
    n_sources = n_distinct(source),
    sources_contributed = paste(sort(unique(source)), collapse = "; "),
    first_year = min(year),
    last_year  = max(year),
    pct_post_decl = 100 * mean(post_decl),
    .groups = "drop"
  ) %>%
  arrange(desc(n_records))


mpa_summary_tbl <- records_mpa %>%
  filter(!is.na(year), !is.na(DECL_YEAR)) %>%
  mutate(post_decl = year >= DECL_YEAR) %>%
  group_by(MPA_NAME, DECL_YEAR) %>%
  summarise(
    n_records = n(),
    n_sources = n_distinct(source),
    sources_contributed = paste(sort(unique(source)), collapse = "; "),
    first_year = min(year),
    last_year  = max(year),
    pct_post_decl = 100 * mean(post_decl),
    .groups = "drop"
  ) %>%
  arrange(desc(n_records))


print(mpa_summary_tbl, n = Inf)
print(mpa_summary_tbl_clean, n = Inf)
mpa_summary_tbl_clean <- mpa_summary_tbl %>%
  group_by(MPA_NAME) %>%
  summarise(
    DECL_YEAR = min(DECL_YEAR, na.rm = TRUE),
    n_records = sum(n_records),
    n_sources = max(n_sources),
    sources_contributed = paste(sort(unique(unlist(strsplit(sources_contributed, ";\\s*")))), collapse = "; "),
    first_year = min(first_year, na.rm = TRUE),
    last_year  = max(last_year, na.rm = TRUE),
    # IMPORTANT: recompute pct_post_decl properly (see next section)
    .groups = "drop"
  )

mpas_proj <- mpas_marine %>%
  st_transform(st_crs(master_sa_proj)) %>%
  select(MPA_NAME = CUR_NME, DECL_YEAR, DECL1, DECL2) %>%
  st_make_valid()

mpas_proj <- mpas_proj %>%
  mutate(MPA_UNIT = paste0(MPA_NAME, " (", DECL_YEAR, ")"))


records_mpa2 <- records_mpa %>%
  mutate(
    MPA_NAME,
    rec_year  = year,                 # whatever your record year column is
    decl_year = DECL_YEAR,
    period = if_else(rec_year >= decl_year, "Post", "Pre")
  )

mpa_summary_tbl2 <- records_mpa2 %>%
  group_by(MPA_NAME) %>%
  summarise(
    DECL_YEAR = min(decl_year, na.rm = TRUE),
    n_records = n(),
    n_records_post = sum(period == "Post", na.rm = TRUE),
    n_records_pre  = sum(period == "Pre", na.rm = TRUE),
    pct_post_decl  = 100 * n_records_post / n_records,
    n_sources = n_distinct(source),
    sources_contributed = paste(sort(unique(source)), collapse = "; "),
    first_year = min(rec_year, na.rm = TRUE),
    last_year  = max(rec_year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_records))

mpa_summary_tbl2 <- mpa_summary_tbl2 %>%
  filter(!is.infinite(first_year))
print(mpa_summary_tbl2, n = Inf, width = Inf)

mpa_summary_tbl2 <- mpa_summary_tbl2 %>%
  filter(MPA_NAME != "West Coast National Park")


records_mpa2 <- records_mpa %>%
  mutate(
    MPA_NAME,
    rec_year  = year,                 # whatever your record year column is
    decl_year = DECL_YEAR,
    period = if_else(rec_year >= decl_year, "Post", "Pre")
  )
mpa_species_post <- records_mpa2 %>%
  filter(!is.na(species), period == "Post") %>%
  group_by(MPA_NAME) %>%
  summarise(
    n_species_post = n_distinct(species),
    .groups = "drop"
  )

unique(master_dat$species)
unique(master_dat$species)[
  unique(master_dat$species) %in% c("Chelon richardsonii", "Liza richardsonii")
]
mpa_species_richness <- records_mpa %>%
  filter(!is.na(species)) %>%
  group_by(MPA_NAME) %>%
  summarise(
    n_species_total = n_distinct(species),
    .groups = "drop"
  ) %>%
  arrange(desc(n_species_total))

mpa_summary_tbl2 <- mpa_summary_tbl2 %>%
  left_join(mpa_species_richness, by = "MPA_NAME")


mpa_summary_tbl2 <- mpa_summary_tbl2 %>%
  left_join(mpa_species_post, by = "MPA_NAME")



mpa_species_prepost <- records_mpa2 %>%
  filter(!is.na(species), !is.na(period)) %>%
  group_by(MPA_NAME, period) %>%
  summarise(
    n_species = n_distinct(species),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = period,
    values_from = n_species,
    names_prefix = "n_species_"
  )

mpa_summary_tbl2 <- mpa_summary_tbl2 %>%
  left_join(mpa_species_prepost, by = "MPA_NAME")



#OTHER RESULTS (TAKEN FROM MERGE SCRIPT) 
#-------------------------------------------------------------------------------
#OUTPUTS
#-------------------------------------------------------------------------------
#rangeof lat and lon
summary(master_dat$longitude)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -60.63   18.08   21.26   22.88   27.40  189.16    5574 
summary(master_dat$latitude)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -75.43  -34.96  -34.15  -33.42  -31.84    0.00    5516

#year range
sorted_year <- sort(unique(master_dat$year), na.last = TRUE)
sorted_year
#1803 1820 1825 1827 1828 1829 1831 1832 1837 1838 1839 1840 1843 1849 1853 1860 1861 1862 1863
#1864 1869 1870 1873 1876 1877 1879 1880 1881 1882 1883 1884 1885 1886 1887 1889 1890 1891 1893
#1894 1896 1897 1898 1899 1900 1901 1902 1903 1904 1905 1906 1907 1908 1909 1910 1911 1912 1913
#1914 1915 1916 1917 1918 1919 1920 1921 1922 1923 1924 1925 1926 1927 1928 1929 1930 1931 1932
#1933 1934 1935 1936 1937 1938 1939 1940 1941 1942 1943 1944 1945 1946 1947 1948 1949 1950 1951
#1952 1953 1954 1955 1956 1957 1958 1959 1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970
#1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989
#1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008
#2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025   NA
#-------------------------------------------------------------------------------
# Top species and families
#-------------------------------------------------------------------------------

taxon_summary <- master_dat_march %>%
  count(taxonomic_resolution)

# View summary
print(taxon_summary)
#  taxonomic_resolution       n
#             family        397
#              genus      112649
#               order          1
#            species      1 022 936

master_dat_march %>%
  filter(taxonomic_resolution == "order")

# Bar plot
ggplot(taxon_summary, aes(x = reorder(taxonomic_resolution, -n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 3, family = "serif") +
  labs(
    title = "Number of Records per Taxonomic Resolution in Integrated Dataset",
    x = "Taxonomic Resolution",
    y = "Number of Records"
  ) +
  theme_classic(base_family = "serif") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10), size = 11),
    axis.title.x = element_text(margin = margin(t = 10), size = 11),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )


tax_levels <- c("species", "genus", "family", "order")

taxon_summary$taxonomic_resolution <- factor(
  taxon_summary$taxonomic_resolution,
  levels = tax_levels
)

taxon_summary_source$taxonomic_resolution <- factor(
  taxon_summary_source$taxonomic_resolution,
  levels = tax_levels
)


# ============================================================
# 3) Taxonomic resolution by source (patterned plot)
# ============================================================
library(ggpattern)

# ============================================================
# PANEL A: Stacked totals (linear scale, CORRECT)
# ============================================================
p_A <- ggplot(
  taxon_summary_source,
  aes(
    x = source,
    y = n,
    pattern = taxonomic_resolution
  )
) +
  
  geom_col_pattern(
    fill = "white",
    color = "black",
    pattern_fill = "black",
    pattern_density = 0.2,
    pattern_spacing = 0.03
  ) +
  
  scale_pattern_manual(
    values = c(
      "species" = "stripe",
      "genus"   = "pch",
      "family"  = "crosshatch",
      "order"   = "none"
    ),
    breaks = tax_levels
  ) +
  
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0.02, 0.1))
  ) +
  
  scale_x_discrete(labels = c(
    "BRUV"        = "BRUV (S)",
    "CAPFISH"     = "CapMarine (O)",
    "DEM_TRAWL"   = "Demersal trawl (S)",
    "INAT"        = "iNaturalist (S)",
    "LINEFISH"    = "Angling (C,O)",
    "LITERATURE"  = "Literature (S,C,O)",
    "MUSEUM"      = "Museum (S)",
    "MW_TRAWL"    = "MW Trawl (O)"
  )) +
  
  labs(
    x = "Data Source",
    y = "Number of Records (n)",
    pattern = "Taxonomic Resolution"
  ) +
  
  theme_classic(base_family = "serif") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3)
  )

p_A

ggsave(
  "taxonomic_resolution_by_source.pdf",
  width = 8.5,
  height = 5,
  units = "in"
)

#faceted
# ============================================================
# 1) Ensure correct ordering
# ============================================================
tax_levels <- c("species", "genus", "family", "order")

taxon_summary_source <- master_dat_march %>%
  count(source, taxonomic_resolution) %>%
  mutate(
    taxonomic_resolution = factor(
      taxonomic_resolution,
      levels = tax_levels
    )
  )

# ============================================================
# 2) Faceted plot (by source)
# ============================================================
p_tax_faceted <- ggplot(
  taxon_summary_source,
  aes(
    x = taxonomic_resolution,
    y = n,
    fill = taxonomic_resolution
  )
) +
  
  geom_col(
    color = "black",
    width = 0.7
  ) +
  
  facet_wrap(
    ~ source,
    ncol = 4, labeller = labeller(source = c(
      "BRUV"        = "BRUV (S)",
      "CAPFISH"     = "CapMarine (O)",
      "DEM_TRAWL"   = "Demersal trawl (S)",
      "INAT"        = "iNaturalist (S)",
      "LINEFISH"    = "Angling (C,O)",
      "LITERATURE"  = "Literature (S,C,O)",
      "MUSEUM"      = "Museum (S)",
      "MW_TRAWL"    = "MW Trawl (O)"
    ))
  ) +
  
  scale_fill_manual(
    values = c(
      "species" = "#2b5c7a",
      "genus"   = "#6ba3c3",
      "family"  = "#b7c9d3",
      "order"   = "#e0e0e0"
    ),
    breaks = tax_levels,
    name = "Taxonomic Resolution"
  ) +
  
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000),
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  ) +
  
  labs(
    x = "Taxonomic Resolution",
    y = "Number of Records (pseudo-log scale)"
  ) +
  
  theme_classic(base_family = "serif") +
  theme(
    strip.text = element_text(size = 9, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3)
  )

p_tax_faceted


ggsave(
  "taxonomic_resolution_faceted.pdf",
  width = 8.5,
  height = 5,
  units = "in"
)
names(master_dat)
#-----master_dat#-------------------------------------------------------------------------------
#species and families no.
#-------------------------------------------------------------------------------
length(unique(master_dat$species)) #2366

library(dplyr)
library(stringr)
species_table <- master_dat %>%
  distinct(species) %>%
  arrange(species)

master_known <- master_dat %>%
  filter(!is.na(species)) %>%
  mutate(
    species = str_squish(species),
    species = str_replace_all(species, "\\s+", " "),
    species = str_trim(species),
    
    # optional but recommended: standardise case
    species = str_to_sentence(species)
  ) %>%
  filter(
    str_count(species, " ") == 1,
    !str_detect(species, "\\bsp\\b|\\bsp\\.\\b|\\bspp\\b|\\bspp\\.\\b"),
    !str_detect(species, "\\bcf\\b|\\bcf\\.\\b|\\baff\\b|\\baff\\.\\b")
  )

known_species_n <- master_known %>%
  distinct(species) %>%
  nrow()

known_species_n



known_species <- master_dat_march %>%
  filter(
    str_count(species, " ") == 1,        # binomial
    !str_detect(species, "\\bsp\\b"),    # exclude sp / sp.
    !str_detect(species, "cf\\.|aff\\.") # exclude cf./aff.
  ) %>%
  distinct(species, genus, family) %>%  # include whichever columns exist
  arrange(family, species)

nrow(known_species) #2164

write.csv(known_species, "~/Desktop/wd/masters/known_species.csv", row.names = FALSE)


#202 had "sp. designations


length(unique(master_dat_march$family)) #248


family_table <- master_dat %>%
  distinct(family) %>%
  arrange(family)


#-------------------------------------------------------------------------------
# Top species and families
#-------------------------------------------------------------------------------
#top 20 spp
top_species <- master_dat %>%
  filter(!is.na(species)) %>%
  count(species, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_species, aes(x = reorder(species, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequently Recorded Species across all Datasets",
    x = "Species",
    y = "Number of Records"
  ) +
  theme_classic(base_family = "Times") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10), size = 12),
    axis.title.x = element_text(margin = margin(t = 10), size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
  )
#top 20 families
top_families <- master_dat %>%
  filter(!is.na(family)) %>%
  count(family, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_families, aes(x = reorder(family, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequently Recorded Families across all Datasets",
    x = "Family",
    y = "Number of Records"
  ) +
  theme_classic(base_family = "Times") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10), size = 12),
    axis.title.x = element_text(margin = margin(t = 10), size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
  )
#-------------------------------------------------------------------------------
#records and spp per coast
#-------------------------------------------------------------------------------

coast_summary <- master_dat %>%
  filter(coast != "other") %>%
  group_by(coast) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species)
  )

print(coast_summary)
#  coast n_records n_species
#east     295473      1911
#south    328231       783
#west     505815       798

#records and spp by source
source_summary <- master_dat %>%
  group_by(source) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species)
  )

print(source_summary)
#source     n_records n_species
#BRUV           20164       637
#CAPFISH       128971       131
#DEM_TRAWL     101756       144
#INAT           10101       696
#LINEFISH      822211       168
#LITERATURE      6464       628
#MUSEUM         32835      1954
#MW_TRAWL       13481        45


#-------------------------------------------------------------------------------
# Count records and species  per source
#-------------------------------------------------------------------------------

source_pie <- master_dat %>% 
  filter(!is.na(source)) %>%         
  count(source, name = "n") %>% 
  mutate(
    prop = n / sum(n),
    label = paste0(source, "\n", percent(prop, accuracy = 0.1))
  )

# Plot
ggplot(source_pie, aes(x = "", y = prop, fill = source)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  
  # Remove labels from the pie
  geom_text(label = "", position = position_stack(vjust = 0.5)) +
  
  # Blue colour scale
  scale_fill_brewer(palette = "Set1") +
  
  labs(
    title = "Proportion of Records by Data Source",
    fill = "Data source"
  ) +
  
  theme_void(base_family = "Times") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, family = "Times"),
    legend.title = element_text(family = "Times"),
    legend.text = element_text(family = "Times")
  )

species_pie <- master_dat %>% 
  filter(!is.na(source), !is.na(species)) %>% 
  group_by(source) %>% 
  summarise(n_species = n_distinct(species)) %>% 
  ungroup() %>% 
  mutate(prop = n_species / sum(n_species))

ggplot(species_pie, aes(x = "", y = prop, fill = source)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  
  # Remove all text on pie slices
  geom_text(label = "", position = position_stack(vjust = 0.5)) +
  
  # Blue palette
  scale_fill_brewer(palette = "Set1") +
  
  labs(
    title = "Proportion of Species by Data Source",
    fill = "Data source"
  ) +
  
  theme_void(base_family = "Times") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, family = "Times"),
    legend.title = element_text(family = "Times"),
    legend.text  = element_text(family = "Times")
  )

#-------------------------------------------------------------------------------
# Count records and species by gear
#-------------------------------------------------------------------------------
names(master_dat)
unique(master_dat$method)

#ANGLING = "LINE" "ShoreAngling"   "BoatAngling" "Angling" "SHORTHAND_ROD"   
#LONGLINE = "SAHLLA"   
#MIDWATER TRAWL = "Midwater trawl" 
#DEMSERSAL TRAWL =  "Trawl" , "BeamTrawl" , "DnetTrawl"  , "Demersal trawl" , "SADSTIA", "SECIFA"     
#SEINE NET = "SeineNet"  "SAPFIA"   
#PLANKTON = "PlanktonNet" 
#GILLNET = "GillNet"      
#FYKENET and trap = "FykeNet" , "Trap"   
#SPEAR "SPEAR"  "SHORT_SPEAR"  
#POLE =    "POLE" 
#BRUV = "BRUV", "BRUV survey"   
#UVC = "UVC" 
#CHEMICAL = "Chemical" 
#COLLECTION OF DEAD FISH = "DeadCollection"    
#MIXED METHODS = "Mixed"   
#CITIZEN SCIENCE HUMAN OBSERVATION = "HUMAN_OBSERVATION" 
#Historical records and museum specimens = "Records" , "PRESERVED_SPECIMEN"
#Visual Estimate  = "VisualEstimate" 

master_dat <- master_dat %>%
  mutate(
    gear_grouped = case_when(
      # ANGLING
      method %in% c("LINE", "ShoreAngling", "BoatAngling", "Angling", "SHORTHAND_ROD") ~ "Angling",
      # LONGLINE
      method == "SAHLLA" ~ "Longline",
      # MIDWATER TRAWL
      method == "Midwater trawl" ~ "Midwater trawl",
      # DEMERSAL TRAWL
      method %in% c("Trawl", "BeamTrawl", "DnetTrawl", "Demersal trawl",
                    "SADSTIA", "SECIFA") ~ "Demersal trawl",
      # SEINE NET
      method %in% c("SeineNet", "SAPFIA") ~ "Seine net",
      # PLANKTON
      method == "PlanktonNet" ~ "Plankton",
      # GILLNET
      method == "GillNet" ~ "Gillnet",
      # FYKENET AND TRAP
      method %in% c("FykeNet", "Trap") ~ "Fykenet / trap",
      # SPEAR
      method %in% c("SPEAR", "SHORT_SPEAR") ~ "Spear",
      # POLE
      method == "POLE" ~ "Pole",
      # BRUV
      method %in% c("BRUV", "BRUV survey") ~ "BRUV",
      # UVC
      method == "UVC" ~ "UVC",
      # CHEMICAL
      method == "Chemical" ~ "Chemical",
      # COLLECTION OF DEAD FISH
      method == "DeadCollection" ~ "Collection of dead fish",
      # MIXED METHODS
      method == "Mixed" ~ "Mixed methods",
      # CITIZEN SCIENCE HUMAN OBSERVATION
      method == "HUMAN_OBSERVATION" ~ "Citizen science (human observation)",
      # HISTORICAL RECORDS AND MUSEUM SPECIMENS
      method %in% c("Records", "PRESERVED_SPECIMEN") ~ "Historical records & museum specimens",
      # VISUAL ESTIMATE
      method == "VisualEstimate" ~ "Visual estimate",
      # FALLBACK
      TRUE ~ "Other / unknown"
    )
  )

gear_summary <- master_dat %>%
  group_by(gear_grouped) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species)
  )

print(gear_summary)
#gear_grouped                          n_records n_species
# Angling                                  788475       276
# BRUV                                      20425       649
# Chemical                                    485       134
# Citizen science (human observation)       10101       696
# Collection of dead fish                      24        21
# Demersal trawl                           194623       260
# Fykenet / trap                              141        46
# Gillnet                                     759        85
# Historical records & museum specimens     33056      1971
# Longline                                  19395        64
# Midwater trawl                            13481        45
# Mixed methods                               169        68
# Plankton                                    531       196
# Pole                                      35048        46
# Seine net                                 18223       208
# Spear                                        55        32
# UVC                                         957       132
# Visual estimate                              35        24

ggplot(gear_summary, aes(x = reorder(gear_grouped, -n_records), y = n_records)) +
  geom_col(fill = "grey70", color = "black", width = 0.7) +
  geom_text(aes(label = n_records), vjust = -0.5, size = 3.5, family = "Times") +
  labs(
    title = "Number of Marine Teleost Records by Sampling or Capture Method",
    x = "Gear Group",
    y = "Number of Records"
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  theme_classic(base_family = "Times", base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#DISTRIBUTION
#-------------------------------------------------------------------------------
#SPATIAL FILES
world <- ne_countries(scale = "medium", returnclass = "sf")
sa_map <- world %>% filter(admin == "South Africa")
#load shp files
mpas <- st_read("MAPPING/SANBI_PA", layer = "SANBI_PA_2023Q4_July2024", options = "PROMOTE_TO_MULTI=YES") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_make_valid() %>%
  st_transform(4326)

eez <- st_read("MAPPING/EEZ/eez_v12.shp", quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(4326)

#-------------------------------------------------------------------------------
# Convert to sf
masterdat_sf <- master_dat %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
#Identify points that fall inside SA land polygon
is_on_land <- st_intersects(masterdat_sf, sa_map, sparse = FALSE)[,1]
#Keep only marine points
masterdat_marine <- masterdat_sf[!is_on_land, ]
#-------------------------------------------------------------------------------
masterdat_marine <- st_transform(masterdat_marine, crs = 4326)

#all
ggplot() +
  geom_sf(data = sa_map, fill = "grey85", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red", linewidth = 0.7) +
  geom_sf(
    data = masterdat_marine,
    aes(color = source),
    alpha = 0.6,
    size = 1.2
  ) +
  coord_sf(xlim = c(10, 40), ylim = c(-40, -20), expand = FALSE) +
  labs(
    title = "Distribution of Marine Teleost Occurrence Data from Integrated Dataset",
    x = "Longitude", y = "Latitude",
    color = "Data Source"
  ) +
  theme_classic(base_size = 14, base_family = "Times") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Check how many were removed
n_removed <- sum(is_on_land)
cat("Points removed as terrestrial:", n_removed, "\n")
#Points removed as terrestrial: 18213 

#-------------------------------------------------------------------------------
#TIME
#-------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggpattern)
library(scales)

year_source <- master_dat %>%
  filter(!is.na(year), !is.na(source)) %>%
  mutate(year = as.integer(year)) %>%
  count(year, source, name = "n")

year_source <- year_source %>%
  mutate(source = factor(source, levels = c(
    "LINEFISH","DEM_TRAWL","CAPFISH","MW_TRAWL","BRUV","MUSEUM","LITERATURE","INAT"
  )))

ggplot(year_source, aes(x = year, y = n, fill = source)) +
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


ggsave(
  "time series.pdf",
  width = 8.5,
  height = 5,
  units = "in"
)


#DECADE
ggplot(dec_source, aes(x = decade, y = n, fill = source)) +
  geom_col(color = "black", linewidth = 0.2) +
  scale_fill_manual(values = c(
    "LINEFISH"   = "lightblue",
    "DEM_TRAWL"  = "steelblue",
    "CAPFISH"    = "navy",
    "MW_TRAWL"   = "grey20",
    "BRUV"       = "#1b9e77",
    "MUSEUM"     = "#666666",
    "LITERATURE" = "grey80",
    "INAT"       = "white"
  )) +
  scale_y_continuous(trans = "sqrt", labels = scales::comma) +
  scale_x_continuous(breaks = seq(1800, 2030, by = 20)) +
  labs(
    title = "Time Series of the Number of Marine Teleost Records",
    x = "Decade",
    y = "Number of Records (square-root scale)",
    fill = "Source"
  ) +
  theme_classic(base_family = "Times") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

#-------------------------------------------------------------------------------
#number of records by source
ggplot(source_summary, aes(x = reorder(source, -n_records), y = n_records)) +
  geom_col(fill = "grey70", color = "black", width = 0.7) +
  geom_text(aes(label = n_records), vjust = -0.5, size = 3.5, family = "Times") +
  labs(
    title = "Number of Marine Teleost Records by Data Source",
    x = "Data Source",
    y = "Number of Records"
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  theme_classic(base_family = "Times", base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
#-------------------------------------------------------------------------------
# number of records by coast
#-------------------------------------------------------------------------------
master_dat_noNA <- master_dat %>%
  filter(!is.na(coast))
ggplot(coast_summary, aes(x = reorder(coast, -n_records), y = n_records)) +
  geom_col(fill = "grey70", color = "black", width = 0.7) +
  geom_text(aes(label = n_records), vjust = -0.5, size = 3.5, family = "Times") +
  labs(
    title = "Distribution of Marine Teleost Occurence Data by Coast",
    x = "Coast",
    y = "Number of Records"
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  theme_classic(base_family = "Times", base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggplot(master_dat_noNA, aes(x = coast, fill = source)) +
  geom_bar(color = "black", width = 0.7) +
  labs(
    title = "Distribution of Marine Teleost Occurrence Data by Coast and Source",
    x = "Coast",
    y = "Number of Records",
    fill = "Source"
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  theme_classic(base_family = "Times", base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#I want to now do an analysis of redundancy in sampling coverage (which methods, areas, 
#species, etc are over sampled and where does critical effort need to be applied. this will be used for policy informing


#  Where is sampling effort spatially redundant or lacking?
#  Which methods are giving you overlapping vs complementary information?
#  Which species are over-sampled vs under-sampled relative to everything else?

analysis_dat <- master_dat %>%
  filter(!is.na(longitude), !is.na(latitude), !is.na(year)) %>%
  mutate(
    decade = floor(year / 10) * 10,
    decade = factor(decade, levels = sort(unique(decade)))
  )

#CREATE GRID
grid_size <- 20 * 1852   # 37,040 metres
eez_proj <- st_transform(eez, 32735)  # UTM Zone 35S

grid_20nm <- st_make_grid(
  eez_proj,
  cellsize = grid_size,
  square = TRUE
)

# Convert grid to sf object with IDs
grid_20nm <- st_sf(
  grid_id = paste0("G", seq_along(grid_20nm)),
  geometry = grid_20nm
)

# Convert to sf
masterdat_sf <- master_dat %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
#Identify points that fall inside SA land polygon
is_on_land <- st_intersects(masterdat_sf, sa_map, sparse = FALSE)[,1]
#Keep only marine points
masterdat_marine <- masterdat_sf[!is_on_land, ]
#-------------------------------------------------------------------------------
masterdat_marine <- st_transform(masterdat_marine, crs = 4326)


st_crs(masterdat_sf)
st_crs(grid_20nm)
masterdat_sf_utm <- masterdat_sf %>%
  st_transform(st_crs(grid_20nm))   # EPSG:32735

master_dat_gridded <- st_join(masterdat_sf_utm, grid_20nm)

master_dat_gridded_df <- master_dat_gridded %>%
  st_drop_geometry()

#effort gridded per cell
grid_effort <- master_dat_gridded_df %>%
  group_by(grid_id) %>%
  summarise(n_records = n(), n_species = n_distinct(species))
grid_effort 
#Method redundancy per grid
grid_method <- master_dat_gridded_df %>%
  group_by(grid_id, gear_grouped) %>%
  summarise(n_records = n(), .groups = "drop")
grid_method
#speciesrichness heat map
grid_richness <- master_dat_gridded_df %>%
  group_by(grid_id) %>%
  summarise(richness = n_distinct(species))
grid_richness



grid_effort_sf <- grid_20nm %>%
  left_join(grid_effort, by = "grid_id")

#Map effort category (low / intermediate / high redundancy)
ggplot() +
  geom_sf(data = sa_map, fill = "grey90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red", linewidth = 0.6) +
  geom_sf(
    data = grid_effort_sf,
    aes(fill = effort_category),
    color = NA
  ) +
  scale_fill_manual(
    name = "Sampling effort",
    values = c(
      "Low effort"       = "grey85",
      "Intermediate"     = "grey60",
      "High redundancy"  = "black"
    )
  ) +
  coord_sf(xlim = c(10, 40), ylim = c(-40, -20), expand = FALSE) +
  labs(
    title = "Sampling Effort Category (20 nm Grid)",
    x = "Longitude", y = "Latitude"
  ) +
  theme_classic(base_family = "Times", base_size = 14) +
  theme(
    panel.border  = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.title.y  = element_text(margin = margin(r = 10)),
    plot.title    = element_text(face = "bold", hjust = 0.5),
    legend.title  = element_text(face = "bold")
  )

#maprichness
ggplot() +
  geom_sf(data = sa_map, fill = "grey90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red", linewidth = 0.6) +
  geom_sf(
    data = grid_richness_sf,
    aes(fill = richness),
    color = NA
  ) +
  scale_fill_gradient(
    name = "Species richness",
    low = "grey85",
    high = "black"
  ) +
  coord_sf(xlim = c(10, 40), ylim = c(-40, -20), expand = FALSE) +
  labs(
    title = "Marine Teleost Species Richness (20 nm Grid)",
    x = "Longitude", y = "Latitude"
  ) +
  theme_classic(base_family = "Times", base_size = 14) +
  theme(
    panel.border  = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.title.y  = element_text(margin = margin(r = 10)),
    plot.title    = element_text(face = "bold", hjust = 0.5),
    legend.title  = element_text(face = "bold")
  )

#under vs oversampled
q10 <- quantile(grid_effort$n_records, 0.1)
q90 <- quantile(grid_effort$n_records, 0.9)

grid_effort <- grid_effort %>%
  mutate(effort_category = case_when(
    n_records >= q90 ~ "High redundancy",
    n_records <= q10 ~ "Low effort",
    TRUE ~ "Intermediate"
  ))

#effort by grid and gear
grid_gear_effort <- master_dat_gridded %>%
  group_by(grid_id, gear_grouped) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species),
    .groups = "drop"
  )

grid_gear_sf <- grid_20nm %>%
  left_join(grid_gear_effort, by = "grid_id")
#choose subset

gears_focus <- c("Demersal trawl", "Midwater trawl", "Angling", "BRUV", "UVC")

ggplot() +
  geom_sf(data = sa_map, fill = "grey90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red", linewidth = 0.6) +
  geom_sf(
    data = grid_gear_sf %>%
      filter(gear_grouped %in% gears_focus, n_records > 0),
    aes(fill = n_records),
    color = NA
  ) +
  scale_fill_gradient(
    name = "Records",
    low = "grey85",
    high = "black"
  ) +
  coord_sf(xlim = c(10, 40), ylim = c(-40, -20), expand = FALSE) +
  facet_wrap(~ gear_grouped) +
  labs(
    title = "Sampling Effort by Gear Group (20 nm Grid)",
    x = "Longitude", y = "Latitude"
  ) +
  theme_classic(base_size = 14, base_family = "Times") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(color = "black", fill = "grey90"),
    strip.text = element_text(face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_text(face = "bold")
  )


#time series
master_dat_gridded <- st_join(master_dat_sf, grid_20nm) %>%
  st_drop_geometry() %>%    # drop geometry for fast summarising
  filter(!is.na(grid_id)) %>%
  mutate(
    decade = floor(year / 10) * 10,
    decade = factor(decade, levels = sort(unique(decade)))
  )

grid_decade_effort <- master_dat_gridded %>%
  group_by(grid_id, decade) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species),
    .groups = "drop"
  )

grid_decade_sf <- grid_20nm %>%
  left_join(grid_decade_effort, by = "grid_id")

grid_decade_sf <- grid_decade_sf %>%
  mutate(sampled = ifelse(is.na(n_records) | n_records == 0, 0, 1))
#presence/absence or colour by n_records.rather than intensity
#Presence/absence version (policy-friendly 
#“where have we ever sampled in this decade?”)

ggplot() +
  geom_sf(data = sa_map, fill = "grey90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red", linewidth = 0.6) +
  geom_sf(
    data = grid_decade_sf %>% filter(sampled == 1),
    fill = "grey30",
    color = NA
  ) +
  coord_sf(xlim = c(10, 40), ylim = c(-40, -20), expand = FALSE) +
  facet_wrap(~ decade) +
  labs(
    title = "Grid Cells Sampled per Decade (20 nm Grid)",
    x = "Longitude", y = "Latitude"
  ) +
  theme_classic(base_size = 14, base_family = "Times") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(color = "black", fill = "grey90"),
    strip.text = element_text(face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
#which speces are distinct to which sampling method
sp_method <- master_dat %>%
  filter(!is.na(species), !is.na(gear_grouped)) %>%
  distinct(species, gear_grouped)


sp_method_n <- sp_method %>%
  group_by(species) %>%
  summarise(n_methods = n_distinct(gear_grouped), .groups = "drop")

sp_unique <- sp_method %>%
  left_join(sp_method_n, by = "species") %>%
  filter(n_methods == 1) %>%     # only one method
  arrange(gear_grouped, species)
#1328 species distinct to one method (only sampled by one method)


unique_by_gear <- sp_unique %>%
  group_by(gear_grouped) %>%
  summarise(
    n_unique_species = n(),
    unique_species = paste(species, collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(desc(n_unique_species))
#heatmap
ggplot(sp_method_matrix %>% pivot_longer(-species),
       aes(x = name, y = species, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black") +
  labs(
    title = "Species Presence Across Sampling Methods",
    x = "Method",
    y = "Species"
  ) +
  theme_classic(base_family = "Times", base_size = 12) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#MPAS
mpas <- st_transform(mpas, crs = 4326)
# Ensure geometries are valid
mpas <- st_make_valid(mpas)
# Keep only needed columns from mpas
mpas_subset <- mpas %>%
  select(CUR_NME, TYPE, geometry)
# Join with simplified layer
masterdat_mpa_joined <- st_join(masterdat_marine, mpas_subset, join = st_within, left = TRUE)
# Filter
masterdat_marine_matched <- masterdat_mpa_joined %>%
  filter(TYPE == "Marine Protected Area")
#  Counts
n_total <- nrow(masterdat_marine)                        # total marine records
n_pa     <- nrow(masterdat_mpa_joined)                   # in any PA
n_mpa    <- nrow(masterdat_marine_matched)               # in marine PAs
# Summary table
master_mpa_summary <- data.frame(
  total_records = n_total,
  records_in_any_PA = n_pa,
  records_in_marine_PA = n_mpa,
  records_outside_all_PAs = n_total - n_pa
)

print(master_mpa_summary)

(38559/1075958)*100
masterdat_marine_matched %>%
  st_drop_geometry() %>%
  count(CUR_NME, sort = TRUE) %>%
  rename(MPA_Name = CUR_NME, n_records = n)

#no of species per MPA
masterdat_marine_matched %>%
  st_drop_geometry() %>%
  filter(!is.na(CUR_NME), !is.na(species)) %>%
  group_by(CUR_NME) %>%
  summarise(n_species = n_distinct(species)) %>%
  arrange(desc(n_species)) %>%
  print(n = Inf)

