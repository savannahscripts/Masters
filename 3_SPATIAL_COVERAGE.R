# ______________________________________________________________________
#SPATIAL COVERAGE
# ______________________________________________________________________

#follow on from 1_1_3_MERGE_ENV
install.packages(c("ggplot2", "sf", "rnaturalearth", "rnaturalearthdata", "viridis"))
##load libraries
library(readxl)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis) 
library(ggspatial)
library(dplyr)
library(writexl)
library(lubridate)
library(purrr)
library(tidyr)
library(stringr)
library(scales)
library(readr)
library(janitor)
#-------------------------------------------------------------------------------
getwd()
setwd("/Users/savannahanderson/Desktop/wd/masters")
#-------------------------------------------------------------------------------
final_dat <- final_dat %>%
  mutate(
    depth_zone = ifelse(depth <= -200, "Offshore", "Inshore"))
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# source specific coverage (faceted plot) and numbers
#-------------------------------------------------------------------------------
source_coverage <- final_dat %>%
  group_by(source, grid_id) %>%
  summarise(
    sampled = any(!is.na(species)),
    n_records = n(),
    .groups = "drop"
  )

source_grid <- grid_sa %>%
  st_drop_geometry() %>%
  select(grid_id) %>%
  crossing(source = unique(final_dat$source)) %>%
  left_join(source_coverage, by = c("grid_id", "source")) %>%
  mutate(
    sampled   = replace_na(sampled, FALSE),
    n_records = replace_na(n_records, 0L)
  )

source_grid_sf <- grid_sa %>%
  left_join(source_grid, by = "grid_id")

target_crs <- 32735

source_area <- source_grid_sf %>%
  st_transform(target_crs) %>%
  mutate(cell_area = as.numeric(st_area(geometry))) %>%
  group_by(source) %>%
  summarise(
    area_sampled = sum(cell_area[sampled]),
    total_area   = sum(cell_area),
    pct_coverage = 100 * area_sampled / total_area,
    n_cells_sampled = sum(sampled),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_coverage))

source_area

target_crs <- 32735

sa_proj  <- st_transform(sa, target_crs)
eez_proj <- st_transform(eez_sa, target_crs)
grid_proj <- st_transform(source_grid_sf, target_crs)


p_source_points <- ggplot() +
  geom_sf(data = sa, fill = "grey90", colour = "grey40", linewidth = 0.2) +
  geom_sf(data = eez_sa, fill = NA, colour = "black", linewidth = 0.4) +
  geom_sf(
    data = final_dat,
    colour = "navyblue",
    size = 0.5,
    alpha = 0.2
  ) +
  coord_sf(
    xlim = c(10, 36),
    ylim = c(-40, -25),
    expand = FALSE
  ) +
  facet_wrap(~source, ncol = 2) +
  labs(x = "Longitude", y = "Latitude") +
  theme_classic(base_family = "serif", base_size = 10) +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "none"
  )

p_source_points

ggsave
#-------------------------------------------------------------------------------
#-----total sampling coverage of eez grid (percentage) including % offshore (source specfifc for table 4)
#-------------------------------------------------------------------------------
coverage <- final_dat %>%
  group_by(grid_id) %>%
  summarise(
    sampled = TRUE,
    .groups = "drop"
  ) %>%
  st_drop_geometry()

grid_cov <- grid_sa %>%
  left_join(coverage, by = "grid_id") %>%
  mutate(sampled = replace_na(sampled, FALSE))

coords <- st_coordinates(st_centroid(grid_sa))
depth_vals <- terra::extract(depth, coords)

grid_cov$depth <- depth_vals[,1]

# Depth zones
grid_cov <- grid_cov %>%
  mutate(
    depth_zone = case_when(
      is.na(depth) ~ NA_character_,
      depth >= -200 ~ "Inshore",
      depth < -200 ~ "Offshore"
    )
  )

target_crs <- 32735

grid_proj <- st_transform(grid_cov, target_crs)
eez_proj  <- st_transform(eez_sa, target_crs)

grid_proj <- grid_proj %>%
  mutate(cell_area = as.numeric(st_area(geometry)))

eez_area <- as.numeric(st_area(st_union(eez_proj)))

pct_eez_covered <- 100 * sum(
  grid_proj$cell_area[grid_proj$sampled], na.rm = TRUE
) / eez_area

pct_offshore_covered <- grid_proj %>%
  filter(depth_zone == "Offshore") %>%
  summarise(
    pct = 100 * sum(cell_area[sampled], na.rm = TRUE) /
      sum(cell_area, na.rm = TRUE)
  ) %>%
  pull(pct)

# Add depth_zone to source grid
source_grid_sf <- source_grid_sf %>%
  left_join(
    grid_cov %>%
      st_drop_geometry() %>%
      select(grid_id, depth_zone),
    by = "grid_id"
  )

source_grid_sf <- source_grid_sf %>%
  mutate(
    depth_zone = coalesce(depth_zone.y, depth_zone.x)
  ) %>%
  select(-depth_zone.x, -depth_zone.y)

source_stats <- source_grid_sf %>%
  st_transform(target_crs) %>%
  mutate(
    cell_area = as.numeric(st_area(geometry)),
    offshore = depth_zone == "Offshore",
    sampled_offshore = sampled & offshore
  ) %>%
  group_by(source) %>%
  summarise(
    pct_eez = 100 * sum(cell_area[sampled], na.rm = TRUE) / eez_area,
    
    pct_offshore = 100 *
      sum(cell_area[sampled_offshore], na.rm = TRUE) /
      sum(cell_area[offshore], na.rm = TRUE),
    
    n_cells_sampled = sum(sampled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_eez))

pct_eez_covered #66.8991
pct_offshore_covered #61.27108
source_stats

#source       pct_eez   pct_offshore   n_cells_sampled                   
#CAPFISH      52.2        49.3              437 
#MUSEUM       43.4        34.5             388 
#DEM_TRAWL    25.4        14.5             223 
#LINEFISH     13.0         9.82            105 
#MW_TRAWL      9.34        5.04           76 
#INAT          7.39        3.43            93 
#BRUV          4.05        1.36            48 
#LITERATURE    2.03        0.148          31 

437+388+223+105+76+93+48+31

#-------------------------------------------------------------------------------
# 1. COVERAGE + INTENSITY FROM final_dat #-----sampling coverage plot
#-------------------------------------------------------------------------------
coverage <- final_dat %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(
    sampled   = any(!is.na(species)),
    n_records = n(),
    n_species = n_distinct(species),
    .groups = "drop"
  )

#-------------------------------------------------------------------------------
# 2. JOIN BACK TO GRID (to include unsampled cells)
#-------------------------------------------------------------------------------
grid_cov <- grid_sa %>%
  left_join(coverage, by = "grid_id") %>%
  mutate(
    sampled   = replace_na(sampled, FALSE),
    n_records = replace_na(n_records, 0L),
    n_species = replace_na(n_species, 0L)
  )

#-------------------------------------------------------------------------------
# 3. AREA-BASED EEZ COVERAGE (%)
#-------------------------------------------------------------------------------
target_crs <- 32735

grid_cov_proj <- st_transform(grid_cov, target_crs)
eez_proj      <- st_transform(eez_sa, target_crs)

grid_cov_proj <- grid_cov_proj %>%
  mutate(cell_area_m2 = as.numeric(st_area(geometry)))

eez_area_m2 <- as.numeric(st_area(st_union(eez_proj)))

pct_eez_covered <- 100 * sum(
  grid_cov_proj$cell_area_m2[grid_cov_proj$sampled]
) / eez_area_m2

pct_eez_covered

#-------------------------------------------------------------------------------
# 4. PREP FOR PLOTTING
#-------------------------------------------------------------------------------
sa_proj        <- st_transform(sa, target_crs)
final_dat_proj <- st_transform(
  st_as_sf(final_dat, coords = c("longitude", "latitude"), crs = 4326),
  target_crs
)

n_records_total <- nrow(final_dat)
nrow(grid_cov)
nrow(grid_cov_proj)
table(grid_cov$sampled)
table(grid_cov_proj$sampled)
sum(is.na(grid_cov_proj$sampled))
# how many cells actually intersect EEZ?
sum(st_intersects(grid_sa, eez_sa, sparse = FALSE))

#SUMMARY
coverage_summary <- grid_cov %>%
  summarise(
    total_cells = n(),
    sampled_cells = sum(sampled),
    coverage_perc = 100 * sampled_cells / total_cells
  )

subtitle_text <- paste0(
  "20 nm × 20 nm grid; ",
  round(coverage_summary$coverage_perc, 1), "% of EEZ area sampled; ",
  scales::comma(nrow(final_dat_proj)), " records plotted"
)


#-------------------------------------------------------------------------------
# 5. COVERAGE MAP
#-------------------------------------------------------------------------------
p_cov <- ggplot() +
  geom_sf(data = sa_proj, fill = "grey90", color = "grey30", linewidth = 0.2) +
  geom_sf(data = eez_proj, fill = NA, color = "red3", linewidth = 0.6) +
  geom_sf(data = grid_cov_proj, aes(fill = sampled), color = NA, alpha = 0.95) +
  geom_sf(data = final_dat_proj, color = "black", alpha = 0.12, size = 0.15) +
  scale_fill_manual(
    name   = "Sampling status",
    values = c(`TRUE` = "steelblue", `FALSE` = "grey95"),
    labels = c(`TRUE` = "Sampled", `FALSE` = "Unsampled")) +
  coord_sf(
    crs = 4326,
    xlim = c(10, 40),
    ylim = c(-40, -25),
    expand = FALSE) +
  scale_x_continuous(
    name = "Longitude",
    breaks = seq(10, 40, 5),
    labels = function(x) paste0(x, "°E")) +
  scale_y_continuous(
    name = "Latitude",
    breaks = seq(-40, -20, 5),
    labels = function(x) paste0(abs(x), "°S")) +
  theme_classic(base_family = "serif", base_size = 12) +
  labs(
    title = "Sampling coverage of South Africa’s EEZ",
    subtitle = subtitle_text
  ) +
  theme(
    legend.position = "right",
    plot.margin = margin(6, 6, 6, 6)
  )

p_cov

ggsave(
  filename = "FIG8_sampling_coverage.png",
  plot = p_cov,
  width = 180, height = 120, units = "mm",
  dpi = 600
)

coverage_summary 
#total_cells   sampled_cells coverage_perc  
#  909           587         64.57646 

#-------------------------------------------------------------------------------
# SAMPLING INTENSITY (records per grid cell) #-----sampling intensity
#-------------------------------------------------------------------------------

library(sf)
library(dplyr)
library(ggplot2)
library(scales)

#-------------------------------------------------------------------------------
# 1. USE grid_cov (already contains n_records per grid)
#-------------------------------------------------------------------------------

grid_heat <- grid_cov   # rename for clarity if you like

#-------------------------------------------------------------------------------
# 2. PROJECT FOR MAPPING
#-------------------------------------------------------------------------------

target_crs <- 32735

sa_proj   <- st_transform(sa, target_crs)
eez_proj  <- st_transform(eez_sa, target_crs)
grid_proj <- st_transform(grid_heat, target_crs)

#-------------------------------------------------------------------------------
# 3. SUMMARY STATS (for subtitle)
#-------------------------------------------------------------------------------

n_records_total <- sum(grid_proj$n_records, na.rm = TRUE)
n_cells_sampled <- sum(grid_proj$n_records > 0, na.rm = TRUE)

#-------------------------------------------------------------------------------
# 4. PLOT
#-------------------------------------------------------------------------------

p_int <- ggplot() +
  geom_sf(data = sa_proj, fill = "grey90", color = "grey30", linewidth = 0.2) +
  geom_sf(data = eez_proj, fill = NA, color = "red3", linewidth = 0.6) +
  geom_sf(data = grid_proj, aes(fill = n_records), color = NA, alpha = 0.95) +
  scale_fill_viridis_c(
    name   = "Records per cell",
    trans  = scales::pseudo_log_trans(base = 10),
    option = "plasma",
    breaks = c(0, 1, 10, 100, 1000, 10000, 100000),
    labels = comma
  ) +
  coord_sf(
    crs = 4326,
    xlim = c(10, 40),
    ylim = c(-40, -25),
    expand = FALSE
  ) +
  scale_x_continuous(
    name   = "Longitude",
    breaks = seq(10, 40, 5),
    labels = function(x) paste0(x, "°E")
  ) +
  scale_y_continuous(
    name   = "Latitude",
    breaks = seq(-40, -20, 5),
    labels = function(x) paste0(abs(x), "°S")
  ) +
  theme_classic(base_family = "serif", base_size = 10) +
  theme(
    legend.position = "right",
    legend.title    = element_text(size = 9),
    legend.text     = element_text(size = 8),
    plot.title      = element_text(face = "bold", size = 11),
    plot.subtitle   = element_text(size = 9),
    axis.text  = element_text(size = 8),
    axis.title = element_text(size = 9),
    plot.margin = margin(6, 6, 6, 6)
  )

p_int

ggsave(
  filename = "FIG9_sampling_intensity.png",
  plot = p_int,
  width = 180, height = 120, units = "mm",
  dpi = 600
)

#-------------------------------------------------------------------------------
#-----MPA coverage in % and source specific MPA coverage (Table 5)
#-------------------------------------------------------------------------------
# EEZ-level MPA coverage
target_crs <- 32735

eez_proj  <- st_transform(eez_sa, target_crs)
mpa_proj  <- st_transform(mpas_sa, target_crs)

eez_area <- as.numeric(st_area(st_union(eez_proj)))
mpa_area <- as.numeric(st_area(st_union(mpa_proj)))

pct_mpa_eez <- 100 * mpa_area / eez_area
pct_mpa_eez #5.366536

mpa_record_summary <- final_dat %>%
  summarise(
    total_records = n(),
    records_in_mpa = sum(in_mpa),
    pct_records_in_mpa = 100 * records_in_mpa / total_records
  )

mpa_record_summary
#  total_records   records_in_mpa  pct_records_in_mpa 
# 1106336          66415           6.003149 

source_mpa <- final_dat %>%
  group_by(source) %>%
  summarise(
    n_records = n(),
    n_in_mpa  = sum(in_mpa),
    pct_in_mpa = 100 * n_in_mpa / n_records,
    n_species = n_distinct(species[in_mpa == 1]),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_in_mpa))

source_mpa
# source      n_records  n_in_mpa   pct_in_mpa n_species                                
#INAT            6387     5022     78.6         552 
#BRUV           20164    14859     73.7         598 
#LITERATURE      2557     1505     58.9         332 
#MUSEUM         19173     8963     46.7        1316 
#DEM_TRAWL     100577     3795      3.77         80 
#LINEFISH      817090    28587      3.50         85 
#CAPFISH       126929     3603      2.84         59 
#MW_TRAWL       13459       81      0.602         8 

#-------------------------------------------------------------------------------
#-----MPA, decleration year, species and % records recorded post declaration
#-------------------------------------------------------------------------------
# Keep only needed fields

mpa_years <- mpas_sa %>%
  select(
    mpa_name = ORIG_NME,        # <-- this is your MPA name
    declaration_year = DECL_YEAR
  )

# Join to points
dat_sf_mpa <- st_as_sf(final_dat, coords = c("longitude","latitude"), crs = 4326)

dat_sf_mpa <- st_join(dat_sf_mpa, mpa_years)

mpa_temporal <- dat_sf_mpa %>%
  filter(
    in_mpa == 1,
    !is.na(declaration_year),
    !is.na(year)
  ) %>%
  mutate(
    post_decl = year >= declaration_year
  ) %>%
  summarise(
    total_mpa_records = n(),
    post_decl_records = sum(post_decl),
    pct_post_decl = 100 * post_decl_records / total_mpa_records
  )

mpa_temporal
#  total_mpa_records post_decl_records    pct_post_decl   
#  51556             26820                 52.0211 

#for each mpa, table with decl year, n records, species n, % records post decl.
dat_sf_mpa <- dat_sf_mpa %>%
  filter(
    !is.na(declaration_year),
    !is.na(year)
  ) %>%
  mutate(
    post_decl = year >= declaration_year
  )

mpa_summary <- dat_sf_mpa %>%
  st_drop_geometry() %>%
  group_by(mpa_name, declaration_year) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species),
    pct_post_decl = 100 * sum(post_decl) / n_records,
    .groups = "drop"
  ) %>%
  arrange(desc(n_records))

mpa_summary <- mpa_summary %>%
  filter(
    !is.na(mpa_name),                # remove missing names
    mpa_name != "NA"                 # remove string NA
  )

mpa_summary <- mpa_summary %>%
  filter(str_detect(mpa_name, "Marine"))

print(mpa_summary, n = Inf)
#   mpa_name                                                    declaration_year n_records n_species pct_post_decl
#
# Agulhas Bank Complex Marine Protected Area                              2019     11926        67         0.679
# St. Lucia Marine Reserve                                                1979      5529       762        84.6  
# Aliwal Shoal Marine Protected Area                                      2004      3878       597        46.9  
# Table Mountain National Park Marine Protected Area                      2004      2981       190        80.6  
# iSimangaliso Marine Protected Area                                      2019      2529       268         3.20 
# Addo Elephant Marine Protected Area                                     2019      1257       127         0.875
# Pondoland Marine Protected Area                                         2004       876       168         0.342
# Cape Canyon Marine Protected Area                                       2019       848        42         0    
# uThukela Marine Protected Area                                          2019       811       238         0.863
# Orange Shelf Edge Marine Protected Area                                 2019       721        57         2.64 
# Sixteen Mile Beach Marine Protected Area                                2000       541        11        99.4  
# Maputaland Marine Reserve                                               1987       492       336        96.1  
# Robben Island Marine Protected Area                                     2019       420        32         1.19 
# Southwest Indian Seamount Marine Protected Area                         2019       309        52         9.71 
# Benguela Muds Marine Protected Area                                     2019       234        17         2.56 
# Agulhas Muds Marine Protected Area                                      2019       171        26         0    
# Walker Bay Whale Sanctuary Marine Protected Area                        2001       168        15        95.8  
# Protea Banks Marine Protected Area                                      2019       146        99         0.685
# Betty's Bay Marine Protected Area                                       2000       134        40        88.8  
# Namaqua National Park Marine Protected Area                             2019        91        20         8.79 
# Childs Bank Marine Protected Area                                       2019        88        20         0    
# Namaqua Fossil Forest Marine Protected Area                             2019        88        16         0    
# Port Elizabeth Corals Marine Protected Area                             2019        86        22         0    
# Amathole Offshore Marine Protected Area                                 2019        76        42         1.32 
# De Hoop Marine Protected Area                                           2000        68        47        32.4  
# Gonubie Marine Protected Area                                           2011        38        33        57.9  
# Agulhas Front Marine Protected Area                                     2019        32         7         0    
# Goukamma Marine Protected Area                                          2000        32        29        87.5  
# Southeast Atlantic Seamounts Marine Protected Area                      2019        29        26         0    
# Dwesa-Cwebe Marine Protected Area                                       2000        28        21        21.4  
# Kei Marine Protected Area                                               2011        25        18        20    
# Browns Bank Corals Marine Protected Area                                2019        21        10         0    
# Robberg Marine Protected Area                                           2000        16        16        50    
# Sardinia Bay Marine Protected Area                                      2000        12        12        16.7  
# Stilbaai Marine Protected Area                                          2008         3         3       100    
# Tsitsikamma Marine Protected Area                                       2000         2         2        50    
# Gxulu Marine Protected Area                                             2011         1         1       100    
# Stilbaai Marine Protected Area (Geelkraans Restricted Zone)             2008         1         1       100   

#-------------------------------------------------------------------------------
#-----bioregions coverage
#-------------------------------------------------------------------------------
bioregion_summary <- final_dat %>%
  group_by(region9) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species),
    n_cells   = n_distinct(grid_id),
    .groups = "drop"
  ) %>%
  arrange(desc(n_records))

bioregion_summary

# BIOREGION                  n_records  n_species n_cells   
# South-west Indian Offshore    321453       624     174
# Agulhas                       262209       675     106
# South-Western Cape            220888       339      44
# Atlantic Offshore             150483       443     167
# Namaqua                       104785       212      82
# Indo-Pacific Offshore          18318       120      51
# Natal                          13162      1185      28
# Delagoa                        13116      1042       6
# West Indian Offshore            1891       312      42
# NA                                31        13       3. #inspect

# Attach bioregion to grid (via spatial join)
grid_bio <- st_join(grid_sa, bioregions_9[, "region9"])

# Mark sampled cells
grid_bio <- grid_sa %>%
  mutate(
    region9 = st_join(
      st_centroid(grid_sa),
      bioregions_9[, "region9"]
    )$region9
  )   # bring geometry back

grid_bio <- grid_bio %>%
  left_join(
    final_dat %>%
      st_drop_geometry() %>%
      distinct(grid_id) %>%
      mutate(sampled = TRUE),
    by = "grid_id"
  ) %>%
  mutate(sampled = replace_na(sampled, FALSE))

grid_bio_proj <- st_transform(grid_bio, target_crs)

grid_bio_proj <- grid_bio_proj %>%
  mutate(area = as.numeric(st_area(geometry)))

bioregion_cov <- grid_bio_proj %>%
  group_by(region9) %>%
  summarise(
    pct_coverage = 100 * sum(area[sampled]) / sum(area),
    n_cells_sampled = sum(sampled),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_coverage))

bioregion_cov

# BIOREGION                  pct_coverage    n_cells_sampled    
# South-west Indian Offshore    73.5             146
# Agulhas                       100.0              85
# South-Western Cape            100                25
# Atlantic Offshore             69.9             150
# Namaqua                       99.8              71
# Indo-Pacific Offshore         26.1              49
# Natal                          99.8              17
# Delagoa                        100                 2
# West Indian Offshore           81.1              41
# NA                               99.9               1 #inspect

#UNDERPROTECTED BIOREGIONS
grid_mpa <- grid_sa %>%
  mutate(in_mpa = lengths(st_intersects(geometry, mpas_sa)) > 0)


grid_full <- grid_sa %>%
  left_join(
    grid_cov %>% st_drop_geometry(),
    by = "grid_id"
  ) %>%
  left_join(
    grid_mpa %>% st_drop_geometry() %>% select(grid_id, in_mpa),
    by = "grid_id"
  ) %>%
  st_join(bioregions_9[, "region9"]) %>%
  mutate(
    sampled      = replace_na(sampled, FALSE),
    in_mpa       = replace_na(in_mpa, FALSE)
  )

grid_full <- grid_full %>%
  mutate(
    not_sampled = !sampled,
    not_protected = !in_mpa,
    
    critical_gap = not_sampled & not_protected
  )

bio_gap_summary <- grid_full %>%
  st_drop_geometry() %>%
  group_by(region9) %>%
  summarise(
    total_cells = n(),
    sampled_cells = sum(sampled),
    protected_cells = sum(in_mpa),
    
    undersampled_cells = sum(not_sampled),
    under_protected_cells = sum(not_protected),
    
    critical_gap_cells = sum(critical_gap),
    
    pct_sampled = 100 * sampled_cells / total_cells,
    pct_protected = 100 * protected_cells / total_cells,
    pct_critical_gap = 100 * critical_gap_cells / total_cells,
    
    .groups = "drop"
  ) %>%
  arrange(desc(pct_critical_gap))

bio_gap_summary

#   region9             total_cells sampled_cells protected_cells undersampled_cells under_protected_cells critical_gap_cells pct_sampled pct_protected pct_critical_gap
# NA                            1             0               0                  1                     1                  1         0            0              100   
# Indo-Pacific Offsh…         241            71              14                170                   227                161        29.5          5.81            66.8 
# Atlantic Offshore           251           172              34                 79                   217                 75        68.5         13.5             29.9 
# South-west Indian …         286           198              55                 88                   231                 81        69.2         19.2             28.3 
# West Indian Offsho…          63            46              14                 17                    49                 16        73.0         22.2             25.4 
# Natal                        33            30              23                  3                    10                  3        90.9         69.7              9.09
# Namaqua                      90            84              25                  6                    65                  5        93.3         27.8              5.56
# Agulhas                     111           109              62                  2                    49                  0        98.2         55.9              0   
# Delagoa                       7             7               7                  0                     0                  0       100          100                0   
# South-Western Cape           45            45              21                  0                    24                  0       100           46.7              0 

#COAST
grid_coast_lookup <- final_dat %>%
  st_drop_geometry() %>%
  distinct(grid_id, coast)

grid_coast_cov <- grid_sa %>%
  left_join(grid_coast_lookup, by = "grid_id") %>%
  left_join(
    final_dat %>%
      st_drop_geometry() %>%
      distinct(grid_id) %>%
      mutate(sampled = TRUE),
    by = "grid_id"
  ) %>%
  mutate(
    sampled = replace_na(sampled, FALSE),
    not_sampled = !sampled
  )

coast_gap_summary <- grid_coast_cov %>%
  st_drop_geometry() %>%
  left_join(
    grid_full %>% st_drop_geometry() %>% select(grid_id, in_mpa),
    by = "grid_id"
  ) %>%
  mutate(
    not_protected = !in_mpa,
    critical_gap = not_sampled & not_protected
  ) %>%
  group_by(coast) %>%
  summarise(
    total_cells = n(),
    sampled_cells = sum(sampled),
    undersampled_cells = sum(not_sampled),
    protected_cells = sum(in_mpa),
    under_protected_cells = sum(not_protected),
    critical_gap_cells = sum(critical_gap),
    percent_sampled = 100 * sampled_cells / total_cells,
    percent_undersampled = 100 * undersampled_cells / total_cells,
    percent_protected = 100 * protected_cells / total_cells,
    percent_critical_gap = 100 * critical_gap_cells / total_cells,
    .groups = "drop"
  )

coast_gap_summary
#did not work

#GAP PLOT
p_gap <- ggplot() +
  geom_sf(data = grid_full, aes(fill = critical_gap), color = NA) +
  scale_fill_manual(
    values = c("TRUE" = "red", "FALSE" = "grey90"),
    name = "Critical gap"
  ) +
  theme_classic()
