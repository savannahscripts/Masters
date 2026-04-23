#5.depth
#-----depth summary
#-----records vs depth raw numbers plot y=records, depth bin = x axis
#-----violin depth by data source and by sampling method with tests and effect sizes

#-------------------------------------------------------------------------------
# 5. DEPTH ANALYSES
#-------------------------------------------------------------------------------
# OBJECTIVES:
# - Quantify depth distribution of occurrence records across the EEZ
# - Assess sampling bias across depth gradients
# - Compare depth distributions among sampling methods and data sources
# - Evaluate how sampling effort varies with depth (records + richness)
#-------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)
library(forcats)
install.packages("ggridges")
library(ggridges)

final_dat <- final_dat %>%
  mutate(
    gear_grouped = case_when(
      method %in% c("BRUV", "BRUV survey") ~ "BRUV",
      method %in% c("SECIFA") ~ "Inshore trawl",
      method %in% c("SADSTIA") ~ "Offshore trawl",
      method %in% c("SAPFIA") ~ "Pelagic seine",
      method == "SAHLLA" ~ "Longline",
      method %in% c("Demersal trawl", "Trawl", "BeamTrawl", "DnetTrawl") ~ "Demersal trawl",
      method == "HUMAN_OBSERVATION" ~ "CS (iNaturalist)",
      method == "Midwater trawl" ~ "Midwater trawl observer",
      method %in% c("PRESERVED_SPECIMEN") ~ "Unknown gear (museum)",
      method %in% c("Records") ~ "Unknown gear (literature records)",
      method == "VisualEstimate" ~ "Visual estimate",
      method %in% c("SeineNet") ~ "Seine net estuarine",
      method %in% c("LINE") ~ "NMLS Angling data",
      method %in% c("ShoreAngling") ~ "Shore angling",
      method %in% c("BoatAngling") ~ "Boat angling",
      method %in% c("Angling", "SHORTHAND_ROD") ~ "Unspecified angling",
      method == "PlanktonNet" ~ "Plankton net",
      method == "GillNet" ~ "Gillnet estuarine",
      method %in% c("FykeNet", "Trap") ~ "Fykenet / trap",
      method %in% c("SPEAR", "SHORT_SPEAR") ~ "Spear",
      method == "POLE" ~ "Pole",
      method == "UVC" ~ "UVC",
      method == "Chemical" ~ "Chemical intertidal sampling",
      method == "DeadCollection" ~ "Collection of dead fish",
      method == "Mixed" ~ "Mixed gears",
      TRUE ~ "Other / unknown"
    )
  )

depth_dat <- final_dat %>%
  filter(!is.na(depth)) %>%
  mutate(
    depth_pos = abs(as.numeric(depth)),
    gear      = str_squish(as.character(gear_grouped)),
    source    = as.character(source)
  ) %>%
  filter(
    !is.na(gear),
    gear != "",
    is.finite(depth_pos),
    depth_pos > 0
  )

#manage sample sizes
min_n <- 200

gear_n <- depth_dat %>%
  count(gear, name = "n") %>%
  arrange(desc(n))

depth_dat <- depth_dat %>%
  st_drop_geometry() %>%
  left_join(gear_n, by = "gear") %>%
  mutate(
    low_n = n < min_n,
    gear_label = paste0(gear, "\n(n=", n, ")")
  ) 

#order methods by depth
depth_dat <- depth_dat %>%
  mutate(
    gear_label = fct_reorder(
      gear_label,
      depth_pos,
      .fun = median,
      na.rm = TRUE
    )
  )

p_depth_violin_method <- ggplot(depth_dat, aes(x = gear_label, y = depth_pos)) +
  geom_violin(
    aes(alpha = !low_n),
    trim = FALSE,
    fill = "grey90",              
    color = "black",             
    linewidth = 0.35,
    draw_quantiles = c(0.25, 0.5, 0.75),
    adjust = 1.5,
    scale = "width"
  ) +
  geom_boxplot(
    width = 0.08,             
    outlier.shape = NA,
    fill = "white",
    color = "black",
    linewidth = 0.3
  ) +
  stat_summary(                 
    fun = median,
    geom = "point",
    size = 0.8,
    color = "black"
  ) +
  scale_alpha_manual(
    values = c(0.4, 1),
    guide = "none"
  ) +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    breaks = c(1, 10, 50, 100, 200, 500, 1000, 2000, 5000),
    labels = scales::comma
  ) +
  labs(
    title = "Depth distribution of occurrence records by sampling method",
    y = "Depth (m; pseudo-log scale)"
  ) +
  theme_classic(base_family = "serif", base_size = 10) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3)
  )

p_depth_violin_method

#RECORDS VS DEPTH RAW
depth_bins <- final_dat %>%
  filter(!is.na(depth)) %>%
  mutate(
    depth_pos = abs(depth),
    depth_bin = cut(
      depth_pos,
      breaks = c(0, 50, 100, 200, 500, 1000, 2000, Inf),
      labels = c("0–50", "50–100", "100–200", "200–500",
                 "500–1000", "1000–2000", ">2000"),
      include.lowest = TRUE
    )
  )

depth_summary <- depth_bins %>%
  group_by(depth_bin) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species),
    .groups = "drop"
  )

ggplot(depth_summary, aes(x = depth_bin, y = n_records)) +
  geom_col(fill = "grey70", color = "black") +
  theme_classic(base_family = "serif") +
  labs(
    title = "Sampling intensity across depth",
    x = "Depth interval (m)",
    y = "Number of records"
  )

ggplot(depth_dat, aes(x = depth_pos, colour = source)) +
  geom_density(linewidth = 1, adjust = 1) +
  scale_x_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    labels = comma
  ) +
  theme_classic(base_family = "serif") +
  labs(
    title = "Depth distribution across data sources",
    x = "Depth (m)",
    y = "Density"
  )

#TEST
df <- depth_dat %>%
  select(depth_pos, source) %>%
  drop_na()

kruskal.test(depth_pos ~ source, data = df)

#Kruskal-Wallis rank sum test
#data:  depth_pos by source
#Kruskal-Wallis chi-squared = 170755, df = 7,
#p-value < 2.2e-16


#BYSOURCE
breaks_shelf <- c( 0, 5, 10, 20, 30, 50, 75, 100, 150, 200, 300, 500, 750, 1000, 1500, 2000, 3000, 4000, 6000 )
#-----------------------------------
# DATA PREP
#-----------------------------------
dat_depth <- final_dat %>%
  st_drop_geometry() %>%
  filter(!is.na(depth), is.finite(depth)) %>%
  mutate(
    depth_m   = as.numeric(depth),
    depth_pos = abs(depth),
    source    = str_trim(as.character(source))
  ) %>%
  mutate(
    source = recode(source,
                    "INAT"       = "iNaturalist",
                    "MW_TRAWL"   = "DFFE MW Trawl",
                    "DEM_TRAWL"  = "DFFE Trawl",
                    "CAPFISH"    = "CapMarine",
                    "LINEFISH"   = "NMLS",
                    "BRUV"       = "BRUV",
                    "MUSEUM"     = "Museum",
                    "LITERATURE" = "Literature",
                    .default     = source
    )
  ) %>%
  mutate(
    source = factor(source, levels = c(
      "CapMarine",
      "NMLS",
      "DFFE Trawl",
      "DFFE MW Trawl",
      "BRUV",
      "Museum",
      "Literature",
      "iNaturalist"
    ))
  )

#-----------------------------------
# PLOT
#-----------------------------------
p_depth_violin_source <- ggplot(dat_depth, aes(x = source, y = depth_pos)) +
  
  geom_violin(
    trim = FALSE,
    fill = "grey90",              
    color = "black",
    linewidth = 0.35,
    draw_quantiles = c(0.25, 0.5, 0.75)
  ) +
  
  geom_boxplot(
    width = 0.10,
    outlier.shape = NA,
    fill = "white",
    color = "black",
    linewidth = 0.3
  ) +
  
  stat_summary(
    fun = median,
    geom = "point",
    size = 0.8,
    color = "black"
  ) +
  
  coord_flip() +
  
  scale_y_continuous(
    name   = "Depth (m; pseudo-log scale)",
    trans  = scales::pseudo_log_trans(base = 10),
    breaks = breaks_shelf,
    labels = scales::comma
  ) +
  
  labs(
    title = "Depth distribution of occurrence records by data source",
    x = "Data source"
  ) +
  
  theme_classic(base_family = "serif", base_size = 11) +
  theme(
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3)
  )

p_depth_violin_source

ggsave(
  filename = "depth_distribution_method.tiff",
  plot = p_depth_violin_method,
  width = 8,
  height = 5,
  dpi = 600,
  compression = "lzw"
)

ggsave(
  "depth_distribution_sources.pdf",
  plot = p_depth_violin_source,
  width = 200,
  height = 130,
  units = "mm",
  dpi = 600
)


#density on pseudo log
ggplot(dat_depth, aes(x = depth_pos, colour = source)) +
  geom_density(linewidth = 1, adjust = 1) +
  scale_x_continuous(
    "Depth (m) [pseudo-log scale]",
    trans  = scales::pseudo_log_trans(base = 10),
    breaks = breaks_shelf,
    labels = breaks_shelf,
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  scale_y_continuous("Density") +
  labs(colour = "Source") +
  theme_classic(base_family = "Times New Roman") +
  theme(
    legend.position = "right",
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 9)
  )

# ______________________________________________________________________
#Species richness patterns across depth
# ______________________________________________________________________
master_dat_xy <- master_dat %>%
  filter(!is.na(latitude), !is.na(longitude))

master_pts <- st_as_sf(
  master_dat_xy,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)
#only keep points inside eez
master_sa <- master_pts[eez, ]      # spatial subset
nrow(master_sa)

#convert to mercator
lonlat_to_world_merc <- function(lon, lat, R = 6378137) {
  lon_rad <- lon * pi / 180
  lat_rad <- lat * pi / 180
  
  x <- R * lon_rad
  y <- R * log(tan(pi/4 + lat_rad/2))
  
  cbind(x, y)
}

#apply
# Get lon/lat from the sf object
coords_ll <- st_coordinates(master_sa)   # X = lon, Y = lat

# Convert to World Mercator
coords_merc <- lonlat_to_world_merc(
  lon = coords_ll[, "X"],
  lat = coords_ll[, "Y"]
)
# coords_merc = x/y matrix
depth_vals <- terra::extract(depth_rast, coords_merc)

head(depth_vals)

master_sa$depth_m <- depth_vals$SA_bathymetry_100m_v1

summary(master_sa$depth_m)
head(master_sa$depth_m)


#create bins
pts_depth <- master_sa %>%
  filter(!is.na(depth_m)) %>%
  mutate(
    depth_bin = cut(
      depth_m,
      breaks = c(0, 50, 100, 200, 500, 1000, 2000, Inf),
      labels = c("0–50", "50–100", "100–200", "200–500",
                 "500–1000", "1000–2000", ">2000"),
      include.lowest = TRUE,
      right = FALSE
    )
  )

rich_depth <- pts_depth |>
  group_by(depth_bin) |>
  summarise(
    n_species = n_distinct(species),
    n_records = n(),
    .groups = "drop"
  ) %>%
  arrange(depth_bin)

print(rich_depth, n = Inf) #mostly NAS

#troubleshooting
summary(master_sa$depth_m)
table(is.na(master_sa$depth_m))
master_sa <- master_sa %>%
  mutate(depth_m = abs(depth_m))
pts_depth <- master_sa %>%
  filter(!is.na(depth_m))


pts_depth <- pts_depth %>%
  mutate(
    depth_bin = cut(
      depth_m,
      breaks = c(0, 50, 100, 200, 500, 1000, 2000, Inf),
      labels = c("0–50", "50–100", "100–200", "200–500",
                 "500–1000", "1000–2000", ">2000"),
      include.lowest = TRUE,
      right = FALSE
    )
  )

rich_depth <- pts_depth %>%
  group_by(depth_bin) %>%
  summarise(
    n_species = n_distinct(species),
    n_records = n(),
    .groups = "drop"
  ) %>%
  arrange(depth_bin)

print(rich_depth, n = Inf)

ggplot(rich_depth, aes(x = depth_bin, y = n_species)) +
  geom_col(fill = "grey70", color = "black", width = 0.7) +
  labs(
    title = "Species Richness Across Depth",
    x = "Depth interval (m)",
    y = "Number of species"
  ) +
  theme_classic(base_family = "Times", base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #nice plot

#GAM
# Use mid-point of depth bins
bin_mid <- c(25, 75, 150, 350, 750, 1500, 3000)

gam_df <- data.frame(
  mid_depth = bin_mid,
  n_species = rich_depth$n_species
)

mod_gam <- gam(n_species ~ s(mid_depth, k = 5), data = gam_df)

summary(mod_gam)

# Plot
plot(mod_gam, shade = TRUE, rug = FALSE)

#using other bins
master_sa <- master_sa %>%
  mutate(depth_pos = abs(depth_m))   # convert negative depths to positive metres


# Create ecological depth bins
master_sa <- master_sa %>%
  mutate(depth_ecozone = cut(
    depth_pos,
    breaks = c(
      0, 10, 30, 50, 100, 200, 300, 400, 500, 800, 1000, 1500, 3000, Inf
    ),
    labels = c(
      "0–10 m",
      "10-30 m",
      "30–50 m",
      "50-100 m",
      "100–200 m",
      "200–300 m",
      "300–400 m",
      "400–500 m",
      "500–800 m",
      "800–1000 m",
      "1000–1500 m",
      "1500–3000 m",
      ">3000 m"
    ),
    include.lowest = TRUE,
    right = FALSE
  ))

# Summarise richness and record counts per ecological zone
rich_depth_ecozone <- master_sa %>%
  st_drop_geometry() %>%   # remove geometry for summarising
  filter(!is.na(depth_ecozone), !is.na(species)) %>%
  group_by(depth_ecozone) %>%
  summarise(
    n_species = n_distinct(species),
    n_records = n()
  ) %>%
  arrange(depth_ecozone)

rich_depth_ecozone

#fit to bellcurve
df <- rich_depth_ecozone %>%
  mutate(mid_depth = c(5, 20, 40, 75, 150, 250, 350, 450, 650, 900, 1250, 2250, 3500))

# Starting values from visual inspection
start_vals <- list(a = max(df$n_species), 
                   mu = df$mid_depth[which.max(df$n_species)],
                   sigma = 500)

# Gaussian bell curve model
gauss_model <- nls(
  n_species ~ a * exp(-(mid_depth - mu)^2 / (2 * sigma^2)),
  data = df,
  start = start_vals
)

summary(gauss_model)
#quadratic model?
quad_mod <- lm(n_species ~ poly(mid_depth, 2, raw = TRUE), data = df)
summary(quad_mod)

#GAM incres smothness detect unimodality
gam_mod <- gam(n_species ~ s(mid_depth, k = 7), data = df)
summary(gam_mod)
plot(gam_mod, shade = TRUE, scale = 0)

