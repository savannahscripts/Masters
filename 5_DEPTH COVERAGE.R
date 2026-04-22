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

final_dat <- final_dat %>%
  mutate(
    gear_grouped = case_when(
      method %in% c("BRUV", "BRUV survey") ~ "BRUV",
      method %in% c("SECIFA") ~ "Inshore trawl",
      method %in% c("SADSTIA") ~ "Offshore trawl",
      method %in% c("SAPFIA") ~ "Pelagic seine",
      method == "SAHLLA" ~ "Longline",
      method %in% c("Demersal trawl", "Trawl", "BeamTrawl", "DnetTrawl") ~ "Demersal trawl",
      method == "HUMAN_OBSERVATION" ~ "Citizen science (iNaturalist)",
      method == "Midwater trawl" ~ "Midwater trawl observer",
      method %in% c("PRESERVED_SPECIMEN") ~ "GBIF museum records",
      method %in% c("Records") ~ "Records from literature",
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
      method == "Mixed" ~ "Mixed methods",
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

p_depth_violin <- ggplot(depth_dat, aes(x = gear_label, y = depth_pos)) +
  
  geom_violin(
    aes(alpha = !low_n),
    trim = FALSE,
    fill = "grey85",
    color = "grey20",
    linewidth = 0.25
  ) +
  
  geom_boxplot(
    width = 0.10,
    outlier.shape = NA,
    fill = "white",
    color = "black",
    linewidth = 0.25
  ) +
  
  scale_alpha_manual(values = c(0.4, 1), guide = "none") +
  
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    breaks = c(1, 10, 50, 100, 200, 500, 1000, 2000, 5000),
    labels = comma
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

p_depth_violin

#spear, boat angling, records from literature should be investigated
#wrong here definitely (might have to remove)

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




# ============================================================
# 3) Prepare dataset (KEEP ALL GEARS)
# ============================================================
depth_method_all <- master_sa %>%
  st_drop_geometry() %>%
  filter(!is.na(gear_grouped)) %>%   # keep ALL methods
  mutate(
    gear      = str_squish(as.character(gear_grouped)),
    depth_pos = abs(as.numeric(depth_m))
  ) %>%
  filter(gear != "")

# ============================================================
# 4) Count sample sizes (for labels + transparency)
# ============================================================
min_n <- 200

gear_n <- depth_method_all %>%
  count(gear, name = "n") %>%
  arrange(desc(n))

# Join counts back
depth_method_all <- depth_method_all %>%
  left_join(gear_n, by = "gear") %>%
  mutate(
    low_n = n < min_n
  )

# ============================================================
# 5) Filter ONLY for plotting (depth must exist)
# ============================================================
depth_method <- depth_method_all %>%
  filter(
    !is.na(depth_pos),
    is.finite(depth_pos),
    depth_pos > 0
  )

# ============================================================
# 6) Label gears with sample sizes (ALL GEARS INCLUDED)
# ============================================================
depth_method <- depth_method %>%
  mutate(
    gear_label = paste0(gear, "\n(n=", n, ")")
  )

# ============================================================
# 7) Order by median depth
# ============================================================
depth_method <- depth_method %>%
  mutate(
    gear_label = forcats::fct_reorder(
      gear_label,
      depth_pos,
      .fun = median,
      na.rm = TRUE
    )
  )

# ============================================================
# 8) Plot
# ============================================================
p_depth_violin <- ggplot(depth_method, aes(x = gear_label, y = depth_pos)) +
  
  geom_violin(
    aes(alpha = !low_n),   # low sample sizes slightly faded
    trim = FALSE,
    fill = "grey85",
    color = "grey20",
    linewidth = 0.25
  ) +
  
  geom_boxplot(
    width = 0.10,
    outlier.shape = NA,
    fill = "white",
    color = "black",
    linewidth = 0.25
  ) +
  
  scale_alpha_manual(values = c(0.4, 1), guide = "none") +
  
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    breaks = c(1, 10, 50, 100, 200, 500, 1000, 2000, 5000),
    labels = scales::comma,
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  
  labs(
    title = "Depth distribution of occurrence records by sampling method",
    y = "Depth (m; pseudo-log scale)"
  ) +
  
  theme_classic(base_family = "serif", base_size = 10) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 10),
    axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    axis.text.y  = element_text(size = 8),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 11, hjust = 0),
    plot.subtitle = element_text(size = 9, hjust = 0),
    plot.margin = margin(6, 8, 6, 6)
  )

p_depth_violin

#_____________________________________________________________________________
#_____________________________________________________________________________
#_____________________________________________________________________________
#OLD CODE
#_____________________________________________________________________________
#_____________________________________________________________________________
#_____________________________________________________________________________

# 5) Build plotting data (clean + filter)
min_n <- 200  # threshold for display
names(master_sa)

depth_method <- master_sa %>%
  st_drop_geometry() %>%
  filter(!is.na(depth_m), !is.na(gear_grouped)) %>%
  mutate(
    depth_pos = abs(as.numeric(depth_m)),               # convert to positive depth (m)
    gear      = str_squish(as.character(gear_grouped))
  ) %>%
  filter(
    gear != "",
    is.finite(depth_pos),
    depth_pos > 0
  )

# Drop tiny groups for readability
gear_n <- depth_method %>%
  count(gear, name = "n") %>%
  arrange(desc(n))

##FILTERING BLOCK
#keep_gears <- gear_n %>%
 # filter(n >= min_n) %>%
 # pull(gear)

#depth_method <- depth_method %>%
#  filter(gear %in% keep_gears)

# Add sample sizes to labels
gear_n_keep <- gear_n %>% filter(gear %in% keep_gears)
depth_method <- depth_method %>%
  mutate(
    gear_label = paste0(gear, "\n(n=", gear_n_keep$n[match(gear, gear_n_keep$gear)], ")")
  )

# Order methods by median depth (shallow → deep)
gear_order <- depth_method %>%
  group_by(gear_label) %>%
  summarise(med = median(depth_pos), .groups = "drop") %>%
  arrange(med) %>%
  pull(gear_label)

depth_method$gear_label <- factor(depth_method$gear_label, levels = gear_order)

depth_method <- depth_method %>%
  mutate(
    gear_label = forcats::fct_reorder(gear_label, depth_pos, .fun = median, na.rm = TRUE)
  )
# 6) Plot
p_depth_violin <- ggplot(depth_method, aes(x = gear_label, y = depth_pos)) +
  geom_violin(
    trim = FALSE,
    fill = "grey85",
    color = "grey20",
    linewidth = 0.25
  ) +
  geom_boxplot(
    width = 0.10,
    outlier.shape = NA,
    fill = "white",
    color = "black",
    linewidth = 0.25
  ) +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    breaks = c(1, 10, 50, 100, 200, 500, 1000, 2000, 5000),
    labels = scales::comma,
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  labs(
    title = "Depth distribution of occurrence records by sampling method",
    subtitle = paste0(
      "Sampling methods with \u2265 ", min_n,
      " records; violin widths indicate record density"
    ),
    y = "Depth (m; pseudo-log scale)"
  ) +
  theme_classic(base_family = "serif", base_size = 10) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 10),
    axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    axis.text.y  = element_text(size = 8),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 11, hjust = 0),
    plot.subtitle = element_text(size = 9, hjust = 0),
    plot.margin = margin(6, 8, 6, 6)
  )

p_depth_violin

# 7) Export (PDF + high-res PNG)
ggsave(
  filename = "depth_distribution_by_sampling_method.pdf",
  plot = p_depth_violin,
  width = 180, height = 120, units = "mm"
)

ggsave(
  filename = "FigureX_depth_distribution_by_sampling_method_600dpi.png",
  plot = p_depth_violin,
  width = 180, height = 120, units = "mm",
  dpi = 600
)
library(dplyr)
library(ggplot2)
library(sf)
library(stringr)

depth_rast <- rast("SA_bathymetry_100m_v1/SA_bathymetry_100m_v1.tif")
depth_rast

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
# 111004 = pts inside eez

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
# coords_merc is your x/y matrix
depth_vals <- terra::extract(depth_rast, coords_merc)

head(depth_vals)

master_sa$depth_m <- depth_vals$SA_bathymetry_100m_v1


# prep a clean plotting df
depth_method <- master_sa %>%
  st_drop_geometry() %>%
  filter(!is.na(depth_m)) %>%
  mutate(
    depth_pos = abs(as.numeric(depth_m)),
    gear = as.character(gear_grouped)
  ) %>%
  filter(!is.na(gear), gear != "", is.finite(depth_pos), depth_pos > 0)

# OPTIONAL: drop tiny groups so the figure is readable (choose threshold)
min_n <- 200
keep_gears <- depth_method %>%
  count(gear, name = "n") %>%
  filter(n >= min_n) %>%
  pull(gear)

depth_method <- depth_method %>%
  filter(gear %in% keep_gears)


gear_counts <- depth_method %>%
  count(gear)

depth_method <- depth_method %>%
  mutate(
    gear = paste0(gear, "\n(n=", gear_counts$n[match(gear, gear_counts$gear)], ")")
  )

# order gears by median depth (nice for interpretation)
gear_order <- depth_method %>%
  group_by(gear) %>%
  summarise(med = median(depth_pos), n = n(), .groups = "drop") %>%
  arrange(med) %>%
  pull(gear)

depth_method$gear <- factor(depth_method$gear, levels = gear_order)


depth_method <- depth_method %>%
  mutate(gear = reorder(gear, depth_pos, median))
# plot (log depth is usually essential)
p_depth_violin <- ggplot(depth_method, aes(x = gear, y = depth_pos)) +
  geom_violin(trim = TRUE, fill = "grey85", color = "grey20", linewidth = 0.2) +
  geom_boxplot(width = 0.12, outlier.size = 0.2, outlier.alpha = 0.2,
               fill = "white", color = "black", linewidth = 0.25) +
  scale_y_log10(
    name = "Depth (m, log10 scale)",
    breaks = c(1, 10, 50, 100, 200, 500, 1000, 2000, 5000),
    labels = scales::comma
  ) +
  labs(
    title = "Depth distribution of occurrence records by sampling method",
    subtitle = paste0("Methods with ≥ ", min_n, " records; violins show record density")
  ) +
  theme_classic(base_family = "serif", base_size = 10) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9),
    plot.margin = margin(6, 6, 6, 6)
  )

p_depth_violin



####depth







install.packages("terra")  
library(terra)
library(sf)
library(dplyr)
getwd()

depth_rast <- rast("SA_bathymetry_100m_v1/SA_bathymetry_100m_v1.tif")
depth_rast

plot(depth_rast, main = "South Africa bathymetry (100 m)")

summary(master_dat)
summary(eez)
summary(depth_rast)

plot(st_geometry(eez))
#drop na coords
names(master_dat)

summary(master_sa$depth_m)
head(master_sa$depth_m)

ggplot(master_sa |> st_drop_geometry(), aes(depth_m)) +
  geom_histogram(binwidth = 100, fill = "steelblue", color = "white") +
  labs(
    title = "Depth coverage of all SA-EEZ fish records",
    x = "Depth (m)",
    y = "Record count"
  ) +
  scale_x_reverse() +
  theme_classic(base_family = "Times New Roman") +
  theme(
    strip.text = element_text(size = 12),
    legend.position = "right"
  )


################
# Drop geometry + keep finite depths
dat_depth <- master_sa %>%
  st_drop_geometry() %>%
  filter(!is.na(depth_m), is.finite(depth_m)) %>%
  mutate(
    depth_m = as.numeric(depth_m),
    source  = as.factor(source)
  )

dat_depth <- dat_depth %>%
  mutate(depth_pos = abs(depth_m))


#depth class
dat_depth <- master_sa %>%
  st_drop_geometry() %>%
  filter(!is.na(depth_m), is.finite(depth_m)) %>%
  mutate(
    depth_pos = abs(depth_m),
    source = factor(source),
    depth_zone = ifelse(depth_pos < 500, "Shallow (<500 m)", "Deep (>500 m)")
  )

#faceted density plot
ggplot(dat_depth, aes(x = depth_pos, colour = source)) +
  geom_density(linewidth = 1, adjust = 1) +
  facet_wrap(~ depth_zone, scales = "free_x") +
  scale_x_continuous("Depth (m)") +
  scale_y_continuous("Density") +
  labs(colour = "Source") +
  theme_classic(base_family = "Times New Roman") +
  theme(
    strip.text = element_text(size = 12),
    legend.position = "right"
  )

sum(dat_depth$depth_m > 0)
mean(dat_depth$depth_m > 0) * 100
#depth density by source

ggplot(dat_depth, aes(x = depth_pos, colour = source)) +
  geom_density(linewidth = 1, adjust = 1) +
  scale_x_continuous("Depth (m)", expand = expansion(mult = c(0.01, 0.05))) +
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

#boxplot by source
ggplot(master_sa |> st_drop_geometry(),
       aes(x = source, y = depth_m)) +
  geom_boxplot(outlier.size = 0.5) +
  coord_flip() +
  labs(
    title = "Depth range sampled by each data source",
    x = "Source dataset",
    y = "Depth (m)"
  ) +
  theme_minimal()
names(master_sa)

#violin box plot
ggplot(dat_depth, aes(x = source, y = depth_pos)) +
  geom_violin(trim = TRUE, fill = "grey85", colour = "grey40") +
  geom_boxplot(width = 0.15, outlier.size = 0.3) +
  coord_flip() +
  labs(
    x = "Source dataset",
    y = "Depth (m)"
  ) +
  theme_classic(base_family = "serif", base_size = 14)

ggsave(
  "violin boxplot.tiff",
  width = 8,
  height = 5,
  dpi = 600,
  compression = "lzw"
)


library(scales)


#pseudo log scale
p_depth_violin <- ggplot(dat_depth, aes(x = source, y = depth_pos)) +
  geom_violin(trim = TRUE, fill = "grey85", colour = "grey40") +
  geom_boxplot(width = 0.15, outlier.size = 0.3) +
  coord_flip() +
  scale_y_continuous(
    name = "Depth (m) [pseudo-log scale]",
    trans = scales::pseudo_log_trans(base = 10),
    breaks = c(0, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 4000),
    labels = c("0", "5", "10", "20", "50", "100", "200", "500", "1000", "2000", "4000")
  ) +
  labs(x = "Source dataset") +
  theme_classic(base_family = "serif", base_size = 14)

p_depth_violin


#ppure log scale 

dat_depth <- dat_depth %>%
  mutate(depth_log10p1 = log10(depth_pos + 1))

ggplot(dat_depth, aes(x = source, y = depth_log10p1)) +
  geom_violin(trim = TRUE, fill = "grey85", colour = "grey40") +
  geom_boxplot(width = 0.15, outlier.size = 0.3) +
  coord_flip() +
  scale_y_continuous(
    name = expression(log[10]("(Depth + 1)")),
    breaks = log10(c(0, 10, 50, 100, 200, 500, 1000, 2000, 4000) + 1),
    labels = c("0", "10", "50", "100", "200", "500", "1000", "2000", "4000")
  ) +
  labs(x = "Source dataset") +
  theme_classic(base_family = "serif", base_size = 14)



###SHELF BREAKS #####
library(scales)

breaks_shelf <- c(
  0, 5, 10, 20, 30, 50, 75, 100, 150, 200,
  300, 500, 750, 1000, 1500, 2000, 3000, 4000, 6000
)

library(scales)
unique(dat_depth$source)

dat_depth <- master_sa %>%
  st_drop_geometry() %>%
  filter(!is.na(depth_m), is.finite(depth_m)) %>%
  mutate(
    depth_m   = as.numeric(depth_m),
    depth_pos = abs(depth_m),
    source    = as.character(source)   # <- key change
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

p_depth_violin <- ggplot(dat_depth, aes(x = source, y = depth_pos)) +
  geom_violin(trim = TRUE, fill = "grey85", colour = "grey40") +
  geom_boxplot(width = 0.15, outlier.size = 0.2) +
  coord_flip() +
  scale_y_continuous(
    name   = "Depth (m)",
    trans  = scales::pseudo_log_trans(base = 10),
    breaks = breaks_shelf) +
  labs(x = "Data source") +
  theme_classic(base_family = "serif", base_size = 12) +
  theme(
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5, hjust = 1)
  ) 

p_depth_violin



ggsave(
  filename = "violin_boxplot_logscale.tiff",
  plot = p_depth_violin,
  width = 8,
  height = 5,
  dpi = 600,
  compression = "lzw"
)

ggsave(
  "depth_distribution_sources.pdf",
  plot = p_depth_violin,
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


#boxplot by method
ggplot(master_sa |> st_drop_geometry(),
       aes(x = gear_grouped , y = depth_m)) +
  geom_boxplot(outlier.alpha = 0.3) +
  coord_flip() +
  labs(
    title = "Depth sampling distribution by method",
    x = "Sampling method",
    y = "Depth (m)"
  ) +
  theme_minimal()
install.packages("ggridges")
library(ggridges)
ggplot(master_sa |> st_drop_geometry(),
       aes(x = depth_m, y = source, fill = source)) +
  geom_density_ridges(scale = 1.2, alpha = 1.0, color = "white") +
  labs(
    title = "Depth distribution across data sources",
    x = "Depth (m)",
    y = "Source"
  ) +
  theme_ridges() +
  theme_classic(base_family = "Times", base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#TEST
df <- master_sa |> st_drop_geometry()

kruskal.test(depth_m ~ source, data = df)
install.packages("effectsize")
library(effectsize)

kruskal_result <- kruskal.test(depth_m ~ source, data = df)
kruskal_result
eta_squared(kruskal_result)


# Kruskal–Wallis 
kruskal_result <- kruskal.test(depth_m ~ source, data = df)

# Pull out numbers
H <- as.numeric(kruskal_result$statistic)      # chi-squared value
k <- length(unique(df$source))                 # number of sources
n <- sum(complete.cases(df$depth_m, df$source))# non-NA depth + source rows

H; k; n

# Effect sizes
eta2 <- (H - (k - 1)) / (n - 1)
eps2 <- (H - k + 1) / (n - k)

c(eta_squared = eta2, epsilon_squared = eps2)

library(effsize)

cliff.delta(df$depth_m[df$source == "BRUV"],
            df$depth_m[df$source == "DEM_TRAWL"])





df <- master_sa |> st_drop_geometry()

gear_groups <- unique(df$gear_grouped)
gear_groups
# Pairwise Cliff's delta function
pairwise_cliff <- function(g1, g2) {
  x <- df$depth_m[df$gear_grouped == g1]
  y <- df$depth_m[df$gear_grouped == g2]
  
  # drop NAs
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  
  cd <- cliff.delta(x, y)
  
  tibble(
    gear_1    = g1,
    gear_2    = g2,
    delta     = cd$estimate,
    magnitude = cd$magnitude
  )
}
# all unique pairs of gear groups
pairs_mat <- t(combn(gear_groups, 2))   # each row: c(g1, g2)

pairwise_results <- apply(
  pairs_mat,
  1,
  function(z) pairwise_cliff(z[1], z[2])
) |> bind_rows()

pairwise_results %>% print(n = Inf)




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

