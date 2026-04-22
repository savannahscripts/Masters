# ============================================================
#NMDS
# ============================================================
library(dplyr)
library(tidyr)
library(vegan)

# GRID × METHOD × FAMILY matrix
nmds_df <- final_dat %>%
  filter(!is.na(grid_id), !is.na(family), !is.na(method)) %>%
  group_by(grid_id, method, family) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(row_id = paste(grid_id, method, sep = "_")) %>%
  pivot_wider(
    names_from = family,
    values_from = n,
    values_fill = 0
  )
# metadata
meta <- nmds_df %>%
  select(row_id, grid_id, method)
# matrix
mat <- nmds_df %>%
  select(-row_id, -grid_id, -method)
# presence/absence 
mat <- (mat > 0) * 1
# remove rare families
mat <- mat[, colSums(mat) >= 5]
# remove weak rows
keep <- rowSums(mat) >= 5
mat <- mat[keep, ]
meta <- meta[keep, ]
# NMDS
set.seed(123)
nmds <- metaMDS(mat, distance = "bray", k = 2, trymax = 40)

scores_df <- as.data.frame(scores(nmds)) %>%
  mutate(row_id = rownames(.)) %>%
  left_join(meta, by = "row_id")
#P1 RAW METHODS
top_methods <- scores_df %>%
  count(method, sort = TRUE) %>%
  slice_head(n = 15) %>%
  pull(method)

scores_raw <- scores_df %>%
  mutate(
    method_plot = ifelse(method %in% top_methods, method, "Other")
  )

p_methods <- ggplot(scores_raw, aes(NMDS1, NMDS2, colour = method_plot)) +
  geom_point(size = 2.2, alpha = 0.7) +
  stat_ellipse(aes(group = method_plot), linewidth = 0.6, alpha = 0.3) +
  theme_classic(base_family = "Times") +
  labs(
    title = "NMDS – Assemblage structure by sampling method",
    colour = "Method"
  )

#P2 ECOLOGY
scores_ecol <- scores_df %>%
  mutate(
    method_ecol = case_when(
      
      # DEMERSAL
      method %in% c("Demersal trawl","Offshore trawl","Inshore trawl") ~ "Demersal",
      
      # PELAGIC
      method %in% c("Pole","Midwater trawl Observer","Longline","Pelagic seine") ~ "Pelagic",
      
      # COASTAL / NEARSHORE
      method %in% c(
        "Spear","BRUV","Seine net","Gillnet","Fykenet / trap",
        "UVC","Citizen science (iNaturalist)",
        "Boat angling","Shore angling","Unspecified angling",
        "NMLS Angling data","Chemical"
      ) ~ "Coastal",
      
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(method_ecol))

p_ecol <- ggplot(scores_ecol, aes(NMDS1, NMDS2, colour = method_ecol)) +
  geom_point(size = 2.5, alpha = 0.8) +
  stat_ellipse(aes(group = method_ecol), linewidth = 0.7, alpha = 0.3) +
  scale_colour_manual(values = c(
    "Demersal" = "#1b3b6f",
    "Pelagic"  = "#2a9d8f",
    "Coastal"  = "#f4a261"
  )) +
  theme_classic(base_family = "Times") +
  labs(
    title = "NMDS – Assemblage structure by ecological sampling domain",
    colour = "Domain"
  )

#P3 ENVIRONMENTAL GRADIENT
meta_env <- dat %>%
  group_by(grid_id, method) %>%
  summarise(
    mean_lon = mean(longitude, na.rm = TRUE),
    mean_depth = mean(depth, na.rm = TRUE),
    .groups = "drop"
  )

scores_env <- scores_df %>%
  left_join(meta_env, by = c("grid_id", "method"))

p_env <- ggplot(scores_env, aes(NMDS1, NMDS2)) +
  
  geom_point(
    aes(
      colour = mean_lon,
      size = abs(mean_depth)
    ),
    alpha = 0.6
  ) +
  
  scale_colour_viridis_c(option = "G", end = 0.95) +
  
  scale_size_continuous(range = c(1, 5)) +
  
  theme_classic(base_family = "Times") +
  
  labs(
    title = "NMDS – Environmental gradients in assemblage structure",
    colour = "Longitude (°E)",
    size = "Depth (m)"
  )

#FINAL PLOTS
p_methods  
p_ecol 
p_env 

# ============================================================
###SAVE
# ============================================================
ggsave("p_methods.png", p_gears_sorted,
       width = 7, height = 5, dpi = 600)

ggsave("p_ecol.png", p_eco,
       width = 7, height = 5.5, dpi = 600)

ggsave("p_env.png", p_env,
       width = 7, height = 5, dpi = 600)
# ============================================================
#TESTS
# ============================================================
# PERMANOVA
adonis_method <- adonis2(
  mat ~ method,
  data = meta,
  method = "bray",
  permutations = 999
)

adonis_method

# DISPERSION
disp_method <- betadisper(
  vegdist(mat, "bray"),
  meta$method
)

anova(disp_method)
permutest(disp_method)

# ============================================================
#HEATMAP CODE 
# ============================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan)
library(tibble)
library(forcats)
library(scales)

dat_sf <- dat_all %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  mutate(
    lon_orig = longitude,
    lat_orig = latitude
  ) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

dat <- final_dat %>%
  st_drop_geometry() %>%
  mutate(
    longitude = lon_orig,
    latitude  = lat_orig
  )

dat %>%
  summarise(
    min_lon = min(longitude, na.rm = TRUE),
    max_lon = max(longitude, na.rm = TRUE),
    n_unique = n_distinct(floor(longitude))
  )

true_mono_fams <- c("Xiphiidae", "Rachycentridae")

heat_dat <- dat %>%
  filter(
    !is.na(longitude),
    !is.na(family),
    family != ""
  ) %>%
  mutate(
    lon_bin = floor(longitude)
  ) %>%
  filter(!family %in% true_mono_fams)

heat_tab <- heat_dat %>%
  distinct(family, lon_bin, species) %>%
  count(family, lon_bin, name = "value")

heat_mat <- heat_tab %>%
  pivot_wider(
    names_from = lon_bin,
    values_from = value,
    values_fill = 0
  ) %>%
  mutate(total = rowSums(across(-family))) %>%
  filter(total >= 20) %>% #can change this here keeps total of at least 20 across all bins
  select(-total)

mat <- heat_mat %>%
  tibble::column_to_rownames("family") %>%
  as.matrix()

#scale each family by own max (1 = peak bin, 0.5 half peak, 0 absent)
mat_scaled <- mat / apply(mat, 1, max)

dist_fam <- vegdist(mat_scaled, method = "bray")
hc <- hclust(dist_fam, method = "average")

#order families
family_order <- rownames(mat_scaled)[hc$order]

# convert back to long format
heat_long <- as.data.frame(mat_scaled) %>%
  rownames_to_column("family") %>%
  pivot_longer(
    cols = -family,
    names_to = "longitude",
    values_to = "value_scaled"
  ) %>%
  mutate(
    longitude = as.numeric(longitude),
    family = factor(family, levels = family_order)
  ) %>%
  filter(longitude >= 10, longitude <= 35) 
#%>%
# group_by(longitude) %>%
#  filter(sum(value_scaled) > 0) %>%
# ungroup()

# optional: order families by centroid while keeping clustered order available
family_centroid <- heat_long %>%
  group_by(family) %>%
  summarise(
    centroid = weighted.mean(longitude, value_scaled, na.rm = TRUE),
    .groups = "drop"
  )

heat_long <- heat_long %>%
  left_join(family_centroid, by = "family")

family_order_centroid <- family_centroid %>%
  arrange(centroid) %>%
  pull(family)

heat_long <- heat_long %>%
  mutate(family = factor(family, levels = family_order_centroid))

# split into two balanced family groups
families_in_order <- family_order
n_fam <- length(families_in_order)

split_point <- ceiling(n_fam / 2)

fam_group1 <- families_in_order[1:split_point]
fam_group2 <- families_in_order[(split_point + 1):n_fam]

dat_1 <- heat_long %>%
  filter(as.character(family) %in% fam_group1) %>%
  mutate(family = factor(as.character(family), levels = fam_group1))

dat_2 <- heat_long %>%
  filter(as.character(family) %in% fam_group2) %>%
  mutate(family = factor(as.character(family), levels = fam_group2))

plot_family_heat <- function(dat, plot_title = NULL, plot_subtitle = NULL) {
  ggplot(dat, aes(x = longitude, y = family, fill = value_scaled)) +
    geom_tile(width = 1) +
    geom_vline(
      xintercept = c(20, 27),
      linetype = "dashed",
      linewidth = 0.4,
      colour = "white"
    ) +
    scale_x_continuous(
      limits = c(9.5, 35.5),
      breaks = seq(10, 35, by = 5)
    ) +
    scale_fill_viridis_c(
      option = "C",
      limits = c(0, 1),
      oob = scales::squish,
      labels = scales::label_percent(accuracy = 1),
      name = "Relative family\noccurrence"
    ) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "Longitude (°E)",
      y = "Teleost family"
    ) +
    theme_classic(base_family = "serif", base_size = 11) +
    theme(
      axis.text.y = element_text(size = 7),
      axis.text.x = element_text(size = 9),
      axis.title = element_text(size = 11),
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10),
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      panel.grid = element_blank()
    ) + 
    coord_cartesian(clip = "off") +
    annotate("text", x = 17,   y = Inf, label = "West coast",  vjust = -0.8, size = 4) +
    annotate("text", x = 23.5, y = Inf, label = "South coast", vjust = -0.8, size = 4) +
    annotate("text", x = 31,   y = Inf, label = "East coast",  vjust = -0.8, size = 4) +
    theme (
      plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
    )
}

p_heat_1 <- plot_family_heat(
  dat_1,
  plot_subtitle = paste0("(n = ", length(fam_group1), " families)")
)

p_heat_2 <- plot_family_heat(
  dat_2,
  plot_subtitle = paste0("(n = ", length(fam_group2), " families)")
)

dat_1
dat_2
p_heat_1
p_heat_2

ggsave(
  "family_heatmap_1.pdf",
  p_heat_1,
  width = 7.5,
  height = 10,
  units = "in"
)

ggsave(
  "family_heatmap_2.pdf",
  p_heat_2,
  width = 7.5,
  height = 10,
  units = "in"
)

# ============================================================
#SINGLE SECTOR
# ============================================================
#firstly for this;
# I want to be able to list the families with the highest relative occurrence across all coasts 
# and, importantly, identify which ones occur only in one of the sectors
heat_long2 <- heat_long %>%
  mutate(
    coast = case_when(
      longitude < 20              ~ "W",
      longitude >= 20 & longitude < 27 ~ "S",
      longitude >= 27             ~ "E",
      TRUE ~ NA_character_
    )
  )

family_overall <- heat_long2 %>%
  group_by(family) %>%
  summarise(
    total_relative = sum(value_scaled, na.rm = TRUE),
    mean_relative  = mean(value_scaled, na.rm = TRUE),
    occupied_bins  = sum(value_scaled > 0),
    .groups = "drop"
  ) %>%
  arrange(desc(total_relative)) %>% 
  mutate(
    dominance = total_relative / occupied_bins
  )

family_overall

family_overall %>%
  slice_head(n = 20)


top10_by_sector <- heat_long2 %>%
  group_by(coast, family) %>%
  summarise(
    total_relative = sum(value_scaled),
    .groups = "drop"
  ) %>%
  group_by(coast) %>%
  slice_max(total_relative, n = 10, with_ties = FALSE)


fam_raw_sector <- dat %>%
  filter(!is.na(longitude), !is.na(family), family != "") %>%
  mutate(
    coast = case_when(
      longitude < 20 ~ "W",
      longitude < 27 ~ "S",
      TRUE ~ "E"
    )
  ) %>%
  distinct(family, coast)

fam_raw_single <- fam_raw_sector %>%
  count(family, name = "n_sectors") %>%
  filter(n_sectors == 1) %>%
  left_join(fam_raw_sector, by = "family")

fam_counts <- dat %>%
  count(family)

valid_fams <- fam_counts %>%
  filter(n >= 5) %>%   # threshold adjustable
  pull(family)

fam_raw_single_clean <- fam_raw_single %>%
  filter(family %in% valid_fams)

fam_raw_single_clean

table(fam_raw_single_clean$coast)
# E  S  W 
# 27  1  4 

# ============================================================
#TURNOVER PLOT
# ============================================================
# map the assemblage turnover intensity along the South African coast, 
library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)
install.packages("zoo")
library(zoo)

# Build longitude-bin x family table from original data
turnover_tab <- dat_all %>%
  filter(
    !is.na(longitude),
    !is.na(family),
    !is.na(species),
    family != ""
  ) %>%
  mutate(
    lon_bin = floor(longitude)   # 1-degree bins to match your final heatmap
  ) %>%
  filter(lon_bin >= 10, lon_bin <= 35) %>%
  distinct(lon_bin, family, species) %>%
  count(lon_bin, family, name = "value")

# 2. Wide matrix: rows = longitude bins, cols = families
turnover_mat <- turnover_tab %>%
  pivot_wider(
    names_from = family,
    values_from = value,
    values_fill = 0
  ) %>%
  arrange(lon_bin)

# 3. Keep bin labels
lon_bins <- turnover_mat$lon_bin

# 4. Numeric matrix for dissimilarity
turn_mat <- turnover_mat %>%
  select(-lon_bin) %>%
  as.matrix()

# 5. Bray-Curtis turnover between adjacent bins
turnover_df <- data.frame(
  lon_bin = lon_bins[-1],
  turnover = sapply(2:nrow(turn_mat), function(i) {
    as.numeric(vegdist(rbind(turn_mat[i - 1, ], turn_mat[i, ]), method = "bray"))
  })
)

# 6. Optional smoothing
turnover_df <- turnover_df %>%
  mutate(
    turnover_smooth = zoo::rollmean(turnover, k = 3, fill = NA, align = "center")
  )

# 7. Plot
p_turnover <- ggplot(turnover_df, aes(x = lon_bin, y = turnover)) +
  geom_line(linewidth = 0.7, alpha = 0.5) +
  geom_point(size = 1.8, alpha = 0.7) +
  geom_line(aes(y = turnover_smooth), linewidth = 1) +
  geom_vline(
    xintercept = c(20, 27),
    linetype = "dashed",
    linewidth = 0.4,
    colour = "black"
  ) +
  scale_x_continuous(
    limits = c(10, 35),
    breaks = seq(10, 35, by = 2)
  ) +
  labs(
    title = "Assemblage turnover intensity along the South African coast",
    subtitle = "Bray–Curtis dissimilarity between adjacent 1° longitude bins",
    x = "Longitude (°E)",
    y = "Turnover intensity"
  ) +
  theme_classic(base_family = "serif", base_size = 11)

p_turnover

ggsave(
  "p_turnover.png",
  p_turnover,
  width = 10,
  height = 5,
  units = "in"
)



