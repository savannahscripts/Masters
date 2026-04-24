#-------------------------------------------------------------------------------
# 8. ASSEMBLAGE PATTERNS
#-------------------------------------------------------------------------------
install.packages("zoo")
library(tidyr)
library(vegan)
library(dplyr)
library(ggplot2)
library(tibble)
library(forcats)
library(scales)
library(zoo)
#-------------------------------------------------------------------------------
# NMDS
#-------------------------------------------------------------------------------
# grid × method × family matrix
nmds_df <- final_dat %>%
  filter(!is.na(grid_id), !is.na(family), !is.na(gear_grouped)) %>%
  group_by(grid_id, gear_grouped, family) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(row_id = paste(grid_id, gear_grouped, sep = "_")) %>%
  pivot_wider(
    names_from = family,
    values_from = n,
    values_fill = 0
  )
#-------------------------------------------------------------------------------
# metadata
meta <- nmds_df %>%
  select(row_id, grid_id, gear_grouped)
# matrix
mat <- nmds_df %>%
  select(-row_id, -grid_id, -gear_grouped) %>%
  as.data.frame()
rownames(mat) <- nmds_df$row_id
# clean matrix
# presence/absence
mat <- (mat > 0) * 1
# remove rare families
mat <- mat[, colSums(mat) >= 10]
# remove weak rows
keep <- rowSums(mat) >= 3
mat <- mat[keep, ]
# match metadata AFTER filtering
meta <- meta[keep, ]
#-------------------------------------------------------------------------------
# NMDS
set.seed(123)
nmds <- metaMDS(mat, distance = "bray", k = 2, trymax = 40)
# Join
scores_df <- as.data.frame(scores(nmds, display = "sites"))
scores_df$row_id <- rownames(scores_df)

scores_df <- scores_df %>%
  left_join(meta, by = "row_id")
#-------------------------------------------------------------------------------
# check
head(scores_df)
#-------------------------------------------------------------------------------
# P RAW METHODS
#-------------------------------------------------------------------------------
top_methods <- scores_df %>%
  count(gear_grouped, sort = TRUE) %>%
  slice_head(n = 15) %>%   
  pull(gear_grouped)

top_methods

scores_raw <- scores_df %>%
  mutate(
    method_plot = ifelse(gear_grouped %in% top_methods,
                         gear_grouped,
                         "Other"))

p_methods <- ggplot(scores_raw, aes(NMDS1, NMDS2, colour = method_plot)) +
  geom_point(size = 2.2, alpha = 0.7) +
   stat_ellipse(
    aes(group = method_plot),
    linewidth = 0.6,
    alpha = 0.5,
    na.rm = TRUE
  )  +  
  scale_colour_manual(values = c(
    "Longline" = "#4f6d8a",
    "Demersal trawl"  = "#1f3b5c",
    "NMLS Angling"  = "#88a6b9",
    "Pelagic seine"  = "#2a9d8f",
    "CS (iNaturalist)"  = "#f4a261",
    "Pole"  = "#6a994e",
    "Shore angling"  = "#c08552",
    "UVC"  = "#b56576",
    "Museum"  = "#e76f51",
    "Offshore trawl"  = "#a7c957",
    "Inshore trawl"  = "#a8c0b3",
    "Midwater trawl observer"  = "#6b4f4f",
    "BRUV"  = "#c2a98a",
    "Gillnet estuarine"  = "#7a6c8f",
    "Seine net estuarine"  = "#c0b8cc"
  )) +
  theme_classic(base_family = "Times") +
  labs(
    title = "NMDS – Assemblage structure by sampling method",
    colour = "Method"
  )
#-------------------------------------------------------------------------------
# P1 CLEAN METHODS
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
scores_methods_clean <- scores_df %>%
  filter(!gear_grouped %in% c("Museum", "Mixed gears", "Literature records")) %>%
  mutate(
    gear_clean = case_when(
      gear_grouped %in% c("NMLS Angling","Boat angling","Shore angling", "Unspecified angling") ~ "Angling",
      gear_grouped %in% c("BRUV","UVC","CS (iNaturalist)") ~ "Underwater visual",
      gear_grouped %in% c("Chemical intertidal sampling") ~ "Chemical",
      gear_grouped %in% c("Demersal trawl","Offshore trawl","Inshore trawl") ~ "Demersal trawl",
      gear_grouped %in% c("Seine net estuarine","Gillnet estuarine","Plankton net") ~ "Inshore nets",
      gear_grouped %in% c("Longline") ~ "Longline",
      gear_grouped %in% c("Pelagic seine","Midwater trawl observer") ~ "Midwater trawl",
      gear_grouped %in% c("Pole") ~ "Pole",
      gear_grouped %in% c("Spear") ~ "Spear",
      TRUE ~ NA_character_   
    )
  ) %>%
  filter(!is.na(gear_clean))

p_methods_clean <- ggplot(scores_methods_clean, aes(NMDS1, NMDS2, colour = gear_clean)) +
  geom_point(size = 2.2, alpha = 0.7) +
  stat_ellipse(
    aes(group = gear_clean),
    linewidth = 0.6,
    alpha = 0.3,
    na.rm = TRUE
  ) +
  scale_colour_manual(values = c(
    "Angling"             = "#88a6b9", #113
    "Underwater visual"   = "#b56576", #101
    "Chemical"            = "#7a6c8f", #only 3
    "Demersal trawl"      = "#1f3b5c", #427
    "Longline"            = "#f4a261", #311
    "Inshore nets"        = "#e76f51", #18
    "Midwater trawl"      = "#6b4f4f", #123
    "Pole"                = "#6a994e", #20
    "Spear"               = "#a7c957"  #only 3
  )) +
  theme_classic(base_family = "serif") +
  labs(
    title = "NMDS – Assemblage structure by sampling method",
    colour = "Method"
  )
table(scores_methods_clean$gear_clean)
#-------------------------------------------------------------------------------                 
#-------------------------------------------------------------------------------
# P2 ECOLOGY
#-------------------------------------------------------------------------------
#remove museum here
scores_ecol <- scores_df %>%
  filter(gear_grouped != "Museum") %>%
  mutate(
    method_ecol = case_when(
      gear_grouped %in% c("Demersal trawl","Offshore trawl","Inshore trawl") ~ "Demersal",
      gear_grouped %in% c("Pole","Midwater trawl observer","Longline","Pelagic seine") ~ "Pelagic",
      gear_grouped %in% c("NMLS Angling","BRUV","Unspecified angling","Boat angling","Plankton net","Spear","Literature records","Mixed gears") ~ "Nearshore",
      gear_grouped %in% c("CS (iNaturalist)","Chemical intertidal sampling","Seine net estuarine", 
                          "Gillnet estuarine","Shore angling", "UVC") ~ "Onshore",
      TRUE ~ NA_character_   
    )
  ) %>%
  filter(!is.na(method_ecol))

#"Museum"  removed as it does not fit into any one of the defined domains                 
p_ecol <- ggplot(scores_ecol, aes(NMDS1, NMDS2, colour = method_ecol)) +
  geom_point(size = 2.5, alpha = 0.8) +
  stat_ellipse(aes(group = method_ecol), linewidth = 0.7, alpha = 0.3) +
  scale_colour_manual(values = c(
    "Demersal" = "#1b3b6f",
    "Pelagic"  = "#2a9d8f",
    "Nearshore"  = "#a7c957",
    "Onshore"  = "#6a994e"
  )) +
  theme_classic(base_family = "Times") +
  labs(
    title = "NMDS – Assemblage structure by ecological sampling domain",
    colour = "Domain"
  )
#-------------------------------------------------------------------------------
# P3 ENVIRONMENTAL GRADIENT
#-------------------------------------------------------------------------------
meta_env <- dat %>%
  group_by(grid_id, gear_grouped) %>%
  summarise(
    mean_lon = mean(longitude, na.rm = TRUE),
    mean_depth = mean(depth, na.rm = TRUE),
    .groups = "drop"
  )

scores_env <- scores_df %>%
  left_join(meta_env, by = c("grid_id", "gear_grouped"))

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
#-------------------------------------------------------------------------------
# FINAL PLOTS
#-------------------------------------------------------------------------------
#p_methods 
p_methods_clean
p_ecol 
p_env 
#-------------------------------------------------------------------------------
# SAVE
#-------------------------------------------------------------------------------
ggsave("figure22_p_methods.png", p_methods_clean,
       width = 7, height = 5, dpi = 600)

ggsave("figure23_p_ecol.png", p_ecol,
       width = 7, height = 5.5, dpi = 600)

ggsave("figure24_p_env.png", p_env,
       width = 7, height = 5, dpi = 600)
#-------------------------------------------------------------------------------
# TESTS
#-------------------------------------------------------------------------------
# RAW METHOD
adonis_raw <- adonis2(
  mat ~ gear_grouped,
  data = meta,
  method = "bray",
  permutations = 999
)

adonis_raw
#dispersion
disp_raw <- betadisper(
  vegdist(mat, "bray"),
  meta$gear_grouped
)

anova(disp_raw)
permutest(disp_raw)
#-------------------------------------------------------------------------------
# CLEANED METHODS
meta_clean <- scores_methods_clean %>%
  select(row_id, gear_clean)
# match to matrix
meta_clean <- meta_clean %>%
  filter(row_id %in% rownames(mat))
mat_clean <- mat[meta_clean$row_id, ]

adonis_clean <- adonis2(
  mat_clean ~ gear_clean,
  data = meta_clean,
  method = "bray",
  permutations = 999
)

adonis_clean

#dispersion
disp_clean <- betadisper(
  vegdist(mat_clean, "bray"),
  meta_clean$gear_clean
)

anova(disp_clean)
permutest(disp_clean)
#-------------------------------------------------------------------------------
# ECOLOGICAL DOMAIN
meta_ecol <- scores_ecol %>%
  select(row_id, method_ecol) %>%
  filter(row_id %in% rownames(mat))

mat_ecol <- mat[meta_ecol$row_id, ]

adonis_ecol <- adonis2(
  mat_ecol ~ method_ecol,
  data = meta_ecol,
  method = "bray",
  permutations = 999
)

adonis_ecol

disp_ecol <- betadisper(
  vegdist(mat_ecol, "bray"),
  meta_ecol$method_ecol
)

anova(disp_ecol)
permutest(disp_ecol)
#-------------------------------------------------------------------------------
# ENVIRONMENTAL GRADIENT (LONGITUDE VS DEPTH)
#continuous so not in groups must do permanova and no
meta_env_test <- scores_env %>%
  select(row_id, mean_lon, mean_depth) %>%
  filter(row_id %in% rownames(mat))

mat_env <- mat[meta_env_test$row_id, ]

adonis_env <- adonis2(
  mat_env ~ mean_lon * mean_depth,
  data = meta_env_test,
  method = "bray",
  permutations = 999
)

adonis_env
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# HEATMAP 
#-------------------------------------------------------------------------------
true_mono_fams <- c("Xiphiidae", "Rachycentridae")

heat_dat <- final_dat %>%
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
  filter(total >= 20) %>% # can change this here keeps total of at least 20 across all bins
  select(-total)

mat <- heat_mat %>%
  tibble::column_to_rownames("family") %>%
  as.matrix()

# scale each family by own max (1 = peak bin, 0.5 half peak, 0 absent)
mat_scaled <- mat / apply(mat, 1, max)

dist_fam <- vegdist(mat_scaled, method = "bray")
hc <- hclust(dist_fam, method = "average")

# order families
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
# %>%
# group_by(longitude) %>%
# filter(sum(value_scaled) > 0) %>%
# ungroup()

# order families by centroid while keeping clustered order available
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

# split into two groups
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
#-------------------------------------------------------------------------------
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
#-------------------------------------------------------------------------------
p_heat_1 <- plot_family_heat(
  dat_1,
  plot_subtitle = paste0("(n = ", length(fam_group1), " families)")
)

p_heat_2 <- plot_family_heat(
  dat_2,
  plot_subtitle = paste0("(n = ", length(fam_group2), " families)")
)
#-------------------------------------------------------------------------------
dat_1
dat_2
#-------------------------------------------------------------------------------
p_heat_1
p_heat_2
#-------------------------------------------------------------------------------
ggsave(
  "figure25_family_heatmap_1.pdf",
  p_heat_1,
  width = 7.5,
  height = 10,
  units = "in")
#-------------------------------------------------------------------------------
ggsave(
  "figure25_family_heatmap_2.pdf",
  p_heat_2,
  width = 7.5,
  height = 10,
  units = "in")
#-------------------------------------------------------------------------------
#SINGLE SECTOR
#-------------------------------------------------------------------------------
# list the families with the highest relative occurrence across all coasts 
# and, identify which ones occur only in one of the sectors
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
# 31  1  3 
#-------------------------------------------------------------------------------
#TURNOVER PLOT
#-------------------------------------------------------------------------------
# map the assemblage turnover intensity along the South African coast, 
# Build longitude-bin x family table from original data
turnover_tab <- dat_all %>% #using dat_all for this because final_dat is cut off past 14, pattern is the same
  filter(
    !is.na(longitude),
    !is.na(family),
    !is.na(species),
    family != ""
  ) %>%
  mutate(
    lon_bin = floor(longitude)   # 1-degree bins 
  ) %>%
  filter(lon_bin >= 5, lon_bin <= 35) %>%
  distinct(lon_bin, family, species) %>%
  count(lon_bin, family, name = "value")

# Wide matrix: rows = longitude bins, cols = families
turnover_mat <- turnover_tab %>%
  pivot_wider(
    names_from = family,
    values_from = value,
    values_fill = 0
  ) %>%
  arrange(lon_bin)

#Keep bin labels
lon_bins <- turnover_mat$lon_bin

# Numeric matrix for dissimilarity
turn_mat <- turnover_mat %>%
  select(-lon_bin) %>%
  as.matrix()

# Bray-Curtis turnover between adjacent bins
turnover_df <- data.frame(
  lon_bin = lon_bins[-1],
  turnover = sapply(2:nrow(turn_mat), function(i) {
    as.numeric(vegdist(rbind(turn_mat[i - 1, ], turn_mat[i, ]), method = "bray"))
  })
)

# Optional smoothing
turnover_df <- turnover_df %>%
  mutate(
    turnover_smooth = zoo::rollmean(turnover, k = 3, fill = NA, align = "center")
  )
#-------------------------------------------------------------------------------
# PLOT
#-------------------------------------------------------------------------------
p_turnover <- ggplot(turnover_df, aes(x = lon_bin, y = turnover)) +
  geom_line(linewidth = 0.7, alpha = 0.5) +
  geom_point(size = 1.2, alpha = 0.7) +
  geom_line(aes(y = turnover_smooth), linewidth = 1, colour = 'red') +
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
    y = "Bray Curtis dissimilarity"
  ) +
  theme_classic(base_family = "serif", base_size = 11)
#-------------------------------------------------------------------------------
p_turnover
#-------------------------------------------------------------------------------
ggsave(
  "figure26_p_turnover.png",
  p_turnover,
  width = 10,
  height = 5,
  units = "in"
)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------