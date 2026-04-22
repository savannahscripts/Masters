#7.patterns in species richness
#-----across data sources and sampling methods
#-----species accumulation curves
#-----across longitude and coasts
#-----across depth (could combine with records too?)

#RICHNESS

length(unique(final_dat$species)) #2121

taxon_summary <- taxon_summary %>% st_drop_geometry()

taxon_summary
#TAX RES PLOT

#taxonomic_resolution      n
#1               family    350
#2                genus 112014
#3                order      1
#4              species 997639

#-------------------------------------------------------------------------------
# SETUP
#-------------------------------------------------------------------------------
#clean

dat <- final_dat %>%
  st_drop_geometry() %>%
  mutate(
    species = str_squish(as.character(species))
  ) %>%
  filter(
    !is.na(species),
    species != "",
    str_count(species, "\\S+") == 2,
    !str_detect(species, "\\bsp\\b|\\bspp\\b"),
    !str_detect(species, "\\bcf\\.?\\b|\\baff\\.?\\b"),
    !str_detect(species, regex("unknown|unidentified|indet", ignore_case = TRUE))
  )

total_richness <- n_distinct(dat$species)
total_richness #1945

#-------------------------------------------------------------------------------
# RICHNESS PER SOURCE
#-------------------------------------------------------------------------------
rich_source <- dat %>%
  filter(!is.na(source)) %>%
  distinct(source, species) %>%
  count(source, name = "richness") %>%
  arrange(desc(richness))

ggplot(rich_source, aes(x = reorder(source, richness), y = richness)) +
  geom_col(fill = "grey70") +
  coord_flip() +
  labs(y = "Species richness", x = "Data source") +
  theme_classic(base_family = "serif")

#METHOD RICHNESS
rich_method <- dat %>%
  filter(!is.na(gear_grouped)) %>%
  distinct(gear_grouped, species) %>%
  count(gear_grouped, name = "richness") %>%
  arrange(desc(richness))

ggplot(rich_method, aes(x = reorder(gear_grouped, richness), y = richness)) +
  geom_col(fill = "grey75") +
  coord_flip() +
  labs(y = "Species richness", x = "Sampling method") +
  theme_classic(base_family = "serif")


#depth richness
dat <- dat %>%
  mutate(
    longitude = as.numeric(X_1),
    latitude  = as.numeric(Y_1),
    depth_pos = abs(as.numeric(depth))
  )

depth_breaks <- c(0,10,20,50,100,200,500,1000,2000,5000,Inf)

rich_depth <- dat %>%
  filter(!is.na(depth_pos), is.finite(depth_pos), depth_pos > 0) %>%
  mutate(depth_bin = cut(depth_pos, breaks = depth_breaks)) %>%
  group_by(depth_bin) %>%
  summarise(richness = n_distinct(species), .groups = "drop")

ggplot(rich_depth, aes(depth_bin, richness, group = 1)) +
  geom_line() +
  geom_point() +
  theme_classic(base_family = "serif")

#longitude and effort
library(mgcv)

dat <- final_dat %>%
  mutate(
    longitude = sf::st_coordinates(.)[,1],
    latitude  = sf::st_coordinates(.)[,2],
    depth_pos = abs(as.numeric(depth))
  ) %>%
  st_drop_geometry()

summary(dat$longitude)

dat_lon <- dat %>%
  filter(!is.na(longitude), longitude >= 10, longitude <= 40)

rich_lon <- dat_lon %>%
  mutate(lon_bin = floor(longitude)) %>%
  group_by(lon_bin) %>%
  summarise(
    richness = n_distinct(species),
    effort = n(),
    .groups = "drop"
  )
nrow(rich_lon)
summary(rich_lon)
# GAM (effort-controlled)
gam_lon <- gam(
  richness ~ s(lon_bin, k = 6) + log1p(effort),
  data = rich_lon,
  method = "REML"
)

summary(gam_lon)


ggplot(rich_lon, aes(lon_bin, richness)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
  labs(x = "Longitude (°E)", y = "Species richness") +
  theme_classic(base_family = "serif")


rich_lon$fit <- predict(gam_lon)

ggplot(rich_lon, aes(lon_bin, richness)) +
  geom_point(size = 2) +
  geom_line(aes(y = fit), linewidth = 1) +
  labs(
    x = "Longitude (°E)",
    y = "Species richness"
  ) +
  theme_classic(base_family = "serif")

ggplot(rich_lon, aes(lon_bin, richness)) +
  geom_point(aes(size = effort), alpha = 0.7) +
  geom_line(aes(y = fit), linewidth = 1) +
  scale_size_continuous(name = "Sampling effort") +
  labs(
    x = "Longitude (°E)",
    y = "Species richness"
  ) +
  theme_classic(base_family = "serif")

#coastal richness
rich_coast <- dat %>%
  filter(!is.na(coast)) %>%
  group_by(coast) %>%
  summarise(
    richness = n_distinct(species),
    effort = n(),
    .groups = "drop"
  )

ggplot(rich_coast, aes(coast, richness)) +
  geom_col(fill = "grey65") +
  labs(x = "Coast", y = "Species richness") +
  theme_classic(base_family = "serif")

#-------------------------------------------------------------------------------
#SPEC ACCUM
pa_source <- dat %>%
  distinct(source, species) %>%
  mutate(pres = 1) %>%
  pivot_wider(names_from = species, values_from = pres, values_fill = 0)

mat_source <- pa_source %>% select(-source)

specaccum_source <- specaccum(mat_source, method = "exact")

plot(specaccum_source)


pa_method <- dat %>%
  distinct(gear_grouped, species) %>%
  mutate(pres = 1) %>%
  pivot_wider(names_from = species, values_from = pres, values_fill = 0)

mat_method <- pa_method %>% select(-gear_grouped)

specaccum_method <- specaccum(mat_method, method = "exact")

plot(specaccum_method)


#old

# RECORD COUNTS PER SOURCE
#-------------------------------------------------------------------------------

records_source <- final_dat %>%
  filter(!is.na(source), source != "") %>%
  mutate(source = recode_source(source)) %>%
  count(source, name = "records")

#-------------------------------------------------------------------------------
# BUILD PA MATRIX
#-------------------------------------------------------------------------------

pa_sources <- final_dat %>%
  filter(!is.na(source), source != "", !is.na(species)) %>%
  mutate(source = recode_source(source)) %>%
  distinct(source, species) %>%
  mutate(present = 1L) %>%
  pivot_wider(
    names_from  = species,
    values_from = present,
    values_fill = 0
  )

sources_vec <- pa_sources$source

pa_matrix <- pa_sources %>%
  select(-source) %>%
  as.data.frame()

pa_matrix[] <- lapply(pa_matrix, as.integer)

#-------------------------------------------------------------------------------
# ORDER SOURCES BY RICHNESS
#-------------------------------------------------------------------------------

source_order <- rich_source %>%
  arrange(desc(richness)) %>%
  pull(source)

ord <- match(source_order, sources_vec)

if(any(is.na(ord))) stop("Mismatch between source_order and PA matrix")

pa_matrix_ord <- pa_matrix[ord, ]

#-------------------------------------------------------------------------------
# SPECIES ACCUMULATION
#-------------------------------------------------------------------------------

library(vegan)

specaccum_sources <- specaccum(pa_matrix_ord, method = "exact")

accum_df <- data.frame(
  step = seq_along(specaccum_sources$richness),
  richness = specaccum_sources$richness,
  source = source_order
) %>%
  mutate(
    gain = richness - lag(richness, default = 0),
    prop_gain = gain / max(richness),
    pct_gain = scales::percent(prop_gain, accuracy = 0.1)
  )

#-------------------------------------------------------------------------------
# SHORT LABELS
#-------------------------------------------------------------------------------

accum_df <- accum_df %>%
  mutate(
    source_short = recode(source,
                          "CapMarine observer records" = "CapMarine",
                          "Angling commercial and observer records" = "Angling",
                          "Demersal trawl survey records" = "Demersal trawl",
                          "Midwater trawl observer records" = "MW trawl",
                          "BRUV survey records" = "BRUV",
                          "Museum specimen records" = "Museum",
                          "Literature-derived records" = "Literature",
                          "iNaturalist records" = "iNaturalist"
    )
  )

# Join record counts
accum_df <- accum_df %>%
  left_join(records_source, by = "source")

#-------------------------------------------------------------------------------
# SCALE SECONDARY AXIS
#-------------------------------------------------------------------------------

scale_factor <- max(accum_df$richness) / max(accum_df$records)

accum_df <- accum_df %>%
  mutate(records_scaled = records * scale_factor)

#-------------------------------------------------------------------------------
# PLOT
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# FINAL SOURCE ACCUMULATION PLOT
#-------------------------------------------------------------------------------

p_accum_source <- ggplot(accum_df, aes(x = step)) +
  
  geom_line(aes(y = richness), linewidth = 1, color = "black") +
  geom_point(aes(y = richness), size = 2) +
  
  # secondary axis (records)
  geom_line(
    aes(y = records_scaled),
    linetype = "dashed",
    color = "grey50",
    linewidth = 0.8
  ) +
  
  # % gain labels
  geom_text(
    aes(y = richness, label = pct_gain),
    vjust = -0.8,
    size = 3,
    family = "serif"
  ) +
  
  # source labels (clean positioning)
  geom_text(
    aes(y = richness, label = source_short),
    nudge_x = 0.15,
    vjust = 1.8,
    size = 3,
    family = "serif"
  ) +
  
  scale_y_continuous(
    name = "Cumulative known species richness",
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Number of records",
      labels = scales::comma
    )
  ) +
  
  labs(
    x = "Cumulative addition of data sources (most to least speciose)"
  ) +
  
  theme_classic(base_family = "serif") +
  theme(
    plot.margin = margin(10, 60, 10, 10)
  )

p_accum_source

# SAVE (FIXED)
ggsave(
  "p_accum_source.pdf",
  plot = p_accum_source,
  width = 10,
  height = 7,
  dpi = 600
)



#FIXED JACCARD
library(vegan)

jaccard_sources <- vegdist(pa_matrix_ord, method = "jaccard")

summary(jaccard_sources)
median(jaccard_sources)

jaccard_mat_sources <- as.matrix(jaccard_sources)


p_source_gain <- ggplot(accum_df,
                        aes(x = reorder(source, gain), y = gain)) +
  
  geom_col(fill = "grey60", color = "black") +
  
  geom_text(
    aes(label = gain),
    vjust = -0.3,
    size = 3,
    family = "serif"
  ) +
  
  coord_flip() +
  
  labs(
    x = NULL,
    y = "Additional species contributed"
  ) +
  
  theme_classic(base_family = "serif")

p_source_gain


#-------------------------------------------------------------------------------
# RICHNESS PER GEAR (FIXED)
#-------------------------------------------------------------------------------

rich_method <- final_dat %>%
  filter(!is.na(gear_grouped), gear_grouped != "", !is.na(species)) %>%
  mutate(gear = as.character(gear_grouped)) %>%
  distinct(gear, species) %>%
  count(gear, name = "richness") %>%
  mutate(
    prop_total = richness / total_known_richness,
    gear = forcats::fct_reorder(gear, richness, .desc = TRUE)
  )


pa_gears <- final_dat %>%
  filter(!is.na(gear_grouped), gear_grouped != "", !is.na(species)) %>%
  mutate(gear = as.character(gear_grouped)) %>%
  distinct(gear, species) %>%
  mutate(present = 1L) %>%
  pivot_wider(
    names_from = species,
    values_from = present,
    values_fill = 0
  )

gear_vec <- pa_gears$gear

pa_matrix_gears <- pa_gears %>%
  select(-gear) %>%
  as.data.frame()

pa_matrix_gears[] <- lapply(pa_matrix_gears, as.integer)


gear_order <- rich_method %>%
  arrange(desc(richness)) %>%
  pull(gear)

ord <- match(gear_order, gear_vec)

if(any(is.na(ord))) stop("Mismatch in gear ordering")

pa_matrix_gears_ord <- pa_matrix_gears[ord, ]

specaccum_gears <- specaccum(pa_matrix_gears_ord, method = "exact")

accum_df_gear <- data.frame(
  step = seq_along(specaccum_gears$richness),
  richness = specaccum_gears$richness,
  gear = gear_order
) %>%
  mutate(
    gain = richness - lag(richness, default = 0),
    prop_gain = gain / max(richness),
    pct_gain = scales::percent(prop_gain, accuracy = 0.1)
  )


p_accum_gear <- ggplot(accum_df_gear,
                       aes(x = step, y = richness)) +
  
  geom_line(linewidth = 1, color = "black") +
  geom_point(size = 2) +
  
  geom_text(
    aes(label = pct_gain),
    vjust = -1.5,
    size = 3,
    family = "serif"
  ) +
  
  geom_text(
    data = accum_df_gear %>% slice(c(1, n())),
    aes(label = gear),
    hjust = -0.1,
    nudge_x = 0.2,
    size = 3,
    family = "serif"
  ) +
  
  coord_cartesian(clip = "off") +
  
  labs(
    x = "Cumulative addition of sampling methods",
    y = "Cumulative known species richness"
  ) +
  
  theme_classic(base_family = "serif") +
  theme(plot.margin = margin(10, 80, 10, 10))

p_accum_gear

ggsave(
  "p_accum_gear.pdf",
  plot = p_accum_gear,
  width = 10,
  height = 7,
  dpi = 600
)

jaccard_methods <- vegdist(pa_matrix_gears_ord, method = "jaccard")

summary(jaccard_methods)
median(jaccard_methods)

jaccard_mat_methods <- as.matrix(jaccard_methods)

