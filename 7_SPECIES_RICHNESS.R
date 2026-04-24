#-------------------------------------------------------------------------------
# 7. SPECIES RICHNESS PATTERNS (Section 3.5)
#-------------------------------------------------------------------------------
install.packages("pheatmap")
library(vegan)
library(mgcv)
library(pheatmap)
library(ggplot2)
library(dplyr)
library(scales)
#-------------------------------------------------------------------------------
#RICHNESS BY SOURCES
#-------------------------------------------------------------------------------
rich_source <- dat %>%
  filter(!is.na(source), source != "", !is.na(species)) %>%
  group_by(source) %>%
  summarise(
    richness = n_distinct(species),
    n_families = n_distinct(family),
    .groups = "drop"
  ) %>%
  arrange(desc(richness))

rich_source
#source      richness   n_families
#MUSEUM          1728        229
# INAT            606        102
# BRUV            514         76
# LITERATURE      376        104
# LINEFISH        144         42
# DEM_TRAWL       133         61
# CAPFISH         112         57
# MW_TRAWL         38         22
#-------------------------------------------------------------------------------
# SPECIES ACCUMULATION CURVES
#-------------------------------------------------------------------------------
source_order <- rich_source$source

# RECORD COUNTS (RAW EFFORT)
records_source <- dat %>%
  filter(!is.na(source), source != "") %>%
  count(source, name = "records")
specaccum_sources <- specaccum(pa_matrix_ord, method = "exact")

# PRESENCE-ABSENCE MATRIX
pa_sources <- dat %>%
  filter(!is.na(source), source != "", !is.na(species)) %>%
  distinct(source, species) %>%
  mutate(present = 1L) %>%
  pivot_wider(
    names_from = species,
    values_from = present,
    values_fill = 0
  )

source_vec <- pa_sources$source

pa_matrix <- pa_sources %>%
  select(-source) %>%
  as.data.frame()

pa_matrix[] <- lapply(pa_matrix, as.integer)

# MATCH ORDER
ord <- match(source_order, source_vec)
if(any(is.na(ord))) stop("Mismatch between source_order and PA matrix")
pa_matrix_ord <- pa_matrix[ord, ]

specaccum_sources <- specaccum(pa_matrix_ord, method = "exact")

#SPECIES ACCUMULATION
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

# CLEANER LABELS
accum_df <- accum_df %>%
  mutate(
    source_short = recode(source,
                          "CAPFISH" = "CapMarine",
                          "LINEFISH" = "Angling",
                          "DEM_TRAWL" = "Demersal trawl",
                          "MW_TRAWL" = "MW trawl",
                          "BRUV" = "BRUV",
                          "MUSEUM" = "Museum",
                          "LITERATURE" = "Literature",
                          "INAT" = "iNaturalist"
    )
  )

# Join raw record counts (effort)
accum_df <- accum_df %>%
  left_join(records_source, by = "source")

# SCALE SECONDARY AXIS
scale_factor <- max(accum_df$richness) / max(accum_df$records)

accum_df <- accum_df %>%
  mutate(records_scaled = records * scale_factor)

# FINAL PLOTS (FIRST SOURCE THEN GEARS)
p_accum_source <- ggplot(accum_df, aes(x = step)) +
  geom_line(aes(y = richness), linewidth = 1.2, colour = "black") +
  geom_point(aes(y = richness), size = 2) +
  geom_point(
    aes(y = records_scaled),
    shape = 21,
    fill = "grey70",
    colour = "black",
    size = 2.5
  ) +
  geom_line(
    aes(y = records_scaled),
    linetype = "dashed",
    colour = "grey50",
    linewidth = 0.7,
    alpha = 0.7
  ) +
  geom_text(
    aes(y = richness, label = pct_gain),
    vjust = -0.8,
    size = 3,
    family = "serif"
  ) +
  geom_text(
    aes(y = richness, label = source_short),
    nudge_x = 0.2,
    vjust = 1.6,
    size = 3,
    family = "serif"
  ) +
  scale_y_continuous(
    name = "Cumulative known species richness",
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Records per source",
      labels = scales::comma
    )
  ) +
  scale_x_continuous(
    breaks = accum_df$step
  ) +
  labs(
    x = "Cumulative addition of data sources (ordered by richness)"
  ) +
  theme_classic(base_family = "serif") +
  theme(
    axis.text.x = element_blank(),
    plot.margin = margin(10, 60, 10, 10)
  )

p_accum_source

#METHOD RICHNESS
rich_method <- dat %>%
  filter(!is.na(gear_grouped), gear_grouped != "", !is.na(species)) %>%
  distinct(gear_grouped, species) %>%
  count(gear_grouped, name = "richness") %>%
  arrange(desc(richness))

gear_order <- rich_method$gear_grouped

#GEAR MATRIX
pa_gears <- dat %>%
  filter(!is.na(gear_grouped), gear_grouped != "", !is.na(species)) %>%
  distinct(gear_grouped, species) %>%
  mutate(present = 1L) %>%
  pivot_wider(
    names_from = species,
    values_from = present,
    values_fill = 0
  )

gear_vec <- pa_gears$gear_grouped

pa_matrix_gears <- pa_gears %>%
  select(-gear_grouped) %>%
  as.data.frame()

pa_matrix_gears[] <- lapply(pa_matrix_gears, as.integer)

#ORDER
ord <- match(gear_order, gear_vec)
if(any(is.na(ord))) stop("Mismatch in gear ordering")
pa_matrix_gears_ord <- pa_matrix_gears[ord, ]

specaccum_gears <- vegan::specaccum(pa_matrix_gears_ord, method = "exact")

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

p_accum_gear <- ggplot(accum_df_gear, aes(x = step, y = richness)) +
  geom_line(linewidth = 1.2, colour = "black") +
  geom_point(size = 2) +
  geom_text(
    aes(label = pct_gain),
    vjust = -0.9,
    size = 2.8,
    family = "serif"
  ) +
  geom_text(
    aes(label = gear),
    hjust = -0.05,
    nudge_x = 0.25,
    size = 2.7,
    family = "serif"
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    breaks = accum_df_gear$step
  ) +
  labs(
    x = "Cumulative addition of sampling methods (ordered by richness)",
    y = "Cumulative known species richness"
  ) +
  theme_classic(base_family = "serif") +
  theme(
    axis.text.x = element_blank(),
    plot.margin = margin(10, 90, 10, 10)
  )

p_accum_gear
#-------------------------------------------------------------------------------
# SAVE plots
ggsave(
  "figure18_accum_source.pdf",
  plot = p_accum_source,
  width = 10,
  height = 7,
  dpi = 600
)

ggsave(
  "figure19_p_accum_gear.pdf",
  plot = p_accum_gear,
  width = 10,
  height = 7,
  dpi = 600
)
#-------------------------------------------------------------------------------
#JACCARD TESTS
#-------------------------------------------------------------------------------
#Sources
jaccard_sources <- vegdist(pa_matrix_ord, method = "jaccard")
# summary stats
summary(jaccard_sources)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.5244  0.7470  0.8667  0.8389  0.9253  0.9786 
median(jaccard_sources)
# 0.8667326


#Methods
jaccard_methods <- vegdist(pa_matrix_gears_ord, method = "jaccard")
# summary stats
summary(jaccard_methods)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5190  0.8507  0.9178  0.8908  0.9627  1.0000 
median(jaccard_methods)
# 0.9178082

#-------------------------------------------------------------------------------
# RICHNESS VS LONGITUDE
#-------------------------------------------------------------------------------
dat_lon <- final_dat %>%
  filter(!is.na(longitude), longitude >= 10, longitude <= 40)

rich_lon <- dat_lon %>%
  mutate(lon_bin = floor(longitude)) %>%
  group_by(lon_bin) %>%
  summarise(
    richness = n_distinct(species),
    effort = n(),
    .groups = "drop"
  )
#-------------------------------------------------------------------------------
#MODELS
#-------------------------------------------------------------------------------
# Linear model
lm_lon <- lm(richness ~ lon_bin, data = rich_lon)

# GAM (no effort correction)
gam_lon <- gam(richness ~ s(lon_bin, k = 6),
               data = rich_lon,
               method = "REML")

# GAM (effort-corrected)
gam_effort <- gam(richness ~ s(lon_bin, k = 6) + log1p(effort),
                  data = rich_lon,
                  method = "REML")

# Compare models
AIC(lm_lon, gam_lon, gam_effort)
#                df      AIC
#lm_lon     3.000000 346.5549
#gam_lon    6.564036 338.5591
#gam_effort 6.916170 328.3538
summary(gam_lon)
#Parametric coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   329.42      48.83   6.746  1.9e-06 ***
#Approximate significance of smooth terms:
#            edf    Ref.df     F      p-value  
#s(lon_bin)  3.987  4.564    2.989   0.0316 * *
#R-sq.(adj) =  0.379   Deviance explained = 48.6%
#-REML = 158.92  Scale est. = 57220     n = 24
summary(gam_effort)
#Parametric coefficients:
#               Estimate   Std. Error t value   Pr(>|t|)    
#(Intercept)    -791.14     278.11  -2.845 0.010476 * 
#log1p(effort)   118.78      29.18    4.070     0.000672 ***
#Approximate significance of smooth terms:
#            edf     Ref.df  F      p-value   
#s(lon_bin)  3.304   3.926   4.997  0.0103 *
#R-sq.(adj) =  0.599   Deviance explained = 67.4%
#-REML = 148.37  Scale est. = 36935     n = 24

# Predictions
rich_lon$fit_gam        <- predict(gam_lon)
rich_lon$fit_gam_effort <- predict(gam_effort)
#standardise both variables
rich_lon <- rich_lon %>%
  mutate(
    richness_std = as.numeric(scale(richness)),
    effort_std   = as.numeric(scale(effort)),
    fit_std      = (fit_gam_effort - mean(richness)) / sd(richness)
  ) #GAM line is directly comparable to richness Not artificially shifted
#-------------------------------------------------------------------------------
#PLOT
#-------------------------------------------------------------------------------
cities <- data.frame(
  city = c("Cape Town", "Gqeberha", "East London", "Durban", "Richards Bay"),
  lon  = c(18.42, 25.60, 27.90, 31.00, 32.10)
)

p_rich_vs_long <- ggplot(rich_lon, aes(x = lon_bin)) +
  geom_point(aes(y = richness_std), size = 2) +
  geom_line(aes(y = fit_std),
            linewidth = 1.2,
            color = "black") +
  geom_line(aes(y = effort_std),
            color = "red",
            linewidth = 1,
            linetype = "dashed") +

  geom_vline(data = cities,
             aes(xintercept = lon),
             linetype = "dotted",
             color = "grey30") +
  geom_text(data = cities,
            aes(x = lon,
                y = max(rich_lon$richness_std, na.rm = TRUE) + 0.5,
                label = city),
            angle = 90,
            vjust = -0.4,
            size = 3) +
  labs(
    x = "Longitude (°E)",
    y = "Standardised value (z-score)"
  ) +
  theme_classic(base_family = "serif")

p_rich_vs_long
#-------------------------------------------------------------------------------
ggsave(
  "figure20_p_rich_vs_long.pdf",
  plot = p_rich_vs_long,
  width = 10,
  height = 7,
  dpi = 600
)
#-------------------------------------------------------------------------------
#coastal richness
#-------------------------------------------------------------------------------
rich_coast <- final_dat %>%
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
# RICHNESS AGAINST DEPTH
#-------------------------------------------------------------------------------
final_dat <- readr::read_csv("final_dat.csv") #now has gear grouped
#Rows: 1106305 Columns: 18 
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

# Prepare data
pts_depth <- dat %>%
  filter(
    !is.na(latitude),
    !is.na(longitude),
    !is.na(depth)
  ) %>%
  mutate(
    depth_m = abs(as.numeric(depth))  # ensure positive depth
  ) %>%
  filter(is.finite(depth_m), depth_m > 0)

# Depth bins
depth_levels <- c("0–10", "10–20", "20–50", "50–100", "100–200",
                  "200–500", "500–1000", "1000–2000", "2000–5000", ">5000")

pts_depth <- pts_depth %>%
  mutate(
    depth_bin = cut(
      depth_m,
      breaks = c(0, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, Inf),
      labels = depth_levels,
      include.lowest = TRUE,
      right = FALSE
    )
  )

# Summarise
rich_depth <- pts_depth %>%
  group_by(depth_bin) %>%
  summarise(
    richness = n_distinct(species),
    effort   = n(),
    .groups = "drop"
  ) %>%
  mutate(
    depth_bin = factor(depth_bin, levels = depth_levels)
  ) %>%
  arrange(depth_bin)

rich_depth
# depth_bin  richness | effort
# 0–10           955  | 7591
# 10–20          993  | 13660
# 20–50         1152  | 41109
# 50–100         608  | 94767
# 100–200        433  | 185360
# 200–500        501  | 284146
# 500–1000       482  | 40258
# 1000–2000      389  | 156566
# 2000–5000      409  | 170331
# >5000           10  | 21
#-------------------------------------------------------------------------------
# PLOT 
#-------------------------------------------------------------------------------
p_rich_vs_depth <- ggplot(rich_depth, aes(x = depth_bin, y = richness, group = 1)) +
  
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(color = "black", size = 2.5) +
  
  labs(
    x = "Depth bin (m)",
    y = "Species richness"
  ) +
  
  theme_classic(base_family = "Times", base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p_rich_vs_depth

ggsave(
  "figure21_p_rich_vs_depth.pdf",
  plot = p_rich_vs_depth,
  width = 10,
  height = 7,
  dpi = 600
)

#-------------------------------------------------------------------------------
#TESTS
## Add numeric midpoint for each bin
depth_mid <- c(5, 15, 35, 75, 150, 350, 750, 1500, 3500, 6000)

rich_depth <- rich_depth %>%
  mutate(
    depth_mid = depth_mid
  )
#spearman's test
cor_test <- cor.test(
  rich_depth$depth_mid,
  rich_depth$richness,
  method = "spearman"
)

cor_test
#	Spearman's rank correlation rho
#data:  rich_depth$depth_mid and rich_depth$richness
#S = 314, p-value = 0.0008802
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#rho 
#-0.9030303 
#-------------------------------------------------------------------------------
#GAM
gam_depth <- gam(
  richness ~ s(depth_mid, k = 5),
  data = rich_depth,
  method = "REML"
)

summary(gam_depth)
#Parametric coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   593.20      77.92   7.613 6.23e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Approximate significance of smooth terms:
#  edf Ref.df     F p-value  
#s(depth_mid)   1      1 9.547  0.0149 *
#  ---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#R-sq.(adj) =  0.487   Deviance explained = 54.4%
#-REML =  57.71  Scale est. = 60717     n = 10
#-------------------------------------------------------------------------------
#OTHER
df <- rich_depth %>%
  rename(
    mid_depth = depth_mid,
    n_species = richness
  )

#test for unimodality
quad_mod <- lm(n_species ~ poly(mid_depth, 2, raw = TRUE), data = df)
summary(quad_mod)
#Call:
#  lm(formula = n_species ~ poly(mid_depth, 2, raw = TRUE), data = df)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-316.22 -153.32  -75.41  197.25  376.55 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                      7.835e+02  1.084e+02   7.228 0.000173 ***
#  poly(mid_depth, 2, raw = TRUE)1 -2.315e-01  1.622e-01  -1.428 0.196443    
#poly(mid_depth, 2, raw = TRUE)2  1.880e-05  2.817e-05   0.667 0.525947    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 255.4 on 7 degrees of freedom
#Multiple R-squared:  0.5714,	Adjusted R-squared:  0.4489 
#F-statistic: 4.666 on 2 and 7 DF,  p-value: 0.05155
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
