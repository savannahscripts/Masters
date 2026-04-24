#-------------------------------------------------------------------------------
# 5. DEPTH ANALYSES
#-------------------------------------------------------------------------------
# OBJECTIVES:
# - Quantify depth distribution of occurrence records across the EEZ
# - Assess sampling bias across depth gradients
# - Compare depth distributions among sampling methods and data sources
# - Evaluate how sampling effort varies with depth (records + richness)
# depth summary
# records vs depth raw numbers plot y=records, depth bin = x axis
# violin depth by data source and by sampling method with tests and effect sizes
#-------------------------------------------------------------------------------
install.packages("ggridges")
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)
library(forcats)
library(ggridges)
#-------------------------------------------------------------------------------
#median sampled
summary(final_dat$depth)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0   135.9   322.6   926.3  1429.8  5270.3 

#OVERALL

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
#-------------------------------------------------------------------------------
#BYSOURCE
breaks_shelf <- c( 0, 5, 10, 20, 30, 50, 75, 100, 150, 200, 300, 500, 750, 1000, 1500, 2000, 3000, 4000, 6000 )

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
      method %in% c("PRESERVED_SPECIMEN") ~ "Museum",
      method %in% c("Records") ~ "Literature records",
      method == "VisualEstimate" ~ "Visual estimate",
      method %in% c("SeineNet") ~ "Seine net estuarine",
      method %in% c("LINE") ~ "NMLS Angling",
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
#-------------------------------------------------------------------------------
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
  "figure12_depth_distribution_sources.pdf",
  plot = p_depth_violin_source,
  width = 200,
  height = 130,
  units = "mm",
  dpi = 600
)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#BY METHOD
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
#-------------------------------------------------------------------------------
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

ggsave(
  filename = "figure13_depth_distribution_method.tiff",
  plot = p_depth_violin_method,
  width = 8,
  height = 5,
  dpi = 600,
  compression = "lzw"
)
#-------------------------------------------------------------------------------
#TESTS (KW and episolon squared eff sizes)
#-------------------------------------------------------------------------------
table(dat_depth$source)
table(depth_dat$gear)
#by source
kw_source <- kruskal.test(depth_pos ~ source, data = dat_depth)
kw_source
#Kruskal-Wallis rank sum test
#data:  depth_pos by source
#Kruskal-Wallis chi-squared = 170759, df = 7, p-value < 2.2e-16

H <- kw_source$statistic
k <- length(unique(dat_depth$source))
n <- nrow(dat_depth)

epsilon2_source <- (H - k + 1) / (n - k)
epsilon2_source
#Kruskal-Wallis chi-squared 
#0.1543451

#pairwise
pairwise.wilcox.test(
  dat_depth$depth_pos,
  dat_depth$source,
  p.adjust.method = "BH"
)

#Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
#data:  dat_depth$depth_pos and dat_depth$source 

#               CapMarine NMLS   DFFE Trawl DFFE MW Trawl
#NMLS          <2e-16    -      -          -            
#DFFE Trawl    <2e-16    <2e-16 -          -            
#DFFE MW Trawl <2e-16    <2e-16 <2e-16     -            
#BRUV          <2e-16    <2e-16 <2e-16     <2e-16       
#Museum        <2e-16    <2e-16 <2e-16     <2e-16       
#Literature    <2e-16    <2e-16 <2e-16     <2e-16       
#iNaturalist   <2e-16    <2e-16 <2e-16     <2e-16       
#              BRUV   Museum Literature
 # NMLS          -      -      -         
 # DFFE Trawl    -      -      -         
 # DFFE MW Trawl -      -      -         
 # BRUV          -      -      -         
 # Museum        <2e-16 -      -         
 # Literature    <2e-16 <2e-16 -         
 # iNaturalist   <2e-16 <2e-16 <2e-16    

#P value adjustment method: BH 
#-------------------------------------------------------------------------------
#by method
kw_method <- kruskal.test(depth_pos ~ gear, data = depth_dat)
kw_method
#Kruskal-Wallis rank sum test
#data:  depth_pos by gear
#Kruskal-Wallis chi-squared = 230003, df = 21, p-value < 2.2e-16

H <- kw_method$statistic
k <- length(unique(depth_dat$gear))
n <- nrow(depth_dat)

epsilon2_method <- (H - k + 1) / (n - k)
epsilon2_method
#Kruskal-Wallis chi-squared 
#0.207887 

#test differences between method and sources
data.frame(
  grouping = c("Source", "Method"),
  epsilon2 = c(epsilon2_source, epsilon2_method)
)

#   grouping  epsilon2
#   Source    0.1543451
#   Method    0.2078870

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
