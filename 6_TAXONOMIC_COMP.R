#-------------------------------------------------------------------------------
# 6. TAXONOMIC COMPOSITION AND HEIRARCHICAL ORGANISATION 
#-------------------------------------------------------------------------------
install.packages("plotly")
install.packages("ggdendro")
install.packages("treemap")
install.packages("gt")
install.packages("processx")
library(treemap)
library(dplyr)
library(plotly)
library(patchwork)
library(gt)
library(htmlwidgets)
#-------------------------------------------------------------------------------
#RESOLUTION SUMMARY
#-------------------------------------------------------------------------------
#before species clean
length(unique(final_dat$species)) #2121

taxon_summary <- final_dat %>%
  st_drop_geometry() %>%
  count(taxonomic_resolution)                     

taxon_summary
#family    346
#genus   111973
#order      1
#species 993985

#-------------------------------------------------------------------------------
# cleaned (using only known species) 
# final_dat --> cleaned = most refined and filtered version
#-------------------------------------------------------------------------------
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
total_richness #1945 species

#176 species entries contained ambiguous epithets

taxon_summary_clean <- dat %>%
  st_drop_geometry() %>%  
  count(taxonomic_resolution)  

taxon_summary_clean 
#family                    1
#genus                    25
#species              993783

#-------------------------------------------------------------------------------
#TAX RES PLOT FACETED BY SOURCE (using final_dat) not removing epithets
#-------------------------------------------------------------------------------
tax_levels <- c("species", "genus", "family", "order")

taxon_by_source <- final_dat %>%
  st_drop_geometry() %>%
  count(source, taxonomic_resolution, name = "n_records") %>%
  group_by(source) %>%
  mutate(
    perc = round(100 * n_records / sum(n_records), 2)
  ) %>%
  ungroup()

#order 
taxon_by_source <- taxon_by_source %>%
  mutate(
    taxonomic_resolution = factor(
      taxonomic_resolution,
      levels = c("species", "genus", "family", "order")
    )
  )
#-------------------------------------------------------------------------------
#PLOT
#-------------------------------------------------------------------------------
p_tax_faceted <- ggplot(
  taxon_by_source,
  aes(
    x = taxonomic_resolution,
    y = n_records,
    fill = taxonomic_resolution
  )) +
  geom_col(
    color = "black",
    width = 0.7 ) +
  facet_wrap(
    ~ source,
    ncol = 4,
    labeller = labeller(source = c(
      "BRUV"        = "BRUV (S)",
      "CAPFISH"     = "CapMarine (O)",
      "DEM_TRAWL"   = "Demersal trawl (S)",
      "INAT"        = "iNaturalist (S)",
      "LINEFISH"    = "Angling (C,O)",
      "LITERATURE"  = "Literature (S,C,O)",
      "MUSEUM"      = "Museum (S)",
      "MW_TRAWL"    = "MW Trawl (O)"
    ))) +
  scale_fill_manual(
    values = c(
      "species" = "#2b5c7a",
      "genus"   = "#6ba3c3",
      "family"  = "#b7c9d3",
      "order"   = "#e0e0e0"
    )) +
  scale_y_log10(labels = scales::comma) +
  labs(
    x = "Taxonomic Resolution",
    y = "Number of Records (log scale)") +
  theme_classic(base_family = "serif") +
  theme(
    strip.text = element_text(size = 9, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3))

p_tax_faceted

ggsave("figure14_p_tax_faceted.png", p_tax_faceted,
       width = 8, height = 6, dpi = 600)
#-------------------------------------------------------------------------------
#HIERARCHICAL
#-------------------------------------------------------------------------------
total_richness_s <- n_distinct(dat$species)
total_richness_f <- n_distinct(dat$genus)
total_richness_g <- n_distinct(dat$family)
total_richness_o <- n_distinct(dat$order)

total_richness_s #1945 species
total_richness_f #866
total_richness_g #237
total_richness_o #31
#-------------------------------------------------------------------------------
#Structure across order
#order summary 
order_summary <- dat %>%
  distinct(order, family, species) %>%
  group_by(order) %>%
  summarise(
    n_families = n_distinct(family),
    n_species  = n_distinct(species),
    .groups = "drop"
  ) %>%
  arrange(desc(n_species))

print(order_summary, n = Inf)
#   order             n_families n_species
# Perciformes               97      1027
# Myctophiformes             2       110
# Anguilliformes            13       104
# Tetraodontiformes          9       104
# Scorpaeniformes           14        91
# Gadiformes                 7        65
# Pleuronectiformes          8        61
# Lophiiformes              11        53
# Aulopiformes              12        50
# Gasterosteiformes          6        38
# Ophidiiformes              5        37
# Beryciformes               6        33
# Beloniformes               4        24
# Clupeiformes               5        20
# Mugiliformes               1        19
# Osmeriformes               4        18
# Argentiniformes            2        15
# Zeiformes                  6        13
# Lampriformes               6        11
# Notacanthiformes           2         8
# Stomiiformes               4         8
# Batrachoidiformes          1         7
# Siluriformes               2         7
# Atheriniformes             3         6
# NA                         4         5 #inspect 🚩
# Ateleopodiformes           1         3
# Gonorynchiformes           2         3
# Syngnathiformes            1         3
# Albuliformes               1         2
# Elopiformes                2         2
# Polymixiiformes            1         1

fam_summary <- dat %>%
  distinct(family, species) %>%
  group_by(family) %>%
  summarise(
    n_species  = n_distinct(species),
    .groups = "drop"
  ) %>%
  arrange(desc(n_species))
print(fam_summary, n = Inf)

#44 families have >10 species = 81.4%

#-------------------------------------------------------------------------------
#SUNBURST PLOT
#-------------------------------------------------------------------------------
# species counts (clean and remove NAs)
tax_summary <- dat %>%
  filter(
    !is.na(order),
    !is.na(family),
    !is.na(genus),
    !is.na(species)
  ) %>%
  distinct(order, family, genus, species) %>%
  count(order, family, genus, name = "n_species")
# GENERA (leaf nodes)
genera <- tax_summary %>%
  mutate(
    id = paste(order, family, genus, sep = "_"),
    parent = paste(order, family, sep = "_"),
    label = genus
  ) %>%
  select(id, label, parent, value = n_species)
# FAMILIES
families <- tax_summary %>%
  group_by(order, family) %>%
  summarise(value = sum(n_species), .groups = "drop") %>%
  mutate(
    id = paste(order, family, sep = "_"),
    parent = order,
    label = family
  ) %>%
  select(id, label, parent, value)
# ORDERS (root level)
orders <- tax_summary %>%
  group_by(order) %>%
  summarise(value = sum(n_species), .groups = "drop") %>%
  mutate(
    id = order,
    parent = "",
    label = order
  ) %>%
  select(id, label, parent, value)
#COMBINE
tree_data <- bind_rows(orders, families, genera)

p_sunburst <- plot_ly(
  data = tree_data,
  ids = ~id,
  labels = ~label,
  parents = ~parent,
  values = ~value,
  type = "sunburst",
  branchvalues = "total"
)

p_sunburst

save_image(p_sunburst, file = "figure15_sunburst.png")

htmlwidgets::saveWidget(
  p_sunburst,
  "figure15_sunburst.html",
  selfcontained = FALSE
)

getwd()

# upload html to github pages
#“An interactive version of this figure is available at: [link]” 
# or include as supplementary material and submit html and msc together --> “Supplementary Figure S15 (interactive)”
#-------------------------------------------------------------------------------
#TOP CONTRIBUTING FAMILIES TO SPECIWES RICHNESS
top_families_plot <- tax_summary %>%
  group_by(family) %>%
  summarise(n_species = sum(n_species)) %>%
  arrange(desc(n_species)) %>%
  slice_head(n = 15)

ggplot(top_families_plot, aes(x = reorder(family, n_species), y = n_species)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Family",
    y = "Number of species",
    title = "Top contributing families to species richness"
  ) +
  theme_classic(base_family = "Times")

#-------------------------------------------------------------------------------
# RELATIVE WEIGHTING VERSUS RELATIVELY WEIGHTED
#-------------------------------------------------------------------------------
# Drop geometry ONLY where needed
dat_nogeo <- final_dat %>% st_drop_geometry()
# Source totals
source_totals <- dat_nogeo %>%
  count(source, name = "n_source_records")
# Join back to original sf safely
dat_weighted <- final_dat %>%
  left_join(source_totals, by = "source") %>%
  mutate(record_weight = 1 / n_source_records)
#-------------------------------------------------------------------------------
# TOP RAW COUNTS
#-------------------------------------------------------------------------------
top_species <- dat_weighted %>%
  st_drop_geometry() %>%
  filter(!is.na(species)) %>%
  count(species, sort = TRUE) %>%
  slice_max(n, n = 20)

top_families <- dat_weighted %>%
  st_drop_geometry() %>%
  filter(!is.na(family)) %>%
  count(family, sort = TRUE) %>%
  slice_max(n, n = 20)
#-------------------------------------------------------------------------------
# RAW PLOTS
#-------------------------------------------------------------------------------
raw_species <- ggplot(top_species,
                      aes(x = reorder(species, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Most Frequently Recorded Species (raw count)",
    x = "Species",
    y = "Number of Records"
  ) +
  theme_classic(base_family = "serif", base_size = 9) + 
  theme(
    axis.text.y = element_text(size = 8),   
    axis.text.x = element_text(size = 8),
    plot.title  = element_text(size = 10)
  )

raw_family <- ggplot(top_families,
                     aes(x = reorder(family, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Most Frequently Recorded Families (raw count)",
    x = "Family",
    y = "Number of Records"
  ) +
  theme_classic(base_family = "serif", base_size = 9) + 
  theme(
    axis.text.y = element_text(size = 8),   
    axis.text.x = element_text(size = 8),
    plot.title  = element_text(size = 10)
  )
#-------------------------------------------------------------------------------
# WEIGHTED SPECIES
#-------------------------------------------------------------------------------
species_weighted <- dat_weighted %>%
  st_drop_geometry() %>%
  group_by(species) %>%
  summarise(
    weighted_records = sum(record_weight, na.rm = TRUE),
    n_sources = n_distinct(source),
    .groups = "drop"
  ) %>%
  arrange(desc(weighted_records))

top_species_weighted <- species_weighted %>%
  slice_max(weighted_records, n = 20) %>%
  mutate(
    species = if_else(
      species == "Argyrosomus inodorus and japonicus",
      "Argyrosomus inodorus / japonicus",
      species
    )
  )
# Proper ordering
top_species_weighted_plot_df <- top_species_weighted %>%
  arrange(weighted_records) %>%
  mutate(species = factor(species, levels = species))

weight_species_plot <- ggplot(top_species_weighted_plot_df,
                              aes(x = species,
                                  y = weighted_records,
                                  fill = n_sources)) +
  geom_col(color = "black", width = 0.7) +
  geom_text(aes(label = round(weighted_records, 3)),
            hjust = -0.1, size = 2.8, family = "serif") +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "darkblue",
                      name = "Number of Sources") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Top Marine Teleost Species by Weighted Record Score",
    x = "Species",
    y = "Weighted Record Score"
  ) +
  theme_classic(base_family = "serif", base_size = 9) +
  theme(
    axis.text.y = element_text(size = 8), 
    axis.text.x = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    plot.title  = element_text(size = 10)
  )
#-------------------------------------------------------------------------------
# WEIGHTED FAMILY
#-------------------------------------------------------------------------------
family_weighted <- dat_weighted %>%
  st_drop_geometry() %>%
  filter(!is.na(family)) %>%
  group_by(family) %>%
  summarise(
    weighted_records = sum(record_weight, na.rm = TRUE),
    n_sources = n_distinct(source),
    .groups = "drop"
  ) %>%
  arrange(desc(weighted_records))

top_family_weighted <- family_weighted %>%
  slice_max(weighted_records, n = 20)

top_family_weighted_plot_df <- top_family_weighted %>%
  arrange(weighted_records) %>%
  mutate(family = factor(family, levels = family))

weight_family_plot <- ggplot(top_family_weighted_plot_df,
                             aes(x = family,
                                 y = weighted_records,
                                 fill = n_sources)) +
  geom_col(color = "black", width = 0.7) +
  geom_text(aes(label = round(weighted_records, 3)),
            hjust = -0.1, size = 2.8, family = "serif") +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "darkblue",
                      name = "Number of Sources") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Top Marine Teleost Families by Weighted Record Score",
    x = "Family",
    y = "Weighted Record Score"
  ) +
  theme_classic(base_family = "serif", base_size = 9) +
  theme(
    axis.text.y = element_text(size = 8), 
    axis.text.x = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    plot.title  = element_text(size = 10)
  )
#-------------------------------------------------------------------------------
# PANELS
#-------------------------------------------------------------------------------
p1 <- raw_species | weight_species_plot +
  plot_annotation(title = "Raw vs Weighted Species Dominance")

p2 <- raw_family | weight_family_plot +
  plot_annotation(title = "Raw vs Weighted Family Dominance")

p1
p2

ggsave("figure16_species_freq.png", p1, width = 14, height = 8, units = "in")
ggsave("figure17_family_freq.png", p2, width = 14, height = 8, units = "in")
#-------------------------------------------------------------------------------
top_species
top_species_weighted
#-------------------------------------------------------------------------------
top_families
top_family_weighted
#-------------------------------------------------------------------------------
#COMPARE
#-------------------------------------------------------------------------------
#SPECIES RAW
raw_species_rank <- top_species %>%
  arrange(desc(n)) %>%
  mutate(
    raw_rank = row_number(),
    raw_n = n
  ) %>%
  select(species, raw_rank, raw_n)
#SPECIES WEIGHTED
weighted_species_rank <- species_weighted %>%
  arrange(desc(weighted_records)) %>%
  mutate(
    weighted_rank = row_number()
  ) %>%
  select(species, weighted_rank, weighted_records, n_sources)
#SPECIES JOIN
species_compare <- raw_species_rank %>%
  full_join(weighted_species_rank, by = "species") %>%
  mutate(
    rank_change = raw_rank - weighted_rank,
    rank_change = replace_na(rank_change, 0),
    raw_present = !is.na(raw_rank)
  ) %>%
  arrange(weighted_rank)
#-------------------------------------------------------------------------------
species_compare <- species_compare %>%
  mutate(
    change_flag = case_when(
      rank_change >= 5  ~ "↑ Strong increase",
      rank_change <= -5 ~ "↓ Strong decrease",
      TRUE ~ "Stable"
    )
  )

species_compare %>%
  slice(1:20) 
#-------------------------------------------------------------------------------
#FAMILIES RAW
raw_family_rank <- top_families %>%
  arrange(desc(n)) %>%
  mutate(
    raw_rank = row_number(),
    raw_n = n
  ) %>%
  select(family, raw_rank, raw_n)
#FAMILIES WEIGHTED
weighted_family_rank <- family_weighted %>%
  arrange(desc(weighted_records)) %>%
  mutate(
    weighted_rank = row_number()
  ) %>%
  select(family, weighted_rank, weighted_records, n_sources)
#FAMILIES JOIN
family_compare <- raw_family_rank %>%
  full_join(weighted_family_rank, by = "family") %>%
  mutate(
    rank_change = raw_rank - weighted_rank,
    rank_change = replace_na(rank_change, 0),
    raw_present = !is.na(raw_rank)
  ) %>%
  arrange(weighted_rank)
#-------------------------------------------------------------------------------
family_compare <- family_compare %>%
  mutate(
    change_flag = case_when(
      rank_change >= 5  ~ "↑ Strong increase",
      rank_change <= -5 ~ "↓ Strong decrease",
      TRUE ~ "Stable"
    )
  )

family_compare %>%
  slice(1:20) 

#NA interpretation: Several species that did not rank highly in raw record 
#counts emerged as dominant when weighted by source contribution. These species
#were typically supported across multiple independent datasets, suggesting that 
#they represent broadly distributed and consistently observed taxa rather than 
#artefacts of sampling intensity.

#High weighted rank is driven by multi-source consistency, not sampling intensity
#e.g. Scomber japonicus n_sources = 7

#NA means	            | Interpretation
#Missing raw rank	    | Not dominant in any one dataset
#High weighted rank	  | Strong cross-dataset support
#High n_sources	      | Robust ecological signal
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------