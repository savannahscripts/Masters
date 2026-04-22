#6.taxonomic composition
#-----overal reoslution of integrated dataset
#-----filtering stats
#-----final stats of species, order, family etc,
#-----taxonomic resolution per data source
#-----raw record count versus weighted
# treemap or sunburst here

#compare species count between dat_all, dat_species and after intersected with EEZ !!


tax_richness_summary <- tibble::tibble(
  Level = c("Order", "Family", "Species"),
  n     = c(
    n_distinct(final_dat$order),
    n_distinct(final_dat$family),
    n_distinct(final_dat$species)
  )
)
# after second filtering step = 2121 species
# 243 families
# 35 orders


#order summary 
order_summary <- final_dat %>%
distinct(order, family, species) %>%
  group_by(order) %>%
  summarise(
    n_families = n_distinct(family),
    n_species  = n_distinct(species),
    .groups = "drop"
  ) %>%
  arrange(desc(n_species))

order_summary

#tax res summary
taxon_summary <- final_dat %>%
  count(taxonomic_resolution)

taxon_summary 


#by source
taxon_by_source <- final_dat %>%
  count(source, taxonomic_resolution, name = "n_records") %>%
  group_by(source) %>%
  mutate(
    perc = round(100 * n_records / sum(n_records), 2)
  ) %>%
  ungroup()

print(taxon_by_source, n = Inf)
#-----------------------------#
# DENDROGRAM TO SHOW PHYLOGENETICS
#-----------------------------#
install.packages("plotly")
install.packages("ggdendro")
library(dplyr)

library(dplyr)

# species counts
tax_summary <- dat_species %>%
  distinct(order, family, genus, species) %>%
  count(order, family, genus, name = "n_species")

# GENERA (leaf nodes)
genera <- tax_summary %>%
  transmute(
    label = genus,
    parent = family,
    value = n_species
  )

# FAMILIES (aggregate from genera)
families <- tax_summary %>%
  group_by(order, family) %>%
  summarise(value = sum(n_species), .groups = "drop") %>%
  transmute(
    label = family,
    parent = order,
    value = value
  )

# ORDERS (aggregate from families)
orders <- tax_summary %>%
  group_by(order) %>%
  summarise(value = sum(n_species), .groups = "drop") %>%
  transmute(
    label = order,
    parent = "",
    value = value
  )

# FINAL TREE
#treemap
install.packages("treemap")
library(treemap)

#working
treemap(
  tax_summary,
  index = c("order", "family", "genus"),
  vSize = "n_species",
  title = "Taxonomic structure of marine teleost diversity"
)

#sunburst
tree_data <- bind_rows(orders, families, genera)

library(plotly)

fig <- plot_ly(
  data = tree_data,
  labels = ~label,
  parents = ~parent,
  values = ~value,
  type = "sunburst",
  branchvalues = "total"
)

fig

#simplified sunburst

top_families <- tax_summary %>%
  group_by(family) %>%
  summarise(n = sum(n_species)) %>%
  arrange(desc(n)) %>%
  slice_head(n = 15) %>%
  pull(family)

tax_summary_small <- tax_summary %>%
  filter(family %in% top_families)

# GENERA (leaf nodes)
genera_small <- tax_summary_small %>%
  transmute(
    label = genus,
    parent = family,
    value = n_species
  )

# FAMILIES (aggregate from genera)
families_small <- tax_summary_small %>%
  group_by(order, family) %>%
  summarise(value = sum(n_species), .groups = "drop") %>%
  transmute(
    label = family,
    parent = order,
    value = value
  )

# ORDERS (aggregate from families)
orders_small <- tax_summary_small %>%
  group_by(order) %>%
  summarise(value = sum(n_species), .groups = "drop") %>%
  transmute(
    label = order,
    parent = "",
    value = value
  )

#sunburst
tree_data_small <- bind_rows(orders_small, families_small, genera_small)


sun_small <- plot_ly(
  data = tree_data_small,
  labels = ~label,
  parents = ~parent,
  values = ~value,
  type = "sunburst",
  branchvalues = "total"
)

sun_small

#FINAL
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
# RELATIVE WEIGHTING
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

#-------------------------------------------------------------------------------
# RAW PLOTS
#-------------------------------------------------------------------------------

raw_species <- ggplot(top_species,
                      aes(x = reorder(species, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Most Frequently Recorded Species (raw count)",
    x = "Species",
    y = "Number of Records"
  ) +
  theme_classic(base_family = "serif")

raw_family <- ggplot(top_families,
                     aes(x = reorder(family, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Most Frequently Recorded Families (raw count)",
    x = "Family",
    y = "Number of Records"
  ) +
  theme_classic(base_family = "serif")

#-------------------------------------------------------------------------------
# WEIGHTED SPECIES PLOT
#-------------------------------------------------------------------------------

weight_species_plot <- ggplot(top_species_weighted_plot_df,
                              aes(x = species,
                                  y = weighted_records,
                                  fill = n_sources)) +
  geom_col(color = "black", width = 0.7) +
  geom_text(aes(label = round(weighted_records, 3)),
            hjust = -0.1, size = 4, family = "serif") +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "darkblue",
                      name = "Number of Sources") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Top Marine Teleost Species by Weighted Record Score",
    x = "Species",
    y = "Weighted Record Score"
  ) +
  theme_classic(base_family = "serif")

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
            hjust = -0.1, size = 4, family = "serif") +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "darkblue",
                      name = "Number of Sources") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Top Marine Teleost Families by Weighted Record Score",
    x = "Family",
    y = "Weighted Record Score"
  ) +
  theme_classic(base_family = "serif")

#-------------------------------------------------------------------------------
# PANELS
#-------------------------------------------------------------------------------

library(patchwork)

p1 <- raw_species | weight_species_plot +
  plot_annotation(title = "Raw vs Weighted Species Dominance")

p2 <- raw_family | weight_family_plot +
  plot_annotation(title = "Raw vs Weighted Family Dominance")


p1
p2
#-------------------------------------------------------------------------------
# SAVE
#-------------------------------------------------------------------------------

ggsave("species_freq.png", p1, width = 14, height = 8, units = "in")
ggsave("family_freq.png", p2, width = 14, height = 8, units = "in")



top_family_weighted
top_species_weighted

top_families
top_species




#-------------------------------------------------------------------------------
#RELATIVE WEIGHTING
#-------------------------------------------------------------------------------
class(final_dat)

source_totals <- final_dat %>%
  st_drop_geometry() %>%
  count(source, name = "n_source_records")

dat_weighted <- final_dat %>%
  left_join(source_totals, by = "source")

dat_weighted <- dat_weighted %>%
  mutate(record_weight = 1 / n_source_records)

#top 20 spp
top_species <- dat_weighted%>%
  filter(!is.na(species)) %>%
  count(species, sort = TRUE) %>%
  slice_max(n, n = 20)

#top 20 spp
top_families <- dat_weighted %>%
  filter(!is.na(family)) %>%
  count(family, sort = TRUE) %>%
  slice_max(n, n = 20)


species_weighted <- dat_weighted %>%
  group_by(species) %>%
  summarise(
    weighted_records = sum(record_weight, na.rm = TRUE),
    n_sources       = n_distinct(source),
    .groups = "drop"
  ) %>%
  arrange(desc(weighted_records))


top_species_weighted <- species_weighted %>%
  slice_max(order_by = weighted_records, n = 20)

top_species_weighted

top_species_weighted_plot <- top_species_weighted %>%
  arrange(weighted_records) %>%                       # ascending
  mutate(species = factor(species, levels = species)) # factor in that order


ggplot(top_species_weighted_plot,
       aes(x = species, y = weighted_records)) +
  geom_col(fill = "grey70", color = "black", width = 0.7) +
  geom_text(aes(label = round(weighted_records, 3)),
            hjust = -0.1, size = 3.2, family = "Times") +
  coord_flip() +
  labs(
    title = "Top Marine Teleost Species by Source-Weighted Record Frequency",
    x = "Species",
    y = "Weighted Record Score"
  ) +
  theme_classic(base_family = "serif", base_size = 14) +
  theme(
    panel.border  = element_rect(color = "black", fill = NA, linewidth = 0.7),
    axis.line     = element_line(color = "black"),
    axis.title.y  = element_text(margin = margin(r = 10)),
    axis.title.x  = element_text(margin = margin(t = 5)),
    plot.title    = element_text(face = "bold", hjust = 0.5)
  )

top_species_weighted_plot <- top_species_weighted %>%
  mutate(
    species = if_else(
      species == "Argyrosomus inodorus and japonicus",
      "Argyrosomus inodorus / japonicus",
      species
    )
  ) %>%
  arrange(weighted_records) %>%
  mutate(species = factor(species, levels = species))

raw_species <- ggplot(top_species, aes(x = reorder(species, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Most Frequently Recorded Species (raw count)",
    x = "Species",
    y = "Number of Records"
  ) +
  theme_classic(base_family = "serif") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10), size = 11),
    axis.title.x = element_text(margin = margin(t = 10), size = 11),
    axis.text = element_text(size = 11),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )

raw_family <- ggplot(top_families, aes(x = reorder(family, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  scale_y_continuous(labels = scales::label_number()) +
  coord_flip() +
  labs(
    title = "Most Frequently Recorded Families (raw count)",
    x = "Family",
    y = "Number of Records"
  ) +
  theme_classic(base_family = "serif") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10), size = 11),
    axis.title.x = element_text(margin = margin(t = 10), size = 11),
    axis.text = element_text(size = 11),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )

weight_species_plot <- ggplot(top_species_weighted_plot,
                              aes(x = species,
                                  y = weighted_records,
                                  fill = n_sources)) +
  
  geom_col(color = "black", width = 0.7) +
  
  # Labels: weighted score, 3 dp
  geom_text(aes(label = round(weighted_records, 3)),
            hjust = -0.1, size = 4, family = "serif") +
  
  coord_flip() +
  
  scale_fill_gradient(
    low = "lightblue",
    high = "darkblue",
    name = "Number of Sources"
  ) +
  
  labs(
    title = "Top Marine Teleost Species by Weighted Record Score",
    x = "Species",
    y = "Weighted Record Score"
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  
  theme_classic(base_family = "serif", base_size = 12) +
  theme(
    panel.border  = element_rect(color = "black", fill = NA, linewidth = 0.7),
    axis.line     = element_line(color = "black"),
    axis.title.y  = element_text(margin = margin(r = 10)),
    axis.text.y   = element_text(size = 11),
    legend.position = "right",
    plot.title    = element_text(face = "bold", size = 12, hjust = 0.5)
  )


family_weighted <- dat_weighted %>%
  filter(!is.na(family)) %>% 
  group_by(family) %>%
  summarise(
    weighted_records = sum(record_weight, na.rm = TRUE),
    n_sources       = n_distinct(source),
    .groups = "drop"
  ) %>%
  arrange(desc(weighted_records))


top_family_weighted <- family_weighted %>%
  slice_max(order_by = weighted_records, n = 20)

top_family_weighted

top_family_weighted_plot <- top_family_weighted %>%
  arrange(weighted_records) %>%                       # ascending
  mutate(family = factor(family, levels = family)) # factor in that order


weight_family_plot <- ggplot(top_family_weighted_plot,
                             aes(x = family,
                                 y = weighted_records,
                                 fill = n_sources)) +
  
  geom_col(color = "black", width = 0.7) +
  
  # Labels: weighted score, 3 dp
  geom_text(aes(label = round(weighted_records, 3)),
            hjust = -0.1, size = 4, family = "serif") +
  
  coord_flip() +
  
  scale_fill_gradient(
    low = "lightblue",
    high = "darkblue",
    name = "Number of Sources"
  ) +
  
  labs(
    title = "Top Marine Teleost Families by Weighted Record Score",
    x = "Family",
    y = "Weighted Record Score"
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  
  theme_classic(base_family = "serif", base_size = 12) +
  theme(
    panel.border  = element_rect(color = "black", fill = NA, linewidth = 0.7),
    axis.line     = element_line(color = "black"),
    axis.title.y  = element_text(margin = margin(r = 10)),
    axis.text.y   = element_text(size = 11),
    legend.position = "right",
    plot.title    = element_text(face = "bold", size = 12, hjust = 0.5)
  )

#PANEL
library(patchwork)

p1 <- raw_species | weight_species_plot +
  plot_annotation(
    title = "Comparison of Raw and Source-Weighted Species Dominance",
    tag_levels = "A"
  )
p2 <- raw_family | weight_family_plot +
  plot_annotation(
    title = "Comparison of Raw and Source-Weighted Family Dominance",
    tag_levels = "A"
  )

p1
p2


ggsave(
  "species freq.png",
  p1,
  width = 14,
  height = 8,
  units = "in"
)

ggsave(
  "family freq.png",
  p2,
  width = 14,
  height = 8,
  units = "in"
)
