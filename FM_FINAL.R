#LIT DAT
lit_dat <- read.csv("FINALDATA/outputs/OCTDAT/final_FM.csv") 

length(unique(lit_dat$species)) #642

sort(unique(lit_dat$species))
taxa_df <- lit_dat %>%
  select(species, family, order) %>%
  distinct() %>%
  arrange(species)

write.csv(taxa_df, "species_family_order.csv", row.names = FALSE)

#remove from lit_dat
#Aetobatus narinari, Alopias sp., Alopias superciliosus
#Cerebratulus fuscus
#Excirolana latipes
#Galaxias zebratus
#Myliobatus aquila
#Rhizoprionodon acutus
#Selachii sp.
#Unidentified sp.
remove_species <- c(
  "Aetobatus narinari",
  "Alopias sp.",
  "Alopias superciliosus",
  "Cerebratulus fuscus",
  "Excirolana latipes",
  "Galaxias zebratus",
  "Myliobatus aquila",
  "Rhizoprionodon acutus",
  "Selachii sp.",
  "Unidentified sp.",
  "Paratrypauchen microcephalus"
)

#RENAME in lit dat
#Gerres methurni to Gerres methueni
#Secutor insidiator to Deveximentum insidiator
#Gobiidae sp.. to Gobiidae sp.
#Labrid sp. to Labridae sp.
#Liza richardsoni to Liza richardsonii
#Melanostomiidae sp. to Grammatostomia sp.
# Mugilidae sp.. to Mugilidae sp.
#Trachurus sp. to Trachurus trachurus
#Tripterygiidae sp.. to Tripterygiidae sp.
rename_species <- c(
  "Gerres methurni"        = "Gerres methueni",
  "Secutor insidiator"     = "Deveximentum insidiator",
  "Gobiidae sp.."          = "Gobiidae sp.",
  "Labrid sp."             = "Labridae sp.",
  "Liza richardsoni"       = "Liza richardsonii",
  "Melanostomiidae sp."    = "Grammatostomia sp.",
  "Mugilidae sp.."         = "Mugilidae sp.",
  "Trachurus sp."          = "Trachurus trachurus",
  "Tripterygiidae sp.."    = "Tripterygiidae sp."
)

lit_dat_cleaned <- lit_dat %>%
  # remove invalid species
  filter(!species %in% remove_species) %>%
  # rename incorrect species entries
  mutate(species = recode(species, !!!rename_species))

length(unique(lit_dat_cleaned$species)) #629
library(dplyr)
#read in teleost taxonomy updated and left join to litdat
teleost_taxonomy <- read_excel("~/Desktop/TELEOST_TAXONOMY_UPDATED.xlsx")
lit_dat_joined <- lit_dat_cleaned %>%
  left_join(teleost_taxonomy, by = c("species", "family", "order"))

colnames(lit_dat_joined)

#-------------------------------------------------------------------------------
# Taxonomic resolution plot
#-------------------------------------------------------------------------------

lit_dat_final %>% count(taxonomic_resolution, sort = TRUE)
#Species 6041
#Genus  246
#Family  176
#Order    1
lit_dat_final<- lit_dat_joined%>%
  filter(!is.na(taxonomic_resolution))


length(unique(lit_dat_final$species)) #628 marine teleost species 
length(unique(lit_dat_final$family)) #123 families

taxon_summary <- lit_dat_final %>%
  count(taxonomic_resolution)
# Bar plot
ggplot(taxon_summary, aes(x = reorder(taxonomic_resolution, -n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 3, family = "Times") +
  labs(
    title = "Number of Records per Taxonomic Resolution for Literature-Sourced Data",
    x = "Taxonomic Resolution",
    y = "Number of Records"
  ) +
  theme_classic(base_family = "Times") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10), size = 11),
    axis.title.x = element_text(margin = margin(t = 10), size = 11),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )
#-------------------------------------------------------------------------------
# Top species and families
#-------------------------------------------------------------------------------
#top 20 families and species
top_families <- lit_dat_final %>%
  filter(!is.na(family)) %>%
  count(family, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_families, aes(x = reorder(family, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Most Frequently Occuring Familes in Literature-Sourced Data",
    x = "Family",
    y = "Number of Records"
  ) +
  theme_classic(base_family = "Times") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10), size = 12),
    axis.title.x = element_text(margin = margin(t = 10), size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
  )

#top 20 species
top_species <- lit_dat_final %>%
  filter(!is.na(species)) %>%
  count(species, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_species, aes(x = reorder(species, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "20 Most Frequently Occuring Species in Literature-Sourced Data",
    x = "Species",
    y = "Number of Records"
  ) +
  theme_classic(base_family = "Times") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10), size = 12),
    axis.title.x = element_text(margin = margin(t = 10), size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
  )

#-------------------------------------------------------------------------------
# spatial analyses
#-------------------------------------------------------------------------------

# lat and lon range
summary(lit_dat_final$longitude)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
 # 16.72   22.11   25.59   25.11   29.94   32.88     
summary(lit_dat_final$latitude)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# -34.81  -34.05  -33.99  -32.80  -31.36  -26.90 
missing_coords <- lit_dat_final %>%
  filter(is.na(latitude) | is.na(longitude))
missing_coords
#dataset 130 missing coords FIX

manual_coords <- tribble(
  ~datasetID, ~method, ~yearstart, ~sitename, ~latitude, ~longitude,
  130, "PlanktonNet", 1991, "Elands sandy deep 0.35km offshore", -34.047222, 24.076667,
  130, "PlanktonNet", 1991, "Elands sandy deep 1.26km offshore", -34.056111, 24.073889,
  130, "PlanktonNet", 1991, "Elands sandy surface 0.35km offshore", -34.047222, 24.076667,
  130, "PlanktonNet", 1991, "Elands sandy surface 3.83km offshore", -34.078333, 24.066389,
  130, "PlanktonNet", 1991, "Knoll high profile reef deep 0.35km offshore", -34.025833, 23.908333,
  130, "PlanktonNet", 1991, "Knoll high profile reef deep 1.26km offshore", -34.033611, 23.905556,
  130, "PlanktonNet", 1991, "Knoll high profile reef surface 0.35km offshore", -34.025833, 23.908333,
  130, "PlanktonNet", 1991, "Knoll high profile reef surface 3.83km offshore", -34.055556, 23.896389,
  130, "PlanktonNet", 1991, "Rheeders high profile reef deep 0.35km offshore", -34.031944, 23.945278,
  130, "PlanktonNet", 1991, "Rheeders high profile reef deep 1.26km offshore", -34.038611, 23.943611,
  130, "PlanktonNet", 1991, "Rheeders high profile reef surface 0.35km offshore", -34.031944, 23.945278,
  130, "PlanktonNet", 1991, "Rheeders high profile reef surface 3.83km offshore", -34.060556, 23.938333,
  130, "PlanktonNet", 1991, "Sanddrif sand and reef deep 0.35km offshore", -34.043333, 24.0325,
  130, "PlanktonNet", 1991, "Sanddrif sand and reef deep 1.26km offshore", -34.050833, 24.029722,
  130, "PlanktonNet", 1991, "Sanddrif sand and reef surface 0.35km offshore", -34.043333, 24.0325,
  130, "PlanktonNet", 1991, "Sanddrif sand and reef surface 3.83km offshore", -34.075556, 24.020833
)
lit_dat_final <- lit_dat_final %>%
  mutate(datasetID = as.character(datasetID)) %>%   # ensure character type
  left_join(
    manual_coords %>%
      mutate(datasetID = as.character(datasetID)) %>%  # ensure same type
      select(sitename, datasetID,
             latitude_manual = latitude,
             longitude_manual = longitude),
    by = c("sitename", "datasetID")
  ) %>%
  mutate(
    latitude  = if_else(is.na(latitude) | latitude == 0, latitude_manual, latitude),
    longitude = if_else(is.na(longitude) | longitude == 0, longitude_manual, longitude)
  ) %>%
  select(-latitude_manual, -longitude_manual)


# Re-classify coastline
lit_dat_final <- lit_dat_final %>%
  mutate(
    coast = case_when(
      longitude >= 14 & longitude < 20 & latitude <= -28 & latitude >= -36.5 ~ "west",
      longitude >= 20 & longitude < 27 & latitude <= -33 & latitude >= -36.5 ~ "south",
      longitude >= 27 & longitude <= 35 & latitude <= -26 & latitude >= -35 ~ "east",
      TRUE ~ "west"   # force anything else into West
    )
  )

# Summarise records and species per coast
coast_summary <- lit_dat_final %>%
  group_by(coast) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species)
  )

coast_summary
#. coast   n_records  n_species
#1 East       2276       514
#2 South      2971       237
#3 West       1217       162

# Summarise records and species per gear type
source_summary <- lit_dat_final %>%
  group_by(datatype) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species)
  )
source_summary 
# datatype.   n_records. n_species
#1 C          841        185
#2 O          1022       256
#3 S          4601       510


#for each data type
method_summary_species <- lit_dat_final%>%
  group_by(datatype, method) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species),
    .groups = "drop"
  ) %>%
  arrange(datatype, desc(n_records))

method_summary_species %>%
  filter(datatype == "C") %>%
  arrange(desc(n_records))

method_summary_species %>%
  filter(datatype == "O") %>%
  arrange(desc(n_records))

method_summary_species %>%
  filter(datatype == "S") %>%
  arrange(desc(n_records))

#  Records per DATATYPE & CLASSIFICATION
records_summary <- lit_dat_final %>%
  count(datatype, dataclass, name = "n_records")

# Species richness per data scheme
species_summary <- lit_dat_final%>%
  group_by(datatype, dataclass) %>%
  summarise(n_species = n_distinct(species), .groups = "drop")

#Plot
ggplot(records_summary, aes(x = datatype, y = n_records, fill = dataclass)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  labs(
    title = "Number of Records by Data Classification",
    x = "Data Type",
    y = "Number of Records"
  ) +
  theme_classic(base_family = "Times") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10), size = 12),
    axis.title.x = element_text(margin = margin(t = 10), size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
  )

#-------------------------------------------------------------------------------
# spatial plots
#-------------------------------------------------------------------------------
#main
ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_point(
    data = lit_dat_final,
    aes(x = longitude, y = latitude, color = datatype), 
    alpha = 0.6, size = 2.0
  ) +
  scale_color_manual(
    values = c(
      "S" = "navyblue",       
      "O" = "limegreen",  
      "C" = "purple"  
    )
  ) +
  coord_sf(xlim = c(15, 35), ylim = c(-36, -25), expand = FALSE) +
  labs(
    title = "Distribution of Marine Teleost Occurence Data Sourced from Literature",
    x = "Longitude", y = "Latitude"
  ) +
  theme_classic(base_family = "Times") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10), size = 11),
    axis.title.x = element_text(margin = margin(t = 10), size = 11),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10))

#all
ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_point(
    data = lit_dat,
    aes(x = longitude, y = latitude),
    alpha = 0.3, size = 0.5,
    color = "navyblue"
  ) +
  coord_sf(xlim = c(10, 35), ylim = c(-40, -25), expand = FALSE) +
  labs(
    title = "Distribution of Marine Teleost Occurrence Data Sourced from Literature",
    x = "Longitude", y = "Latitude"
  ) +
  theme_classic(base_family = "Times") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10), size = 11),
    axis.title.x = element_text(margin = margin(t = 10), size = 11),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )
names(lit_dat)
sort(unique(lit_dat$yearend), na.last = TRUE)
write.csv(lit_dat_final, "~/Desktop/wd/masters/RESULTSPLOTS_NOVEMBER/litdatfinal.csv", row.names = FALSE)


