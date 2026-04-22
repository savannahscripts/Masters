# ================================================
# Cleaning GBIF AND GETTING READY FOR MERGE
# ================================================
# Install packages 
# install.packages(c("sf", "ggplot2", "rnaturalearth", "rnaturalearthdata", "rgeos"))

# Load libraries
library(sf)
library(writexl)
library(readxl)
library(ggplot2)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(stringr)
getwd()
# Set working directory
setwd("/Users/savannahanderson/Desktop/wd/masters")
# ================================================
# Read GBIF data
# ================================================
#-------------------------------------------------------------------------------
#1 Preserved specimens
#-------------------------------------------------------------------------------
museumdat <- read.delim("SOURCE DATA/GBIF_Specimen/occurrence.txt", 
                      header = TRUE, sep = "\t", quote = "")
colnames(museumdat)
head(museumdat)
# Select relevant columns
museumdat <- museumdat %>%
  select(
    gbifID, species, order, family, genus,
    decimalLatitude, decimalLongitude,
    eventDate, year,
    basisOfRecord,
    institutionCode,
    taxonomicStatus
  )
#-------------------------------------------------------------------------------
# 2. Human observations (inaturalist data)
#-------------------------------------------------------------------------------
inatdat <- read.delim("SOURCE DATA/GBIF_INAT/occurrence.txt", 
                        header = TRUE, sep = "\t", quote = "")
colnames(inatdat)
head(inatdat)
# Select relevant columns
inatdat <- inatdat %>%
  select(
    gbifID, species, order, family, genus,
    decimalLatitude, decimalLongitude,
    eventDate, year,
    basisOfRecord,
    institutionCode,
    taxonomicStatus
  )
#-------------------------------------------------------------------------------
# view orders, families and species high level
#-------------------------------------------------------------------------------
unique(museumdat$order) %>% sort()
unique(museumdat$family)  %>% sort()
length(unique(museumdat$species)) #2471
unique(inatdat$order) %>% sort()
unique(inatdat$family)  %>% sort()
length(unique(inatdat$species)) #745
#-------------------------------------------------------------------------------
# Remove known freshwater families
#-------------------------------------------------------------------------------
freshwater_families <- c("Cichlidae", "Bagridae", "Pimelodidae", "Siluridae", 
                         "Callichthyidae", "Doradidae", "Clariidae", 
                         "Claroteidae", "Amphiliidae", "Austroglanididae",
                         "Loricariidae", "Malapteruridae", "Centrarchidae", 
                         "Mochokidae", "Anabantidae","Galaxiidae", "Schilbeidae", 
                         "Pangasiidae","Mormyridae", "Kneriidae","Mastacembelidae", 
                         "Percidae")
freshwater_orders <- c(
  "Osteoglossiformes", 
  "Synbranchiformes"
)

museumdat <- museumdat %>%
  filter(
    !order %in% freshwater_orders,
    !family %in% freshwater_families
  )

inatdat <- inatdat %>%
  filter(
    !order %in% freshwater_orders,
    !family %in% freshwater_families
  )

#-------------------------------------------------------------------------------
# Remove rows with missing values 
#-------------------------------------------------------------------------------

inatdat <- inatdat %>%
  filter(
    !is.na(species) & species != "",
    !is.na(decimalLatitude) & !is.na(decimalLongitude),
    !is.na(year),
    !is.na(taxonomicStatus) & taxonomicStatus != ""
  )

museumdat <- museumdat %>%
  filter(
    !is.na(species) & species != "",
    !is.na(decimalLatitude) & !is.na(decimalLongitude),
    !is.na(year),
    !is.na(taxonomicStatus) & taxonomicStatus != ""
  )

#-------------------------------------------------------------------------------
# Keep only accepted taxonomy statuses
#-------------------------------------------------------------------------------

inatdat <- inatdat %>%
  filter(!taxonomicStatus %in% c("DOUBTFUL", "SYNONYM"))

museumdat <- museumdat %>%
  filter(!taxonomicStatus %in% c("DOUBTFUL", "SYNONYM"))

#-------------------------------------------------------------------------------
# n species
#-------------------------------------------------------------------------------

length(unique(museumdat$species)) #1987
length(unique(inatdat$species)) #696

#-------------------------------------------------------------------------------
# review divisions
#-------------------------------------------------------------------------------
unique(museumdat$order)
unique(inatdat$order)

unique(museumdat$basisOfRecord)
#"PRESERVED_SPECIMEN" "MATERIAL_SAMPLE"   
unique(inatdat$basisOfRecord)
#"HUMAN_OBSERVATION"

#-------------------------------------------------------------------------------
# distinct species
#-------------------------------------------------------------------------------

#MUSEUMDAT
museum_spp <- museumdat %>%
  distinct(species) %>% 
  arrange(species)  

#spp by BoR
species_by_basis <- museumdat %>%
  distinct(basisOfRecord, species) %>%  
  arrange(basisOfRecord, species) 

# Split by BoR 
species_by_basis %>%
  group_split(basisOfRecord) %>%
  walk2(
    unique(species_by_basis$basisOfRecord),
    ~ write.csv(.x, paste0("unique_species_", .y, ".csv"), row.names = FALSE)
  )

#686 Material Sample
#1955 Preserved Specimen

###INAT
inat_spp <- inatdat %>%
  distinct(species) %>% 
  arrange(species)  

#-------------------------------------------------------------------------------
#dist of records in eez
#-------------------------------------------------------------------------------
install.packages(c("sf", "rnaturalearth", "rnaturalearthdata", "ggplot2"))
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

eez <- st_read("MAPPING/EEZ/eez_v12.shp", quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(4326)
world <- ne_countries(scale = "medium", returnclass = "sf")
sa_map <- world %>% filter(admin == "South Africa")
sa <- ne_countries(scale = "medium", country = "South Africa", returnclass = "sf")

# uncleaned museum
ggplot() +
  geom_sf(data = sa, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_point(
    data = museumdat,
    aes(x = decimalLongitude, y = decimalLatitude),
    color = "blue", size = 0.5, alpha = 0.3
  ) +
  coord_sf(xlim = c(10, 35), ylim = c(-40, -20), expand = FALSE) +
  labs(
    title = "Record Distribution of Preserved Specimen GBIF data",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 14, base_family = "Times New Roman")

#comment: very messy, need to filter against eez and inspect spp

# uncleaned inat
head(inatdat)
ggplot() +
  geom_sf(data = sa, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_point(
    data = inatdat,
    aes(x = decimalLongitude, y = decimalLatitude),
    color = "blue", size = 0.5, alpha = 0.3
  ) +
  coord_sf(xlim = c(10, 35), ylim = c(-40, -20), expand = FALSE) +
  labs(
    title = "Record Distribution of Human Observation iNaturalist data",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 14, base_family = "Times New Roman")

#comment: much cleaner, mostly shallow, coastal (expected) need to be cleaned, some inland points 

#================================================================================
# convert to sf
#================================================================================

museum_sf <- st_as_sf(museumdat, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
inat_sf <- st_as_sf(inatdat, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
#===============================================================================
# MAKE SURE TO INTERSECT GBIF WITH EEZ to eliminate terrestrial records
#===============================================================================
museum_in_eez <- st_filter(museum_sf, eez, .predicate = st_intersects)
inat_in_eez <- st_filter(inat_sf, eez, .predicate = st_intersects)

cat("museum Points inside EEZ:", nrow(museum_in_eez), "\n") #
#museum Points inside EEZ: 23288 
cat("inat Points inside EEZ:", nrow(inat_in_eez), "\n") #
#inat Points inside EEZ: 7415 

length(unique(museum_in_eez$species)) #1814
unique(museum_in_eez$basisOfRecord)
# "PRESERVED_SPECIMEN" "MATERIAL_SAMPLE" 
#only include preserved specimen

museum_in_eez_preserved <- museum_in_eez %>%
  filter(basisOfRecord == "PRESERVED_SPECIMEN")

length(unique(museum_in_eez_preserved$species)) #1777 #123

length(unique(inat_in_eez$species)) #636 #106

#===============================================================================
#PLOT INSIDE EEZ
#===============================================================================
#dist of museum records in eez
ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_sf(data = museum_in_eez_preserved, color = "navyblue", size = 0.5, alpha = 0.3) +
  coord_sf(xlim = c(10, 35), ylim = c(-40, -25), expand = FALSE) +
  labs(
    title = "Distribution of Marine Teleost Occurence Data  (GBIF-Sourced Preserved Museum Specimens)",
    x = "Longitude", y = "Latitude") + 
  theme_classic(base_family = "Times") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10), size = 12),
    axis.title.x = element_text(margin = margin(t = 10), size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
  )

#dist of inat records in eez
ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_sf(data = inat_in_eez, color = "navyblue", size = 0.5, alpha = 0.3) +
  coord_sf(xlim = c(10, 35), ylim = c(-40, -25), expand = FALSE) +
  labs(
    title = "Distribution of Marine Teleost Occurence Data  Sourced from iNaturalist",
    x = "Longitude", y = "Latitude") +
  theme_classic(base_family = "Times") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10), size = 12),
    axis.title.x = element_text(margin = margin(t = 10), size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
  )


#===============================================================================
# PLOTS MUSEUM
#===============================================================================
#-------------------------------------------------------------------------------
# tax res bar plot 
#-------------------------------------------------------------------------------
museum_in_eez_preserved <- museum_in_eez_preserved %>%
  mutate(
    taxonomic_resolution = case_when(
      is.na(species) ~ "genus",
      str_detect(species, "dae") ~ "family",
      str_detect(species, "formes") ~ "order",
      str_detect(species, "\\bsp\\.") ~ "genus",
      TRUE ~ "species"
    )
  )
taxon_summary <- museum_in_eez_preserved %>%
  count(taxonomic_resolution)

ggplot(taxon_summary, aes(x = reorder(taxonomic_resolution, -n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 3, family = "Times") +
  labs(
    title = "Record Count by Taxonomic Resolution (GBIF:Preserved Specimen Data)",
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

#top 20 species
top_species_PS <- museum_in_eez_preserved %>%
  filter(!is.na(species)) %>%
  count(species, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_species_PS, aes(x = reorder(species, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequently Recorded Species (GBIF:Preserved Specimen Data)",
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

#top 20 families
top_families_PS <- museum_in_eez_preserved %>%
  filter(!is.na(family)) %>%
  count(family, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_families_PS, aes(x = reorder(family, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequently Recorded Families (GBIF:Preserved Specimen Data)",
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

#===============================================================================
# PLOTS INAT
#===============================================================================
#-------------------------------------------------------------------------------
# tax res bar plot 
#-------------------------------------------------------------------------------
inat_in_eez <- inat_in_eez %>%
  mutate(
    taxonomic_resolution = case_when(
      is.na(species) ~ "genus",
      str_detect(species, "dae") ~ "family",
      str_detect(species, "formes") ~ "order",
      str_detect(species, "\\bsp\\.") ~ "genus",
      TRUE ~ "species"
    )
  )
taxon_summary <- inat_in_eez %>%
  count(taxonomic_resolution)

ggplot(taxon_summary, aes(x = reorder(taxonomic_resolution, -n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 3, family = "Times") +
  labs(
    title = "Record Count by Taxonomic Resolution (GBIF: iNaturalist Data)",
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

#top 20 species
top_species_IN <- inat_in_eez %>%
  filter(!is.na(species)) %>%
  count(species, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_species_IN, aes(x = reorder(species, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequently Recorded Species (GBIF: iNaturalist Data)",
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

#top 20 families
top_families_IN <- inat_in_eez %>%
  filter(!is.na(family)) %>%
  count(family, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_families_IN, aes(x = reorder(family, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequently Recorded Families (GBIF: iNaturalist Data)",
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

#spatial coverage
#-------------------------------------------------------------------------------
summary(inatdat$decimalLatitude)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-34.99  -34.18  -30.31  -31.13  -27.54  -22.49 
summary(inatdat$decimalLongitude)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#16.55   18.85   30.75   26.92   32.68   33.21
summary(museumdat$decimalLatitude)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-57.482 -33.683 -30.333 -31.181 -29.489  -3.333 
summary(museumdat$decimalLongitude)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -60.63   25.60   30.70   27.82   31.50   64.97 

#-------------------------------------------------------------------------------
#COAST COVERAGE
#-------------------------------------------------------------------------------
#INAT
inatdat <- inatdat %>%
  mutate(
    coast = case_when(
      decimalLongitude >= 14 & decimalLongitude < 20 & decimalLatitude <= -28 & decimalLatitude >= -36.5 ~ "west",
      decimalLongitude >= 20 & decimalLongitude < 27 & decimalLatitude <= -33 & decimalLatitude >= -36.5 ~ "south",
      decimalLongitude >= 27 & decimalLongitude <= 35 & decimalLatitude <= -26 & decimalLatitude >= -35 ~ "east",
      TRUE ~ "west"   # force anything else into West
    )
  )

inat_coast_summary <- inatdat %>%
  group_by(coast) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species),
    .groups = "drop"
  )
print(inat_coast_summary)
#  coast n_records n_species
# east       5897       595
# south      1456       165
# west       2748       146

#GBIF
museumdat <- museumdat %>%
  mutate(
    coast = case_when(
      decimalLongitude >= 14 & decimalLongitude < 20 & decimalLatitude <= -28 & decimalLatitude >= -36.5 ~ "west",
      decimalLongitude >= 20 & decimalLongitude < 27 & decimalLatitude <= -33 & decimalLatitude >= -36.5 ~ "south",
      decimalLongitude >= 27 & decimalLongitude <= 35 & decimalLatitude <= -26 & decimalLatitude >= -35 ~ "east",
      TRUE ~ "west"   # force anything else into West
    )
  )

museum_coast_summary <- museumdat %>%
  group_by(coast) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species),
    .groups = "drop"
  )
print(museum_coast_summary)
#  coast n_records n_species
# east      24181      1696
# south      6479       659
# west        6319       713


colnames(museumdat)
colnames(inatdat)
unique(museumdat$taxonomicStatus)
museumdat <- museumdat %>%
  select(
-taxonomicStatus
  )

inatdat <- inatdat %>%
  select(
    -taxonomicStatus
  )

inatdat <- inatdat %>%
  mutate(
    taxonomic_resolution = case_when(
      is.na(species) ~ "genus",
      str_detect(species, "dae") ~ "family",
      str_detect(species, "formes") ~ "order",
      str_detect(species, "\\bsp\\.") ~ "genus",
      TRUE ~ "species"
    )
  )
taxon_summary <- inatdat %>%
  count(taxonomic_resolution)


museumdat <- museumdat %>%
  mutate(
    taxonomic_resolution = case_when(
      is.na(species) ~ "genus",
      str_detect(species, "dae") ~ "family",
      str_detect(species, "formes") ~ "order",
      str_detect(species, "\\bsp\\.") ~ "genus",
      TRUE ~ "species"
    )
  )
taxon_summary <- museumdat %>%
  count(taxonomic_resolution)

sort(unique(museumdat$year), na.last = TRUE)

names(inatdat)
write.csv(museumdat, "~/Desktop/wd/masters/RESULTSPLOTS_NOVEMBER/GBIFmuseumdat.csv", row.names = FALSE)
write.csv(inatdat, "~/Desktop/wd/masters/RESULTSPLOTS_NOVEMBER/GBIFinatdat.csv", row.names = FALSE)



#SCALE MUSEUM

library(dplyr)
library(geosphere)

museum_pts <- museumdat %>%
  mutate(
    latitude  = as.numeric(decimalLatitude),
    longitude = as.numeric(decimalLongitude),
    
    # grouping level for spacing calculations
    group_id = case_when(
      !is.na(eventID) & eventID != "" ~ paste0("event_", eventID),
      !is.na(datasetKey) & datasetKey != "" ~ paste0("dataset_", datasetKey),
      TRUE ~ paste0("inst_", institutionCode, "_", collectionCode)
    )
  ) %>%
  filter(
    !is.na(latitude), !is.na(longitude),
    latitude  != 0, longitude != 0,
    between(latitude, -90, 90),
    between(longitude, -180, 180)
  )

museum_unique <- museum_pts %>%
  distinct(group_id, latitude, longitude, .keep_all = TRUE)

museum_steps <- museum_unique %>%
  arrange(group_id, latitude, longitude) %>%   # deterministic ordering
  group_by(group_id) %>%
  mutate(
    step_km = distHaversine(
      cbind(longitude, latitude),
      cbind(lag(longitude), lag(latitude))
    ) / 1000
  ) %>%
  ungroup() %>%
  filter(!is.na(step_km), step_km > 0, step_km < 500)
#overaall
museum_spatial_summary <- museum_steps %>%
  summarise(
    n_pairs = n(),
    median_spacing_km = median(step_km),
    q25 = quantile(step_km, 0.25),
    q75 = quantile(step_km, 0.75),
    max_spacing_km = max(step_km)
  )

museum_spatial_summary
#bydataset

museum_steps2 <- museum_steps %>%
  mutate(dataset_group = ifelse(!is.na(datasetName) & datasetName != "", datasetName, datasetKey))

museum_spatial_by_dataset <- museum_steps2 %>%
  group_by(dataset_group) %>%
  summarise(
    n_pairs = n(),
    median_spacing_km = median(step_km),
    q25 = quantile(step_km, 0.25),
    q75 = quantile(step_km, 0.75),
    max_spacing_km = max(step_km),
    .groups = "drop"
  ) %>%
  arrange(desc(n_pairs))

print(museum_spatial_by_dataset, n =Inf)

museum_spatial_by_dataset_stable <- museum_spatial_by_dataset %>%
  filter(n_pairs >= 20)

museum_spatial_by_dataset_stable


#INAT SCALE
library(dplyr)
library(geosphere)

inat_points <- inatdat %>%
  transmute(
    lat  = as.numeric(decimalLatitude),
    lon  = as.numeric(decimalLongitude),
    obs  = recordedBy,                 # iNat user
    date = as.Date(eventDate),         # optional, not required for spatial
    dataset_group = datasetKey         # often constant for iNat exports
  ) %>%
  filter(!is.na(lat), !is.na(lon))

# overall spacing: sort within group, then lag distance
inat_spatial <- inat_points %>%
  arrange(obs, lat, lon) %>%           # you can also arrange(obs, date) if date is dense
  group_by(obs) %>%
  mutate(
    step_km = distHaversine(
      cbind(lon, lat),
      cbind(lag(lon), lag(lat))
    ) / 1000
  ) %>%
  ungroup() %>%
  filter(!is.na(step_km), step_km > 0, step_km < 500)

inat_spatial_summary <- inat_spatial %>%
  summarise(
    n_pairs = n(),
    median_spacing_km = median(step_km),
    q25 = quantile(step_km, 0.25),
    q75 = quantile(step_km, 0.75),
    max_spacing_km = max(step_km)
  )

inat_spatial_summary

inat_spatial_by_observer <- inat_spatial %>%
  group_by(obs) %>%
  summarise(
    n_pairs = n(),
    median_spacing_km = median(step_km),
    q25 = quantile(step_km, 0.25),
    q75 = quantile(step_km, 0.75),
    max_spacing_km = max(step_km),
    .groups = "drop"
  ) %>%
  arrange(desc(n_pairs))

inat_spatial_by_observer

inat_spatial_by_dataset <- inat_points %>%
  arrange(dataset_group, lat, lon) %>%
  group_by(dataset_group) %>%
  mutate(
    step_km = distHaversine(
      cbind(lon, lat),
      cbind(lag(lon), lag(lat))
    ) / 1000
  ) %>%
  ungroup() %>%
  filter(!is.na(step_km), step_km > 0, step_km < 500) %>%
  group_by(dataset_group) %>%
  summarise(
    n_pairs = n(),
    median_spacing_km = median(step_km),
    q25 = quantile(step_km, 0.25),
    q75 = quantile(step_km, 0.75),
    max_spacing_km = max(step_km),
    .groups = "drop"
  ) %>%
  arrange(desc(n_pairs))

inat_spatial_by_dataset

inat_observer_medians <- inat_spatial_by_observer %>%
  filter(n_pairs >= 5) %>%   # optional minimum stability threshold
  summarise(
    n_observers = n(),
    median_of_medians_km = median(median_spacing_km, na.rm = TRUE),
    q25_of_medians_km = quantile(median_spacing_km, 0.25, na.rm = TRUE),
    q75_of_medians_km = quantile(median_spacing_km, 0.75, na.rm = TRUE)
  )

inat_observer_medians


