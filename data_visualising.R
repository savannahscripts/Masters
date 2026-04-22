
# author: "Savannah Anderson"
#install packages

install.packages(c("ggplot2", "sf", "rnaturalearth", "rnaturalearthdata", "viridis"))
##load libraries
library(readxl)
library(writexl)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis) 
library(ggspatial)
library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(stringr)
library(worrms)

getwd()
setwd("/Users/savannahanderson/Desktop/wd/masters")

#-------------------------------------------------------------------------------
#FMDAT (RESEARCH) #DATASET CLEANED
#-------------------------------------------------------------------------------

FMdat <- read.csv("FINALDATA/final_FM.csv")
length(unique(FMdat$species))
length(unique(FMdat$family))
# Order yearstart values
sorted_yearstart <- sort(unique(FMdat$yearstart), na.last = TRUE)
print(sorted_yearstart)

# Order yearend values
sorted_yearend <- sort(unique(FMdat$yearend), na.last = TRUE)
print(sorted_yearend)

# Count number of records per yearstart
yearstart_counts <- FMdat %>%
  count(yearstart, sort = TRUE)

# Count number of records per yearend
yearend_counts <- FMdat %>%
  count(yearend, sort = TRUE)

# View the tables
print(yearstart_counts)
print(yearend_counts)

# Plot yearstart
ggplot(yearstart_counts, aes(x = yearstart, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Number of Records per Start Year",
       x = "Year Start", y = "Number of Records") +
  theme_minimal(base_size = 14, base_family = "Times New Roman")

# Plot yearend
ggplot(yearend_counts, aes(x = yearend, y = n)) +
  geom_col(fill = "darkorange") +
  labs(title = "Number of Records per End Year",
       x = "Year End", y = "Number of Records") +
  theme_minimal(base_size = 14, base_family = "Times New Roman")

#nmber of sampling methods
unique(FMdat$method)
length(unique(FMdat$sitename))
#number of species per sampling method
colnames(FMdat)
# number of records per tax res
FMdat %>%
  count(taxres) %>%
  ggplot(aes(x = reorder(taxres, -n), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(
    title = "Record Count by Taxonomic Resolution",
    x = "Taxonomic Resolution",
    y = "Number of Records"
  ) +
  theme_minimal(base_size = 14, base_family = "Times New Roman")



#range of coordinates
# Order latitude values
sorted_lat <- sort(unique(FMdat$latitude), na.last = TRUE)
print(sorted_lat)

# Order longitude values
sorted_lon <- sort(unique(FMdat$longitude), na.last = TRUE)
print(sorted_lon)

# Count number of records per lat
lat_counts <- FMdat %>%
  count(latitude, sort = TRUE)

# Count number of records per longitude
lon_counts <- FMdat %>%
  count(longitude, sort = TRUE)


# Define bounding boxes for coastlines
bbox_west  <- c(xmin = 14, xmax = 20, ymin = -35.5, ymax = -28)
bbox_south <- c(xmin = 19, xmax = 27, ymin = -36.5, ymax = -33)
bbox_east  <- c(xmin = 26, xmax = 35, ymin = -35, ymax = -26)

# Filter FMdat by coastline bounding boxes
FM_west <- FMdat %>%
  filter(longitude >= 14, longitude <= 20,
         latitude >= -35.5, latitude <= -28)

FM_south <- FMdat %>%
  filter(longitude >= 19, longitude <= 27,
         latitude >= -36.5, latitude <= -33)

FM_east <- FMdat %>%
  filter(longitude >= 26, longitude <= 35,
         latitude >= -35, latitude <= -26)

# Summarise: records and species
summary_df <- tibble(
  coast = c("West", "South", "East"),
  n_records = c(nrow(FM_west), nrow(FM_south), nrow(FM_east)),
  n_species = c(n_distinct(FM_west$species),
                n_distinct(FM_south$species),
                n_distinct(FM_east$species))
)

# View result
summary_df

# Load clean EEZ shapefile
sa_eez <- st_read("/Users/savannahanderson/Desktop/wd/masters/MAPPING/EEZ/eez_v12.shp", quiet = TRUE)

mpas <- st_read("MAPPING/SANBI_PA", layer = "SANBI_PA_2023Q4_July2024", options = "PROMOTE_TO_MULTI=YES") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_make_valid() %>%
  st_transform(4326)

# Step 1: Convert FMdat to sf points (if not already)
FM_sf <- FMdat %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Step 2: Check intersection with MPAs
FM_in_mpa <- st_intersects(FM_sf, mpas, sparse = FALSE)[,1]  # logical vector

# Step 3: Add as column
FM_sf$in_mpa <- FM_in_mpa

# Step 4: Count total records in and out of MPAs
mpa_summary <- FM_sf %>%
  st_drop_geometry() %>%
  count(in_mpa)

# View summary
print(mpa_summary)

st_crs(FM_sf)
st_crs(mpas)

#how many recods fall into MPAs
plot(st_geometry(mpas), border = "black", col = NA, main = "FM points over SANBI MPAs")
plot(st_geometry(FM_sf), col = "red", pch = 16, cex = 0.5, add = TRUE)

names(mpas)
unique(mpas$TYPE)

marine_mpas <- mpas %>%
  filter(grepl("Marine Protected Area", TYPE, ignore.case = TRUE))
# Try intersect again
FM_sf$in_mpa <- st_intersects(FM_sf, marine_mpas, sparse = FALSE)[,1]
table(FM_sf$in_mpa)


FM_mpa_joined <- st_join(FM_sf, mpas, join = st_within, left = FALSE)
nrow(FM_mpa_joined)  # this should be > 0 if anything intersects

# Or use st_intersects with a `left = TRUE` join to retain all
FM_mpa_all <- st_join(FM_sf, mpas, join = st_intersects, left = TRUE)

# Then check how many matched
sum(!is.na(FM_mpa_all$ORIG_NME))  # assuming NAME is MPA name column


marine_matched <- FM_mpa_joined %>%
  filter(TYPE == "Marine Protected Area")

nrow(marine_matched)
length(unique(marine_matched$ORIG_NME))  # how many unique MPAs

# Total FM records
n_total <- nrow(FM_sf)

# Records in *any* protected area
n_pa <- nrow(FM_mpa_joined)

# Records in marine PAs
n_mpa <- nrow(marine_matched)

# Summary table
data.frame(
  total_records = n_total,
  records_in_any_PA = n_pa,
  records_in_marine_PA = n_mpa,
  records_outside_all_PAs = n_total - n_pa
)

marine_matched %>%
  st_drop_geometry() %>%
  count(ORIG_NME, sort = TRUE) %>%
  rename(MPA_Name = ORIG_NME, n_records = n)


#no of species per MPA
marine_matched %>%
  st_drop_geometry() %>%
  filter(!is.na(CUR_NME), !is.na(species)) %>%
  group_by(CUR_NME) %>%
  summarise(n_species = n_distinct(species)) %>%
  arrange(desc(n_species))


colnames(FMdat)

gear_summary <- FMdat %>%
  group_by(method) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species),
    .groups = "drop"
  )
print(gear_summary)


ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_point(data = FMdat, aes(x = longitude, y = latitude), 
             color = "darkblue", alpha = 0.6, size = 1.5) +
  coord_sf(xlim = c(10, 35), ylim = c(-40, -20), expand = FALSE) +
  labs(
    title = "Record Distribution of Research data",
    x = "Longitude", y = "Latitude") + 
  theme_minimal(base_size = 14, base_family = "Times New Roman")

#-------------------------------------------------------------------------------
#CAPFISH OBSERVER PROGRAMME
#-------------------------------------------------------------------------------
CAPFISH <- read_excel("outputs/CAPFISH/CAPFISH.xlsx")

length(unique(CAPFISH_clean$species))
length(unique(CAPFISH_clean$family))

colnames(CAPFISH_clean)

# Define bounding boxes for coastlines
bbox_west  <- c(xmin = 14, xmax = 20, ymin = -35.5, ymax = -28)
bbox_south <- c(xmin = 19, xmax = 27, ymin = -36.5, ymax = -33)
bbox_east  <- c(xmin = 26, xmax = 35, ymin = -35, ymax = -26)

summary(CAPFISH$longitude)
summary(CAPFISH$latitude)

CAPFISH <- CAPFISH %>%
  mutate(latitude = ifelse(latitude > 0, -latitude, latitude))

#

CAPFISH_clean <- CAPFISH %>%
  filter(
    longitude >= 14, longitude <= 35,
    latitude <= -26, latitude >= -36.5
  )

#remove
#years
# Order yearstart values
sorted_year <- sort(unique(CAPFISH_clean$year), na.last = TRUE)
print(sorted_year)

#remove "NA" remove "2029"
CAPFISH_clean <- CAPFISH_clean %>%
  filter(!is.na(year) & year != 2029)

#rangeof lat and lon
summary(CAPFISH_clean$longitude)
summary(CAPFISH_clean$latitude)
nrow(CAPFISH_clean)

# Re-classify coastline
CAPFISH_clean <- CAPFISH_clean %>%
  mutate(
    coast = case_when(
      longitude >= 14 & longitude <= 20 & latitude >= -35.5 & latitude <= -28 ~ "West",
      longitude >= 19 & longitude <= 27 & latitude >= -36.5 & latitude <= -33 ~ "South",
      longitude >= 26 & longitude <= 35 & latitude >= -35 & latitude <= -26 ~ "East",
      TRUE ~ "Other"
    )
  )

# Summarise records and species per coast
capfish_summary <- CAPFISH_clean %>%
  filter(coast != "Other") %>%
  group_by(coast) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species)
  )

capfish_summary

source_summary <- CAPFISH_clean %>%
  filter(gear != "Other") %>%
  group_by(gear) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species)
  )
source_summary 

#taxres
CAPFISH_clean <- CAPFISH_clean %>%
  mutate(
    tax_res= case_when(
      !is.na(species) & str_count(species, "\\S+") >= 2 ~ "species",  # Two words
      !is.na(genus) ~ "genus",
      !is.na(family) ~ "family",
      TRUE ~ "unidentified"
    )
  )
# number of records per tax res
CAPFISH_clean %>%
  count(tax_res) %>%
  ggplot(aes(x = reorder(tax_res, -n), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(
    title = "Record Count by Taxonomic Resolution",
    x = "Taxonomic Resolution",
    y = "Number of Records"
  ) +
  theme_minimal(base_size = 14, base_family = "Times New Roman")

#spatial dist
ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_point(data = CAPFISH_clean, aes(x = longitude, y = latitude), 
             color = "darkblue", alpha = 0.6, size = 1.5) +
  coord_sf(xlim = c(10, 35), ylim = c(-40, -20), expand = FALSE) +
  labs(
    title = "Record Distribution of CAPFISH surveys",
    x = "Longitude", y = "Latitude") + 
  theme_minimal(base_size = 14, base_family = "Times New Roman")

#find weird coords
# Step 1: Get only South Africa’s land polygon
world <- ne_countries(scale = "medium", returnclass = "sf")
sa_map <- world %>% filter(admin == "South Africa") %>%
  st_transform(4326)

# Step 2: Convert CAPFISH_clean to sf
capfish_sf <- CAPFISH_clean %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Step 3: Identify points that fall inside SA land polygon
is_on_land <- st_intersects(capfish_sf, sa_map, sparse = FALSE)[,1]

# Step 4: Keep only marine (non-land) points
capfish_marine <- capfish_sf[!is_on_land, ]

# Check how many were removed
n_removed <- sum(is_on_land)
cat("Points removed as terrestrial:", n_removed, "\n")

#plot clened
ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_sf(data = capfish_marine, color = "darkblue", alpha = 0.6, size = 1.5) +
  coord_sf(xlim = c(10, 35), ylim = c(-40, -20), expand = FALSE) +
  labs(
    title = "Cleaned Record Distribution of CAPFISH Surveys",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 14, base_family = "Times New Roman")

# Extract coords back into longitude / latitude columns
capfish_marine <- capfish_marine %>%
  mutate(
    longitude = st_coordinates(.)[, "X"],
    latitude = st_coordinates(.)[, "Y"]
  )
capfish_marine <- capfish_marine %>%
  mutate(
    coast = case_when(
      longitude >= 14 & longitude <= 20 & latitude >= -35.5 & latitude <= -28 ~ "West",
      longitude >= 19 & longitude <= 27 & latitude >= -36.5 & latitude <= -33 ~ "South",
      longitude >= 26 & longitude <= 35 & latitude >= -35 & latitude <= -26 ~ "East",
      TRUE ~ "Other"
    )
  )

capfish_summary <- capfish_marine %>%
  filter(coast != "Other") %>%
  group_by(coast) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species),
    .groups = "drop"
  )

capfish_summary

source_summary <- capfish_marine %>%
  filter(gear != "Other") %>%
  group_by(gear) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species),
    .groups = "drop"
  )

source_summary

summary(capfish_marine$longitude)
summary(capfish_marine$latitude)

length(unique(capfish_marine$species))
length(unique(capfish_marine$family))
sorted_year <- sort(unique(capfish_marine$year), na.last = TRUE)
sorted_year <- sorted_year[!is.na(sorted_year) & sorted_year != 2029]

sorted_year

names(capfish_mpa_joined)
#number of records per MPAS

capfish_marine <- st_transform(capfish_marine, crs = 4326)
mpas <- st_transform(mpas, crs = 4326)
# Ensure geometries are valid
mpas <- st_make_valid(mpas)

# Step 1: Keep only needed columns from mpas
mpas_subset <- mpas %>%
  select(CUR_NME, TYPE, geometry)

# Step 2: Join with simplified layer
capfish_mpa_joined <- st_join(capfish_marine, mpas_subset, join = st_within, left = TRUE)

# Step 3: Now you can filter safely
capfish_marine_matched <- capfish_mpa_joined %>%
  filter(TYPE == "Marine Protected Area")

unique(mpas$CUR_NME)
# 3. Counts
n_total <- nrow(capfish_marine)                        # total marine records
n_pa     <- nrow(capfish_mpa_joined)                   # in any PA
n_mpa    <- nrow(capfish_marine_matched)               # in marine PAs

# 4. Summary table
capfish_mpa_summary <- data.frame(
  total_records = n_total,
  records_in_any_PA = n_pa,
  records_in_marine_PA = n_mpa,
  records_outside_all_PAs = n_total - n_pa
)

print(capfish_mpa_summary)

matched <- capfish_marine_matched %>%
  st_drop_geometry() %>%
  count(CUR_NME, sort = TRUE) %>%
  rename(MPA_Name = CUR_NME, n_records = n)

# Export to Excel
write.csv(CAPFISH_clean, "~/Desktop/wd/masters/outputs/CAPFISH/capfish_final.csv", row.names = FALSE)

#-------------------------------------------------------------------------------
#DTSDAT (DEMERSAL TRAWL)
#-------------------------------------------------------------------------------
#needs order, family, genus, lat+lng avrg, year, depth, & column names standardized


DEMTRAWL <- read.csv("outputs/DEMTRAWL/demtrawldat_final.csv")
length(unique(DEMTRAWL$species))

species_df <- data.frame(species = unique(DEMTRAWL$species))
unique(DEMTRAWL$order)

# Filter unique species names with only one word
single_word_species <- DEMTRAWL %>%
  distinct(species) %>%
  filter(!is.na(species)) %>%
  filter(str_count(species, "\\s+") == 0)  # no spaces = one word

#inspect spp for these orders
#NA                          "Eupercaria incertae sedis" "Saccopharyngiformes"      


#476 spp

#inspect NA
DEMTRAWL %>%
  filter(is.na(order)) %>%
  select(species, family, genus, aphiaID, order) %>%
  distinct()
#remove "teleostei"

#Seriola lalandi  
#order = carangiformes
#aphiaID = 218436

# Hyperoglyphe antarctica 
#ORDER: Scombriformes
#family ; Centrolophidae
#APHIAID: 219737

#Pomadasys kaakan   
#ORDER: Perciformes
#APHIAID: 218566

# Remove the invalid "Teleostei" placeholder row
DEMTRAWL <- DEMTRAWL %>%
  filter(species != "Teleostei")

# Apply specific corrections
DEMTRAWL <- DEMTRAWL %>%
  mutate(
    order = case_when(
      species == "Seriola lalandi" ~ "Carangiformes",
      species == "Hyperoglyphe antarctica" ~ "Scombriformes",
      species == "Pomadasys kaakan" ~ "Perciformes",
      TRUE ~ order
    ),
    family = case_when(
      species == "Hyperoglyphe antarctica" ~ "Centrolophidae",
      TRUE ~ family
    ),
    aphiaID = case_when(
      species == "Seriola lalandi" ~ 218436,
      species == "Hyperoglyphe antarctica" ~ 219737,
      species == "Pomadasys kaakan" ~ 218566,
      TRUE ~ aphiaID
    )
  )

#inspect incertae sedis
DEMTRAWL %>%
  filter(order == "Eupercaria incertae sedis") %>%
  select(species, family, genus, aphiaID, order) %>%
  distinct()
#Heteropriacanthus cruentatus 
#order = "Acanthurifomes" 
#genus = "Heteropriacanthus"


#Cookeolus japonicus
#order = "Acanthurifomes" 
#aphiaID = 127003

DEMTRAWL <- DEMTRAWL %>%
  mutate(
    order = case_when(
      species == "Heteropriacanthus cruentatus" ~ "Acanthuriformes",
      species == "Cookeolus japonicus" ~ "Acanthuriformes",
      TRUE ~ order
    ),
    genus = case_when(
      species == "Heteropriacanthus cruentatus" ~ "Heteropriacanthus",
      TRUE ~ genus
    ),
    aphiaID = case_when(
      species == "Cookeolus japonicus" ~ 127003,
      TRUE ~ aphiaID
    )
  )

colnames(DEMTRAWL)
length(unique(DEMTRAWL$species))
length(unique(DEMTRAWL$mnemonic))
length(unique(DEMTRAWL$family))
length(unique(DEMTRAWL$cruise))
length(unique(DEMTRAWL$station))
length(unique(DEMTRAWL$grid))

# number of records per tax res
DEMTRAWL %>%
  count(taxonomic_resolution) %>%
  ggplot(aes(x = reorder(taxonomic_resolution, -n), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(
    title = "Record Count by Taxonomic Resolution",
    x = "Taxonomic Resolution",
    y = "Number of Records"
  ) +
  theme_minimal(base_size = 14, base_family = "Times New Roman")

#top 20 species
top_species <- DEMTRAWL %>%
  filter(!is.na(species)) %>%
  count(species, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_species, aes(x = reorder(species, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequently Recorded Species",
    x = "Species",
    y = "Record Count"
  ) +
  theme_minimal(base_size = 14, base_family = "Times New Roman")

#top 20 families
top_families <- DEMTRAWL %>%
  filter(!is.na(family)) %>%
  count(family, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_families, aes(x = reorder(family, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequently Recorded Families",
    x = "Family",
    y = "Record Count"
  ) +
  theme_minimal(base_size = 14, base_family = "Times New Roman")

#COAST
DEMTRAWL <- DEMTRAWL %>%
  mutate(
    coast = case_when(
      lon >= 14 & lon <= 20 & lat >= -35.5 & lat <= -28 ~ "West",
      lon >= 19 & lon <= 27 & lat >= -36.5 & lat <= -33 ~ "South",
      lon >= 26 & lon <= 35 & lat >= -35 & lat <= -26 ~ "East",
      TRUE ~ "Other"
    )
  )

# Summarise records and species per coast
demtrawl_summary <- DEMTRAWL %>%
  filter(coast != "Other") %>%
  group_by(coast) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species),
    .groups = "drop"
  )

# View result
demtrawl_summary

#years
summary(DEMTRAWL$lon)
summary(DEMTRAWL$lat)

sorted_year <- sort(unique(DEMTRAWL$year), na.last = TRUE)
sorted_year

colnames(DEMTRAWL)

coord_sf(xlim = c(10, 35), ylim = c(-40, -20), expand = FALSE, crs = 4326)

#dist plot
ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_point(data = DEMTRAWL, aes(x = lon, y = lat), 
             color = "darkblue", alpha = 0.6, size = 1.5) +
  coord_sf(xlim = c(10, 35), ylim = c(-40, -20), expand = FALSE, crs = 4326) +
  labs(
    title = "Record Distribution of Demersal Trawl Surveys",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 14, base_family = "Times New Roman")

#Color by coast
# Convert DEMTRAWL to sf object
DEMTRAWL_sf <- DEMTRAWL %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Plot using geom_sf
ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_sf(data = DEMTRAWL_sf, aes(color = coast), alpha = 0.6, size = 1.5) +
  coord_sf(xlim = c(10, 35), ylim = c(-40, -20), expand = FALSE) +
  labs(
    title = "Demersal Trawl Records by Coastline",
    x = "Longitude", y = "Latitude",
    color = "Coast"
  ) +
  theme_minimal(base_size = 14, base_family = "Times New Roman")

#MPAS
# Join DEMTRAWL with MPA layer
demtrawl_mpa_joined <- st_join(DEMTRAWL_sf, mpas_subset, join = st_within, left = TRUE)

# Filter only Marine Protected Areas
demtrawl_mpa_matched <- demtrawl_mpa_joined %>%
  filter(TYPE == "Marine Protected Area")

n_total <- nrow(DEMTRAWL_sf)
n_pa    <- sum(!is.na(demtrawl_mpa_joined$TYPE))
n_mpa   <- nrow(demtrawl_mpa_matched)

data.frame(
  total_records = n_total,
  records_in_any_PA = n_pa,
  records_in_marine_PA = n_mpa,
  records_outside_all_PAs = n_total - n_pa
)


demtrawl_mpa_matched %>%
  st_drop_geometry() %>%
  count(CUR_NME, sort = TRUE) %>%
  rename(MPA_Name = CUR_NME, n_records = n)

(5728/157995)*100
(4005/128790)*100
(3814/102410)*100

#-------------------------------------------------------------------------------
#MWTS (MIDWATER TRAWL DAT)
#-------------------------------------------------------------------------------
#needs aphiaID, genus, lat+long avrg, gridID, year, depth, & column names standardized
MWTSdat <- read_excel("FINALDATA/MWTOBSdat.xlsx")

#-------------------------------------------------------------------------------
#ANGLING (NMLS)
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#STEP 2: Loading in maps 
#-------------------------------------------------------------------------------
#read in eez
eez <- st_read("/Users/savannahanderson/Desktop/wd/masters/MAPPING/EEZ/eez_v12.shp") %>%
  st_transform(crs = 4326) %>% 
  st_make_valid()
#Load South Africa map (Natural Earth)
sa <- ne_countries(scale = "medium", country = "South Africa", returnclass = "sf")

#check validity
st_is_valid(eez)

#read in commercial 20nm x 20nm grids
gridcodes <- read_excel("MAPPING/commercial_grid_codes_dbo_grids.xlsx")

#====================
# INAT CLEANING
#====================
inat_raw <- read.csv("SOURCE DATA/observations-582319/observations-582319.csv")

inat_cleaned <- inat_raw %>%
  select(-any_of(c(
    "tag_list", "place_guess", "positional_accuracy", "private_place_guess",
    "private_latitude", "private_longitude", "public_positional_accuracy",
    "geoprivacy", "taxon_geoprivacy", "positioning_method"
  )))

write_xlsx(inat_cleaned, "inatdat.xlsx")
#------------------------------------------------------------------------------
#GBIF (CITIZEN SCIENCE)
#-------------------------------------------------------------------------------
#needs aphiaID, gridID, year, depth, & column names standardized
gbif <- read_excel("gbifdat.csv")

