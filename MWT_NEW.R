## Midwater Trawl Observer DATA
# author: "Savannah Anderson"
# date: "2025-04-07"

#wd
setwd("/Users/savannahanderson/Desktop/wd/masters")
getwd()

#install packages
install.packages(c("ggplot2", "sf", "rnaturalearth", "rnaturalearthdata", "viridis"))
##load libraries
library(readxl)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis) 
library(ggspatial)
library(geosphere)
library(janitor)

# Set your folder path
path <- "~/Desktop/MW TRAWL"

#Load tables (DD)
#DD is a combo of directed DFFE surveys and DD observer program
#so for source must say this is survey
DD_SPCOMP <- read_excel(file.path(path, "DD_SPCOMP.xlsx")) %>% clean_names()
DD_SPECIES <- read_excel(file.path(path, "DD_SPECIES.xlsx")) %>% clean_names()
DD_TRAWL <- read_excel(file.path(path, "DD_TRAWL.xlsx")) %>% clean_names()
DD_TRIPP <- read_excel(file.path(path, "DD_TRIPP.xlsx")) %>% clean_names()

#Load tables (OROP)
#OROP is historical data CAPFISH TRAWL 
#this is observer
OROP_Trawl_LF <- read_excel(file.path(path, "OROP_Trawl_LF.xlsx")) %>% clean_names()
OROP_Trawl_Trawl <- read_excel(file.path(path, "OROP_Trawl_Trawl.xlsx")) %>% clean_names()
OROP_Trip <- read_excel(file.path(path, "OROP_Trip.xlsx")) %>% clean_names()
OROP_zref_Species <- read_excel(file.path(path, "OROP_zref_Species.xlsx")) %>% clean_names()

#perform join 
# Join DD_SPCOMP to DD_SPECIES by species_id
DD_join1 <- DD_SPCOMP %>%
  left_join(DD_SPECIES, by = "species_id")

# Then join to DD_TRAWL by trip_no and trawl_no
DD_join2 <- DD_join1 %>%
  left_join(DD_TRAWL, by = c("trip_no", "trawl_no"))

# Then join to DD_TRIPP by trip_no
DD_final <- DD_join2 %>%
  left_join(DD_TRIPP, by = "trip_no")

length(unique(DD_final$scientific_name))
#58?

#Join tables 
# Join length-frequency to species by spec_id
OROP_join1 <- OROP_Trawl_LF %>%
  left_join(OROP_zref_Species, by = "spec_id")

OROP_join1_fixed <- OROP_join1 %>%
  select(-trawl_id.y, -trawl) %>%     # remove unwanted columns
  rename(trawl_id = trawl_id.x)       # restore expected name

# Join trawl metadata
OROP_join2 <- OROP_join1_fixed %>%
  left_join(OROP_Trawl_Trawl, by = c("dd_trip_id", "trawl_id"))

# Join trip metadata
OROP_final <- OROP_join2 %>%
  left_join(OROP_Trip, by = "dd_trip_id")

###

DD_clean <- DD_final %>%
  mutate(
    source = "DD",
    species_id = as.character(species_id),
    scientific_name = tolower(scientific_name)
  ) %>%
  select(
    source, trip_no, trawl_no, species_id, scientific_name, weight, number, start_lat_deg, start_lat_min,
    start_long_deg, start_long_min,
    everything()
  )

OROP_clean <- OROP_final %>%
  select(-trawl_no) %>%  # Drop any pre-existing trawl_no
  mutate(
    source = "OROP",
    species_id = as.character(spec_id),
    scientific_name = tolower(paste(genus, specie)),
    number = rowSums(select(., starts_with("x")), na.rm = TRUE)
  ) %>%
  rename(
    trip_no = dd_trip_id,
    trawl_no = trawl_id
  ) %>%
  select(
    source, trip_no, trawl_no, species_id, scientific_name, weight, number, start_lat_deg, start_lat_min,
    start_long_deg, start_long_min,
    everything()
  )


#FINAL DATASET
MW_TRAWLdat <- bind_rows(
  DD_clean,
  OROP_clean
)
colnames(MW_TRAWLdat)
head(MW_TRAWLdat)
# Clean out nonteleosts #NOTE TO GO BACK IN AND CHECK THIS AGAIN
unique(MW_TRAWLdat$class)

MW_TRAWLdat <- MW_TRAWLdat %>%
  filter(!class %in% c("Cephalopoda", "Elasmobranchii", "Holocephali"))

# ------------------------------------------------------------------------------
# fix orders and families
unique(MW_TRAWLdat$family) %>% sort()
unique(MW_TRAWLdat$tax_order) %>% sort()
# ------------------------------------------------------------------------------
order_corrections <- tribble(
  ~Family,           ~CorrectedOrder,
  "Bramidae",           "Perciformes",
  "Carangidae",           "Perciformes",
  "Clupeidae",           "Clupeiformes",
  "Emmelichthyidae",           "Perciformes",
  "Engraulidae",           "Clupeiformes",
  "Gempylidae",           "Perciformes",
  "Istiophoridae",           "Perciformes",
  "Lophiidae",           "Lophiiformes",
  "Merlucciidae",           "Gadiformes",
  "Molidae",           "Tetraodontiformes",
  "Myctophidae",           "Myctophiformes",
  "Ophidiidae",           "Ophidiiformes",
  "Scombridae",           "Perciformes",
  "Scorpaenidae",           "Scorpaeniformes",
  "Sparidae",           "Perciformes",
  "Tetraodontidae",           "Tetraodontiformes",
  "Trachichthyidae",           "Beryciformes",
  "Trichiuridae",           "Perciformes",
  "Triglidae",           "Scorpaeniformes",
  "Zeidae",           "Zeiformes"
)
# Apply order_corrections to MWTRAWL
MW_TRAWLdat <- MW_TRAWLdat %>%
  left_join(order_corrections, by = c("family" = "Family")) %>%
  mutate(
    tax_order = if_else(!is.na(CorrectedOrder), CorrectedOrder, tax_order)
  ) %>%
  select(-CorrectedOrder)
# ------------------------------------------------------------------------------
# Clean species names 
# ------------------------------------------------------------------------------
length(unique(MW_TRAWLdat$scientific_name))#55
unique(MW_TRAWLdat$scientific_name) %>% sort()

MW_TRAWLdat_cleaned <- MW_TRAWLdat %>%
  separate(scientific_name, into = c("genus", "species"), sep = " ", extra = "merge", fill = "right")

MW_TRAWLdat_cleaned <- MW_TRAWLdat_cleaned %>%
  mutate(genus = str_to_title(genus))                

MW_TRAWLdat_cleaned <- MW_TRAWLdat_cleaned %>%
  unite("scientific_name", genus, species, sep = " ", remove = FALSE)
unique(MW_TRAWLdat_cleaned$scientific_name) %>% sort()

MW_TRAWLdat_cleaned <- MW_TRAWLdat_cleaned %>%
  mutate(scientific_name = case_when(
    scientific_name == "Belonidae sp na" ~ "Belonidae sp.",
    scientific_name == "Chelidonichthys lastoviza" ~ "Chelidonichthys lastoviza africanus",
    scientific_name == "Engraulis japonicus capensis" ~ "Engraulis japonicus",
    scientific_name == "Emmelichthys nitidus nitidus" ~ "Emmelichthys nitidus",
    scientific_name == "Merluccius NA" ~ "Merluccius sp.",
    scientific_name == "NA NA" ~ "Unidentified",
    scientific_name == "Na na" ~ "Unidentified",
    scientific_name == "Sardinops sagax ocellatus" ~ "Sardinops sagax",
    scientific_name == "Teleostei unknown" ~ "Unidentified",
    scientific_name == "Tetraodontidae NA" ~ "Tetraodontidae sp.",
    scientific_name == "Trigloporus lastoriza africanus" ~ "Chelidonichthys lastoviza",
    scientific_name == ". ." ~ "Unidentified",
    TRUE ~ scientific_name
  ))
#remove 
exclude_species <- c("Holohalaelurus regini","Sepia sp.", "Pyura stolonifera", "Toderopsis eblanae", "Loligo vulgaris reynaudii")

MW_TRAWLdat_cleaned  <- MW_TRAWLdat_cleaned %>%
  filter(!scientific_name %in% exclude_species)

length(unique(MW_TRAWLdat_cleaned$scientific_name)) #45

unmatched_trawl <- MW_TRAWLdat_cleaned %>%
  filter(is.na(family) | is.na(tax_order)) %>%
  distinct(family, tax_order)

#just one record 

#clean column names
colnames(MW_TRAWLdat_cleaned)

MW_TRAWLdat <- MW_TRAWLdat_cleaned %>%
  select(source, trip_no, trawl_no, species_id, scientific_name, genus, weight, number, start_lat_deg, start_lat_min,
         start_long_deg, start_long_min, sample_type, mcm_code, phylum, class, tax_order, family, category, date_start, coast,
         date_end, spec_id, total_catch_observer_kg, total_catch, end_lat_deg, end_lat_min, end_long_deg, end_long_min)

#sort out coords
#deg + min

# Convert start latitude and longitude to decimal degrees
MW_TRAWLdat$start_lat <- -1 * (MW_TRAWLdat$start_lat_deg + MW_TRAWLdat$start_lat_min / 60)
MW_TRAWLdat$start_long <- MW_TRAWLdat$start_long_deg + MW_TRAWLdat$start_long_min / 60

MW_TRAWLdat$end_lat <- -1 * (MW_TRAWLdat$end_lat_deg + MW_TRAWLdat$end_lat_min / 60)
MW_TRAWLdat$end_long <- MW_TRAWLdat$end_long_deg + MW_TRAWLdat$end_long_min / 60

MW_TRAWLdat <- MW_TRAWLdat %>%
  select( -start_lat_deg, -start_lat_min,
          -start_long_deg, -start_long_min,- end_lat_deg, -end_lat_min, -end_long_deg, -end_long_min)
colnames(MW_TRAWLdat)

# ------------------------------------------------------------------------------
#  add tax res column
# ------------------------------------------------------------------------------
#tax res columns
#-------------------------------------------------------------------------------
MW_TRAWLdat <- MW_TRAWLdat %>%
  mutate(
    taxonomic_resolution = case_when(
      is.na(scientific_name) ~ "genus",
      str_detect(genus, "dae") ~ "family",
      str_detect(genus, "formes") ~ "order",
      str_detect(scientific_name, "\\bsp\\.") ~ "genus",
      TRUE ~ "species"
    )
  )
sum(is.na(MW_TRAWLdat$start_lat)) #13
sum(is.na(MW_TRAWLdat$start_long)) #13

#get trip no
MW_TRAWLdat %>%
  filter(is.na(start_lat) | is.na(start_long)) %>%
  distinct(trip_no, start_lat, start_long)

#trip_no start_lat start_long
#     172        NA         NA
#      26        NA         NA
#      30        NA         NA
#      32        NA         NA
#      69        NA         NA

#find trip numbers
#clean
MW_TRAWL <- MW_TRAWLdat

#-------------------------------------------------------------------------------
#PLOTS
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# tax res bar plot 
#-------------------------------------------------------------------------------
taxon_summary <- MW_TRAWL %>%
  count(taxonomic_resolution)

ggplot(taxon_summary, aes(x = reorder(taxonomic_resolution, -n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 3, family = "Times") +
  labs(
    title = "Record Count by Taxonomic Resolution for Mid-Water Trawl Observer Programme",
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
#2 top 20 spp for MWtrawk
#-------------------------------------------------------------------------------
top_species <- MW_TRAWL %>%
  filter(!is.na(scientific_name)) %>%
  count(scientific_name, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_species, aes(x = reorder(scientific_name, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequently Recorded Species in Mid-Water Trawl Observer Programmes",
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
#3 top 20 fam 
#-------------------------------------------------------------------------------
top_fam <- MW_TRAWL %>%
  filter(!is.na(family)) %>%
  count(family, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_fam, aes(x = reorder(family, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequently Recorded Families in Mid-Water Trawl Observer Programmes",
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

#-------------------------------------------------------------------------------
# 4 spatial dist for 
#-------------------------------------------------------------------------------
# Spatial Distribution of Species Records

world <- ne_countries(scale = "medium", returnclass = "sf")
sa_map <- world %>% filter(admin == "South Africa")
eez <- st_read("MAPPING/EEZ/eez_v12.shp", quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(4326)
names(mw_trawl_obs_dat)
ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_point(data = mw_trawl_obs_dat, aes(x = start_long, y = start_lat), 
             color = "navyblue", alpha = 0.3, size = 0.5) +
  coord_sf(xlim = c(10, 35), ylim = c(-40, -25), expand = FALSE) +
  labs(
    title = "Record Distribution of Marine Teleost Occurences in Mid-Water Trawl Observer Programmes",
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

#-------------------------------------------------------------------------------
#OUTPUTS
#-------------------------------------------------------------------------------
#species number
#-------------------------------------------------------------------------------
length(unique(MW_TRAWL$scientific_name)) #45
#family number 
#-------------------------------------------------------------------------------
length(unique(MW_TRAWL$family)) #22
#spatial coverage
#-------------------------------------------------------------------------------
summary(MW_TRAWL$start_lat)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's  
#.  -36.77  -34.76  -34.58  -34.80  -34.41  -29.87      13 
summary(MW_TRAWL$start_long)
#. Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  16.48   24.20   25.09   24.42   25.83   27.46      13 
#-------------------------------------------------------------------------------
names(mw_trawl_obs_dat)

sort(unique(mw_trawl_obs_dat$year), na.last = TRUE)


#COAST COVERAGE
#-------------------------------------------------------------------------------
MW_TRAWL <- MW_TRAWL %>%
  mutate(
    coast = case_when(
      start_long >= 14 & start_long < 20 & start_lat <= -28 & start_lat >= -36.5 ~ "west",
      start_long >= 20 & start_long < 27 & start_lat <= -33 & start_lat >= -36.5 ~ "south",
      start_long >= 27 & start_long <= 35 & start_lat <= -26 & start_lat >= -35 ~ "east",
      TRUE ~ "west"   # force anything else into West
    )
  )

trawl_coast_summary <- MW_TRAWL %>%
  group_by(coast) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(scientific_name),
    .groups = "drop"
  )

print(trawl_coast_summary)
# coast     n_records   n_species
# East      2          2
# South     12254      43
# West       1225        21

#year sorted
MW_TRAWL <- MW_TRAWL %>%
  mutate(year = year(date_start))
MW_TRAWL %>%
  count(year) %>%
  arrange(year)
MW_TRAWL<- MW_TRAWL%>%
  mutate(year_end = year(date_end))
MW_TRAWL %>%
  count(year_end) %>%
  arrange(year_end)
sorted_year<- sort(unique(MW_TRAWL$year), na.last = TRUE)
print(sorted_year)
#2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2016 2017 2018
#2019 2020 2021 2022 2023   NA

#bycoast
ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_point(
    data = MW_TRAWL,
    aes(x = start_long, y = start_lat, color = coast),
    alpha = 0.6, size = 1.5
  ) +
  coord_sf(xlim = c(10, 38), ylim = c(-38, -20), expand = FALSE) +
  labs(
    title = "Distribution of Marine Teleost Occurences from Mid-Water Trawl Observer Programmes",
    x = "Longitude", y = "Latitude", color = "coast"
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
# WRITE
#-------------------------------------------------------------------------------
write.csv(MW_TRAWL, "~/Desktop/wd/masters/RESULTSPLOTS_NOVEMBER/MWtrawl.csv", row.names = FALSE)
MW_TRAWL <- read.csv("outputs/MW_TRAWL.csv")





#SCALE

library(dplyr)
library(lubridate)
library(geosphere)
library(stringr)

degmin_to_dec <- function(deg, min) {
  # assumes deg and min are positive in the sheet; sign should be applied if needed
  # If your longitudes are East positive and latitudes South positive, adjust below.
  ifelse(is.na(deg) | is.na(min), NA_real_, deg + (min/60))
}

safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)

MW_haul <- MW_TRAWLdat %>%
  mutate(
    haul_id = paste(trip_no, trawl_no, sep = "_"),
    
    # best available "start date"
    haul_datetime = coalesce(date_start, date_fishing_start, date_start_haul, net_deployed, net_fishing),
    haul_date = as.Date(haul_datetime),
    
    year = year(haul_date),
    month = month(haul_date),
    
    # coords: convert deg/min to decimal
    start_lat = -degmin_to_dec(start_lat_deg, start_lat_min),
    start_lon =  degmin_to_dec(start_long_deg, start_long_min),
    
    end_lat   = -degmin_to_dec(end_lat_deg, end_lat_min),
    end_lon   =  degmin_to_dec(end_long_deg, end_long_min)
  ) %>%
  group_by(haul_id, source, trip_no, trawl_no) %>%
  summarise(
    haul_date = first(haul_date),
    year = first(year),
    month = first(month),
    
    start_lat = first(start_lat),
    start_lon = first(start_lon),
    end_lat   = first(end_lat),
    end_lon   = first(end_lon),
    
    # depth metrics (haul-level)
    depth_start = first(depth_start),
    depth_end   = first(depth_end),
    footrope_min = first(footrope_depth_min),
    footrope_max = first(footrope_depth_max),
    
    trawl_speed_kn = first(trawl_speed_kn),
    vertical_opening_m = first(vertical_opening_m),
    
    # optional: haul effort proxies
    n_species_records = n(),
    total_weight = sum(weight, na.rm = TRUE),
    total_number = sum(number, na.rm = TRUE),
    
    .groups = "drop"
  )
MW_temporal <- MW_haul %>%
  summarise(
    n_hauls = n(),
    n_with_date = sum(!is.na(haul_date)),
    first_year = min(year, na.rm = TRUE),
    last_year  = max(year, na.rm = TRUE),
    n_years    = n_distinct(year),
    start_date = min(haul_date, na.rm = TRUE),
    end_date   = max(haul_date, na.rm = TRUE),
    n_days_sampled = n_distinct(haul_date),
    n_months_sampled = n_distinct(paste(year, month))
  )

MW_temporal


MW_season <- MW_haul %>%
  mutate(
    season = case_when(
      month %in% c(12,1,2) ~ "Summer",
      month %in% c(3,4,5)  ~ "Autumn",
      month %in% c(6,7,8)  ~ "Winter",
      month %in% c(9,10,11)~ "Spring",
      TRUE ~ NA_character_
    )
  ) %>%
  count(season)
MW_season

MW_annual <- MW_haul %>%
  count(year) %>%
  arrange(year)

MW_annual %>%
  summarise(
    mean_hauls_per_year = mean(n, na.rm = TRUE),
    median_hauls_per_year = median(n, na.rm = TRUE),
    min_hauls = min(n, na.rm = TRUE),
    max_hauls = max(n, na.rm = TRUE)
  )

MW_depth <- MW_haul %>%
  mutate(
    haul_depth_m = case_when(
      !is.na(footrope_min) & !is.na(footrope_max) ~ (footrope_min + footrope_max) / 2,
      !is.na(depth_start) & !is.na(depth_end)     ~ (depth_start + depth_end) / 2,
      !is.na(depth_start)                         ~ depth_start,
      !is.na(depth_end)                           ~ depth_end,
      TRUE ~ NA_real_
    ),
    depth_range_m = case_when(
      !is.na(footrope_min) & !is.na(footrope_max) ~ (footrope_max - footrope_min),
      !is.na(depth_start) & !is.na(depth_end)     ~ abs(depth_end - depth_start),
      TRUE ~ NA_real_
    )
  )

MW_depth %>%
  summarise(
    n_hauls = n(),
    n_with_depth = sum(!is.na(haul_depth_m)),
    mean_depth_m = mean(haul_depth_m, na.rm = TRUE),
    median_depth_m = median(haul_depth_m, na.rm = TRUE),
    sd_depth_m = sd(haul_depth_m, na.rm = TRUE),
    min_depth_m = min(haul_depth_m, na.rm = TRUE),
    max_depth_m = max(haul_depth_m, na.rm = TRUE),
    q25 = quantile(haul_depth_m, 0.25, na.rm = TRUE),
    q75 = quantile(haul_depth_m, 0.75, na.rm = TRUE)
  )


MW_depth2 %>%
  summarise(
    footrope_min_m = safe_min(footrope_min),
    footrope_max_m = safe_max(footrope_max),
    median_depth_range_m = median(depth_range_m, na.rm = TRUE),
    q25_depth_range_m = quantile(depth_range_m, 0.25, na.rm = TRUE),
    q75_depth_range_m = quantile(depth_range_m, 0.75, na.rm = TRUE)
  )

MW_space <- MW_depth %>%
  mutate(
    has_coords = !is.na(start_lon) & !is.na(start_lat) & !is.na(end_lon) & !is.na(end_lat),
    haul_extent_km = ifelse(
      has_coords,
      geosphere::distHaversine(cbind(start_lon, start_lat), cbind(end_lon, end_lat)) / 1000,
      NA_real_
    )
  )

# trim insane values using 99.5th percentile
cap_995 <- quantile(MW_space$haul_extent_km, 0.995, na.rm = TRUE)

MW_space_trim <- MW_space %>%
  mutate(
    haul_extent_km_raw = haul_extent_km,
    haul_extent_km = ifelse(!is.na(haul_extent_km) & haul_extent_km <= cap_995, haul_extent_km, NA_real_)
  )

MW_space_trim %>%
  summarise(
    n_hauls = n(),
    n_with_extent = sum(!is.na(haul_extent_km)),
    mean_extent_km = mean(haul_extent_km, na.rm = TRUE),
    median_extent_km = median(haul_extent_km, na.rm = TRUE),
    sd_extent_km = sd(haul_extent_km, na.rm = TRUE),
    min_extent_km = min(haul_extent_km, na.rm = TRUE),
    max_extent_km = max(haul_extent_km, na.rm = TRUE),
    q25 = quantile(haul_extent_km, 0.25, na.rm = TRUE),
    q75 = quantile(haul_extent_km, 0.75, na.rm = TRUE)
  )

MW_steps <- MW_haul %>%
  mutate(
    lat = start_lat,
    lon = start_lon
  ) %>%
  filter(!is.na(lat), !is.na(lon), !is.na(haul_date)) %>%
  arrange(trip_no, haul_date, trawl_no) %>%
  group_by(trip_no) %>%
  mutate(
    step_km = geosphere::distHaversine(cbind(lon, lat), cbind(lag(lon), lag(lat))) / 1000
  ) %>%
  ungroup() %>%
  filter(!is.na(step_km), step_km > 0, step_km < 500)

MW_steps %>%
  summarise(
    n_steps = n(),
    median_step_km = median(step_km),
    q25 = quantile(step_km, 0.25),
    q75 = quantile(step_km, 0.75),
    mean_step_km = mean(step_km),
    max_step_km = max(step_km)
  )

MW_steps <- MW_haul %>%
  mutate(
    lat = start_lat,
    lon = start_lon
  ) %>%
  filter(!is.na(lat), !is.na(lon), !is.na(haul_date)) %>%
  arrange(trip_no, haul_date, trawl_no) %>%
  group_by(trip_no) %>%
  mutate(
    step_km = geosphere::distHaversine(cbind(lon, lat), cbind(lag(lon), lag(lat))) / 1000
  ) %>%
  ungroup() %>%
  filter(!is.na(step_km), step_km > 0, step_km < 500)

MW_steps %>%
  summarise(
    n_steps = n(),
    median_step_km = median(step_km),
    q25 = quantile(step_km, 0.25),
    q75 = quantile(step_km, 0.75),
    mean_step_km = mean(step_km),
    max_step_km = max(step_km)
  )

MW_summary <- MW_space_trim %>%
  summarise(
    dataset = "MW_TRAWL",
    n_hauls = n(),
    start_year = min(year, na.rm = TRUE),
    end_year = max(year, na.rm = TRUE),
    
    median_extent_km = median(haul_extent_km, na.rm = TRUE),
    q25_extent_km = quantile(haul_extent_km, 0.25, na.rm = TRUE),
    q75_extent_km = quantile(haul_extent_km, 0.75, na.rm = TRUE),
    
    median_depth_m = median(haul_depth_m, na.rm = TRUE),
    q25_depth_m = quantile(haul_depth_m, 0.25, na.rm = TRUE),
    q75_depth_m = quantile(haul_depth_m, 0.75, na.rm = TRUE)
  )

MW_summary

MW_depth2 <- MW_depth %>%
  mutate(footrope_min = na_if(footrope_min, 0))

MW_space_trim %>%
  filter(!is.na(haul_extent_km), haul_extent_km > 0) %>%
  summarise(median = median(haul_extent_km), q25 = quantile(haul_extent_km, .25), q75 = quantile(haul_extent_km, .75))


