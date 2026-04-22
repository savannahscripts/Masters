setwd("/Users/savannahanderson/Desktop/wd/masters")
getwd()

# author: "Savannah Anderson"
# date: "2025-10-15

#DFFE TRAWL
##install packages
install.packages("tidyverse")
install.packages("readxl")

##load libraries
library(RSQLite)
library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(worrms)
library(stringr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

setwd("/Users/savannahanderson/Desktop/wd/masters")
# Set your folder path
path <- "~/Desktop/DEMTRAWL"

# Load each table
trawl <- read_excel(file.path(path, "trawl.xlsx")) %>% clean_names()
cruise <- read_excel(file.path(path, "cruise.xlsx")) %>% clean_names()
catch <- read_excel(file.path(path, "catch.xlsx")) %>% clean_names()
worms <- read_excel(file.path(path, "WoRMS match.xlsx")) %>% clean_names()
ref_species <- read_excel(file.path(path, "REF_species.xlsx")) %>% clean_names()
ref_trawl <- read_excel(file.path(path, "REF_trawl_no.xlsx")) %>% clean_names()
ref_grid <- read_excel(file.path(path, "REF_grid.xlsx")) %>% clean_names()

#load REF grid table as well #join by coast to separate south and west coast
#join using ref grid on grid ID #gridID gives option of coast old and coast new

#check column names
colnames(catch)
colnames(cruise)
colnames(ref_species)
colnames(ref_trawl)
colnames(trawl)

colnames(worms)
names(ref_grid)

#join tables (Catch by trawl)
catch_joined <- catch %>%
  left_join(ref_species, by = "mnemonic")
catch_trawl_joined <- catch_joined %>%
  left_join(trawl, by = c("cruise", "station"))
ref_grid_dedup <- ref_grid %>%
  distinct(grid, .keep_all = TRUE)
catch_full <- catch_trawl_joined %>%
  left_join(ref_grid_dedup, by = "grid")
#keeping ones with cords (using start)
catch_with_coords <- catch_full%>%
  filter(!is.na(start_latitude) & !is.na(start_longitude))
demtrawldata <- catch_with_coords
#CLEANING
demtrawldat <- janitor::clean_names(demtrawldata)

names(demtrawldat)
# Rename columns
demtrawldat <- demtrawldat %>%
  rename(
    trawl_id = id_x,                          # Original trawl record ID
    trawl_ref_id = id_y,                      
    scientific_name_ref = scientific_name,    # From REF_species
    lat = start_latitude,
    lon = start_longitude
  )
# pull out columns 
demtrawldat <- demtrawldat %>%
  select(
    trawl_id, trawl_no, cruise, station, year, grid, depth_range = mean_depth_bottom_m,
    lat, lon, start_lat_deg, start_long_deg, end_lat_deg, end_long_deg,
    mnemonic, scientific_name_ref, genus, family, aphia_id, no_species, class, species_name, common_name
  )
# ------------------------------------------------------------------------------
#CLEANING
unique(demtrawldat$class)
#before 255430
demtrawldat <- demtrawldat %>%
  filter(!(class %in% c("Cephalopoda", "Holocephali", "Elasmobranchii", "Malacostraca", "Anthozoa")))
#after 219690
# ------------------------------------------------------------------------------
# families and orders
# ------------------------------------------------------------------------------
family_fix <- c(
  "Coracinidae" = "Sparidae",       
  "Centracanthidae" = "Sparidae",
  "Idiacanthidae" = "Stomiidae",
  "Melanostomiidae" = "Stomiidae",
  "Photichthyidae" = "Phosichthyidae"
)
demtrawldat <- demtrawldat %>%
  mutate(family = recode(family, !!!family_fix))
# ------------------------------------------------------------------------------
# fix orders and families
unique(demtrawldat$family) %>% sort() #64
# ------------------------------------------------------------------------------
order_corrections <- tribble(
  ~Family,           ~CorrectedOrder,
  "Alepocephalidae",           "Argentiniformes",
  "Balistidae",           "Tetraodontiformes",
  "Berycidae",           "Beryciformes",
  "Blenniidae",           "Perciformes",
  "Bramidae",           "Perciformes",
  "Carangidae",           "Perciformes",
  "Centriscidae",           "Gasterosteiformes",
  "Cheilodactylidae",           "Perciformes",
  "Clupeidae",           "Clupeiformes",
  "Congridae",           "Anguilliformes",
  "Cynoglossidae",           "Pleuronectiformes",
  "Diretmidae",            "Trachichthyiformes",
  "Emmelichthyidae",           "Perciformes",
  "Engraulidae",           "Clupeiformes",
  "Evermannellidae",           "Aulopiformes",
  "Gempylidae",           "Perciformes",
  "Gerreidae",           "Perciformes",
  "Gobiidae",           "Perciformes",
  "Gonorynchidae",           "Gonorynchiformes",
  "Haemulidae",           "Perciformes",
  "Himantolophidae",           "Lophiiformes",
  "Linophrynidae",            "Lophiiformes",
  "Liparididae",           "Perciformes",
  "Lophiidae",           "Lophiiformes",
  "Macrouridae",           "Gadiformes",
  "Merlucciidae",           "Gadiformes",
  "Molidae",           "Tetraodontiformes",
  "Monacanthidae",           "Tetraodontiformes",
  "Myctophidae",           "Myctophiformes",
  "Neoscopelidae",           "Myctophiformes",
  "Nomeidae",           "Perciformes",
  "Ogcocephalidae",           "Lophiiformes",
  "Ophidiidae",           "Ophidiiformes",
  "Oplegnathidae",           "Perciformes",
  "Paralepididae",           "Aulopiformes",
  "Parascorpididae",           "Perciformes",
  "Phosichthyidae",         "Stomiiformes",
  "Polyprionidae",           "Perciformes",
  "Psychrolutidae",           "Perciformes",
  "Scomberesocidae",           "Beloniformes",
  "Sciaenidae",           "Perciformes",
  "Scombridae",           "Perciformes",
  "Scorpaenidae",    "Scorpaeniformes",
  "Serranidae",    "Perciformes",
  "Soleidae",           "Pleuronectiformes",
  "Sparidae",           "Perciformes",
  "Sphyraenidae",         "Perciformes",
  "Sternoptychidae",          "Stomiiformes",
  "Stomiidae",         "Stomiiformes",
  "Stromateidae",    "Perciformes",
  "Synodontidae",           "Aulopiformes",
  "Tetraodontidae",           "Tetraodontiformes",
  "Trachichthyidae",           "Beryciformes",
  "Trachipteridae",           "Lampriformes",
  "Trichiuridae",           "Perciformes",
  "Triglidae",           "Scorpaeniformes",
  "Uranoscopidae",           "Perciformes",
  "Xiphiidae",           "Perciformes",
  "Zeidae",           "Zeiformes",
  "Zoarcidae",           "Perciformes"
)
#initialise order column and fill in order for each of these families
demtrawldat <- demtrawldat %>%
  left_join(
    order_corrections %>% rename(family = Family),
    by = "family"
  ) %>%
  rename(order = CorrectedOrder)

unique(demtrawldat$order)

#view species in order "NA"
demtrawldat %>%
  filter(is.na(order)) %>%
  select(family, genus, scientific_name_ref) %>%
  distinct() %>%
  arrange(family, scientific_name_ref)
  
#15 species not assigned to an order yet
     
#Beryx          Beryx decadactylus  = Berycidae = Beryciformes   
#Gaidropsarus   Gaidropsarus capensis  Gadiformes, Gaidropsaridae 
# Macroparalepis Macroparalepis affinis Family = Paralepididae   Order = Aulopiformes 
#Cookeolus      Cookeolus boops change to Cookeolus japonicus Family = Priacanthidae, Order = Perciformes           
#Ebinania       Ebinania costaecanarie rename Ebinania costaecanariae Family = Psychrolutidae, Order = Perciformes
#Halieutaea     Halieutaea spicata Change to Halieutaea indica Order = Lophiiformes Family = Ogcocephalidae     
# Neoscopelus    Neoscopelus spp.    rename to Neoscopelus sp.   Order = Myctophiformes, Family = Neoscopelidae 
# Rhechias       Rhechias wallacei rename to Bathycongrus wallacei Family Congridae, Order = Anguilliformes 
#Trachyrhampus  Trachyrhampus bioarctatus rename to Trachyrhamphus bicoarctatus Syngnathiformes (Order) Syngnathidae (Family)

#change following species names
# Diretmoides parini  = change to Diretmichthys parini   
# Idiacanthus fasiola = change to Idiacanthus fasciola   
# Bathophilus longipinnus, change to =  Bathophilus longipinnis 
# Linophryne denisiramus  = change to Linophryne densiramus                                                                              
#  Lycodes agulhensis  change to Lycodes terraenovae, 
# Coryphaenoides dossinus change to Coryphaenoides dossenus   
# Coelorinchus change to Coelorinchus sp. 
# Kuronezumia (ex Nezumia) leonis change to Kuronezumia leonis 

demtrawldat <- demtrawldat %>%
  filter(!scientific_name_ref %in% c(
    "Teleostei demersal", "Teleostei linefish", "Teleostei pelagic",
    "Unid Fish", NA
  ))

demtrawldat <- demtrawldat %>%
  mutate(scientific_name_ref = case_when(
    scientific_name_ref == "Cookeolus boops" ~ "Cookeolus japonicus",
    scientific_name_ref == "Ebinania costaecanarie" ~ "Ebinania costaecanariae",
    scientific_name_ref == "Halieutaea spicata" ~ "Halieutaea indica",
    scientific_name_ref == "Neoscopelus spp." ~ "Neoscopelus sp.",
    scientific_name_ref == "Rhechias wallacei" ~ "Bathycongrus wallacei",
    scientific_name_ref == "Trachyrhampus bioarctatus" ~ "Trachyrhamphus bicoarctatus",
    scientific_name_ref == "Diretmoides parini" ~ "Diretmichthys parini",
    scientific_name_ref == "Idiacanthus fasiola" ~ "Idiacanthus fasciola",
    scientific_name_ref == "Bathophilus longipinnus" ~ "Bathophilus longipinnis",
    scientific_name_ref == "Linophryne denisiramus" ~ "Linophryne densiramus",
    scientific_name_ref == "Lycodes agulhensis" ~ "Lycodes terraenovae",
    scientific_name_ref == "Coryphaenoides dossinus" ~ "Coryphaenoides dossenus",
    scientific_name_ref == "Coelorinchus" ~ "Coelorinchus sp.",
    scientific_name_ref == "Kuronezumia (ex Nezumia) leonis" ~ "Kuronezumia leonis",
    TRUE ~ scientific_name_ref
  ))

family_order_updates <- tibble::tribble(
  ~scientific_name_ref,           ~family,           ~order,
  "Beryx decadactylus",           "Berycidae",       "Beryciformes",
  "Cookeolus japonicus",          "Priacanthidae",   "Perciformes",
  "Ebinania costaecanariae",      "Psychrolutidae",  "Perciformes",
  "Gaidropsarus capensis",        "Gadidae",         "Gadiformes",
  "Halieutaea indica",            "Ogcocephalidae",  "Lophiiformes",
  "Himantolophus appelii",        "Himantolophidae", "Lophiiformes",
  "Macroparalepis affinis",       "Paralepididae",   "Aulopiformes",
  "Neoscopelus sp.",              "Neoscopelidae",   "Myctophiformes",
  "Bathycongrus wallacei",        "Congridae",       "Anguilliformes",
  "Trachyrhamphus bicoarctatus",  "Syngnathidae",    "Syngnathiformes",
  "Myxine capensis",              "Myxinidae",       "Myxiniformes",
  "Petalichthys capensis",        "Monacanthidae",   "Tetraodontiformes",
  "Zenion hololepis",             "Zeniontidae",     "Zeiformes"
)
                                              
demtrawldat <- demtrawldat %>%
  left_join(family_order_updates, by = "scientific_name_ref") %>%
  mutate(
    family = coalesce(family.x, family.y),
    order  = coalesce(order.x, order.y)
  ) %>%
  select(-family.x, -family.y, -order.x, -order.y)

unmatched_trawl <- demtrawldat %>%
  filter(is.na(family) | is.na(order)) %>%
  distinct(family, order)
#okay no 0 records have no order and no family
length(unique(demtrawldat$scientific_name_ref)) #144

unique(demtrawldat$scientific_name_ref) %>% sort()
#finally clean species names 

demtrawldat <- demtrawldat %>%
mutate(scientific_name_ref = case_when(
  scientific_name_ref == "Chelidonichthys" ~ "Chelidonichthys sp.",
  scientific_name_ref == "Balistidae" ~ "Balistidae sp.",
  scientific_name_ref == "Gerres" ~ "Gerres sp.",
  scientific_name_ref == "Diplodus cervinus hottentotus" ~ "Diplodus cervinus",
  scientific_name_ref == "Diplodus sargus capensis" ~ "Diplodus capensis",
  scientific_name_ref == "Monacanthidae" ~ "Monacanthidae sp.",
  scientific_name_ref == "Pagellus bellottii natalensis" ~ "Pagellus natalensis",
  scientific_name_ref == "Sphyraena" ~ "Sphyraena sp.",
  scientific_name_ref == "Synodontidae" ~ "Synodontidae sp.",
  scientific_name_ref == "Epinephelus" ~ "Epinephelus sp.",
  scientific_name_ref == "Tetraodontidae" ~ "Tetraodontidae sp.",
  scientific_name_ref == "Scomberesox saurus scombroides" ~ "Scomberesox saurus",
  scientific_name_ref == "Halieutopsis microps" ~ "Coelophrys micropus",
  TRUE ~ scientific_name_ref
))

length(unique(demtrawldat$scientific_name_ref)) #144     

colnames(demtrawldat)

demtrawldat <- demtrawldat %>%
 select(-cruise, -start_lat_deg, -start_long_deg, -end_lat_deg, -end_long_deg, -no_species, -mnemonic, -aphia_id, -common_name)

#-------------------------------------------------------------------------------
#check for NAs in lat and lon
#-------------------------------------------------------------------------------
sum(is.na(demtrawldat$lat)) #0
sum(is.na(demtrawldat$lon)) #0

#tax res columns
#-------------------------------------------------------------------------------
demtrawldat <- demtrawldat %>%
  mutate(
    taxonomic_resolution = case_when(
      is.na(scientific_name_ref) ~ "genus",
      str_detect(genus, "dae") ~ "family",
      str_detect(genus, "formes") ~ "order",
      str_detect(scientific_name_ref, "\\bsp\\.") ~ "genus",
      TRUE ~ "species"
    )
  )
#-------------------------------------------------------------------------------
#PLOTS
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# tax res bar plot 
#-------------------------------------------------------------------------------
taxon_summary <- demtrawldat %>%
  count(taxonomic_resolution)

ggplot(taxon_summary, aes(x = reorder(taxonomic_resolution, -n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 3, family = "Times") +
  labs(
    title = "Record Count by Taxonomic Resolution Demersal Trawl Surveys",
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
#2 top 20 spp for demtrawl
#-------------------------------------------------------------------------------
top_species <- demtrawldat %>%
  filter(!is.na(scientific_name_ref)) %>%
  count(scientific_name_ref, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_species, aes(x = reorder(scientific_name_ref, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequently Recorded Species in Demersal Trawl Surveys",
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
top_fam <- demtrawldat %>%
  filter(!is.na(family)) %>%
  count(family, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_fam, aes(x = reorder(family, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequently Recorded Families in Demersal Trawl Surveys",
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

ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_point(data = trawl_survey_dat, aes(x = lon, y = lat), 
             color = "navyblue", alpha = 0.3, size = 0.5) +
  coord_sf(xlim = c(10, 35), ylim = c(-40, -25), expand = FALSE) +
  labs(
    title = "Record Distribution of Demersal Trawl Surveys",
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
# WRITE
#-------------------------------------------------------------------------------
write.csv(demtrawldat, "~/Desktop/wd/masters/RESULTSPLOTS_NOVEMBER/DEMTRAWLDAT.csv", row.names = FALSE)
demtrawldat <- read.csv("outputs/DEMTRAWLDAT.csv")

#-------------------------------------------------------------------------------
#OUTPUTS
#-------------------------------------------------------------------------------
#species number
#-------------------------------------------------------------------------------
length(unique(demtrawldat$scientific_name_ref)) #144
#family number 
#-------------------------------------------------------------------------------
length(unique(demtrawldat$family)) #63
#spatial coverage
#-------------------------------------------------------------------------------
summary(demtrawldat$lat)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-36.91  -34.83  -34.07  -33.24  -31.55  -17.31 
summary(demtrawldat$lon)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#11.31   16.89   18.71   19.47   21.62   49.82 
#-------------------------------------------------------------------------------
#COAST COVERAGE
#-------------------------------------------------------------------------------
demtrawldat <- demtrawldat %>%
  mutate(
    coast = case_when(
      lon >= 14 & lon < 20 & lat <= -28 & lat >= -36.5 ~ "west",
      lon >= 20 & lon < 27 & lat <= -33 & lat >= -36.5 ~ "south",
      lon >= 27 & lon <= 35 & lat <= -26 & lat >= -35 ~ "east",
      TRUE ~ "west"   # force anything else into West
    )
  )

trawl_coast_summary <- demtrawldat %>%
  group_by(coast) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(scientific_name_ref),
    .groups = "drop"
  )

print(trawl_coast_summary)
# coast     n_records   n_species
# East      11          11
# South     42830       114
# West      58915       99

#year sorted
sorted_year<- sort(unique(demtrawldat$year), na.last = TRUE)
print(sorted_year)
#1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1999 2000
#2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017
#2019 2020 2021 2022 2023 2024

#bycoast
ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_point(
    data = demtrawldat,
    aes(x = lon, y = lat, color = coast),
    alpha = 0.6, size = 1.5
  ) +
  coord_sf(xlim = c(10, 38), ylim = c(-38, -20), expand = FALSE) +
  labs(
    title = "Coastal Distribution of Marine Teleost Occurences from Demersal Trawl Surveys",
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




#SCALE
dem_trawl <- trawl %>%
  mutate(
    station_type_l = str_to_lower(coalesce(station_type, "")),
    gear_type_l    = str_to_lower(coalesce(gear_type, ""))
  ) %>%
  filter(
    str_detect(station_type_l, "^dem") | str_detect(gear_type_l, "^dem")
  )

dem_trawl %>% count(station_type, gear_type, sort = TRUE)

dem_trawl_valid <- dem_trawl %>%
  filter(!str_detect(station_type_l, "aborted|failed"))

library(geosphere)

dem_trawl_scale <- dem_trawl %>%
  mutate(
    # 1) best tow distance available (nm -> km)
    tow_distance_nm = coalesce(distance_nm_best, distance_measured_nm, distance_calculated_nm),
    tow_distance_km = tow_distance_nm * 1.852,
    
    # 2) great-circle start-end distance (km) as fallback / QC
    has_coords = !is.na(start_longitude) & !is.na(start_latitude) &
      !is.na(end_longitude)   & !is.na(end_latitude),
    
    gc_dist_km = ifelse(
      has_coords,
      distHaversine(
        cbind(start_longitude, start_latitude),
        cbind(end_longitude, end_latitude)
      ) / 1000,
      NA_real_
    ),
    
    # 3) your primary "spatial scale" metric per haul
    haul_extent_km = coalesce(tow_distance_km, gc_dist_km),
    
    # 4) optional footprint metric
    area_swept_km2 = area_swept_nm2 * (1.852^2)
  )

dem_trawl_scale %>%
  summarise(
    n = n(),
    n_dist = sum(!is.na(tow_distance_km)),
    n_gc   = sum(!is.na(gc_dist_km)),
    max_tow_km = max(tow_distance_km, na.rm = TRUE),
    max_gc_km  = max(gc_dist_km, na.rm = TRUE)
  )


dem_trawl_scale %>%
  filter(haul_extent_km > 100) %>%   # threshold you can tune (50/100/200)
  select(join_id, cruise, station, trawl_no, station_type, gear_type,
         tow_distance_nm, tow_distance_km, gc_dist_km, haul_extent_km,
         start_latitude, start_longitude, end_latitude, end_longitude) %>%
  arrange(desc(haul_extent_km)) %>%
  head(50)

dem_trawl_scale <- dem_trawl_scale %>%
  mutate(
    tow_distance_km_plaus = ifelse(!is.na(tow_distance_km) & tow_distance_km > 0 & tow_distance_km <= 100,
                                   tow_distance_km, NA_real_),
    haul_extent_km = coalesce(tow_distance_km_plaus, gc_dist_km)
  )

MAX_TOW_KM <- 100   # start here; you can tighten to 50 after checking
MAX_GC_KM  <- 100

dem_trawl_scale <- dem_trawl %>%
  mutate(
    tow_distance_nm = coalesce(distance_nm_best, distance_measured_nm, distance_calculated_nm),
    tow_distance_km = tow_distance_nm * 1.852,
    
    has_coords = !is.na(start_longitude) & !is.na(start_latitude) &
      !is.na(end_longitude)   & !is.na(end_latitude),
    
    gc_dist_km = ifelse(
      has_coords,
      geosphere::distHaversine(
        cbind(start_longitude, start_latitude),
        cbind(end_longitude, end_latitude)
      ) / 1000,
      NA_real_
    ),
    
    # plausibility screens
    tow_km_plaus = ifelse(!is.na(tow_distance_km) & tow_distance_km > 0 & tow_distance_km <= MAX_TOW_KM,
                          tow_distance_km, NA_real_),
    
    gc_km_plaus  = ifelse(!is.na(gc_dist_km) & gc_dist_km > 0 & gc_dist_km <= MAX_GC_KM,
                          gc_dist_km, NA_real_),
    
    # final spatial scale per haul
    haul_extent_km = coalesce(tow_km_plaus, gc_km_plaus),
    
    area_swept_km2 = area_swept_nm2 * (1.852^2)
  )

dem_trawl_scale %>%
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

dem_trawl_scale %>%
  summarise(
    n_gt10 = sum(haul_extent_km > 10, na.rm = TRUE),
    n_gt20 = sum(haul_extent_km > 20, na.rm = TRUE),
    n_gt50 = sum(haul_extent_km > 50, na.rm = TRUE),
    max_extent = max(haul_extent_km, na.rm = TRUE)
  )

cap_995 <- quantile(dem_trawl_scale$haul_extent_km, 0.995, na.rm = TRUE)

cap_995

dem_trawl_scale <- dem_trawl_scale %>%
  mutate(
    haul_extent_km = ifelse(!is.na(haul_extent_km) & haul_extent_km <= cap_995, haul_extent_km, NA_real_)
  )


dem_trawl_scale %>%
  group_by(station_type) %>%
  summarise(
    n = n(),
    median_km = median(haul_extent_km, na.rm = TRUE),
    q25 = quantile(haul_extent_km, 0.25, na.rm = TRUE),
    q75 = quantile(haul_extent_km, 0.75, na.rm = TRUE),
    max_km = max(haul_extent_km, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n))


safe_max <- function(x) {
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

dem_trawl_scale %>%
  group_by(station_type) %>%
  summarise(
    n = n(),
    median_km = median(haul_extent_km, na.rm = TRUE),
    q25 = quantile(haul_extent_km, 0.25, na.rm = TRUE),
    q75 = quantile(haul_extent_km, 0.75, na.rm = TRUE),
    max_km = safe_max(haul_extent_km),
    n_with_extent = sum(!is.na(haul_extent_km)),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

dem_trawl_scale_core <- dem_trawl_scale %>%
  filter(
    !is.na(station_type),
    !str_detect(station_type_l, "aborted|ctd"),
    !str_detect(station_type_l, "failed")  # optional; keep if you want
  )

dem_trawl_scale_core %>%
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

cap_995 <- 10.26757

dem_trawl_scale <- dem_trawl_scale %>%
  mutate(
    haul_extent_km_raw = haul_extent_km,
    haul_extent_km = ifelse(!is.na(haul_extent_km) & haul_extent_km <= cap_995, haul_extent_km, NA_real_)
  )

dem_trawl_scale_core %>%
  filter(station_type %in% c("Dem_Abundance", "Experimental")) %>%
  group_by(station_type) %>%
  summarise(
    n = n(),
    n_with_extent = sum(!is.na(haul_extent_km)),
    median_km = median(haul_extent_km, na.rm = TRUE),
    q25 = quantile(haul_extent_km, 0.25, na.rm = TRUE),
    q75 = quantile(haul_extent_km, 0.75, na.rm = TRUE),
    max_km = safe_max(haul_extent_km),
    .groups = "drop"
  )

dem_trawl_depth <- dem_trawl_scale_core %>%
  mutate(
    haul_depth_m = coalesce(
      mean_depth_bottom_m,
      (start_depth_bottom_m + end_depth_bottom_m) / 2
    )
  )

dem_trawl_depth %>%
  summarise(
    min_depth = min(haul_depth_m, na.rm = TRUE),
    max_depth = max(haul_depth_m, na.rm = TRUE),
    q01 = quantile(haul_depth_m, 0.01, na.rm = TRUE),
    q99 = quantile(haul_depth_m, 0.99, na.rm = TRUE)
  )

dem_trawl_depth <- dem_trawl_depth %>%
  mutate(
    haul_depth_m = ifelse(
      !is.na(haul_depth_m) & haul_depth_m > 0 & haul_depth_m <= 1500,
      haul_depth_m,
      NA_real_
    )
  )

dem_trawl_depth %>%
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

dem_trawl_depth %>%
  filter(station_type %in% c("Dem_Abundance", "Experimental")) %>%
  group_by(station_type) %>%
  summarise(
    n = n(),
    n_with_depth = sum(!is.na(haul_depth_m)),
    median_depth_m = median(haul_depth_m, na.rm = TRUE),
    q25 = quantile(haul_depth_m, 0.25, na.rm = TRUE),
    q75 = quantile(haul_depth_m, 0.75, na.rm = TRUE),
    min_depth_m = min(haul_depth_m, na.rm = TRUE),
    max_depth_m = max(haul_depth_m, na.rm = TRUE),
    .groups = "drop"
  )

dem_trawl_time <- dem_trawl_scale_core %>%
  mutate(
    haul_date = as.Date(paste(year, month, day, sep = "-"))
  )

dem_trawl_time %>%
  summarise(
    n_hauls = n(),
    first_year = min(year, na.rm = TRUE),
    last_year  = max(year, na.rm = TRUE),
    n_years    = n_distinct(year),
    start_date = min(haul_date, na.rm = TRUE),
    end_date   = max(haul_date, na.rm = TRUE)
  )

yearly_trawl <- dem_trawl_time %>%
  count(year) %>%
  arrange(year)

yearly_trawl


yearly_trawl %>%
  summarise(
    mean_hauls_per_year = mean(n),
    median_hauls_per_year = median(n),
    min_hauls = min(n),
    max_hauls = max(n)
  )

monthly_trawl <- dem_trawl_time %>%
  count(month) %>%
  arrange(month)

monthly_trawl

dem_trawl_time <- dem_trawl_time %>%
  mutate(
    season = case_when(
      month %in% c(12, 1, 2)  ~ "Summer",
      month %in% c(3, 4, 5)   ~ "Autumn",
      month %in% c(6, 7, 8)   ~ "Winter",
      month %in% c(9, 10, 11) ~ "Spring"
    )
  )

seasonal_trawl <- dem_trawl_time %>%
  count(season)

seasonal_trawl


dem_trawl_time %>%
  summarise(
    n_days_sampled = n_distinct(haul_date),
    n_months_sampled = n_distinct(paste(year, month)),
    mean_hauls_per_day = n() / n_days_sampled
  )
