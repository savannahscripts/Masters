# author: "Savannah Anderson"
#install packages
install.packages(c("ggplot2", "sf", "rnaturalearth", "rnaturalearthdata", "viridis", "stringdist"))
##load libraries
library(readxl)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis) 
library(ggspatial)
library(dplyr)
library(writexl)
library(lubridate)
library(purrr)
library(tidyr)
library(stringr)
library(scales)
library(readr)
library(janitor)
library(stringdist)
library(tidyverse)


#from internal source cleaning = this script establishes merge and standardised framework of taxonomy
getwd()
setwd("/Users/savannahanderson/Desktop/wd/masters")
#-------------------------------------------------------------------------------
#read in all data sets 
#-------------------------------------------------------------------------------
#NMLS
line_dat <- read.csv("RESULTSPLOTS_NOVEMBER/NMLS.csv") 
#LIT DAT
lit_dat <- read.csv("RESULTSPLOTS_NOVEMBER/litdatfinal.csv") 
#DEM TRAWL
trawl_survey_dat <- read.csv("RESULTSPLOTS_NOVEMBER/DEMTRAWLDAT.csv") 
#MWTRWAWL
mw_trawl_obs_dat <- read.csv("RESULTSPLOTS_NOVEMBER/MWtrawl.csv") 
#GBIF INAT
inat_dat <- read.csv("RESULTSPLOTS_NOVEMBER/GBIFinatdat.csv") 
#GBIF MUSEUM
museum_ps_dat <- read.csv("RESULTSPLOTS_NOVEMBER/GBIFmuseumdat.csv")
#CAPFISH OBS
capfish_obs_dat <- read.csv("RESULTSPLOTS_NOVEMBER/capfish_final.csv")
#SAIAB BRUV
bruv_dat <- read.csv("RESULTSPLOTS_NOVEMBER/BRUVfinal.csv")
# eight individual datasets
#-------------------------------------------------------------------------------
#column names
#-------------------------------------------------------------------------------
colnames(line_dat)
colnames(lit_dat)
colnames(trawl_survey_dat)
colnames(mw_trawl_obs_dat)
colnames(inat_dat)
colnames(museum_ps_dat)
colnames(capfish_obs_dat)
colnames(bruv_dat)
#------------------------------------------------------------------------------
#clean columns and standardize
#-------------------------------------------------------------------------------
#standard column names 
#source = which dataset it came from

#method (either use existing column name or create new)
# for line_dat method = gear_std
# for lit_dat method = method
# for inat_dat method = basisOfRecord
# for museum_ps_dat method = basisOfRecord
# for capfish_obs_dat method = SOurce

#for bruv_dat method = "bruv survey"
# for trawl_survey_dat method = "demersal trawl"
# for mw_trawl_obs_dat method = "midwater trawl"

#datatype = commercial, observer, survey, research

# for line_dat datatype = data_source if COM then C if OBS then O
# for lit_dat datatype = datatype
# for inat_dat datatype = S
# for museum_ps_dat datatype = S
# for capfish_obs_dat datatype = O
#for bruv_dat datatype = S
# for trawl_survey_dat datatype = S
# for mw_trawl_obs_dat datatype = o

#species
#mw_trawl_obs_dat = scientif_name
#trawl_survey_dat = scientific_name_ref
#line_dat = scientific_name

# othrs all already species family and genus fine, drop order, 

#make taxonomic resolution columns for museumn and inat
#otherwise all other datasets have taxonomic_resolution

#year,
#linedat: catch_year, litdat: year_start, 
#trawl_survey_dat, museum_ps_dat, capfish_obs_dat & inat_dat : year
#mw_trawl_obs_dat:  "date_start" turn into year
#bruvdat no value for year yet!! go find

#latitude and longitude standardise

#coast add to lit_dat

#-------------------------------------------------------------------------------
# Define helper functions
#-------------------------------------------------------------------------------
to_year <- function(x) suppressWarnings(year(parse_date_time(x, orders = c("Ymd", "Y-m-d", "d/m/Y", "m/d/Y", "Y"))))

std_cols <- c(
  "source", "species", "genus", "family", "order", "method", "datatype",
  "taxonomic_resolution", "year", "latitude", "longitude", "coast", "datatype"
)

#line
line_std <- line_dat %>%
  mutate(
    source   = "LINEFISH",
    taxonomic_resolution = tolower(taxonomic_resolution),
    coast = tolower(coast),
    species  = scientific_name,
    method   = gear_std,
    datatype = case_when(
      str_detect(data_source, regex("COM", ignore_case = TRUE)) ~ "C",
      str_detect(data_source, regex("OBS", ignore_case = TRUE)) ~ "O",
      TRUE ~ "R"
    ),
    year     = coalesce(catch_year, year),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) %>%
  select(source, species, genus, family, order, method, datatype,
         taxonomic_resolution, year, latitude, longitude, coast)

# Re-classify coastline
lit_dat <- lit_dat %>%
  mutate(
    coast = case_when(
      longitude >= 14 & longitude < 20 & latitude <= -28 & latitude >= -36.5 ~ "west",
      longitude >= 20 & longitude < 27 & latitude <= -33 & latitude >= -36.5 ~ "south",
      longitude >= 27 & longitude <= 35 & latitude <= -26 & latitude >= -35 ~ "east",
      TRUE ~ "west"   # force anything else into West
    )
  )

#lit

colnames(lit_dat)

unique(lit_dat$method)
lit_std <- lit_dat %>%
  mutate(
    source   = "LITERATURE",
    method   = method,
    taxonomic_resolution = tolower(taxonomic_resolution),
    coast = tolower(coast),
    datatype = datatype,
    year     = coalesce(yearstart, yearend),
    coast    = NA_character_
  ) %>%
  select(source, species, genus, family, order, method, datatype,
         taxonomic_resolution, year, latitude, longitude, coast)
#trawl
trawl_std <- trawl_survey_dat %>%
  mutate(
    source   = "DEM_TRAWL",
    taxonomic_resolution = tolower(taxonomic_resolution),
    coast = tolower(coast),
    species  = scientific_name_ref,
    method   = "Demersal trawl",
    datatype = "S",
    latitude = lat,
    longitude = lon
  ) %>%
  select(source, species, genus, family, order, method, datatype,
         taxonomic_resolution, year, latitude, longitude, coast)
# mwtrawl
mwtrawl_std <- mw_trawl_obs_dat %>%
  mutate(
    source   = "MW_TRAWL",
    taxonomic_resolution = tolower(taxonomic_resolution),
    coast = tolower(coast),
    species  = scientific_name,
    method   = "Midwater trawl",
    datatype = "O",
    year     = to_year(date_start),
    order = tolower(tax_order),
    latitude = as.numeric(start_lat),
    longitude = as.numeric(start_long)
  ) %>%
  select(source, species, genus, family, order, method, datatype,
         taxonomic_resolution, year, latitude, longitude, coast)

inat_std <- inat_dat %>%
  mutate(
    source   = "INAT",
    taxonomic_resolution = tolower(taxonomic_resolution),
    coast = tolower(coast),
    method   = basisOfRecord,
    datatype = "S",
    latitude = decimalLatitude,
    longitude = decimalLongitude
  ) %>%
  select(source, species, genus, family, order, method, datatype,
         taxonomic_resolution, year, latitude, longitude, coast)

museum_std <- museum_ps_dat %>%
  filter(basisOfRecord == "PRESERVED_SPECIMEN") %>%
  mutate(
    source   = "MUSEUM",
    taxonomic_resolution = tolower(taxonomic_resolution),
    coast = tolower(coast),
    method   = basisOfRecord,
    datatype = "S",
    latitude = decimalLatitude,
    longitude = decimalLongitude
  ) %>%
  select(source, species, genus, family, order, method, datatype,
         taxonomic_resolution, year, latitude, longitude, coast)
#capfish
capfish_std <- capfish_obs_dat %>%
  mutate(
    source   = "CAPFISH",
    taxonomic_resolution = tolower(taxonomic_resolution),
    coast = tolower(coast),
    method   = Source,
    datatype = "O"
  ) %>%
  select(source, species, genus, family, order, method, datatype,
         taxonomic_resolution, year, latitude, longitude, coast)
#BRUV
bruv_std <- bruv_dat %>%
  mutate(
    source   = "BRUV",
    taxonomic_resolution = tolower(taxonomic_resolution),
    order = tolower(CorrectedOrder),
    coast = tolower(coast),
    method   = "BRUV survey",
    datatype = "S",
    order    = CorrectedOrder,
    year     = NA_integer_   
  ) %>%
  select(source, species, genus, family, method, datatype,
         taxonomic_resolution, year, latitude, longitude, coast)

#-------------------------------------------------------------------------------
# write individual final data source sets
#-------------------------------------------------------------------------------
output_dir <- "~/Desktop/wd/masters/MERGE-READY"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Write each standardized dataset
write.csv(line_std,   file.path(output_dir, "linedat_std.csv"),   row.names = FALSE)
write.csv(lit_std,    file.path(output_dir, "litdat_std.csv"),    row.names = FALSE)
write.csv(trawl_std,  file.path(output_dir, "demtrawldat_std.csv"), row.names = FALSE)
write.csv(mwtrawl_std,file.path(output_dir, "mwtrawldat_std.csv"),  row.names = FALSE)
write.csv(inat_std,   file.path(output_dir, "inatdat_std.csv"),   row.names = FALSE)
write.csv(museum_std, file.path(output_dir, "museumdat_std.csv"), row.names = FALSE)
write.csv(capfish_std,file.path(output_dir, "capfishdat_std.csv"),row.names = FALSE)
write.csv(bruv_std,   file.path(output_dir, "bruvdat_std.csv"),   row.names = FALSE)
#-------------------------------------------------------------------------------
# merge and write integrated dataset
#-------------------------------------------------------------------------------
master_dat <- bind_rows(
  line_std,
  lit_std,
  trawl_std,
  mwtrawl_std,
  inat_std,
  museum_std,
  capfish_std,
  bruv_std
)
write.csv(master_dat, "~/Desktop/wd/masters/integrated_16_April.csv", row.names = FALSE)

dat <- read.csv("~/Desktop/wd/masters/integrated_16_April.csv")
#-------------------------------------------------------------------------------------------
#CLEAN
#-------------------------------------------------------------------------------------------
# species-level corrections
species_corrections <- c(
  #CERTAIN
  "Trachurus trachrus"  = "Trachurus capensis",
  "Trachurus trachurus" = "Trachurus capensis",
  "Liza richardsonii"   = "Chelon richardsonii",
  "Dipterodon capensis" = "Dichistius capensis",
  "Coracinus capensis"  = "Dichistius capensis",
  "Acanthurus tennenti" = "Acanthurus tennentii", 
  "Carangoides caeruleopinnatus" = "Turrum coeruleopinnatum", #Carangidae
  "Carangoides coeruleopinnatus" = "Turrum coeruleopinnatum", #Carangidae
  "Centropyge multispinus" = "Centropyge multispinis",
  "Epinephelus caeruleopunctatus" = "Epinephelus coeruleopunctatus",
  "Halichoeres hortulans" = "Halichoeres hortulanus",
  "Helcogramma obtusirostre" = "Helcogramma obtusirostris",
  "Pellona ditchella" = "Pellona ditchela",
  "Pomadasys maculatum" = "Pomadasys maculatus",
  "Pomadasys olivaceum" = "Pomadasys olivaceus",
  "Thalassoma amblycephalus" = "Thalassoma amblycephalum",
  "Etrumeus teres" = "Etrumeus sadina",   
  "Sphyraena putnamiae" = "Sphyraena putnamae",
  "Pomadasys multimaculatum" = "Pomadasys multimaculatus"
)

genus_family_lookup <- tribble(
  ~genus, ~correct_family,
  "Etrumeus", "Dussumieriidae",
  "Engraulis", "Engraulidae",
  "Sardinops", "Clupeidae",
  "Scarus", "Scaridae",
  "Chlorurus", "Scaridae",
  "Calotomus", "Scaridae",
  "Gerres", "Gerreidae",
  "Leiognathus", "Leiognathidae",
  "Rhabdosargus", "Sparidae",
  "Scomber", "Scombridae",
  "Sarda", "Scombridae",
  "Katsuwonus", "Scombridae",
  "Thyrsites", "Gempylidae",
  "Trachurus", "Carangidae",
  "Epinephelus", "Epinephelidae",
  "Cephalopholis", "Epinephelidae",
  "Plectropomus", "Epinephelidae",
  "Hyporthodus", "Epinephelidae",
  "Aethaloperca", "Epinephelidae",
  "Dermatolepis", "Epinephelidae",
  "Grammistes", "Epinephelidae",
  "Variola", "Epinephelidae",
  "Coloconger", "Colocongridae",
  "Hyperoglyphe", "Centrolophidae",
  "Paraliparis", "Liparidae",
  "Paralichthodes", "Paralichthodidae",
  "Merluccius", "Merlucciidae",
  "Chelidonichthys", "Triglidae",
  "Helicolenus", "Sebastidae",
  "Sebastes", "Sebastidae",
  "Clinus", "Clinidae",
  "Malacanthus", "Malacanthidae",
  "Leptocephalus", NA_character_,  # larval form 
  "Plotosus", "Plotosidae",
  "Hippocampus", "Syngnathidae"
)

family_corrections <- c(
  "Sebastidae" = "Scorpaenidae", 
  "Grammistidae" = "Epinephelidae", 
  "Gaidropsaridae" = "Lotidae", 
  "Malacanthidae" = "Branchiostegidae", 
  "Ehiravidae" = "Clupeidae", 
  "Dorosomatidae" = "Clupeidae", 
  "Alosidae" = "Clupeidae", 
  "Dichistiidae" = "Sparidae"
)

order_corrections <- tribble(
  ~Family,           ~CorrectedOrder,
  "Acanthuridae",         "Perciformes",
  "Chaetodontidae",         "Perciformes",
  "Drepaneidae",         "Perciformes",
  "Ephippidae",         "Perciformes",
  "Leiognathidae",         "Perciformes",
  "Lobotidae",         "Perciformes",
  "Pomacanthidae",         "Perciformes",
  "Siganidae",         "Perciformes",
  "Creediidae",         "Perciformes",
  "Pempheridae",         "Perciformes",
  "Scombropidae",         "Perciformes",
  "Blenniidae",         "Perciformes",
  "Clinidae",         "Perciformes",
  "Tripterygiidae",         "Perciformes",
  "Callionymidae",     "Perciformes",
  "Menidae",         "Perciformes",
  "Polynemidae",         "Perciformes",
  "Sphyraenidae",         "Perciformes",
  "Carangidae",         "Perciformes",
  "Coryphaenidae",     "Perciformes",
  "Echeneidae",         "Perciformes",
  "Istiophoridae",         "Perciformes",
  "Rachycentridae",         "Perciformes",
  "Cheilodactylidae",         "Perciformes",
  "Cirrhitidae",     "Perciformes",
  "Dichistiidae",         "Perciformes",
  "Kuhliidae",         "Perciformes",
  "Kyphosidae",         "Perciformes",
  "Oplegnathidae",         "Perciformes",
  "Parascorpididae",     "Perciformes",
  "Terapontidae",         "Perciformes",
  "Alosidae",         "Clupeiformes",
  "Chirocentridae",         "Clupeiformes",
  "Engraulidae",         "Clupeiformes",
  "Pristigasteridae",     "Clupeiformes",
  "Dactylopteridae",    "Scorpaeniformes",
  "Caesionidae",    "Perciformes",
  "Dinopercidae",    "Perciformes",
  "Gerreidae",    "Perciformes",
  "Haemulidae",    "Perciformes",
  "Labridae",    "Perciformes",
  "Lethrinidae",    "Perciformes",
  "Lutjanidae",    "Perciformes",
  "Monodactylidae",    "Perciformes",
  "Priacanthidae",    "Perciformes",
  "Scaridae",    "Perciformes",
  "Sciaenidae",    "Perciformes",
  "Sillaginidae",    "Perciformes",
  "Sparidae",    "Perciformes",
  "Gobiesocidae",    "Perciformes",
  "Eleotridae",    "Perciformes",
  "Gobiidae",    "Perciformes",
  "Schindleriidae",    "Perciformes",
  "Trichonotidae",    "Perciformes",
  "Apogonidae",    "Perciformes",
  "Mullidae",    "Perciformes",
  "Ambassidae",    "Perciformes",
  "Pomacentridae",    "Perciformes",
  "Pseudochromidae",    "Perciformes",
  "Ammodytidae",    "Perciformes",
  "Anthiadidae",    "Perciformes",
  "Apistidae",    "Scorpaeniformes",
  "Congiopodidae",    "Scorpaeniformes",
  "Epinephelidae",    "Perciformes",
  "Peristediidae",    "Scorpaeniformes",
  "Pinguipedidae",    "Perciformes",
  "Platycephalidae",    "Scorpaeniformes",
  "Scorpaenidae",    "Scorpaeniformes",
  "Serranidae",    "Perciformes",
  "Tetrarogidae",    "Scorpaeniformes",
  "Triglidae",    "Scorpaeniformes",
  "Ariommatidae",    "Perciformes",
  "Gempylidae",    "Perciformes",
  "Nomeidae",    "Perciformes",
  "Pomatomidae",    "Perciformes",
  "Scombridae",    "Perciformes",
  "Stromateidae",    "Perciformes",
  "Trichiuridae",    "Perciformes",
  "Aulostomidae",    "Gasterosteiformes",
  "Centriscidae",    "Gasterosteiformes",
  "Fistulariidae",    "Gasterosteiformes",
  "Syngnathidae",    "Gasterosteiformes",
  "Macroramphosidae",    "Gasterosteiformes",
  "Alepocephalidae", "Argentiniformes",
  "Ariidae", "Siluriformes",
  "Balistidae", "Tetraodontiformes",
  "Bathylagidae", "Argentiniformes",
  "Belonidae", "Beloniformes",
  "Berycidae", "Beryciformes",
  "Bothidae", "Pleuronectiformes",
  "Branchiostegidae", "Perciformes",
  "Callanthiidae", "Perciformes",
  "Chanidae", "Gonorynchiformes",
  "Clupeidae", "Clupeiformes",
  "Congridae", "Anguilliformes",
  "Diodontidae", "Tetraodontiformes",
  "Diretmidae", "Beryciformes",   
  "Holocentridae", "Beryciformes",
  "Latridae", "Perciformes",
  "Merlucciidae", "Gadiformes",
  "Microdesmidae", "Perciformes",
  "Molidae", "Tetraodontiformes",
  "Monacanthidae", "Tetraodontiformes",
  "Mugilidae", "Mugiliformes",
  "Muraenesocidae", "Anguilliformes",
  "Muraenidae", "Anguilliformes",
  "Nemipteridae", "Perciformes",
  "Ophichthidae", "Anguilliformes",
  "Ophidiidae", "Ophidiiformes",
  "Ostraciidae", "Tetraodontiformes",
  "Polyprionidae", "Perciformes",
  "Psychrolutidae", "Perciformes", 
  "Soleidae", "Pleuronectiformes",
  "Synodontidae", "Aulopiformes",
  "Tetraodontidae", "Tetraodontiformes",
  "Trachichthyidae", "Beryciformes",
  "Triodontidae", "Tetraodontiformes",
  "Zanclidae", "Perciformes",
  "Zeidae", "Zeiformes"
)

#before initial clean = 1135983 obs
#-----------------------------# 
# INITIAL CLEAN
#-----------------------------#
dat <- dat %>%
  mutate(
    species_raw = species,
    species = species %>%
      str_replace_all("\u00A0", " ") %>%
      str_squish(),
    genus  = str_to_title(genus),
    family = str_to_title(family),
    order  = str_to_title(order)
  )
#-----------------------------#
# SPECIES CORRECTIONS
#-----------------------------#
dat <- dat %>%
  mutate(
    species = recode(species, !!!species_corrections)
  )
#-----------------------------#
# STANDARDISE BINOMIAL
#-----------------------------#
dat <- dat %>%
  mutate(
    genus = word(species, 1) %>% str_to_title(),
    species_epithet = word(species, 2) %>% str_to_lower(),
    
    species = if_else(
      !is.na(genus) & !is.na(species_epithet),
      paste(genus, species_epithet),
      NA_character_
    )
  )
#-----------------------------#
# FILTER VALID TAXA
#-----------------------------# 
#before hard filter 
dat_all <- dat
#-----------------------------#
# GENUS → FAMILY STANDARDISATION
#-----------------------------#
dat_all <- dat_all %>%
  left_join(genus_family_lookup, by = "genus") %>%
  mutate(
    family = coalesce(correct_family, family)
  ) %>%
  select(-correct_family)
#-----------------------------#
# FAMILY CORRECTIONS
#-----------------------------#
dat_all <- dat_all %>%
  mutate(
    family = recode(family, !!!family_corrections)
  )
#-----------------------------#
# ORDER STANDARDISATION
#-----------------------------#
order_lookup_full <- order_corrections %>%
  mutate(Family = str_trim(Family)) %>%
  distinct(Family, .keep_all = TRUE)

dat_all <- dat_all %>%
  mutate(family = str_trim(family)) %>%
  left_join(order_lookup_full, by = c("family" = "Family")) %>%
  mutate(
    order = coalesce(CorrectedOrder, order)
  ) %>%
  select(-CorrectedOrder)

dat_all <- dat_all %>%
  mutate(
    order = case_when(
      family == "Dussumieriidae" ~ "Clupeiformes",
      family == "Liparidae" ~ "Scorpaeniformes",
      family == "Plotosidae" ~ "Siluriformes",
      TRUE ~ order
    ),
    family = case_when(
      genus == "Arothron" ~ "Tetraodontidae",
      genus == "Aulacocephalus" ~ "Ostraciidae",
      genus == "Cheilodipterus" ~ "Apogonidae",
      genus == "Chirodactylus" ~ "Cheilodactylidae",
      genus == "Emmelichthys" ~ "Emmelichthyidae",
      genus == "Lepidopus" ~ "Trichiuridae",
      genus == "Sphyraena" ~ "Sphyraenidae",
      TRUE ~ family
    ),
    order = case_when(
      family == "Emmelichthyidae" ~ "Perciformes",
      TRUE ~ order
    )
  )

#DAT_ALL will be merged to ENV to create FINAL_DAT
#-----------------------------#
# HARD FILTER
#-----------------------------#
dat_species <- dat_all %>%
  filter(
    str_count(species, " ") == 1,
    !str_detect(species, "\\b(sp|cf|aff)\\b"),
    !str_detect(species, "dae|formes"),
    !is.na(species),
    species != ""
  )
#dat_species = 1 022 750

# 113 233 observations lost to species cleaning
#DAT_SPECIES will be merged used as validation for species analyses

#-----------------------------#
# FINAL OUTPUT TABLE
#-----------------------------#
known_species <- dat_species %>%
  distinct(order, family, genus, species) %>%
  arrange(order, family, genus, species)
