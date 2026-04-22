#-------------------------------------------------------------------------------
#RESTRUCTURING EACH DATASET TO PREPARE FOR MERGE
#-------------------------------------------------------------------------------
# Install packages 
# install.packages(c("sf", "ggplot2", "rnaturalearth", "rnaturalearthdata", "rgeos"))

#COLUMN NAMES
#species, aphiaID, order, family, genus, latitude, longitude, gridID, year, depth, gear, 

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
library(worrms)

library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# Set working directory
setwd("/Users/savannahanderson/Desktop/wd/masters")

#-------------------------------------------------------------------------------
#CAPFISH
#-------------------------------------------------------------------------------
#needs aphiaID, order, gridID, depth, & column names standardized
CAPFISHdat <- read_excel("capfishdat.xlsx")

colnames(CAPFISHdat)
head(CAPFISHdat)

# select clumns, standardiseand Convert lat/lon to decimal degrees
CAPFISHdat_clean <- CAPFISHdat %>%
  mutate(
    latitude = LatDeg + LatMin / 60,
    longitude = LongDeg + LongMin / 60,
    species = Binomial,
    order = NA_character_,        
    gridID = NA_character_,       
    year = year(ymd(Date)),
    depth = NA_real_,            
    gear = Source                #   = gear type
  ) %>%
  select(
    species,
    order,
    family = Family,
    genus = Genus,
    latitude,
    longitude,
    gridID,
    year,
    depth,
    gear,
    Source
  )

length(unique(CAPFISHdat_clean$species)) #135

CAPFISHdat_clean <- CAPFISHdat_clean %>%
  filter(!tolower(species) %in% tolower(c(elasmos, freshwater)))

all_replacements <- c(species_fix, replace_lookup)
CAPFISHdat_clean  <- CAPFISHdat_clean %>%
  mutate(species = str_replace_all(species, all_replacements))

#135
#remove "NA NA" ". ."  
unique(CAPFISHdat_clean$species) %>% sort()

canonise_species <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("[\\u00A0\\t\\r\\n]+", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    str_replace(regex("\\bsp\\.?\\b(?:\\s*NA)?$", ignore_case = TRUE), "sp.")
}

#fix name changes 
name_fix <- c(
  "Sphyraena baracuda"            = "Sphyraena barracuda",
  "Rexia spp"                     = "Rexea sp.",
  "Petrus rupestrus"              = "Petrus rupestris",
  "Sardinops sagax ocellatus"     = "Sardinops sagax",
  "Engraulis japonicus capensis"  = "Engraulis japonicus",
  "Trachurus sp. trachurus"       = "Trachurus trachurus",
  "Pomatomus saltator"            = "Pomatomus saltatrix",
  "Emmelichthys nitidus nitidus"  = "Emmelichthys nitidus",
  "Merluccius Sp."                = "Merluccius sp.",
  "Atactoscion aequidens"         = "Atractoscion aequidens",
  "Makaira indicta"               = "Istiompax indica",
  "Allocyttus verrucosis"         = "Allocyttus verrucosus",
  "Antimora spp."                 = "Antimora sp.",
  "Thunnus alulunga"              = "Thunnus alalunga",
  "Gonorhynchus gonorhynchus"     = "Gonorynchus gonorynchus",
  "Pomadasys commersonni"         = "Pomadasys commersonnii",
  "Pagellus natalansis"           = "Pagellus natalensis",
  "Anguilla bicolor bicolor"      = "Anguilla bicolor",
  "Elagratis bipinnulata"         = "Elagatis bipinnulata",
  "Rexea  sp."                    = "Rexea sp.",
  "Merluccius sp"                 = "Merluccius sp.",
  "Antimora sp"                   = "Antimora sp.",
  "Genupterus capensis"           = "Genypterus capensis",
  "Argyrosomus ."                 = "Argyrosomus sp.",
  "Caranx NA"                     = "Caranx sp.",
  "Congiopodus sp"                = "Congiopodus sp.",
  "Cynoglossus ."                 = "Cynoglossus sp.",
  "Decapterus NA"                 = "Decapterus sp.",
  "Ebosia ."                      = "Ebosia sp.", 
  "Epinephelus ."                 = "Epinephelus sp.",
  "Gobiidae sp..ae NA"            = "Gobiidae sp.",
  "Hoplostethus ."                = "Hoplostethus sp.",
  "Lepidion ."                    = "Lepidion sp.",
  "Ophichthus ."                  = "Ophichthus sp.",
  "Ophidiidae NA"                 = "Ophidiidae sp.",
  "Rhabdosargus ."                = "Rhabdosargus sp.",
  "Syngnathidae sp. NA"           = "Syngnathidae sp.",
  "Brotula ."                     = "Brotula sp.",
  "Austroglossus ."               = "Austroglossus sp.",
  "Thunnus obesis"            = "Thunnus obesus",
  "Photichthys argenteus"     = "Phosichthys argenteus"
)

CAPFISHdat_clean <- CAPFISHdat_clean %>%
  mutate(species = str_squish(species),
         species = recode(species, !!!name_fix))

# fix families and orders
tax_overrides <- tribble(
  ~species,                    ~family,            ~order,
  "Allothunnus fallai",       "Scombridae",     "Perciformes",
  "Tetrapturus pfluegeri",       "Istiophoridae",     "Perciformes",
  "Euthynnus affinis",            "Scombridae",     "Perciformes",
  "Alepisaurus brevirostris",     "Alepisauridae",     "Aulopiformes",
  "Istiophorus platypterus",      "Istiophoridae",    "Perciformes",
  "Tetrapturus audax",            "Istiophoridae",    "Perciformes",
  "Thunnus maccoyii",             "Scombridae",       "Perciformes",
  "Tetrapturus angustirostris",   "Istiophoridae",    "Perciformes",
  "Acanthocybium solandri",       "Scombridae",       "Perciformes",
  "Makaira nigricans",            "Istiophoridae",    "Perciformes",
  "Champsodon capensis",          "Champsodontidae",  "Perciformes",
  "Chelidonichthys lastoviza",    "Triglidae",        "Scorpaeniformes",
  "Cheilodactylus fasciatus",     "Cheilodactylidae", "Perciformes",
  "Austroglossus microlepis",     "Soleidae",         "Pleuronectiformes",
  "Chirodactylus grandis",        "Latridae",         "Perciformes",
  "Cheimerius nufar",             "Sparidae",         "Perciformes",
  "Parascorpis typus",            "Parascorpididae",  "Perciformes",
  "Diplodus sargus",              "Sparidae",         "Perciformes",
  "Spicara axillaris",            "Sparidae",         "Perciformes",
  "Sarpa salpa",                  "Sparidae",         "Perciformes",
  "Etrumeus whiteheadi",          "Clupeidae",        "Clupeiformes",
  "Lampanyctodes hectoris",       "Myctophidae",      "Myctophiformes",
  "Galeichthys feliceps",         "Ariidae",          "Siluriformes",
  "Thyrsites atun",               "Gempylidae",       "Perciformes",
  "Scomber japonicus",            "Scombridae",       "Perciformes",
  "Pomatomus saltatrix",          "Pomatomidae",      "Perciformes",
  "Emmelichthys nitidus",         "Emmelichthyidae",  "Perciformes",
  "Argyrozona argyrozona",        "Sparidae",         "Perciformes",
  "Chelidonichthys capensis",     "Triglidae",        "Scorpaeniformes",
  "Merluccius paradoxus",         "Merlucciidae",     "Gadiformes",
  "Brama brama",                  "Bramidae",         "Perciformes",
  "Helicolenus dactylopterus",    "Scorpaenidae",     "Scorpaeniformes", 
  "Merluccius capensis",          "Merlucciidae",     "Gadiformes",
  "Alepisaurus ferox",            "Alepisauridae",    "Aulopiformes",
  "Lophius vomerinus",            "Lophiidae",        "Lophiiformes",
  "Lepidion capensis",            "Moridae",          "Gadiformes",
  "Pterogymnus laniarius",        "Sparidae",         "Perciformes",
  "Lepidopus caudatus",           "Trichiuridae",     "Perciformes",
  "Conger wilsoni",               "Congridae",        "Anguilliformes",
  "Zeus faber",                   "Zeidae",           "Zeiformes",
  "Polyprion americanus",         "Polyprionidae",    "Perciformes",
  "Malacocephalus laevis",        "Macrouridae",      "Gadiformes",
  "Coryphaena hippurus",          "Coryphaenidae",    "Perciformes",
  "Hyperoglyphe antarctica",      "Centrolophidae",   "Perciformes",
  "Thunnus albacares",            "Scombridae",       "Perciformes",
  "Coelorinchus simorhynchus",    "Macrouridae",      "Gadiformes",
  "Istiompax indica",             "Istiophoridae",    "Perciformes",
  "Zeus capensis",                "Zeidae",           "Zeiformes",
  "Bassanago albescens",          "Congridae",        "Anguilliformes",
  "Thunnus obesus",               "Scombridae",       "Perciformes",
  "Notacanthus sexspinis",        "Notacanthidae",    "Notacanthiformes",
  "Pentaceros capensis",          "Pentacerotidae",   "Perciformes",
  "Beryx splendens",              "Berycidae",        "Beryciformes",
  "Centrolophus niger",           "Centrolophidae",   "Perciformes",
  "Selachophidium guentheri",     "Ophidiidae",       "Ophidiiformes",
  "Chelidonichthys queketti",     "Triglidae",        "Scorpaeniformes",
  "Psychrolutes macrocephalus",   "Psychrolutidae",   "Scorpaeniformes",
  "Xiphias gladius",              "Xiphiidae",        "Perciformes",
  "Neocyttus rhomboidalis",       "Oreosomatidae",    "Zeiformes",
  "Ruvettus pretiosus",           "Gempylidae",       "Perciformes",
  "Katsuwonus pelamis",           "Scombridae",       "Perciformes",
  "Seriola lalandi",              "Carangidae",       "Perciformes",
  "Congiopodus torvus",           "Congiopodidae",    "Scorpaeniformes",
  "Rhabdosargus globiceps",       "Sparidae",         "Perciformes",
  "Coelorinchus braueri",         "Macrouridae",      "Gadiformes",
  "Congiopodus sp",               "Congiopodidae",    "Scorpaeniformes",
  "Zenopsis conchifer",           "Zeidae",           "Zeiformes",
  "Cynoglossus zanzibarensis",    "Cynoglossidae",    "Pleuronectiformes",
  "Hoplostethus atlanticus",      "Trachichthyidae",  "Beryciformes",
  "Simenchelys parasiticus",      "Synaphobranchidae","Anguilliformes",
  "Notopogon macrosolen",         "Macroramphosidae", "Gasterosteiformes",
  "Congiopodus spinifer",         "Congiopodidae",    "Scorpaeniformes",
  "Hoplostethus mediterraneus",   "Trachichthyidae",  "Beryciformes",
  "Hoplostethus melanopus",       "Trachichthyidae",  "Beryciformes",
  "Chrysoblephus gibbiceps",      "Sparidae",         "Perciformes",
  "Neoepinnula orientalis",       "Gempylidae",       "Perciformes",
  "Rhabdosargus holubi",          "Sparidae",         "Perciformes",
  "Umbrina canariensis",          "Sciaenidae",       "Perciformes",
  "Schedophilus ovalis",          "Centrolophidae",   "Perciformes",
  "Callanthias legras",           "Callanthiidae",    "Perciformes",
  "Pagellus natalensis",          "Sparidae",         "Perciformes",
  "Cubiceps capensis",            "Nomeidae",         "Perciformes",
  "Mola mola",                    "Molidae",          "Tetraodontiformes",
  "Setarches guentheri",          "Setarchidae",      "Scorpaeniformes",
  "Paracallionymus costatus",     "Callionymidae",    "Perciformes",
  "Syngnathus acus",              "Syngnathidae",     "Gasterosteiformes",
  "Porogadus miles",              "Ophidiidae",       "Ophidiiformes",
  "Ateleopus natalensis",         "Ateleopodidae",    "Ateleopodiformes",
  "Spondyliosoma emarginatum",    "Sparidae",         "Perciformes",
  "Sarda sarda",                  "Scombridae",       "Perciformes",
  "Scomberomorus plurilineatus",  "Scombridae",       "Perciformes",
  "Chrysoblephus laticeps",       "Sparidae",         "Perciformes",
  "Pachymetopon aeneum",          "Sparidae",         "Perciformes",
  "Chirodactylus brachydactylus", "Latridae",         "Perciformes",
  "Boopsoidea inornata",          "Sparidae",         "Perciformes",
  "Acanthistius sebastoides",     "Anthiadidae",      "Perciformes",
  "Polysteganus praeorbitalis",   "Sparidae",         "Perciformes",
  "Stromateus fiatola",           "Stromateidae",     "Perciformes",
  "Neoscorpis lithophilus",       "Kyphosidae",       "Perciformes",
  "Diplodus cervinus",            "Sparidae",         "Perciformes",
  #changed names
  "Sphyraena barracuda",       "Sphyraenidae",     "Perciformes",
  "Rexea sp.",                 "Gempylidae",       "Perciformes",
  "Petrus rupestris",          "Sparidae",         "Perciformes",
  "Sardinops sagax",           "Clupeidae",        "Clupeiformes",
  "Engraulis japonicus",       "Engraulidae",      "Clupeiformes",
  "Trachurus trachurus",       "Carangidae",       "Perciformes",
  "Pomatomus saltatrix",       "Pomatomidae",      "Perciformes",
  "Emmelichthys nitidus",      "Emmelichthyidae",  "Perciformes", 
  "Genypterus capensis",       "Ophidiidae",       "Ophidiiformes",
  "Merluccius sp.",            "Merlucciidae",     "Gadiformes",
  "Atractoscion aequidens",    "Sciaenidae",       "Perciformes",
  "Istiompax indica",          "Istiophoridae",    "Perciformes",
  "Antimora sp.",              "Moridae",          "Gadiformes",
  "Thunnus alalunga",          "Scombridae",       "Perciformes",
  "Pagellus natalensis",       "Sparidae",         "Perciformes",
  "Gonorynchus gonorynchus",   "Gonorynchidae",    "Gonorynchiformes", 
  "Allocyttus verrucosus",     "Oreosomatidae",    "Zeiformes",
  "Pomadasys commersonnii",    "Haemulidae",       "Perciformes",
  "Anguilla bicolor",          "Anguillidae",      "Anguilliformes",
  "Elagatis bipinnulata",      "Carangidae",       "Perciformes",
  "Argyrosomus sp.",           "Sciaenidae",       "Perciformes",
  "Caranx sp.",                "Carangidae",       "Perciformes",
  "Congiopodus sp.",           "Congiopodidae",    "Scorpaeniformes",
  "Cynoglossus sp.",           "Cynoglossidae",    "Pleuronectiformes",
  "Decapterus sp.",            "Carangidae",       "Perciformes",
  "Ebosia sp.",                "Scorpaenidae",     "Scorpaeniformes", 
  "Epinephelus sp.",           "Epinephelidae",    "Perciformes",
  "Gobiidae sp",               "Gobiidae",         "Perciformes",
  "Hoplostethus sp.",          "Trachichthyidae",  "Beryciformes",
  "Lepidion sp.",              "Moridae",          "Gadiformes",
  "Ophichthus sp.",            "Ophichthidae",     "Anguilliformes",
  "Ophidiidae sp.",            "Ophidiidae",       "Ophidiiformes",
  "Rhabdosargus sp.",          "Sparidae",         "Perciformes",
  "Syngnathidae sp.",          "Syngnathidae",     "Gasterosteiformes",
  "Phosichthys argenteus",     "Phosichthyidae",   "Stomiiformes",
  "Brotula sp.",               "Ophidiidae",       "Ophidiiformes",
  "Austroglossus sp.",         "Soleidae",         "Pleuronectiformes"
 ) %>%
  distinct(species, .keep_all = TRUE)  # guard against duplicates

sort(tax_overrides$species)
tax_overrides <- tax_overrides %>%
  mutate(species = canonise_species(species)) %>%
  # prefer rows that end with " sp." over " sp" if both exist
  arrange(!str_detect(species, "\\bsp\\.$")) %>%  
  distinct(species, .keep_all = TRUE)
CAPFISHdat_tax <- CAPFISHdat_clean %>%
  rows_update(tax_overrides, by = "species", unmatched = "ignore")

CAPFISHdat_tax %>%
  summarise(
    n_orders  = n_distinct(order,  na.rm = TRUE),
    n_families = n_distinct(family, na.rm = TRUE),
    n_genera  = n_distinct(genus,  na.rm = TRUE),
    n_species = n_distinct(species, na.rm = TRUE)
  )

  #n_orders  n_families  n_genera   n_species
  # 19          64        108       135

unique(CAPFISHdat_tax$species) #135

#fix
# "Sphyraena NA" 
name_fix <- c(
  "Sphyraena NA"            = "Sphyraena sp.")
CAPFISHdat_tax <- CAPFISHdat_tax %>%
  mutate(species = str_squish(species),
         species = recode(species, !!!name_fix))

#remove NAs and other erroneous
CAPFISHdat_tax <- CAPFISHdat_tax %>%
  filter(
    !species %in% c("NA NA", ". ."))
    
length(unique(CAPFISHdat_tax$species)) #133
#-------------------------------------------------------------------------------
#final
capfishfinal <- CAPFISHdat_tax
#-------------------------------------------------------------------------------
#check coords
# lat and lon range
capfishfinal <- capfishfinal %>%
  mutate(
    latitude  = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )
summary(capfishfinal$latitude)
summary(capfish_obs_dat$longitude)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00   33.73   34.54   34.39   35.28   75.43     412
summary(capfishfinal$longitude)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.00   17.86   18.69   19.79   21.04  189.16     470 
#-------------------------------------------------------------------------------
#fix and clean column names
colnames(capfishfinal)
#"species"   "order"     "family"    "genus"     "latitude"  "longitude" "gridID"   
#"year"      "depth"     "gear"      "Source" 
#-------------------------------------------------------------------------------
#check years
#-------------------------------------------------------------------------------
#tax res
capfishfinal <- capfishfinal %>%
  mutate(
    taxonomic_resolution = case_when(
      str_detect(species, "sp\\.$") & str_detect(species, "formes") ~ "order",
      str_detect(species, "sp\\.$") & str_detect(species, "dae") ~ "family",
      str_detect(species, "sp\\.$") ~ "genus",
      str_detect(species, "spp\\.$") & str_detect(species, "formes") ~ "order",
      str_detect(species, "spp\\.$") & str_detect(species, "dae") ~ "family",
      str_detect(species, "spp\\.$") ~ "genus",
      TRUE ~ "species"
    )
  )
print(table(capfishfinal$taxonomic_resolution))
#family     genus     species 
#20         3582     125423 

length(unique(capfishfinal$species)) #133 marine teleost species 
length(unique(capfishfinal$family)) #61 families
#-------------------------------------------------------------------------------
#plots
#-------------------------------------------------------------------------------
taxon_summary <- capfishfinal %>%
  count(taxonomic_resolution)
# Bar plot
ggplot(taxon_summary, aes(x = reorder(taxonomic_resolution, -n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 3, family = "Times") +
  labs(
    title = "Number of Records per Taxonomic Resolution for CapMarine Observer Data",
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
#top 20 species
top_species <- capfishfinal %>%
  filter(!is.na(species)) %>%
  count(species, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_species, aes(x = reorder(species, n), y = n)) +
  geom_bar(stat = "identity", fill = "grey60") +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequently Recorded Species",
    x = "Species",
    y = "Record Count"
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

#top 20 families
top_families <- capfishfinal %>%
  filter(!is.na(family)) %>%
  count(family, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_families, aes(x = reorder(family, n), y = n)) +
  geom_bar(stat = "identity", fill = "grey60") +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequently Recorded Families",
    x = "Family",
    y = "Record Count"
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
#-------------------------------------------------------------------------------
#first convert latitude to southern hemisphere
capfishfinal <- capfishfinal %>%
  mutate(latitude = -abs(latitude))

capfishfinal <- capfishfinal %>%
  mutate(
    coast = case_when(
      longitude >= 14 & longitude < 20 & latitude <= -28 & latitude >= -36.5 ~ "west",
      longitude >= 20 & longitude < 27 & latitude <= -33 & latitude >= -36.5 ~ "south",
      longitude >= 27 & longitude <= 35 & latitude <= -26 & latitude >= -35 ~ "east",
      TRUE ~ "west"   
    )
  )
# Summarise records and species per coast
coast_summary <- capfishfinal %>%
  group_by(coast) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species)
  )
coast_summary
#. coast   n_records  n_species
# east       1847        24
# south     32341       104
# west      94837       101

sorted_year <- sort(unique(capfishfinal$year), na.last = TRUE)
print(sorted_year)
# 1933 1993 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2029   NA
#remove NA and 2029

year_summary <- capfishfinal %>%
  group_by(year) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species)
  )
year_summary

capfishfinal <- capfishfinal %>%
  filter(!is.na(year), year != 2029)

#1933 1993 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011
#-------------------------------------------------------------------------------
# Intersecting with EEZ to ensure that all outlying records removed
#-------------------------------------------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")
sa_map <- world %>% filter(admin == "South Africa")
eez <- st_read("MAPPING/EEZ/eez_v12.shp", quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(4326)
# Convert  to sf
capfish_sf <- capfishfinal %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
# points that fall inside SA (land)
is_on_land <- st_intersects(capfish_sf, sa_map, sparse = FALSE)[,1]
# Keep only marine (non-land) points
capfish_eez <- capfish_sf[!is_on_land, ]
# Check how many were removed
n_removed <- sum(is_on_land)
#480 points removed
#-------------------------------------------------------------------------------
#OUTPUTS
#-------------------------------------------------------------------------------
#128999 obs, 13 variables
#with eez 128050 
# 15632 distinct latitude, 17228 distinct longitude

length(unique(capfish_eez$species)) #133
length(unique(capfish_eez$family))#61


capfishfinal <- capfishfinal %>%
  filter(!is.na(order))
#-------------------------------------------------------------------------------
# records and species  per observer programme
#-------------------------------------------------------------------------------
capfish_eez %>%
  group_by(Source) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species)
  ) %>%
  arrange(desc(n_records))

# Source    n_records  n_species        
# SADSTIA     75479        93 
# SAHLLA      19347        66 
# SECIFA      16802        72 
# SAPFIA      16422        15 

#-------------------------------------------------------------------------------
# Spatial distribution
#-------------------------------------------------------------------------------
capfish_eez_sf <- st_as_sf(
  capfish_eez,
  coords = c("longitude", "latitude"),
  crs = 4326
)
#main
ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_sf(data = capfish_eez_sf, color = "navyblue", alpha = 0.3, size = 0.5) +
  coord_sf(xlim = c(10, 35), ylim = c(-40, -25), expand = FALSE) +
  labs(
    title = "Distribution of Teleost Occurence Data from CapMarine Observer Programmes",
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

is.null(capfish_eez)
class(capfish_eez)

#by source
ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_sf(data = capmarinedat_eez, aes(color = gear), alpha = 0.6, size = 1.0) +
  coord_sf(xlim = c(10, 35), ylim = c(-40, -20), expand = FALSE) +
  labs(
    title = "Distribution of Teleost Occurence Data by CapMarine Observer Programme",
    x = "Longitude", y = "Latitude", color = "Observer Programme"
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

#faceted plot
ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_sf(data = capfish_eez, aes(color = gear), alpha = 0.6, size = 1.0) +
  coord_sf(xlim = c(10, 35), ylim = c(-40, -20), expand = FALSE) +
  labs(
    title = "Distribution of CapMarine Occurence Data by Observer Programme",
    x = "Longitude", y = "Latitude", color = "Observer Programme"
  ) +
  theme_classic(base_family = "Times") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10), size = 11),
    axis.title.x = element_text(margin = margin(t = 10), size = 11),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)) +
    facet_wrap(~ gear)

#number of records per MPAS
capfish_marine <- st_transform(capfish_marine, crs = 4326)
mpas <- st_transform(mpas, crs = 4326)
mpas <- st_make_valid(mpas)
mpas_subset <- mpas %>%
  select(CUR_NME, TYPE, geometry)
capfish_mpa_joined <- st_join(capfish_marine, mpas_subset, join = st_within, left = TRUE)
capfish_marine_matched <- capfish_mpa_joined %>%
  filter(TYPE == "Marine Protected Area")
unique(mpas$CUR_NME)

n_total <- nrow(capfish_marine)                        # total marine records
n_pa     <- nrow(capfish_mpa_joined)                   # in any PA
n_mpa    <- nrow(capfish_marine_matched)               # in marine PAs

# Summary 
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

names(capfish_obs_dat)

#-------------------------------------------------------------------------------
# Export to Excel
write.csv(capfishfinal, "~/Desktop/wd/masters/RESULTSPLOTS_NOVEMBER/capfish_final.csv", row.names = FALSE)
