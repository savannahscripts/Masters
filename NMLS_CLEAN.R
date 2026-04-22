#-------------------------------------------------------------------------------
# NMLS data clean location and taxonomy
#-------------------------------------------------------------------------------
#COLUMN NAMES
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
library(tidyverse)
library(tibble)
library(janitor)

# Set working directory
setwd("/Users/savannahanderson/Desktop/wd/masters")
#-------------------------------------------------------------------------------
# Step 1: check class and filter out non-teleosts
#-------------------------------------------------------------------------------
unique(OBSdat$class) #"Teleostei"      NA               "Elasmobranchii" "Cephalopoda"    "Holocephali" 
unique(COMdat$class) # "Teleostei"      NA               "Elasmobranchii" "Cephalopoda"    "Holocephali" 
#filter out
OBSdat <- OBSdat %>%
  filter(!(class %in% c("Cephalopoda", "Holocephali", "Elasmobranchii")) & !is.na(class))
COMdat <- COMdat %>%
  filter(!(class %in% c("Cephalopoda", "Holocephali", "Elasmobranchii")) & !is.na(class))
# ------------------------------------------------------------------------------
## Step 2: Clean species names 
# ------------------------------------------------------------------------------
length(unique(OBSdat$scientific_name)) #159 names
length(unique(COMdat$scientific_name)) #146
#Argyrosomus thorpei change 
#Trichiurus lepturus TRICHIURIDAE

#REMOVE Teleostei NA, Teleostei redfish 
species_fix <- c(
  "Alectis NA" = "Alectis sp.",
  "Diplodus sargus capensis" = "Diplodus capensis",
  "Anguilliformes NA" = "Anguilliformes sp.",
  "Argyrops NA" = "Argyrops sp.",
  "Balistidae NA" = "Balistidae sp.",
  "Caranx NA" = "Caranx sp.",
  "Epinephelus NA" = "Epinephelus sp.",
  "Galeichthyes NA" = "Galeichthys sp.",
  "Istiophoridae NA" = "Istiophoridae sp.",
  "Labridae NA" = "Labridae sp.",
  "Lethrinus NA" = "Lethrinus sp.",
  "Oplegnathus NA" = "Oplegnathus sp.",
  "Parupeneus NA" = "Parupeneus sp.",
  "Plectorhynchus NA" = "Plectorhinchus sp.",
  "Pomadasys NA" = "Pomadasys sp.",
  "Rhabdosargus NA" = "Rhabdosargus sp.",
  "Scarus NA" = "Scarus sp.",
  "Scombridae NA" = "Scombridae sp.",
  "Sebastinae NA" = "Sebastinae sp.", 
  "Sphyraena NA" = "Sphyraena sp.",
  "Synodontidae NA" = "Synodontidae sp.",
  "Tetraodontidae NA" = "Tetraodontidae sp.",
  "Umbrina NA" = "Umbrina sp.",
  "Acantholatris vemae" = "Nemadactylus vemae", #Latridae
  "Chirocentris dorab" = "Chirocentrus dorab", #Chirocentridae
  "Crenidens Crenidens" = "Crenidens crenidens", #Sparidae
  "Epinephelus andersonii" = "Epinephelus andersoni", #Epinephelidae
  "Epinephelus longispinus" = "Epinephelus longispinis", #Epinephelidae
  "Lutjanus NA" = "Lutjanus sp.", #lutjanidae
  "Lutjanus russelli" = "Lutjanus russellii", #lutjanidae
  "Megalapsis cordyla" = "Megalaspis cordyla", #carangidae
  "Merluccius NA" = "Merluccius sp.", #
  "Plectorhinchus Chubbi" = "Plectorhinchus chubbi", #haemulidae
  "Plectorhynchus flavomaculatus" = "Plectorhinchus flavomaculatus", #haemulidae
  "Dichristius capensis" = "Dichistius capensis", #Sparidae
  "Coracinus capensis" = "Dichistius capensis", #Sparidae
  "Strongylura leiura" = "Strongylura strongylura",
  "Trachurus trachurus capensis" = "Trachurus trachurus",
  "Seriolella antarctica" = "Hyperoglyphe antarctica", #Centrolophidae
  "Dasyatis brevicaudatus" = "Dasyatis brevicaudata",
  "Diplodus cervinus hottentotus" = "Diplodus cervinus",
  "Diplodus hottentotus" = "Diplodus cervinus",
  "Galaxius zebratus" = "Galaxias zebratus",
  "Gobiesocidae sucker fish undescribed" = "Chorisochismus dentex",
  "Lagocephalus intermis" = "Lagocephalus inermis",
  "Lethrinus nebulosis" = "Lethrinus nebulosus",
  "Merluccius or Macruronus capensis" = "Macruronus capensis",
  "Monodactylus falciformes" = "Monodactylus falciformis",
  "Pachymetopon aenum" = "Pachymetopon aeneum",
  "Pomadasys commersonii" = "Pomadasys commersonnii",
  "Pomodasys olivaceum" = "Pomadasys olivaceum",
  "Aplolemichthys kingi" = "Apolemichthys kingi",
  "Epinephelus gauza" = "Epinephelus marginatus",
  "Cafrogobius caffer" = "Caffrogobius caffer",
  "Peroclinus laurentii" = "Pavoclinus laurentii",
  "Blenophis anguillaris" = "Blennophis anguillaris",
  "Johnius dussumieri" = "Johnius amblycephalus",
  "Caranx ignobolis" = "Caranx ignobilis",
  "Blenniella periopthalmus" = "Blenniella periophthalmus",
  "Gaidropsaurus capensis" = "Gaidropsarus capensis",
  "Argyrosomus hololepidotis" = "Argyrosomus hololepidotus",
  "Hyporhampus capensis" = "Hyporhamphus capensis",
  "Hyporphampus capensis" = "Hyporhamphus capensis",
  "Hyporhamphus knysnaensis" = "Hippocampus knysnaensis",
  "Rhabdosargus sasrba" = "Rhabdosargus sarba",
  "Chelinodon laticeps" = "Chelonodontops laticeps", 
  "Sphyraena chysotaenia" = "Sphyraena chrysotaenia",
  "Chemerius nufar" = "Cheimerius nufar",
  "Oxyurichthys opthalmonema" = "Oxyurichthys ophthalmonema",
  "Paralochthodes" = "Paralichthodes algoensis",
  "Parascorpius typus" = "Parascorpis typus",
  "Epinephilus guaza" = "Epinephelus marginatus",
  "Acabnthuris dussumieri" = "Acanthurus dussumieri",
  "Kyphosys vaigiensis" = "Kyphosus vaigiensis",
  "Gyymnothorax flavimarginatus" = "Gymnothorax flavimarginatus",
  "Epinephilus andersoni" = "Epinephelus andersoni",
  "Epinephilus marginatus" = "Epinephelus marginatus",
  "Epinephilus rivulatus" = "Epinephelus rivulatus",
  "Pseudanthis squamipinnis" = "Pseudanthias squamipinnis",
  "Polysterganus praeorbitalis" = "Polysteganus praeorbitalis",
  "Stromateus acus" = "Stromateus fiatola",
  "Bascanichthys mitsukurii" = "Bascanichthys kirkii",
  "Thalassoma interrupta" = "Stethojulis interrupta",
  "Clinus superciliosis" = "Clinus superciliosus",
  "Caffrogobius maxilaris" = "Caffrogobius agulhensis",
  "Istiblennius periopthalmus" = "Blenniella periophthalmus",
  "pagellus bellottii natalensis" = "Pagellus natalensis",
  "etrumeus teres" = "Etrumeus teres",
  "Trigloporus lastoviza africanus" = "Chelidonichthys lastoviza",
  "strongylura leiura" = "Strongylura strongylura",
  "Tripterygiidae gen. nov." = "Cremnochorites capensis",
  "Tripterygidae gen nov" = "Cremnochorites capensis",
  "caranx sexfasciatus" = "Caranx sexfasciatus",
  "Quisquilius cinctus" = "Priolepis cincta",
  "Tripterygiidae sp. gen. nov." = "Cremnochorites capensis",
  "Umbrina spp" = "Umbrina sp.",
  "Untidentified sp." = "Unidentified sp.",
  "trigla spp" = "Trigla sp.",
  "Plotosus nkunga" = "Plotosus lineatus",
  "Pavoclinus mus" = "Fucomimus mus",
  "Kuhlia taeniurus" = "Kuhlia mugil",
  "Synaptura marginata" = "Dagetichthys marginatus",
  "Blennius cornutus" = "Parablennius cornutus",
  "Pseudupeneus pleurotaenia" = "Parupeneus ciliatus",
  "Amblyrhynchotes hypselogenion" = "Torquigener hypselogeneion",
  "Monishia william" = "Coryogalops william",
  "Liza tricuspidens" = "Chelon tricuspidens",
  "Liza dumerili" = "Chelon dumerili",
  "Liza macrolepis" = "Planiliza macrolepis",
  "Liza alata" = "Planiliza alata",
  "Valamugil cunnesius" = "Osteomugil cunnesius",
  "Pagellus bellottii natalensis" = "Pagellus natalensis",
  "Gerres rappi" = "Gerres methurni",
  "Secutor ruconius" = "Deveximentum ruconius",
  "Epinephelus sp.iniger" = "Epinephelus marginatus",
  "Psenes whiteleggii" = "Cubiceps whiteleggii",
  "Drepane longimanus" = "Drepane longimana",
  "Dinematichthys sp." = "Dinematichthys iluocoeteoides",
  "Anthias squamipinnis" = "Pseudanthias squamipinnis",
  "Coris gaimard africana" = "Coris gaimard",
  "Sufflamen fraenatus" = "Sufflamen fraenatum",
  "Embolichthys mitsukurii" = "Bleekeria mitsukurii",
  "Grammonoides opisthodon" = "Grammonus opisthodon",
  "Trypauchen microcephalus" = "Paratrypauchen microcephalus",
  "Cheilinus bimaculatus" = "Oxycheilinus bimaculatus",
  "Mulloides flavolineatus" = "Mulloidichthys flavolineatus",
  "Solea fulvomarginata" = "Barnardichthys fulvomarginata",
  "Torquigener balteatus" = "Torquigener balteus",
  "Blenniella periophthalmus " = "Blenniella periophthalmus",
  "Pontogeloides latipes" = "Excirolana latipes",
  "Caranx sem" = "Caranx heberi",
  "Paralichthodes algoensis sp" = "Paralichthodes algoensis",
  "Chalixodytes chameleontoculis" = "Chalixodytes tauensis",
  "Drepane longimanus" = "Drepane longimana")

OBSdat <- OBSdat %>%
  mutate(scientific_name = str_replace_all(scientific_name, species_fix))
COMdat <- COMdat %>%
  mutate(scientific_name = str_replace_all(scientific_name, species_fix))

length(unique(OBSdat$scientific_name)) #155
length(unique(COMdat$scientific_name)) #144
# ------------------------------------------------------------------------------
## Step 3: fix orders and families
# ------------------------------------------------------------------------------

family_fix <- c(
  "Sebastidae" = "Scorpaenidae", 
  "Grammistidae" = "Epinephelidae", 
  "Gaidropsaridae" = "Lotidae", 
  "Malacanthidae" = "Branchiostegidae", 
  "Ehiravidae" = "Clupeidae", 
  "Dussumieriidae" = "Clupeidae",
  "Dorosomatidae" = "Clupeidae", 
  "Alosidae" = "Clupeidae", 
  "Dichistiidae" = "Sparidae",      # Galjoen now Sparidae
  "Coracinidae" = "Sparidae",       # Spicara spp. → Sparidae
  "Centracanthidae" = "Sparidae",
  "Scorpidae" = "Kyphosidae",     # Neoscorpis lithophilus → Kyphosidae
  "Drepanidae" = "Drepaneidae"      # Drepane longimana → Drepaneidae
)

COMdat <- COMdat %>%
  mutate(family = recode(family, !!!family_fix))
OBSdat <- OBSdat %>%
  mutate(family = recode(family, !!!family_fix))

order_corrections <- tribble(
  ~Family,           ~CorrectedOrder,
  "Acanthuridae",         "Perciformes",
  "Chaetodontidae",         "Perciformes",
  "Drepaneidae",         "Perciformes",
  "Ephippidae",         "Perciformes",
  "Leiognathidae",         "Perciformes",
  "Lobotidae",         "Perciformes",
  "Pomacanthidae",         "Perciformes",
  "Siganidae ",         "Perciformes",
  "Creediidae",         "Perciformes",
  "Pempheridae",         "Perciformes",
  "Scombropidae",         "Perciformes",
  "Blenniidae",         "Perciformes",
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
  "Bramidae",        "Perciformes",
  "Xiphiidae",       "Perciformes",
  "Coracinidae",     "Spariformes",   # old Galjoen name (Dichistius capensis) → Dichistiidae (Spariformes)
  "Chanidae",        "Gonorhynchiformes",
  "Elopidae",        "Elopiformes",
  "Drepanidae",      "Perciformes",   # Drepane longimana (spadefish)
  "Scorpidae",       "Perciformes",   # Neoscorpis lithophilus → Kyphosidae (Perciformes)
  "Albulidae",       "Albuliformes",
  "Mugilidae",         "Mugiliformes"
)
#remove NA

# Apply order_corrections to COMdat
COMdat <- COMdat %>%
  left_join(order_corrections, by = c("family" = "Family")) %>%
  mutate(
    tax_order = if_else(!is.na(CorrectedOrder), CorrectedOrder, tax_order)
  ) %>%
  select(-CorrectedOrder)

# Apply order_corrections to OBSdat
OBSdat<- OBSdat %>%
  left_join(order_corrections, by = c("family" = "Family")) %>%
  mutate(
    tax_order = if_else(!is.na(CorrectedOrder), CorrectedOrder, tax_order)
  ) %>%
  select(-CorrectedOrder)

# ------------------------------------------------------------------------------
## Step 4 quick view of distinct species with orders and families
# ------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#inspect species and get unique numbers
#-------------------------------------------------------------------------------
#final clean up
OBSdat <- OBSdat %>%
  filter(!scientific_name %in% c("Teleostei NA", "Teleostei redfish")) 

COMdat <- COMdat %>%
  filter(!scientific_name %in% c("Teleostei NA", "Teleostei redfish", "Anguilliformes NA"))
    
length(unique(COMdat$scientific_name)) #142 species
length(unique(OBSdat$scientific_name)) #156 species
#-------------------------------------------------------------------------------
# recheck unmatched
unmatched_COM <- COMdat %>%
  filter(is.na(family) | is.na(tax_order)) %>%
  distinct(family, tax_order)

unmatched_OBS <- OBSdat %>%
  filter(is.na(family) | is.na(tax_order)) %>%
  distinct(family, tax_order)

print(unmatched_COM)
print(unmatched_OBS)

COMdat <- COMdat %>%
  mutate(
    family = if_else(is.na(family) & tax_order == "Anguilliformes", 
                     "Anguillidae", family)
  )
#-------------------------------------------------------------------------------
COMdat <- COMdat %>%
  filter(!is.na(scientific_name))

OBSdat <- OBSdat %>%
  filter(!is.na(scientific_name))
#-------------------------------------------------------------------------------
COM_species <- COMdat %>%
  distinct(scientific_name) %>%
  filter(!is.na(scientific_name))

OBS_species <- OBSdat %>%
  distinct(scientific_name) %>%
  filter(!is.na(scientific_name))

nrow(COM_species) #142 species
nrow(OBS_species) #156 species

# Combine both lists and find distinct species
all_species <- bind_rows(COM_species, OBS_species) %>%
  distinct(scientific_name) %>%
  arrange(scientific_name)

# View all distinct species
print(all_species, n = Inf) #169 in total
#### 
COMdat <- COMdat %>%
  mutate(
    scientific_name = case_when(
      scientific_name == "Alectis sp." ~ "Alectis ciliaris",
      scientific_name == "Epinephelus andersonii" ~ "Epinephelus andersoni",
      scientific_name == "Epinephelus longispinus" ~ "Epinephelus longispinis",
      scientific_name == "Galeichthyes sp." ~ "Galeichthys sp.",
      scientific_name == "Johnius dussumieri" ~ "Johnius amblycephalus",
      scientific_name == "Seriolella antarctica" ~ "Hyperoglyphe antarctica",
      scientific_name == "Diplodus cervinus hottentotus" ~ "Diplodus cervinus",
      TRUE ~ scientific_name
    ))
OBSdat <- OBSdat %>%
  mutate(
    scientific_name = case_when(
      scientific_name == "Alectis sp." ~ "Alectis ciliaris",
      scientific_name == "Epinephelus andersonii" ~ "Epinephelus andersoni",
      scientific_name == "Epinephelus longispinus" ~ "Epinephelus longispinis",
      scientific_name == "Galeichthyes sp." ~ "Galeichthys sp.",
      scientific_name == "Johnius dussumieri" ~ "Johnius amblycephalus",
      scientific_name == "Seriolella antarctica" ~ "Hyperoglyphe antarctica",
      scientific_name == "Diplodus cervinus hottentotus" ~ "Diplodus cervinus",
      TRUE ~ scientific_name
    ))
#SPECIES CLEAN
###
#-------------------------------------------------------------------------------
###CHECK UNIQUE SPP AND GENUS AND SCIENTIFIC NAME

unique(OBSdat$species_code)
unique(OBSdat$species_name)
unique(OBSdat$genus)
unique(OBSdat$scientific_name)
unique(COMdat$species_code)
unique(COMdat$species_name)
unique(COMdat$genus)
unique(COMdat$scientific_name)
#-------------------------------------------------------------------------------
#rename columns
#-------------------------------------------------------------------------------
colnames(OBSdat)
OBSdat <- OBSdat %>%
  rename(
    sampling_site = samp_locality,
    order = tax_order,
    longitude = g_long,
    latitude = g_lat
  ) %>%
  select(
    date,
    obs_date,
    sampling_site,
    area_code,
    shore_dist,
    depth,
    number,
    data_source,
    gear_std,
    class,
    order,
    family,
    genus,
    species_name,
    scientific_name,
    grid_id,
    longitude,
    latitude,
    locality_name,
    year
  )
colnames(COMdat)
COMdat <- COMdat %>%
  rename(
    order = tax_order,
    sampling_site = locality,
    longitude = g_long,
    latitude = g_lat
  ) %>%
  select(
    year,
    sampling_site,
    scientific_name,
    species_name,
    shore_dist,
    data_source,
    gear_std,
    class,
    order,
    family,
    genus,
    grid_id,
    longitude,
    latitude,
    locality_name,
    date,
    catch_year
  )

OBSdat <- OBSdat %>%
  select(-depth, -number)
#-------------------------------------------------------------------------------
#check for NAs in lat and lon
#-------------------------------------------------------------------------------
sum(is.na(OBSdat$latitude)) #87
sum(is.na(COMdat$latitude)) #5105

sum(is.na(OBSdat$longitude)) #87
sum(is.na(COMdat$longitude)) #5105

COMdatmissing_coords <- COMdat %>%
  filter(is.na(latitude) | is.na(longitude))
#NA 6713 6647 6855 6591 7404 9999 3162 3090 3230 3039 2952 2975 3260 3210 7423 7413 7397 7409
#followed up with sven on this, need to fill in coords for those sampling sites for COMdat and remove NA, 

unique(COMdatmissing_coords$sampling_site)

OBSdatmissing_coords <- OBSdat %>%
  filter(is.na(latitude) | is.na(longitude))
unique(OBSdatmissing_coords$sampling_site)
#3961 = glong 31,20833333	glat -29,875
#4085  = glong 30,54166667 glat	-30,875
#3709 = glong 32,54166667	glat -28,125

obs_site_coords <- tibble(
  sampling_site = c(3961, 4085, 3709),
  longitude = c(31.20833333, 30.54166667, 32.54166667),
  latitude  = c(-29.875, -30.875, -28.125)
)

OBSdat <- OBSdat %>%
  left_join(obs_site_coords,
            by = "sampling_site",
            suffix = c("", "_new")) %>%  # keep old cols as longitude/latitude
  mutate(
    longitude = if_else(is.na(longitude), longitude_new, longitude),
    latitude  = if_else(is.na(latitude),  latitude_new,  latitude)
  ) %>%
  select(-longitude_new, -latitude_new)
#obsdat fixed now
#-------------------------------------------------------------------------------
#tax res columns
#-------------------------------------------------------------------------------
OBSdat <- OBSdat %>%
  mutate(
    taxonomic_resolution = case_when(
      is.na(species_name) ~ "genus",
      str_detect(genus, "dae") ~ "family",
      str_detect(genus, "formes") ~ "order",
      str_detect(scientific_name, "\\bsp\\.") ~ "genus",
      TRUE ~ "species"
    )
  )
COMdat <- COMdat %>%
  mutate(
    taxonomic_resolution = case_when(
      is.na(species_name) ~ "genus",
      str_detect(genus, "dae") ~ "family",
      str_detect(genus, "formes") ~ "order",
      str_detect(scientific_name, "\\bsp\\.") ~ "genus",
      TRUE ~ "species"
    )
  )
#-------------------------------------------------------------------------------
#PLOTS
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# tax res bar plot OBS, COM 
#-------------------------------------------------------------------------------
taxon_summary_OBS <- OBSdat %>%
  count(taxonomic_resolution)
taxon_summary_COM <- COMdat %>%
  count(taxonomic_resolution)
# OBS
ggplot(taxon_summary_OBS, aes(x = reorder(taxonomic_resolution, -n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 3, family = "Times") +
  labs(
    title = "Record Count by Taxonomic Resolution (Observer Line)",
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
# COM
ggplot(taxon_summary_COM, aes(x = reorder(taxonomic_resolution, -n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 3, family = "Times") +
  labs(
    title = "Record Count by Taxonomic Resolution (Commercial Line)",
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
#2 top 20 spp for OBS, COM
#-------------------------------------------------------------------------------
#OBS
top_speciesOBS <- OBSdat %>%
  filter(!is.na(scientific_name)) %>%
  count(scientific_name, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_speciesOBS, aes(x = reorder(scientific_name, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequently Recorded Species (Observer Linefish Data)",
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

#COM
top_speciesCOM <- COMdat %>%
  filter(!is.na(scientific_name)) %>%
  count(scientific_name, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_speciesCOM, aes(x = reorder(scientific_name, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequently Recorded Species (Commercial Linefish Data)",
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
#3 top 20 fam for OBS, COM
#-------------------------------------------------------------------------------
#OBS
top_famOBS <- OBSdat %>%
  filter(!is.na(family)) %>%
  count(family, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_famOBS, aes(x = reorder(family, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequently Recorded Families (Observer Linefish Data)",
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

#COM
top_famCOM <- COMdat %>%
  filter(!is.na(family)) %>%
  count(family, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_famCOM, aes(x = reorder(family, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequently Recorded Families (Commercial Linefish Data)",
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
# 4 spatial dist for OBS, COM
#-------------------------------------------------------------------------------
# Spatial Distribution of Species Records

world <- ne_countries(scale = "medium", returnclass = "sf")
sa_map <- world %>% filter(admin == "South Africa")
eez <- st_read("MAPPING/EEZ/eez_v12.shp", quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(4326)

#OBS
ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_point(data = OBSdat, aes(x = longitude, y = latitude), 
             color = "darkblue", alpha = 0.6, size = 1.5) +
  coord_sf(xlim = c(10, 35), ylim = c(-40, -20), expand = FALSE) +
  labs(
    title = "Record Distribution of OBSERVER linefish data",
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

#by source
ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_point(
    data = OBSdat,
    aes(x = longitude, y = latitude, color = gear_std),
    alpha = 0.6, size = 1.5
  ) +
  coord_sf(xlim = c(0, 40), ylim = c(-50, -20), expand = FALSE) +
  labs(
    title = "OBSERVER linefish data Distribution by gear ",
    x = "Longitude", y = "Latitude", color = "gear_std"
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

unique(OBSdat$gear_std)

#why are the others not reflecting
#COM
ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_point(data = COMdat, aes(x = longitude, y = latitude), 
             color = "darkblue", alpha = 0.6, size = 1.5) +
  coord_sf(xlim = c(10, 35), ylim = c(-40, -20), expand = FALSE) +
  labs(
    title = "Record Distribution of COMMERCIAL linefish data",
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


#by source
ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_point(
    data = COMdat,
    aes(x = longitude, y = latitude, color = gear_std),
    alpha = 0.6, size = 1.5
  ) +
  coord_sf(xlim = c(0, 40), ylim = c(-50, -20), expand = FALSE) +
  labs(
    title = "COMMERCIAL linefish dat by gear",
    x = "Longitude", y = "Latitude", color = "gear_std"
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
write.csv(OBSdat, "~/Desktop/wd/masters/outputs/line/OBSLINEDAT.csv", row.names = FALSE)
write.csv(COMdat, "~/Desktop/wd/masters/outputs/line/COMLINEDAT.csv", row.names = FALSE)

OBSdat <- read.csv("outputs/line/OBSLINEDAT.csv")
COMdat <- read.csv("outputs/line/COMLINEDAT.csv")
#-------------------------------------------------------------------------------
#OUTPUTS
#-------------------------------------------------------------------------------
#species number for OBS, COM, 
#-------------------------------------------------------------------------------
length(unique(OBSdat$scientific_name)) #155
length(unique(COMdat$scientific_name)) #142
#family number for OBS, COM, 
#-------------------------------------------------------------------------------
length(unique(OBSdat$family)) #46
length(unique(COMdat$family)) #40
#spatial coverage
#-------------------------------------------------------------------------------
summary(OBSdat$latitude)
summary(OBSdat$longitude)

summary(COMdat$latitude)
summary(COMdat$longitude)

#-------------------------------------------------------------------------------
#COAST COVERAGE
#-------------------------------------------------------------------------------
#OBS
OBSdat <- OBSdat %>%
  mutate(
    coast = case_when(
      longitude >= 14 & longitude < 20 & latitude <= -28 & latitude >= -36.5 ~ "west",
      longitude >= 20 & longitude < 27 & latitude <= -33 & latitude >= -36.5 ~ "south",
      longitude >= 27 & longitude <= 35 & latitude <= -26 & latitude >= -35 ~ "east",
      TRUE ~ "west"   # force anything else into West
    )
  )

obs_coast_summary <- OBSdat %>%
  group_by(coast) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(scientific_name),
    .groups = "drop"
  )
print(obs_coast_summary)
# coast     n_records   n_species
# East      25335       143
# South     16884        63
# West      20969       54

#year sorted
sorted_year_obs <- sort(unique(OBSdat$year), na.last = TRUE)
print(sorted_year_obs)
# 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999
#2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010

#gear
unique(OBSdat$gear_std)

obs_gear_summary <- OBSdat %>%
  group_by(gear_std) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(scientific_name),
    .groups = "drop"
  )
print(obs_gear_summary)
# gear_std      n_records   n_species
# LINE              62287       154
# POLE                794        30
# SHORTHAND_ROD        52        12
# SHORT_SPEAR           6         4
# SPEAR                49        30

#COM
COMdat <- COMdat %>%
  mutate(
    coast = case_when(
      longitude >= 14 & longitude < 20 & latitude <= -28 & latitude >= -36.5 ~ "west",
      longitude >= 20 & longitude < 27 & latitude <= -33 & latitude >= -36.5 ~ "south",
      longitude >= 27 & longitude <= 35 & latitude <= -26 & latitude >= -35 ~ "east",
      TRUE ~ "west"   # force anything else into West
    )
  )
# Summarise records and species per coast
com_coast_summary <- COMdat %>%
  group_by(coast) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(scientific_name),
    .groups = "drop"
  )
print(com_coast_summary)
#coast    n_records   n_species
#East     229011       123
#South    208944        71
#West     321068        88

#year sorted
sorted_year_com <- sort(unique(COMdat$year), na.last = TRUE)
print(sorted_year_com)
#1985 1986 1987 2000 2001 2002 2003 2004 2005 2006 2014 2015 2016 2017

#gear
com_gear_summary <- COMdat %>%
  group_by(gear_std) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(scientific_name),
    .groups = "drop"
  )
print(com_gear_summary)
#  gear_std n_records n_species
# LINE        724769       142
# POLE         34254        35

#-------------------------------------------------------------------------------
#COMBINE OBS and LINE

#-------------------------------------------------------------------------------
# Step 1: Standardize column names and fill in missing columns
OBSdat_clean <- OBSdat %>%
  mutate(
    sample_date = as.Date(obs_date),   # ensure Date
    obs_date = as.Date(obs_date),      # force to Date if it's not already
    catch_site = NA_real_,
    catch_year = year
  ) %>%
  select(
    date, sample_date, obs_date, year, catch_year,
    sampling_site, catch_site, area_code,
    shore_dist, data_source, gear_std,
    class, order, family, genus,
    species_name, scientific_name,
    grid_id, longitude, latitude,
    locality_name, taxonomic_resolution, coast
  )

COMdat_clean <- COMdat %>%
  mutate(
    sample_date = as.Date(NA),
    obs_date = as.Date(NA),
    catch_site = NA_real_,
    area_code = NA_character_
  ) %>%
  select(
    date, sample_date, obs_date, year, catch_year,
    sampling_site, catch_site, area_code,
    shore_dist, data_source, gear_std,
    class, order, family, genus,
    species_name, scientific_name,
    grid_id, longitude, latitude,
    locality_name, taxonomic_resolution, coast
  )

# Merge
LINEdat <- bind_rows(OBSdat_clean, COMdat_clean)
#species number for combined
length(unique(LINEdat$scientific_name)) #168
#family number for combined
length(unique(LINEdat$family)) #49
#spatial coverage
#summary
sorted_year_line<- sort(unique(LINEdat$year), na.last = TRUE)
print(sorted_year_line)
# 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998
# 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2014 2015
# 2016 2017

line_gear_summary <- LINEdat %>%
  group_by(gear_std) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(scientific_name),
    .groups = "drop"
  )
print(line_gear_summary)
#gear_std      n_records n_species
# LINE             787056       167
# POLE              35048        46
# SHORTHAND_ROD        52        12
# SHORT_SPEAR           6         4
# SPEAR                49        30

# Summarise records and species per coast
line_coast_summary <- LINEdat %>%
  group_by(coast) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(scientific_name),
    .groups = "drop"
  )
print(line_coast_summary)
#  coast n_records n_species
# East     254346       154
# South    225828        91
# West     342037       93

#spatial coverage
summary(LINEdat$longitude)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#15.67   18.46   21.46   23.37   28.92   33.69    5105  
summary(LINEdat$latitude)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-35.73  -34.97  -34.20  -33.45  -31.70  -26.88    5105 


# Summary of coordinate completeness
colnames(LINEdat)
coord_summary <- LINEdat %>%
  mutate(
    coord_missing = case_when(
      is.na(latitude) & is.na(longitude) ~ "Both missing",
      is.na(latitude) ~ "Latitude missing",
      is.na(longitude) ~ "Longitude missing",
      TRUE ~ "Complete"
    )
  ) %>%
  group_by(locality_name, gear_std, coord_missing) %>%
  summarise(
    n_records = n(),
    .groups = "drop"
  ) %>%
  arrange(locality_name, gear_std, desc(n_records))

# View full summary
print(coord_summary, n = Inf)

write.csv(coord_summary, "~/Desktop/wd/masters/missingcoordinatesNMLS.csv", row.names = FALSE)

# Optional: quick overview counts
coord_overview <- coord_summary %>%
  group_by(coord_missing) %>%
  summarise(total_records = sum(n_records), .groups = "drop")

print(coord_overview)
#  coord_missing total_records
# Both missing           5192
# Complete             817019

#-------------------------------------------------------------------------------
# PLOTS
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# tax res bar plot 
#-------------------------------------------------------------------------------
taxon_summary_LINE <- LINEdat %>%
  count(taxonomic_resolution)

ggplot(taxon_summary_LINE, aes(x = reorder(taxonomic_resolution, -n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 3, family = "Times") +
  labs(
    title = "Record Count by Taxonomic Resolution for Linefish Data",
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

# top 20 spp for combined
top_sppline <- LINEdat %>%
  filter(!is.na(scientific_name)) %>%
  count(scientific_name, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_sppline, aes(x = reorder(scientific_name, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequently Recorded Species in Linefish Data",
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

# top 20 fam for combined
top_famline <- LINEdat %>%
  filter(!is.na(family)) %>%
  count(family, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_famline, aes(x = reorder(family, n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequently Recorded Families in Linefish Data",
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

# spatial dist for combined
class(sa_map)
class(eez)
class(LINEdat)

LINEdat_plot <- line_dat %>% 
  filter(!is.na(longitude) & !is.na(latitude)) #find these coordinates

ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_point(data = LINEdat_plot, aes(x = longitude, y = latitude), 
             color = "navyblue", alpha = 0.3, size = 0.5) +
  coord_sf(xlim = c(10, 35), ylim = c(-40, -25), expand = FALSE) +
  labs(
    title = "Record Distribution of NMLS Linefish Occurence Data",
    x = "Longitude", y = "Latitude"
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

#by gear
ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_point(
    data = LINEdat_plot,
    aes(x = longitude, y = latitude, color = gear_std),
    alpha = 0.6, size = 1.5
  ) +
  coord_sf(xlim = c(10, 38), ylim = c(-38, -25), expand = FALSE) +
  labs(
    title = "Record Distribution of NMLS Linefish Occurence Data by Gear Used",
    x = "Longitude", y = "Latitude", color = "gear_std"
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

#by DATA source
LINEdat <- LINEdat %>%
  mutate(
    data_source = case_when(
      str_starts(data_source, "OBS") ~ "OBS",
      str_starts(data_source, "COM") ~ "COM",
      TRUE ~ data_source  # Keep as-is if unmatched (optional fallback)
    )
  )

line_source_summary <- LINEdat %>%
  group_by(data_source) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(scientific_name),
    .groups = "drop"
  )

print(line_source_summary)
#  data_source.    n_records   n_species
  # COM            759023        142
  # OBS            63188         155



write.csv(LINEdat, "~/Desktop/wd/masters/RESULTSPLOTS_NOVEMBER/NMLS.csv", row.names = FALSE)


LINEdat <- read.csv("outputs/line/LINEDAT.csv")

ggplot() +
  geom_sf(data = sa_map, fill = "gray90", color = "black") +
  geom_sf(data = eez, fill = NA, color = "red") +
  geom_point(
    data = LINEdat_plot,
    aes(x = longitude, y = latitude, color = data_source),
    alpha = 0.6, size = 1.5
  ) +
  coord_sf(xlim = c(10, 38), ylim = c(-38, -25), expand = FALSE) +
  labs(
    title = "Linefish data by source",
    x = "Longitude", y = "Latitude", color = "Data Source" #change to source of data think "BIO,OBS,COM"
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

#5 spatial dist by coast for combined


