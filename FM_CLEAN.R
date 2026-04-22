###### Cleaning Species names in FMdat
## 2nd script in FM series (after load)
# author: "Savannah Anderson"

# ------------------------------------------------------------------------------

setwd("/Users/savannahanderson/Desktop/wd/masters")
getwd()
 
##install packages and load libraries
install.packages("createDB")
install.packages("tidyverse")
library(createDB)
library(tidyverse)
library(dplyr)
library(sf)
library(ggplot2)
library(worrms)
library(rnaturalearth)
library(rnaturalearthdata)
# ------------------------------------------------------------------------------
## Step 1: Clean species names 
# ------------------------------------------------------------------------------

length(unique(final_data_joined$Species)) #942 names

sort(unique(final_data_joined$Species))

# Clean species names in the final_data 
# STEP 1 IS TO REMOVE ALL OBVIOUS ELASMOS

elasmos <- c(
  "Charcharhinus limbatus","Carcharius taurus","Carcharhinidae spp","Carcharhinus brachyurus",
  "Carcharhinus cautus","Carcharhinus leucas","Carcharhinus brevipinna","Carcharhinus limbatus",
  "Carcharhinus obscurus","Carcharhinus plumbeus","Carcharhinus spp","Carcharias taurus",
  "Carcharodon carcharias","Carcharhinus sealei","Squalomorphea","Squalidae spp",
  "Squalomorphea spp","Squalus acutipinnis","Squalus megalops","Squalus acanthias",
  "Rhinobatis annulatus","Rhinobatus holcorhynchus","Rhinobatidae spp","Rhinobatos annulatus",
  "Rhinobatos leucospilus","Rhinobatos spp","Rhinobatos blochi","Hexanchidae spp","Mustelus mustelus",
  "Mustelus palumbes","Mustelus spp","Raja rhizacanthus","Raja alba","Raja clavata","Raja miraletus",
  "Raja straeleni","Raja pullopunctata","Raja wallacei","Isurus oxyrinchus","Galeocerdo cuvier",
  "Sphyrna lewini","Sphyrna sp.","Sphyrna spp","Sphyrna zygaena","Halaelurus lineatus",
  "Halaelurus natalensis","Haploblepharus edwardsii","Haploblepharus fuscus","Haploblepharus pictus",
  "Haploblepharus spp","Prionace glauca","Poroderma africanum","Poroderma pantherinum","Poroderma spp",
  "Galeorhinus galeus","Scylliogaleus quecketii","Scylliogaleus quecketti","Triakidae spp",
  "Triakis megalopterus","Stegostoma fasciatum","Pliotrema warreni","Notorynchus cepedianus",
  "Myliobatidae spp","Myliobatiformes sp.","Myliobatis spp","Myliobatis cervus","Myliobatis aquila",
  "Himantura gerrardi","Himantura uarnak","Gymnura natalensis","Dasyatidae sp","Dasyatidae spp",
  "Dasyatis brevicaudatus","Dasyatis chrysonota","Dasyatis marmorata","Dasyatis brevicaudata",
  "Dasyatis kuhlii","Dasyatis pastinaca","Dasyatis spp","Dasyatis pastinacus","Dasyatis thetidis",
  "Torpedo fuscomaculata","Torpedo marmorata","Torpedo sinuspersici","Pteromyaeus bovinus",
  "Schindleria pitschmanni","Pteromylaeus bovinus","Batoidea spp","Chondrichthyes spp",
  "Rhynchobatus djiddensis","Narke capensis","Electrolux addisoni","Taeniura melanospilos",
  "Alopias vulpinus","Rhinoptera javanica","Scyliorhinus capensis","Rostroraja alba",
  "Callorhinchus capensis","Eptatretus hexatrema")
freshwater <- c(
  "Labeo umbratus","Labeo annulatus","Micropterus salmoides","Micropterus punctulatus",
  "Micropterus dolomieu","Micopterus spp","Sandelia capensis","Galaxias zebratus",
  "Galaxias nebratus","Barbus anoplus","Barbus afer","Barbus tenuis","Barbus serra",
  "Tilapia sparrmanii","Lepomis macrochirus","Clarias gariepinus","Clarius gariepinus",
  "Cyrpinus carpio","Cyprinidae","Cyprinus spp","Cyprinus carpio","Tinca tinca","Micopterus",
  "Eugomphodus taurus","Gambusia affinis")

elasmos <- unique(elasmos)
freshwater <- unique(freshwater)

# Remove all elasmobranch + freshwater species in one go
final_data_cleaned <- final_data_joined %>%
  filter(!tolower(Species) %in% tolower(c(elasmos, freshwater, "Osteichthyes spp",
                                          "Rhynchobatus spp", "#N/A", 
                                          "Unidentified", "No species present")))


#FOR RENAMING 
species_fix <- c(
                    "Dasyatis brevicaudatus" = "Dasyatis brevicaudata",
                    "Dichristius capensis" = "Dichistius capensis",
                    "Coracinus capensis" = "Dichistius capensis",
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
                    "Chalixodytes chameleontoculis" = "Chalixodytes tauensis")
 
replace_lookup <- c(
  "Abudefduf spp" = "Abudefduf sp.", 
  "Acanthopagrus spp" = "Acanthopagrus sp.", 
  "Acanthuridae sp" = "Acanthuridae sp.", 
  "Acanthurus spp" = "Acanthurus sp.", 
  "Alectis sp" = "Alectis sp.", 
  "Alectis spp" = "Alectis sp.",
  "Ambassis sp" = "Ambassis sp.", 
  "Ambassis spp" = "Ambassis sp.", 
  "Anguilla spp" = "Anguilla sp.", 
  "Apogon sp 1" = "Apogon sp.", 
  "Apogon sp 2" = "Apogon sp.", 
  "Apogon sp 3" = "Apogon sp.", 
  "Apogon spp" = "Apogon sp.", 
  "Archamia sp" = "Archamia sp.", 
  "Argyrosomus sp" = "Argyrosomus sp.", 
  "Argyrosomus spp" = "Argyrosomus sp.", 
  "Auxis sp" = "Auxis sp.",
  "Blenniid 1" = "Blenniidae sp.",
  "Blenniid 2" = "Blenniidae sp.",
  "Blenniid 6" = "Blenniidae sp.",
  "Blenniid 7" = "Blenniidae sp.",
  "Blenniidae" = "Blenniidae sp.",
  "Bathygobius spp" = "Bathygobius sp.", 
  "Belonidae sp" = "Belonidae sp.", 
  "Benthosema sp" = "Benthosema sp.",
  "Benthosema sp 1" = "Benthosema sp.",
  "Blenniidae spp" = "Blenniidae sp.", 
  "Bothus spp" = "Bothus sp.",
  "Caffrogobius spp" = "Caffrogobius sp.",
  "Callionymid 3" = "Callionymidae sp.",
  "Callogobius spp" = "Callogobius sp.",
  "Carangid 1" = "Carangidae sp.",
  "Carangidae sp" = "Carangidae sp.",
  "Carangidae spp" = "Carangidae sp.",
  "Carangoides sp 1" = "Carangoides sp.",
  "Caranx sp" = "Caranx sp.",
  "Caranx spp" = "Caranx sp.",
  "Ceratias sp" = "Ceratias sp.",
  "Ceratoscopelus sp" = "Ceratoscopelus sp.",
  "Cheilodactylus spp" = "Cheilodactylus sp.",
  "Chelidonichthys spp" = "Chelidonichthys sp.",
  "Clinid 1" = "Clinidae sp.",
  "Clinidae" = "Clinidae sp.",
  "Clinidae spp" = "Clinidae sp.",
  "Clinus spp" = "Clinus sp.",
  "Clupeid 1" = "Clupeidae sp.",
  "Clupeid 2" = "Clupeidae sp.",
  "Cociella spp" = "Cociella sp.",
  "Congrid 1" = "Congridae sp.",
  "Cubiceps sp 1" = "Cubiceps sp.",
  "Cynoglossid 1" = "Cynoglossidae sp.",
  "Cynoglossid 2" = "Cynoglossidae sp.",
  "Cynoglossus sp 1" = "Cynoglossus sp.",
  "Decapteris sp 2" = "Decapterus sp.",
  "Decapterus sp 1" = "Decapterus sp.",
  "Diaphus sp 1" = "Diaphus sp.",
  "Diaphus sp 2" = "Diaphus sp.",
  "Diaphus sp 3" = "Diaphus sp.",
  "Diaphus sp 4" = "Diaphus sp.",
  "Dinematichthys sp" = "Dinematichthys sp.",
  "Eleotrid 1" = "Eleotridae sp.",
  "Eleotrid 3" = "Eleotridae sp.",
  "Eleotrid spp" = "Eleotridae sp.",
  "Eleotris spp" = "Eleotris sp.",
  "Engraulidae" = "Engraulidae sp.",
  "Enneapterygius sp" = "Enneapterygius sp.",
  "Epinephelus sp" = "Epinephelus sp.",
  "Epinephelus sp 1" = "Epinephelus sp.",
  "Epinephilus sp." = "Epinephelus sp.",
  "Epinephelus spp" = "Epinephelus sp.",
  "Galeichthys sp" = "Galeichthys sp.", 
  "Galeichthys spp" = "Galeichthys sp.",
  "Gerres sp 1" = "Gerres sp.",
  "Gerres spp" = "Gerres sp.", 
  "Glossogobius spp" = "Glossogobius sp.",
  "Gobidae" = "Gobiidae sp.",
  "Gobidae sp" = "Gobiidae sp.", 
  "Gobiesocid 1" = "Gobiesocidae sp.",
  "Gobiesocid 2" = "Gobiesocidae sp.",
  "Gobiesocidae" = "Gobiesocidae sp.",
  "Gobiesocidae spp" = "Gobiesocidae sp.",
  "Gobiid" = "Gobiidae sp.",
  "Gobiid 10" = "Gobiidae sp.",
  "Gobiid 11" = "Gobiidae sp.",
  "Gobiid 15" = "Gobiidae sp.",
  "Gobiid 2" = "Gobiidae sp.",
  "Gobiid 21" = "Gobiidae sp.",
  "Gobiid 22" = "Gobiidae sp.",
  "Gobiid 23" = "Gobiidae sp.",
  "Gobiid 25" = "Gobiidae sp.",
  "Gobiid 26" = "Gobiidae sp.",
  "Gobiid 4" = "Gobiidae sp.",
  "Gobiid 5" = "Gobiidae sp.",
  "Gobiid 6" = "Gobiidae sp.",
  "Gobiid24" = "Gobiidae sp.",
  "Gobiidae sp" = "Gobiidae sp.",
  "Gobiidae spp" = "Gobiidae sp.",
  "Gymnothorax sp" = "Gymnothorax sp.",  
  "Gymnothorax spp" = "Gymnothorax sp.",
  "Haemulid 1" = "Haemulidae sp.",
  "Haemulidae spp" = "Haemulidae sp.", 
  "Halichoeres spp" = "Halichoeres sp.",
  "Hippocampus sp" = "Hippocampus sp.", 
  "Hippocampus spp" = "Hippocampus sp.",
  "Istiblennius so" = "Istiblennius sp.",
  "Labrid 10" = "Labridae sp.",
  "Labrid 11" = "Labridae sp.",
  "Labrid 12" = "Labridae sp.",
  "Labrid 13" = "Labridae sp.",
  "Labrid 14" = "Labridae sp.",
  "Labrid 15" = "Labridae sp.",
  "Labrid 4" = "Labridae sp.",
  "Labrid 7" = "Labridae sp.",
  "Labridae" = "Labridae sp.",
  "Labridae sp" = "Labrid sp.", 
  "Labridae spp" = "Labrid sp.",
  "Lampanyctus sp 1" = "Lampanyctus sp.",
  "Lepadichthys sp 1" = "Lepadichthys sp.",
  "Lepadichthys sp 2" = "Lepadichthys sp.",
  "Lethrinus sp" = "Lethrinus sp.",
  "Letrinus spp" = "Lethrinus sp.",
  "Lithognathus spp" = "Lithognathus sp.", 
  "Liza spp" = "Liza sp.",
  "Lutjanid 1" = "Lutjanidae sp.",
  "Lutjanid 2" = "Lutjanidae sp.",
  "Lutjanidae spp" = "Lutjanidae sp.",
  "Lutjanus sp" = "Lutjanus sp.", 
  "Lutjanus spp." = "Lutjanus sp.",
  "Melanostomiid 1" = "Melanostomiidae sp.",
  "Minous sp" = "Minous sp.",
  "Monacanthid 2" = "Monacanthidae sp.",
  "Monacanthidae" = "Monacanthidae sp.",
  "Monodactylus spp" = "Monodactylus sp.",
  "Mugil spp" = "Mugilidae sp.",
  "Mugilid" = "Mugilidae sp.",
  "Mugilid 1" = "Mugilidae sp.",
  "Mugilidae" = "Mugilidae sp.",
  "Mugilidae spp" = "Mugilidae sp.",
  "Muglidae sp." = "Mugilidae sp.",
  "Mullid 2" = "Mugilidae sp.",
  "Mullid 3" = "Mugilidae sp.",
  "Mullidae spp" = "Mullidae sp.",
  "Muraenesocidae" = "Muraenesocidae sp.",
  "Muraenesocidae sp" = "Muraenesocidae sp.",
  "Muraenidae spp" = "Muraenidae sp.",
  "Myctophid 1" = "Myctophidae sp.",
  "Myctophid 2" = "Myctophidae sp.",
  "Myctophid 3" = "Myctophidae sp.",
  "Myctophid 4" = "Myctophidae sp.",
  "Myctophid 5" = "Myctophidae sp.",
  "Myctophid 6" = "Myctophidae sp.",
  "Myctophid 7" = "Myctophidae sp.",
  "Myctophid 8" = "Myctophidae sp.",
  "Myctophid 9" = "Myctophidae sp.",
  "Nannocampus sp" = "Nannocampus sp.",
  "Ophichthid 1" = "Ophichthidae sp.",
  "Ophichthid 2" = "Ophichthidae sp.",
  "Ophichthus sp 1" = "Ophichthus sp.",
  "Ophidiid 1" = "Ophidiidae sp.",
  "Oplegnathus spp" = "Oplegnathus sp.",
  "Ostraciid 1" = "Ostraciidae sp.",
  "Ostracion spp" = "Ostracion sp.",
  "Parablennius spp" = "Parablennius sp.",  
  "Paralepidid 1" = "Paralepididae sp.",
  "Parapercis sp" = "Parapercis sp.",
  "Parupeneus spp" = "Parupeneus sp.",
  "Pempheris sp 1" = "Pempheris sp.",
  "Pempheris sp 2" = "Pempheris sp.",
  "Percophidae" = "Percophidae sp.",
  "Platycephalid 2" = "Platycephalidae sp.",  
  "Platycephalid 4" = "Platycephalidae sp.",
  "Plectorhinchus sp 2" = "Plectorhinchus sp.",
  "Pomacentridae sp" = "Pomacentridae sp.", 
  "Pomadasys sp" = "Pomadasys sp.", 
  "Pomadasys spp" = "Pomadasys sp.",
  "Psenes sp 1" = "Psenes sp.",
  "Pseudorhombus sp" = "Pseudorhombus sp.",
  "Redigobius spp" = "Redigobius sp.",
  "Rhabdosargus spp" = "Rhabdosargus sp.",
  "Rhinecanthus spp" = "Rhinecanthus sp.",
  "Rock Cod Sp" = "Epinephelus sp.",
  "Scarid 4" = "Scaridae sp.",
  "Scaridae spp" = "Scaridae sp.",
  "Scarus sp" = "Scarus sp.", 
  "Scarus sp 1" = "Scarus sp.",
  "Scarus spp" = "Scarus sp.",
  "Sciaenidae spp" = "Sciaenidae sp.", 
  "Scianidae spp" = "Sciaenidae sp.",
  "Scomberoides spp" = "Scomberoides sp.",
  "Scomberomorus spp" = "Scomberomorus sp.",
  "Scombrid 1" = "Scombridae sp.",  
  "Scombrid 2" = "Scombridae sp.", 
  "Scombrid 3" = "Scombridae sp.",
  "Scombridae spp" = "Scombridae sp.",
  "Scorpaenid 1" = "Scorpaenidae sp.",  
  "Scorpaenid 5" = "Scorpaenidae sp.", 
  "Scorpaenid 9" = "Scorpaenidae sp.",
  "Serranidae" = "Serranidae sp.",
  "Serranidae spp" = "Serranidae sp.", 
  "Serrannidae spp" = "Serranidae sp.",
  "Solea spp" = "Solea sp.",
  "Soleidae" = "Soleidae sp.",
  "Soleidae spp" = "Soleidae sp.",
  "Sparid 1" = "Sparidae sp.", 
  "Sparidae" = "Sparidae sp.", 
  "Sparidae spp" = "Sparidae sp.",
  "Sphyraena spp" = "Sphyraena sp.",
  "Stolephorus spp" = "Stolephorus sp.",
  "Syngnathid 1" = "Syngnathidae sp.", 
  "Syngnathid 2" = "Syngnathidae sp.",
  "Syngnathid 3" = "Syngnathidae sp.",
  "Syngnathidae" = "Syngnathidae sp.",
  "Tetraodontidae sp" = "Tetraodontidae sp.",
  "Tetraodontidae spp" = "Tetraodontidae sp.",
  "Thalassoma spp" = "Thalassoma sp.",
  "Trachurus" = "Trachurus sp.",
  "Trigla spp" = "Trigla sp.",
  "Tripterygidae gen. nov." = "Tripterygiidae sp.",
  "Tripterygiid" = "Tripterygiidae sp.",
  "Tripterygiid 1" = "Tripterygiidae sp.",
  "Tripterygiid 2" = "Tripterygiidae sp.",
  "Tripterygiidae" = "Tripterygiidae sp.",
  "Xyrichtys sp 2" = "Xyrichtys sp.")

all_replacements <- c(species_fix, replace_lookup)

final_data_cleaned <- final_data_cleaned %>%
  mutate(Species = str_replace_all(Species, all_replacements))


length(unique(final_data_cleaned$Species)) #709 
#develop taxonomic table with all species here
species_list <- sort(unique(final_data_cleaned$Species))

# Convert to a data frame
species_df <- data.frame(Species = species_list)

# Export as CSV
write.csv(species_df, "unique_species_list.csv", row.names = FALSE)
 



#assign tax res
taxon_data_all <- taxon_data_all %>%
  mutate(
    taxonomic_resolution = case_when(
      str_detect(Species, "sp\\.$") & str_detect(Species, "formes") ~ "order",
      str_detect(Species, "sp\\.$") & str_detect(Species, "dae") ~ "family",
      str_detect(Species, "sp\\.$") ~ "genus",
      str_detect(Species, "spp\\.$") & str_detect(Species, "formes") ~ "order",
      str_detect(Species, "spp\\.$") & str_detect(Species, "dae") ~ "family",
      str_detect(Species, "spp\\.$") ~ "genus",
      TRUE ~ "species"
    )
  )

cat("\n--- CHECKS ---\n")
cat("Taxonomic records retrieved: ", nrow(taxon_data), "\n")
cat("Columns in taxon_data: ", paste(colnames(taxon_data), collapse = ", "), "\n\n")
cat("Class distribution:\n")
print(table(taxon_data$class))

#1 Aetobatus narinari    Elasmobranchii Myliobatiformes   Aetobatidae   
#2 Alopias superciliosus Elasmobranchii Lamniformes       Alopiidae     
#3 Cerebratulus fuscus   Pilidiophora   Heteronemertea    Lineidae      
#4 Excirolana latipes    Malacostraca   Isopoda           Cirolanidae   
#5 Myliobatus aquila     Elasmobranchii Myliobatiformes   Myliobatidae  
#6 Rhizoprionodon acutus Elasmobranchii Carcharhiniformes Carcharhinidae

#check unmatched
unmatched_species <- setdiff(spp$Species, taxon_data$Species)
length(unmatched_species)
sort(unmatched_species)

non_teleost <- taxon_data_all %>%
  filter(class != "Teleostei") %>%
  select(Species, class, order, family) %>%
  arrange(class, Species)
if (nrow(non_teleost) > 0) print(non_teleost, n = Inf)

taxon_data_all <- taxon_data_all %>%
  select(-any_of(c(
    "rank_Kingdom", "rank_Phylum", "rank_Subphylum", "rank_Infraphylum",
    "rank_Parvphylum", "rank_Gigaclass", "rank_Superclass", "rank_Suborder",
    "rank_Subfamily", "rank_Subclass", "rank_Infraclass", "rank_Superorder",
    "rank_Superfamily"
  )))

# Merge taxonomic data into final dataset
finalFM <- final_data_cleaned %>%
  left_join(taxon_data_all, by = "Species") %>%
  mutate(
    Class  = coalesce(Class, class),
    Order  = coalesce(Order, order),
    Family = coalesce(Family, family),
    Genus  = coalesce(Genus, genus)
  ) %>%
  select(-class, -order, -family, -genus) %>%
  group_by(Species) %>%
  fill(Class, Order, Family, Genus, .direction = "downup") %>%
  ungroup() %>%
  # Keep Teleostei and unresolved placeholders (Genus sp. etc.)
  filter(is.na(Class) | Class == "Teleostei")



finalFM <- final_data_cleaned
finalFM <- finalFM %>%
  mutate(
    taxonomic_resolution = case_when(
      str_detect(Species, "sp\\.$") & str_detect(Species, "formes") ~ "order",
      str_detect(Species, "sp\\.$") & str_detect(Species, "dae") ~ "family",
      str_detect(Species, "sp\\.$") ~ "genus",
      str_detect(Species, "spp\\.$") & str_detect(Species, "formes") ~ "order",
      str_detect(Species, "spp\\.$") & str_detect(Species, "dae") ~ "family",
      str_detect(Species, "spp\\.$") ~ "genus",
      TRUE ~ "species"
    )
  )

# ==============================================================================
#  Final summary
# ==============================================================================
cat("\n--- SUMMARY ---\n")
cat("Unique Teleost species retained: ", n_distinct(finalFM$Species), "\n")
cat("Records still containing 'sp.': ", sum(str_detect(finalFM$Species, "sp\\.")), "\n\n")
cat("Taxonomic resolution breakdown:\n")
print(table(finalFM$taxonomic_resolution))

#Family   Genus   Order Species 
#    211     247       1   6048
# ------------------------------------------------------------------------------
# fix weird orders
# ------------------------------------------------------------------------------
unique(finalFM$Order)
#most updated orders from Heemstra, P.C., Heemstra, E., Ebert, D.A., Holleman, W., & Randall, J.E. (Eds.). (2022). 
#Coastal fishes of the Western Indian Ocean (Volumes 1-5). South African Institute for Aquatic Biodiversity

order_family_list <- finalFM %>%
  filter(!is.na(Order), !is.na(Family)) %>%
  distinct(Order, Family) %>%
  arrange(Order, Family) %>%
  group_by(Order) %>%
  summarise(
    Families = paste(sort(unique(Family)), collapse = ", "),
    n_families = n_distinct(Family)
  ) %>%
  arrange(Order)

order_family_list %>% print(n = Inf)

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
  "Macroramphosidae",    "Gasterosteiformes"
)

#84 families in wrong orders

order_corrections_unique <- order_corrections %>%
  distinct(Family, .keep_all = TRUE)

family_fix <- c(
  "Sebastidae" = "Scorpaenidae", 
  "Grammistidae" = "Epinephelidae", 
  "Gaidropsaridae" = "Lotidae", 
  "Malacanthidae" = "Branchiostegidae", 
  "Ehiravidae" = "Clupeidae", 
  "Dussumieriidae" = "Clupeidae",
  "Dorosomatidae" = "Clupeidae", 
  "Alosidae" = "Clupeidae", 
  "Dichistiidae" = "Sparidae"
)

#9 families misnames

# Join corrections into finalFM
finalFM_corrected <- finalFM %>%
  mutate(
    Family = recode(Family, !!!family_fix)  # apply family fixes first
  ) %>%
  left_join(order_corrections_unique, by = "Family") %>%
  mutate(
    Order = if_else(!is.na(CorrectedOrder), CorrectedOrder, Order)
  ) %>%
  select(-CorrectedOrder)

finalFM_corrected %>%
  filter(Family %in% order_corrections_unique$Family) %>%
  count(Order, sort = TRUE)

finalFM <- finalFM_corrected
# Confirm that no incertae sedis remain (or list those still unresolved)
finalFM %>%
  filter(str_detect(Order, regex("incertae sedis", ignore_case = TRUE))) %>%
  count(Family, sort = TRUE)

finalFM <- finalFM %>%
  mutate(
    Order = if_else(
      Family == "Branchiostegidae" & str_detect(Order, regex("incertae sedis", ignore_case = TRUE)),
      "Perciformes",
      Order
    )
  )

#drop unknowns
finalFM <- finalFM %>%
  filter(!Species %in% c("No species present", "Untidentified sp."))
##
morpho_tax <- read_excel("morpho_tax.xlsx") #another species thing

#check the species in these families: Bathylagidae, Salmonidae
finalFM %>%
  filter(Family %in% c("Bathylagidae", "Salmonidae")) %>%
  select(Species, Family, Order) %>%
  distinct() %>%
  arrange(Family, Species)

finalFM <- finalFM %>%
  filter(
    !Family %in% c("Cichlidae", "Cyprinidae", "Salmonidae", "NA"),
    !Order %in% c("NA")
  )

length(unique(finalFM$Species)) #645 marine teleost species 
unique(finalFM$Species)
unique(finalFM$Order)
length(unique(finalFM$Family)) #123 families

# ------------------------------------------------------------------------------
## Clean column names
# ------------------------------------------------------------------------------
finalFM <- finalFM %>%
  select(-CommonName, -Location, -Year.x, -Year.y, -RecordID, -Season, -English, -subClass, -Superorder, -CPUE.units)
# ------------------------------------------------------------------------------
## Clean locations and ensure they are within the SA EEZ (Marine Environment)
# -----------------------------------------------------------------------------
#NA lats and longs need to be cleaned and validated, ensuring that points MAKE SENSE

location_summary <- finalFM %>%
  select(DataSetID, Species, MethodNew, StartDate, EndDate, SamplingLocation, Latitude, Longitude) %>%
  distinct() %>%
  arrange(DataSetID, SamplingLocation)

unique(finalFM$DataSetID)

missing_coords <- finalFM %>%
  filter(is.na(Latitude) | is.na(Longitude) | Latitude == 0 | Longitude == 0) %>%
  select(SamplingLocation, Location.y, DataSetID, Latitude, Longitude)
print(missing_coords, n = Inf)

# 2543 records have either NA/0 coordinates
# datasets below are the ones to investigate
#"35"  "47"  "49"  "51"  "53"  "82"  "105" "106" "107" "112" "114" "115" "116" "117" "121" "122" "131" "132" "133" "135" "136" "138" "142" "143" "144" "146" "147" "149" "152" "153" "154" "155"
#"156" "157" "158" "159" "160" "161" "162" "163" "164" "165" "166" "167"

missing_FM_location <- finalFM %>%
  filter(is.na(Latitude) | is.na(Longitude) | Latitude == 0 | Longitude == 0) %>%
  group_by(SamplingLocation, Location.y, DataSetID) %>%
  summarise(
    n_records = n(),
    .groups = "drop"
  ) %>%
  arrange(Location.y, SamplingLocation, DataSetID)

print(missing_FM_location, n = Inf)

write_xlsx(missing_FM_location, "~/Desktop/missing_FM_location.xlsx")


# Read in corrected coordinates file
missing_FM_location <- read_excel("~/Desktop/missing_FM_location.xlsx")

glimpse(missing_FM_location)

missing_FM_location <- missing_FM_location %>%
  mutate(DataSetID = as.character(DataSetID))

finalFM_fixed <- finalFM %>%
  left_join(
    missing_FM_location %>%
      select(SamplingLocation, DataSetID,
             Latitude_corrected = latitude,
             Longitude_corrected = longitude),
    by = c("SamplingLocation", "DataSetID")
  ) %>%
  mutate(
    Latitude_corrected = as.numeric(Latitude_corrected),
    Longitude_corrected = as.numeric(Longitude_corrected),
    Latitude = if_else(is.na(Latitude) | Latitude == 0, Latitude_corrected, Latitude),
    Longitude = if_else(is.na(Longitude) | Longitude == 0, Longitude_corrected, Longitude)
  ) %>%
  select(-Latitude_corrected, -Longitude_corrected)

finalFM_fixed %>%
  filter(is.na(Latitude) | is.na(Longitude) | Latitude == 0 | Longitude == 0) %>%
  summarise(n_missing = n())

sum(!is.na(finalFM_fixed$Latitude) & (is.na(finalFM$Latitude) | finalFM$Latitude == 0))
#still missing 94 datasets 107, 112, 135
unique(remaining_missing$DataSetID)
# 107 -34.1345969	18.4353544
# 112		-34.1060956	18.4740552
# 135	-31.81500	18.23389
#135	-31.81500	18.23389
#135	-32.74833	18.01556
#135	-33.04389	17.93056
#135	-33.04389	17.93056
#135	-32.76417	18.14278

manual_coords <- tribble(
  ~DataSetID, ~Latitude,     ~Longitude,   ~SamplingLocation,
  "107",      -34.1345969,   18.4353544,   "False Bay sandy beaches",
  "112",      -34.1060956,   18.4740552,   "False Bay sandy beaches",
  "135",      -31.81500,     18.23389,     "Olifants to Breede River",
  "135",      -32.74833,     18.01556,     "Olifants to Breede River",
  "135",      -33.04389,     17.93056,     "Olifants to Breede River",
  "135",      -32.76417,     18.14278,     "Olifants to Breede River"
)

finalFM_fixed2 <- finalFM_fixed %>%
  left_join(
    manual_coords %>%
      select(SamplingLocation, DataSetID, 
             Latitude_manual = Latitude, 
             Longitude_manual = Longitude),
    by = c("SamplingLocation", "DataSetID")
  ) %>%
  mutate(
    Latitude = if_else(is.na(Latitude) | Latitude == 0, Latitude_manual, Latitude),
    Longitude = if_else(is.na(Longitude) | Longitude == 0, Longitude_manual, Longitude)
  ) %>%
  select(-Latitude_manual, -Longitude_manual)

finalFM_fixed2 %>%
  filter(is.na(Latitude) | is.na(Longitude) | Latitude == 0 | Longitude == 0) %>%
  summarise(n_missing = n())
# ------------------------------------------------------------------------------
## Locations now cleaned finally
# ------------------------------------------------------------------------------
finalFM <- finalFM_fixed2
# ------------------------------------------------------------------------------
# years
# ------------------------------------------------------------------------------
no_date_records <- finalFM %>%
  filter(
      is.na(StartDate) & 
      is.na(EndDate)
  )
# inspect
nrow(no_date_records)
unique(no_date_records$DataSetID)

#DataSetID: 122,123,124,125,126,127,128,129 no dates go back and check 
#comes from ref 81< Whitfield et al 1989, (dates between 1983 and 1989)

finalFM <- finalFM %>%
  mutate(
    StartDate = if_else(
      is.na(StartDate) & is.na(EndDate),
      "1983", StartDate
    ),
    EndDate = if_else(
      is.na(StartDate) & is.na(EndDate),
      "1989", EndDate
    )
  )

# ------------------------------------------------------------------------------
# year column
# ------------------------------------------------------------------------------
finalFM <- finalFM %>%
  mutate(
    start_year = as.numeric(StartDate),
    end_year   = as.numeric(EndDate),
    year_span  = end_year - start_year
  )

# ------------------------------------------------------------------------------
# abundance metrics cleaning
# ------------------------------------------------------------------------------
finalFM <- finalFM %>%
  mutate(
    Count_Metric = case_when(
      AbsoluteCounts == 1 ~ "Absolute Counts",
      MaxN == 1 ~ "Max N",
      ProportionalAbundance == 1 ~ "Proportional Abundance",
      CPUE == 1 ~ "CPUE",
      PresenceAbsence == 1 ~ "Presence Absence",
      TRUE ~ "Unknown"  # If none of the labels are 1
    )
  )

table(finalFM$Count_Metric)
#old
#  Absolute Counts  |   CPUE  |   Max N  |   Presence Absence  |   Proportional Abundance  |   Unknown 
#       3490        |  892    |  265     |      633            |        1217               |   157 
#new
#  Absolute Counts  |   CPUE  |   Max N  |   Presence Absence  |   Proportional Abundance  |   Unknown 
#       3487        |  848    |  265     |      620           |        1127            |   

FMDATABUNDANCE <- finalFM

#keep abundance records somewhere
write.csv(FMDATABUNDANCE, file = "FM_abundance.csv", row.names = FALSE)

#now remove abundance col. for merge
finalFM <- finalFM %>%
  select(-Count, -MaxN, -ProportionalAbundance, -CPUE, - PresenceAbsence, -AbsoluteCounts)
# ------------------------------------------------------------------------------
## final column clean and fix
# ------------------------------------------------------------------------------
colnames(finalFM)
finalFM <- finalFM %>%
  select(-start_year, -end_year, -Origin, - Sampling.Frequency, -Reference, -Method, -DataDescription, -RefText, -Name, -rank_Species, -AphiaID.y)

finalFM <- finalFM %>%
  rename(
    datasetID = DataSetID,
    species = Species,
    class = Class,
    order = Order,
    family = Family,
    genus = Genus, 
    samplingarea = Location.y,
    sitename = SamplingLocation,
    latitude = Latitude,
    longitude = Longitude,
    yearstart = StartDate,
    yearend = EndDate,
    yearspan = year_span,
    method = MethodNew,
    aphiaID = AphiaID.x,
    countmetric = Count_Metric,
    datatype = DATATYPE,
    dataclass = CLASSIFICATION,
    yearend = EndDate
  )


write.csv(finalFM, file = "final_FM.csv", row.names = FALSE)

final_FM <- read.csv("~/Desktop/wd/masters/final_FM.csv")
#done
#-------------------------------------------------------------------------------
#OUTPUTS
#-------------------------------------------------------------------------------
# Summary of taxonomic resolution
finalFM %>%
  filter(taxonomic_resolution == "Species" & is.na(class)) %>%
  distinct(species, family, order) %>%
  arrange(species)

finalFM <- finalFM %>%
  mutate(
    class = if_else(is.na(class) & taxonomic_resolution == "Species", "Teleostei", class)
  )
table(finalFM$order)
finalFM %>%
  filter(is.na(taxonomic_resolution)) %>%
  distinct(species, family, order) %>%
  arrange(species)

#remove last dregs
finalFM <- finalFM %>%
  filter(
    !species %in% c("", 
                    "Aetobatus narinari", 
                    "Alopias superciliosus", 
                    "Cerebratulus fuscus", 
                    "Excirolana latipes", 
                    "Myliobatus aquila", 
                    "Rhizoprionodon acutus")
  )
#-------------------------------------------------------------------------------
# Taxonomic resolution plot
#-------------------------------------------------------------------------------

finalFM %>% count(taxonomic_resolution, sort = TRUE)

#Species               6043
#Genus                  249
#Family                 176
#Order                    1


length(unique(finalFM$species)) #635 marine teleost species 
length(unique(finalFM$family)) #123 families

taxon_summary <- finalFM %>%
  count(taxonomic_resolution)
# Bar plot
ggplot(taxon_summary, aes(x = reorder(taxonomic_resolution, -n), y = n)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 3, family = "Times") +
  labs(
    title = "Number of Records per Taxonomic Resolution for Research Data",
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
top_families <- finalFM %>%
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
top_species <- finalFM %>%
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
missing_coords <- finalFM %>%
  filter(datasetID == "130") %>%
  distinct(sitename, samplingarea, yearstart, method) %>%
  arrange(sitename)
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
finalFM <- finalFM %>%
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

# Define bounding boxes for coastlines
bbox_west  <- c(xmin = 14, xmax = 20, ymin = -36.5, ymax = -28)
bbox_south <- c(xmin = 20, xmax = 27, ymin = -36.5, ymax = -33)
bbox_east  <- c(xmin = 27, xmax = 35, ymin = -35, ymax = -26)

# lat and lon range
summary(finalFM$longitude)
# Min.  1st Qu.   Median   Mean    3rd Qu.    Max.    NA's 
#   16.72   22.11   25.59   25.11   29.94   32.88   0
summary(finalFM$latitude)
#Min.     1st Qu.  Median  Mean    3rd Qu.  Max.    NA's 
#  -34.81  -34.05  -33.99  -32.80  -31.36  -26.90    0

# Re-classify coastline
finalFM <- finalFM %>%
  mutate(
    coast = case_when(
      longitude >= 14 & longitude < 20 & latitude <= -28 & latitude >= -36.5 ~ "West",
      longitude >= 20 & longitude < 27 & latitude <= -33 & latitude >= -36.5 ~ "South",
      longitude >= 27 & longitude <= 35 & latitude <= -26 & latitude >= -35 ~ "East",
      TRUE ~ "West"   # force anything else into West
    )
  )

# Summarise records and species per coast
coast_summary <- finalFM %>%
  group_by(coast) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species)
  )

coast_summary
#. coast   n_records  n_species
#1 East       2280       519
#2 South      2971       238
#3 West       1218       163

# Summarise records and species per gear type
source_summary <- finalFM %>%
  group_by(datatype) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species)
  )
source_summary 
# datatype.   n_records. n_species
#1 C          841        185
#2 O          1022       258
#3 S          4606       516


#for each data type
method_summary_species <- finalFM %>%
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
records_summary <- finalFM %>%
  count(datatype, dataclass, name = "n_records")

# Species richness per data scheme
species_summary <- finalFM %>%
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
    data = finalFM,
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

# Intersecting with EEZ to ensure outlying records removed
#-------------------------------------------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")
sa_map <- world %>% filter(admin == "South Africa")
#load shp files
eez <- st_read("MAPPING/EEZ/eez_v12.shp", quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(4326)

#first convert latitude to sh
finalFM_sh <- finalFM %>%
  mutate(latitude = -abs(latitude))
# Convert  to sf
FMdat_sf <- finalFM_sh %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
# points that fall inside SA (land)
is_on_land <- st_intersects(FMdat_sf, sa_map, sparse = FALSE)[,1]
# Keep only marine (non-land) points
FMdat_eez <- FMdat_sf[!is_on_land, ]
# Check how many were removed
n_removed <- sum(is_on_land)
