###### Cleaning Species names in FMdat
## 2nd script in FM series (after load)
# author: "Savannah Anderson"

# ------------------------------------------------------------------------------

setwd("/Users/savannahanderson/Desktop/wd/masters")
getwd()
 
##install packages and load libraries
install.packages("createDB")
install.packages("tidyverse")
install.packages("worrms")
library(createDB)
library(tidyverse)
library(dplyr)
library(worrms)

# ------------------------------------------------------------------------------
## Step 1: Identify messy entries
# ------------------------------------------------------------------------------
#Locates likely errors of manual entry
species_issues <- final_data %>%
  distinct(Species) %>%
  filter(
    str_detect(Species, regex("sp\\.?|spp\\.?|gen\\.?|unknown|^#N/A$|^\\s*$|\\bgroup\\b", ignore_case = TRUE)) |
      str_detect(Species, "^[a-z]+$") |  # all lowercase
      str_detect(Species, "[0-9]") |     # contains digits
      str_detect(Species, "[:punct:]")   # unusual characters
  ) %>%
  arrange(Species)

#write csv file for species issues
write.csv(species_issues, "species_issues_review.csv", row.names = FALSE)


# ------------------------------------------------------------------------------
## Step 2: Clean species names 
# ------------------------------------------------------------------------------

# Clean species names in the final_data
final_data_cleaned <- final_data %>%
  mutate(Species = str_replace(Species, "Cyrpinus carpio", "Cyprinus carpio")) %>%
  mutate(Species = str_replace(Species, "Dasyatis brevicaudatus", "Dasyatis brevicaudata")) %>%
  mutate(Species = str_replace(Species, "Dichristius capensis", "Dichistius capensis")) %>%
  mutate(Species = str_replace(Species, "Diplodus cervinus hottentotus", "Diplodus cervinus")) %>%
  mutate(Species = str_replace(Species, "Diplodus hottentotus", "Diplodus cervinus")) %>%
  mutate(Species = str_replace(Species, "Galaxius zebratus", "Galaxias zebratus")) %>%
  mutate(Species = str_replace(Species, "Gobiesocidae sucker fish undescribed", "Gobiesocidae")) %>%
  mutate(Species = str_replace(Species, "Istiblennius so", "Istiblennius")) %>%
  mutate(Species = str_replace(Species, "juvenile Muglidae", "Mugilidae")) %>%
  mutate(Species = str_replace(Species, "Lagocephalus intermis", "Lagocephalus inermis")) %>%
  mutate(Species = str_replace(Species, "Lethrinus nebulosis", "Lethrinus nebulosus")) %>%
  mutate(Species = str_replace(Species, "Letrinus", "Lethrinus")) %>%
  mutate(Species = str_replace(Species, "Merluccius or Macruronus capensis", "Merlucciidae")) %>%
  mutate(Species = str_replace(Species, "Monodactylus falciformes", "Monodactylus falciformis")) %>%
  mutate(Species = str_replace(Species, "Muglidae", "Mugilidae")) %>%
  mutate(Species = str_replace(Species, "Pachymetopon aenum", "Pachymetopon aeneum")) %>%
  mutate(Species = str_replace(Species, "Pagellus natalensis", "Pagellus bellottii natalensis")) %>%
  mutate(Species = str_replace(Species, "Pomadasys commersonii", "Pomadasys commersonnii")) %>%
  mutate(Species = str_replace(Species, "Pomodasys olivaceum", "Pomadasys olivaceum")) %>%
  mutate(Species = str_replace(Species, "Rhinobatis annulatus", "Rhinobatos annulatus")) %>%
  mutate(Species = str_replace(Species, "Rhinobatus holcorhynchus", "Rhinobatos holcorhynchus")) %>%
  mutate(Species = str_replace(Species, "Rock Cod Sp", "Epinephelus")) %>%
  mutate(Species = str_replace(Species, "Scylliogaleus quecketii", "Scylliogaleus quecketti")) %>%
  mutate(Species = str_replace(Species, "Tripterygidae gen nov", "Tripterygidae")) %>%
  mutate(Species = str_replace(Species, "Tripterygiidae", "Tripterygidae")) %>%
  mutate(Species = str_replace(Species, "Unknown Damselfish", "Pomacentridae")) %>%
  mutate(Species = str_replace(Species, "Aplolemichthys kingi", "Apolemichthys kingi")) %>%
  mutate(Species = str_replace(Species,"Epinephelus gauza", "Epinephelus guaza")) %>%
  mutate(Species = str_replace(Species,"Cafrogobius caffer", "Caffrogobius caffer")) %>%
  mutate(Species = str_replace(Species,"Kuhlia taeniurus", "Kuhlia mugil")) %>%
  mutate(Species = str_replace(Species,"Peroclinus laurentii", "Pavoclinus laurentii")) %>%
  mutate(Species = str_replace(Species,"Blenophis anguillaris", "Blennophis anguillaris")) %>%
  mutate(Species = str_replace(Species,"Johnius dussumieri", "Johnius amblycephalus")) %>%
  mutate(Species = str_replace(Species,"Scianidae", "Sciaenidae")) %>%
  mutate(Species = str_replace(Species,"Caranx ignobolis", "Caranx ignobilis")) %>%
  mutate(Species = str_replace(Species,"Gobidae", "Gobiidae")) %>%
  mutate(Species = str_replace(Species,"Tripterygidae", "Tripterygiidae")) %>%
  mutate(Species = str_replace(Species,"Blenniella periopthalmus", "Blenniella periophthalmus")) %>%
  mutate(Species = str_replace(Species,"Gaidropsaurus capensis", "Gaidropsarus capensis")) %>%
  mutate(Species = str_replace(Species,"Galaxias nebratus", "Galaxias zebratus")) %>%
  mutate(Species = str_replace(Species,"Argyrosomus hololepidotis", "Argyrosomus hololepidotus")) %>%
  mutate(Species = str_replace(Species,"Raja rhizacanthus", "Raia rhizacanthus")) %>%
  mutate(Species = str_replace(Species,"Hyporhampus capensis", "Hyporphampus capensis")) %>%
  mutate(Species = str_replace(Species,"Micopterus", "Micropterus")) %>%
  mutate(Species = str_replace(Species,"Carcharius taurus", "Carcharias taurus")) %>%
  mutate(Species = str_replace(Species,"Rhabdosargus sasrba", "Rhabdosargus sarba")) %>%
  mutate(Species = str_replace(Species,"Chelinodon laticeps", "Chelonodon laticeps")) %>%
  mutate(Species = str_replace(Species,"Sphyraena chysotaenia", "Sphyraena chrysotaenia")) %>%
  mutate(Species = str_replace(Species,"Squalomorphea", "Squalomorphi")) %>%
  mutate(Species = str_replace(Species,"Chemerius nufar", "Cheimerius nufar")) %>%
  mutate(Species = str_replace(Species,"Oxyurichthys opthalmonema", "Oxyurichthys ophthalmonema")) %>%
  mutate(Species = str_replace(Species,"Decapteris", "Decapterus")) %>%
  mutate(Species = str_replace(Species,"Schindleria pitschmanni", "Schindleria pietschmanni")) %>%
  mutate(Species = str_replace(Species,"Paralochthodes", "Paralichthodes")) %>%
  mutate(Species = str_replace(Species,"Parascorpius typus", "Parascorpis typus")) %>%
  mutate(Species = str_replace(Species,"Juvenile Mugilidae", "Mugilidae")) %>%
  mutate(Species = str_replace(Species,"Epinephilus guaza", "Epinephelus guaza")) %>%
  mutate(Species = str_replace(Species,"Acabnthuris dussumieri", "Acanthurus dussumieri")) %>%
  mutate(Species = str_replace(Species,"Kyphosys vaigiensis", "Kyphosus vaigiensis")) %>%
  mutate(Species = str_replace(Species,"Gyymnothorax flavimarginatus", "Gymnothorax flavimarginatus")) %>%
  mutate(Species = str_replace(Species,"Pteromyaeus bovinus", "Pteromylaeus bovinus")) %>%
  mutate(Species = str_replace(Species,"Epinephilus andersoni", "Epinephelus andersoni")) %>%
  mutate(Species = str_replace(Species,"Epinephilus marginatus", "Epinephelus marginatus")) %>%
  mutate(Species = str_replace(Species,"Epinephilus rivulatus", "Epinephelus rivulatus")) %>%
  mutate(Species = str_replace(Species,"Pseudanthis squamipinnis", "Pseudanthias squamipinnis")) %>%
  mutate(Species = str_replace(Species,"Polysterganus praeorbitalis", "Polysteganus praeorbitalis")) %>%
  mutate(Species = str_replace(Species,"Charcharhinus limbatus", "Carcharhinus limbatus")) %>%
  mutate(Species = str_replace(Species,"Labeo annulatus", "Labeo")) %>%
  mutate(Species = str_replace(Species,"Serrannidae", "Serranidae")) %>%
  mutate(Species = str_replace(Species,"Hyporphampus capensis", "Hyporhamphus capensis")) %>%
  mutate(Species = str_replace(Species,"Stromateus acus", "Stromateus")) %>%
  mutate(Species = str_replace(Species,"Bascanichthys mitsukurii", "Bascanichthys")) %>%
  mutate(Species = str_replace(Species,"Thalassoma interrupta", "Thalassoma")) %>%
  mutate(Species = str_replace(Species,"Clinus superciliosis", "Clinus superciliosus")) %>%
  mutate(Species = str_replace(Species,"Caffrogobius maxilaris", "Caffrogobius")) %>%
  mutate(Species = str_replace(Species,"Istiblennius periopthalmus", "Istiblennius periophthalmus")) %>%
  
  filter(Species != "Labeo umbratus") %>%  # Remove freshwater species
  filter(Species != "Micropterus salmoides") %>%  # Remove freshwater species
  filter(Species != "Sandelia capensis") %>%  # Remove freshwater species
  filter(Species != "Galaxias zebratus") %>%  # Remove freshwater species
  filter(Species != "Micropterus punctulatus") %>%  # Remove freshwater species
  filter(Species != "Barbus anoplus") %>%  # Remove freshwater species
  filter(Species != "Tilapia sparrmanii") %>%  # Remove freshwater species
  filter(Species != "Micropterus dolomieu") %>%  # Remove freshwater species
  filter(Species != "Barbus afer") %>%  # Remove freshwater species
  filter(Species != "Barbus tenuis") %>%  # Remove freshwater species
  filter(Species != "Barbus serra") %>%  # Remove freshwater species
  filter(Species != "Lepomis macrochirus") %>%  # Remove freshwater species
  filter(Species != "Clarias gariepinus") %>%  # Remove freshwater species
  filter(Species != "Clarius gariepinus") %>%  # Remove freshwater species
  filter(Species != "Melanostomiidae")  # Remove freshwater species)

#check how many species there are
length(unique(final_data_cleaned$Species)) #882

#write a separate dataframe for viewing
unique_species_df <- final_data_cleaned %>%
  distinct(Species) %>%
  arrange(Species)

#export to CSV
write.csv(unique_species_df, "unique_species_cleaned.csv", row.names = FALSE)

#check final data cleaned 
view(final_data_cleaned) 
print(unique(final_data_cleaned$Species))

# ------------------------------------------------------------------------------
## Step 3: add in taxonomic information (from WORMS)
# ------------------------------------------------------------------------------

## add in subclass, class, order, superorder, family, genus for each species
## ID from WORMS.

# Get distinct species names
spp <- final_data_cleaned %>%
  distinct(Species) %>%
  filter(!is.na(Species))  # Remove NAs to avoid unnecessary queries

# Function to get AphiaID but return NA if no match
safe_wm_name2id <- possibly(wm_name2id, otherwise = NA_real_)
spp <- spp %>%
  mutate(AphiaID = map_dbl(Species, safe_wm_name2id)) %>%
  filter(!is.na(AphiaID))

safe_wm_classification <- possibly(wm_classification, otherwise = NULL)

taxon_data <- spp %>%
  mutate(tax_info = map(AphiaID, safe_wm_classification)) %>%
  filter(!map_lgl(tax_info, is.null)) %>%
  unnest(tax_info, names_sep = "_") %>%
  pivot_wider(names_from = tax_info_rank, values_from = tax_info_scientificname, names_repair = "unique") %>%
  rename(Species = `Species...1`) %>%
  group_by(AphiaID, Species) %>%
  summarise(
    class = first(na.omit(Class)),
    subclass = first(na.omit(Subclass)),
    superorder = first(na.omit(Superorder)),
    order = first(na.omit(Order)),
    family = first(na.omit(Family)),
    genus = first(na.omit(Genus)),
    .groups = "drop"
  ) %>%
  distinct()

# ------------------------------------------------------------------------------
# Step 4: Merge taxonomic data back into main dataset
# ------------------------------------------------------------------------------

final_FM <- final_FM %>%
  left_join(taxon_data, by = "Species") %>%
  mutate(
    Class = coalesce(Class, class),
    subClass = coalesce(subClass, subclass),
    Superorder = coalesce(Superorder, superorder),
    Order = coalesce(Order, order),
    Family = coalesce(Family, family),
    Genus = coalesce(Genus, genus)
  ) %>%
  select(-class, -subclass, -superorder, -order, -family, -genus) %>%
  group_by(Species) %>%
  fill(Class, subClass, Superorder, Order, Family, Genus, .direction = "downup") %>%
  ungroup()

# Double fill using Family or Genus where possible
final_FM <- final_FM %>%
  group_by(Family) %>%
  fill(Class, subClass, Superorder, Order, Genus, .direction = "downup") %>%
  ungroup() %>%
  group_by(Genus) %>%
  fill(Class, subClass, Superorder, Order, Family, .direction = "downup") %>%
  ungroup()

# Check remaining gaps
final_FM %>%
  summarise(
    missing_Class = sum(is.na(Class)),
    missing_subClass = sum(is.na(subClass)),
    missing_Superorder = sum(is.na(Superorder)),
    missing_Order = sum(is.na(Order)),
    missing_Family = sum(is.na(Family)),
    missing_Genus = sum(is.na(Genus))
  )

#drop subclass and suprorder
final_FM <- final_FM %>%
  select(-subClass, -Superorder)

#look for missing genus
final_FM %>%
  filter(is.na(Genus)) %>%
  count(Species, sort = TRUE) %>%
  head(20)

#fill in genus from species string
final_FM <- final_FM %>%
  mutate(
    Genus = if_else(
      is.na(Genus) & str_detect(Species, "^[A-Z][a-z]+( spp| sp\\.?| sp| spp\\.)$"),
      word(Species, 1),
      Genus
    )
  )
#and from binomial
final_FM <- final_FM %>%
  mutate(
    Genus = if_else(
      is.na(Genus) & str_detect(Species, " "),
      word(Species, 1),
      Genus
    )
  )

#check
final_FM %>%
  summarise(missing_Genus = sum(is.na(Genus)))

#locate
final_FM %>%
  filter(is.na(Genus)) %>%
  distinct(Species)


final_FM <- final_FM %>%
  mutate(
    # Correct naming issues
    Species = case_when(
      Species == "Mugilid" ~ "Mugilidae",         # Standardize to full family name
      Species == "#N/A" ~ NA_character_,          # Convert placeholder to missing value
      Species == "Cyprinidae" ~ NA_character_,    # Remove freshwater family
      TRUE ~ Species
    ),
    
    # Fill in Genus where possible or mark as unknown
    Genus = case_when(
      Species == "Mugilidae" ~ "Mugil",                     # Assign likely genus
      Species %in% c("Gobiid", "Gobiid24") ~ "Gobiidae",    # Assign family-level genus
      Species == "Tripterygiid" ~ "Tripterygiidae",         # Assign triplefin family
      Species == "Percophidae" ~ "Percophidae",             # Assign duckbill family
      TRUE ~ Genus
    )
  ) %>%
  filter(!is.na(Species))  # Remove invalid or excluded records (e.g. '#N/A' and Cyprinidae)

# ------------------------------------------------------------------------------
# Step 5: Final clean
# ------------------------------------------------------------------------------

unique_species <- final_FM %>%
  distinct(Species) %>%
  arrange(Species)

nrow(unique_species) #879 species

# Check what classes are still present
table(final_FM$Class)

#keep only teleosts
final_FM <- final_FM %>%
  filter(Class == "Teleostei")

#species left after removal
n_distinct(final_FM$Species)

n_distinct(final_FM$Family)

#check species again
unique(final_FM$Species)


#add taxonomic resolution column 


final_FM <- final_FM %>%
  mutate(
    TaxonomicResolution = case_when(
      str_detect(Species, "^No species present$|^Unidentified|^#N/A$|^Untidentified|sp$|spp$|sp\\.|spp\\.|unknown") ~ "Unidentified",
      str_count(Species, "\\s+") == 1 & str_detect(Species, "^[A-Z][a-z]+ [a-z]+$") ~ "Species-level",
      str_detect(Species, "idae$") ~ "Family-level",
      str_count(Species, "\\s+") == 0 ~ "Genus-level",
      TRUE ~ "Unidentified"
    )
  )


#view break down
final_FM %>%
  count(TaxonomicResolution)

#93% of data is identified to species 
# ~7% unidentified, can possibly remove


#plot for methods sections to justify removal 
# Create a summary table
taxon_summary <- final_FM %>%
  count(TaxonomicResolution)

# Bar plot
ggplot(taxon_summary, aes(x = reorder(TaxonomicResolution, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#89BBFE") +
  labs(
    title = "Number of Records by Taxonomic Resolution",
    x = "Taxonomic Resolution",
    y = "Number of Records"
  ) +
  theme_minimal(base_size = 11, base_family = "Times") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )
  