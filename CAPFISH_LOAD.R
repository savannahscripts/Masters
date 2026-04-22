#load all capfish files

library(readxl)
library(dplyr)

setwd("/Users/savannahanderson/Desktop/wd/masters")
#-------------------------------------------------------------------------------
#STEP 1: read in all data sets from capfish folder
#-------------------------------------------------------------------------------

SADSTIA       <- read_excel("SOURCE DATA/capfish/qrySADSTIA.xlsx")
DEMERSALWFT   <- read_excel("SOURCE DATA/capfish/qryWETFISH.xlsx")      # offshore wetfish trawler (part of SADSTIA)
INSHORE       <- read_excel("SOURCE DATA/capfish/qryINSHORE.xlsx")      # SECIFA
SMALLPELAGICS <- read_excel("SOURCE DATA/capfish/qrySMALLPELAGICS.xlsx")# SAPFIA
PELAGICPS     <- read_excel("SOURCE DATA/capfish/qry_pelhaul_purseseine.xlsx") # SAPFIA purse seine
HAKELL        <- read_excel("SOURCE DATA/capfish/qryHLL.xlsx")          # SAHLLA
TUNA          <- read_excel("SOURCE DATA/capfish/qryTLL.xlsx")          # SAHLLA tuna longline

#check species no before
#SADSTIA = 272
length(unique(SADSTIA$Species_ID)) #29
length(unique(DEMERSALWFT$Species_ID)) #243

#SECIFA = 179
length(unique(INSHORE$Species_ID))

#SAPFIA = 29
length(unique(SMALLPELAGICS$Spec_ID)) #4
length(unique(PELAGICPS$Spec_ID)) #25

#SAHLLA = 216
length(unique(HAKELL$SpecID)) #126
length(unique(TUNA$SpecId)) #90
#-------------------------------------------------------------------------------
# STEP 2: Filter Teleostei, remove NA species
#-------------------------------------------------------------------------------
clean_teleost_data <- function(df) {
  df %>%
    filter(
      Class == "Teleostei")}

PELAGICPS    <- clean_teleost_data(PELAGICPS)
SMALLPELAGICS <- clean_teleost_data(SMALLPELAGICS)
HAKELL        <- clean_teleost_data(HAKELL)
INSHORE       <- clean_teleost_data(INSHORE)
SADSTIA       <- clean_teleost_data(SADSTIA)
DEMERSALWFT   <- clean_teleost_data(DEMERSALWFT)
TUNALL        <- clean_teleost_data(TUNA %>% filter(Work_Cat_Id == 5))
SHARKLL       <- clean_teleost_data(TUNA %>% filter(Work_Cat_Id == 20))

#-------------------------------------------------------------------------------
# STEP 3: Add source identifiers
#-------------------------------------------------------------------------------
SADSTIA       <- SADSTIA %>% mutate(Source = "SADSTIA_main")
DEMERSALWFT   <- DEMERSALWFT %>% mutate(Source = "SADSTIA_WF")
INSHORE       <- INSHORE %>% mutate(Source = "SECIFA_inshore")
SMALLPELAGICS <- SMALLPELAGICS %>% mutate(Source = "SAPFIA_smallpel")
PELAGICPS     <- PELAGICPS %>% mutate(Source = "SAPFIA_ps")
HAKELL        <- HAKELL %>% mutate(Source = "SAHLLA_hake")
TUNALL        <- TUNALL %>% mutate(Source = "SAHLLA_tuna")
SHARKLL       <- SHARKLL %>% mutate(Source = "SAHLLA_shark")
colnames(SADSTIA)
colnames(DEMERSALWFT)
colnames(INSHORE)
colnames(SMALLPELAGICS)
colnames(PELAGICPS)
colnames(HAKELL)
colnames(TUNALL)
colnames(SHARKLL)
#-------------------------------------------------------------------------------
# STEP 4: Standardize column names across datasets
#-------------------------------------------------------------------------------
standard_names <- c(
  "Work_Cat_Id", "Trip_ID", "Set_ID", "Date",
  "LatDeg", "LatMin", "LongDeg", "LongMin",
  "Spec_ID", "Family", "Genus", "Specie",
  "Weight", "Class"
)

standardize_dataset <- function(df, mapping) {
  df_std <- df %>% rename(!!!mapping)
  missing_cols <- setdiff(standard_names, colnames(df_std))
  df_std[missing_cols] <- NA
  df_std <- df_std[, standard_names]
  return(df_std)
}

# Mapping dictionaries
map_pelagicps <- c(
  "Set_ID" = "HaulId", "Date" = "Date",
  "LatDeg" = "LatDeg", "LatMin" = "LatMin",
  "LongDeg" = "LongDeg", "LongMin" = "LongMin",
  "Spec_ID" = "Spec_ID", "Trip_ID" = "Trip_ID",
  "Work_Cat_Id" = "Work_Cat_Id", "Family" = "Family",
  "Genus" = "Genus", "Specie" = "Specie",
  "Weight" = "Weight", "Class" = "Class"
)

map_hakell <- c(
  "Set_ID" = "SetId", "Date" = "SHaulDate",
  "LatDeg" = "SHaulLatDeg", "LatMin" = "SHaulLatMin",
  "LongDeg" = "SHaulLongDeg", "LongMin" = "SHaulLongMin",
  "Spec_ID" = "SpecID", "Trip_ID" = "Trip_ID",
  "Work_Cat_Id" = "Work_Cat_Id", "Family" = "Family",
  "Genus" = "Genus", "Specie" = "Specie",
  "Weight" = "Weight", "Class" = "Class"
)

map_sadstia <- c(
  "Set_ID" = "Trawl_Id", "Date" = "Date_Start",
  "LatDeg" = "StartLatDeg", "LatMin" = "StartLatMin",
  "LongDeg" = "StartLongDeg", "LongMin" = "StartLongMin",
  "Spec_ID" = "Species_ID", "Trip_ID" = "Trip_ID",
  "Work_Cat_Id" = "Work_Cat_Id", "Family" = "Family",
  "Genus" = "Genus", "Specie" = "Specie",
  "Weight" = "Weight", "Class" = "Class"
)

map_smallpelagics <- map_pelagicps

map_tuna <- c(
  "Set_ID"     = "Set_ID",
  "Date"       = "ES_Date",
  "LatDeg"     = "SS_Lat_Deg",
  "LatMin"     = "SS_Lat_Min",
  "LongDeg"    = "SS_Long_Deg",
  "LongMin"    = "SS_Long_Min",
  "Spec_ID"    = "SpecId",
  "Trip_ID"    = "Trip_ID",
  "Work_Cat_Id"= "Work_Cat_Id",
  "Family"     = "Family",
  "Genus"      = "Genus",
  "Specie"     = "Specie",
  "Weight"     = "TWeight",
  "Class"      = "Class"
)
#-------------------------------------------------------------------------------
# STEP 5: Apply standardization
#-------------------------------------------------------------------------------
PELAGICPS_std     <- standardize_dataset(PELAGICPS, map_pelagicps) %>% mutate(Source = "SAPFIA")
SMALLPELAGICS_std <- standardize_dataset(SMALLPELAGICS, map_smallpelagics) %>% mutate(Source = "SAPFIA")
HAKELL_std        <- standardize_dataset(HAKELL, map_hakell) %>% mutate(Source = "SAHLLA")
SADSTIA_std       <- standardize_dataset(SADSTIA, map_sadstia) %>% mutate(Source = "SADSTIA")
DEMERSALWFT_std   <- standardize_dataset(DEMERSALWFT, map_sadstia) %>% mutate(Source = "SADSTIA")
INSHORE_std       <- standardize_dataset(INSHORE, map_sadstia) %>% mutate(Source = "SECIFA")
TUNALL_std  <- standardize_dataset(TUNALL, map_tuna)  %>% mutate(Source = "SAHLLA")
SHARKLL_std <- standardize_dataset(SHARKLL, map_tuna) %>% mutate(Source = "SAHLLA")

#-------------------------------------------------------------------------------
# STEP 6: Combine all standardized datasets
#-------------------------------------------------------------------------------
all_datasets_combined <- bind_rows(
  PELAGICPS_std,
  SMALLPELAGICS_std,
  HAKELL_std,
  SADSTIA_std,
  DEMERSALWFT_std,
  INSHORE_std,
  SHARKLL_std,
  TUNALL_std
)

#breakdown of sources
table(all_datasets_combined$Source)

#  SADSTIA  SAHLLA  SAPFIA  SECIFA 
#  77683   24399   16939   18294

all_datasets_combined <- all_datasets_combined %>%
  mutate(Binomial = paste(Genus, Specie, sep = " "))

#species richness per source
all_datasets_combined %>%
  group_by(Source) %>%
  summarise(n_species = n_distinct(Specie))

#Source         n_species
#. 1 SADSTIA       77
#. 2 SAHLLA       57
#. 3 SAPFIA       15
#. 4 SECIFA        64

#SPECIES RICHNESS TOTAL
n_distinct(all_datasets_combined$Binomial)
#135

#species richness by family
all_datasets_combined %>%
  group_by(Family) %>%
  summarise(n_species = n_distinct(Binomial)) %>%
  arrange(desc(n_species))

#write
install.packages("writexl")
library(writexl)
write_xlsx(all_datasets_combined, "capfishdat.xlsx")






#SCALE
library(dplyr)
library(lubridate)

cap_time <- all_datasets_combined %>%
  mutate(
    haul_date = as.Date(Date),
    haul_date = ifelse(is.na(haul_date), as.Date(parse_date_time(Date, orders = c("Ymd","dmY","mdY","Y-m-d","d/m/Y","m/d/Y"))), haul_date) %>% as.Date(),
    year = year(haul_date),
    month = month(haul_date)
  )

cap_time %>%
  summarise(
    n = n(),
    n_with_date = sum(!is.na(haul_date)),
    start_date = min(haul_date, na.rm = TRUE),
    end_date = max(haul_date, na.rm = TRUE)
  )

temporal_by_source <- cap_time %>%
  group_by(Source) %>%
  summarise(
    n_records = n(),
    n_events = n_distinct(paste(Trip_ID, Set_ID, sep = "_"), na.rm = TRUE),
    first_year = min(year, na.rm = TRUE),
    last_year  = max(year, na.rm = TRUE),
    n_years    = n_distinct(year),
    start_date = min(haul_date, na.rm = TRUE),
    end_date   = max(haul_date, na.rm = TRUE),
    n_days_sampled = n_distinct(haul_date),
    n_months_sampled = n_distinct(paste(year, month)),
    .groups = "drop"
  ) %>%
  arrange(Source)

temporal_by_source

annual_effort <- cap_time %>%
  filter(!is.na(year)) %>%
  group_by(Source, year) %>%
  summarise(
    n_events = n_distinct(paste(Trip_ID, Set_ID, sep = "_"), na.rm = TRUE),
    n_records = n(),
    .groups = "drop"
  ) %>%
  arrange(Source, year)

annual_effort

annual_effort %>%
  group_by(Source) %>%
  summarise(
    mean_events_per_year = mean(n_events),
    median_events_per_year = median(n_events),
    min_events = min(n_events),
    max_events = max(n_events),
    .groups = "drop"
  )
degmin_to_dec <- function(deg, min) {
  ifelse(is.na(deg) | is.na(min), NA_real_,
         deg + sign(deg) * (min / 60))
}

cap_space <- cap_time %>%
  mutate(
    latitude  = degmin_to_dec(LatDeg, LatMin),
    longitude = degmin_to_dec(LongDeg, LongMin)
  ) %>%
  filter(!is.na(latitude), !is.na(longitude))

spatial_steps <- cap_space %>%
  arrange(Source, Trip_ID, haul_date) %>%
  group_by(Source, Trip_ID) %>%
  mutate(
    step_km = distHaversine(
      cbind(longitude, latitude),
      cbind(lag(longitude), lag(latitude))
    ) / 1000
  ) %>%
  ungroup()

spatial_scale <- spatial_steps %>%
  filter(!is.na(step_km), step_km > 0, step_km < 500) %>%  # remove GPS jumps
  group_by(Source) %>%
  summarise(
    n_steps = n(),
    median_step_km = median(step_km),
    q25 = quantile(step_km, 0.25),
    q75 = quantile(step_km, 0.75),
    mean_step_km = mean(step_km),
    max_step_km = max(step_km),
    .groups = "drop"
  )

spatial_scale


names(all_datasets_combined) %>%
  grep("time|hour|min|start|end|set|haul|shot|lift|deploy|retrieve", ., ignore.case = TRUE, value = TRUE)
library(dplyr)
library(lubridate)

cap_time <- all_datasets_combined %>%
  mutate(
    haul_date = as.Date(Date),
    haul_date = ifelse(
      is.na(haul_date),
      as.Date(parse_date_time(Date, orders = c("Ymd","dmY","mdY","Y-m-d","d/m/Y","m/d/Y"))),
      haul_date
    ) %>% as.Date(),
    year  = year(haul_date),
    month = month(haul_date),
    event_id = paste(Trip_ID, Set_ID, sep = "_")
  )

