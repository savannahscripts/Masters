
setwd("/Users/savannahanderson/Desktop/wd/masters")
getwd()

install.packages("createDB")
##load libraries
library(createDB)
library(tidyverse)
library(dplyr)
library(sf)
# author: "Savannah Anderson"
# date: "2025-02-17"


# ------------------------------------------------------------------------------
## Step 1: Clean locations and ensure they are within the SA EEZ (Marine Environment)
# ------------------------------------------------------------------------------

#looking at location
#unique(final_data$SamplingLocation)
#sum(is.na(tblFishAbundance$Location))
#unique(tblFishAbundance$Location)
#unique(tblLocation$Location)
#head(tblLocation)
#class(final_data$Location.y)


final_data_cleaned<- final_FM %>%
  mutate(Latitude = if_else(SamplingLocation == "Schoenmakerskop to Flat Rocks", -34.046112, Latitude)) %>% mutate(Longitude = if_else(SamplingLocation == "Schoenmakerskop to Flat Rocks", 25.618946, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Schoenmakerskop to Flat Rocks", 8500, NA)) %>%
  mutate(Latitude = if_else(SamplingLocation == "SundaysBeach,SundaysEstuary,CoegaBeach,SwartkopsEstuary,KingsBeach,BirdRock", -33.8658026, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "SundaysBeach,SundaysEstuary,CoegaBeach,SwartkopsEstuary,KingsBeach,BirdRock", 25.6384704, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "SundaysBeach,SundaysEstuary,CoegaBeach,SwartkopsEstuary,KingsBeach,BirdRock", 8500, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "East London to Jeffreys Bay", -33.1683468, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "East London to Jeffreys Bay", 27.1051901, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "East London to Jeffreys Bay", 150000, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "Great Fish River to Kei River", -33.0042613, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Great Fish River to Kei River", 28.1992767, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Great Fish River to Kei River", 75000, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "False Bay sandy beaches", -34.2833737, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "False Bay sandy beaches", 18.7010553, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "False Bay sandy beaches", 47700, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "False Bay shipwrecks", -34.0740347, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "False Bay shipwrecks", 18.5848059, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "False Bay shipwrecks", 14500, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "Kosi Bay to Mbashe river", -30.3158891, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Kosi Bay to Mbashe river", 29.9014515, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Kosi Bay to Mbashe river", 250000, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Knysna zostera absent", -34.0513424, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Knysna zostera absent", 23.0368623, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Knysna zostera absent", 2500, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Knysna zostera dense", -34.0513424, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Knysna zostera dense", 23.0368623, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Knysna zostera dense", 2500, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Knysna zostera sparse", -34.0513424, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Knysna zostera sparse", 23.0368623,Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Knysna zostera sparse", 2500, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Mbashe estuary", -31.6180773, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Mbashe estuary", 28.8727003, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Mbashe estuary", 250, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Kranshoek to Great Fish River", -31.4264357, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Kranshoek to Great Fish River", 27.8374181, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Kranshoek to Great Fish River", 215000, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Swartvlei", -34.0571883, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Swartvlei", 22.8428576, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Swartvlei", 250, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Transkei", -32.575997, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Transkei", 29.9425601, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Transkei", 250000, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Storms river mouth", -32.575997, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Storms river mouth", 29.9425601, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Storms river mouth", 250, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "Wilderness Lakes System", -34.0492698, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Wilderness Lakes System", 22.7610652, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Wilderness Lakes System", 2500, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "St Helena Bay", -32.7241379, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "St Helena Bay", 18.149114, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "St Helena Bay", 250, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "Saldanha to Langebaan", -33.0745543, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Saldanha to Langebaan", 18.0565652, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Saldanha to Langebaan", 2500, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "Olifants to Breede River", -33.0024585, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Olifants to Breede River", 19.6721191, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Olifants to Breede River", 160000, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "Doring to Elands Bay", -32.3354108, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Doring to Elands Bay", 18.3680575, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Doring to Elands Bay", 14000, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "Berg River Estuary mouth to 50km", -32.7804519,Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Berg River Estuary mouth to 50km", 18.1674223, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Berg River Estuary mouth to 50km", 250, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "Berg River", -32.7804519, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Berg River", 18.1674223, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Berg River", 250, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "Tsitsikamma Rocky Shore", -34.0676066, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Tsitsikamma Rocky Shore", 24.0284106, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Tsitsikamma Rocky Shore", 250, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "Struisbaai", -34.7529754, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Struisbaai", 20.1480456, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Struisbaai", 20000, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Kosi Bay 200m from mouth", -26.894444, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Kosi Bay 200m from mouth", 32.879167, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Kosi Bay 200m from mouth", 230, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Langebaan MPA", -33.1275, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Langebaan MPA", 18.062222, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Langebaan MPA", 3700, uncertainty_radius_m)) %>% mutate(Latitude = if_else(SamplingLocation == "Kosi river mouth and lakes", -26.9571438, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Kosi river mouth and lakes", 32.8345799, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Kosi river mouth and lakes", 3700, uncertainty_radius_m)) %>% mutate(Latitude = if_else(SamplingLocation == "Langebaan outside MPA", -33.054722, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Langebaan outside MPA", 18.065833, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Langebaan outside MPA", 1500, uncertainty_radius_m)) %>% mutate(Latitude = if_else(SamplingLocation == "Lower reaches of estuary-summer", -34.080833, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Lower reaches of estuar-summer", 23.128611, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Lower reaches of estuary-summer", 270, uncertainty_radius_m)) %>% mutate(Latitude = if_else(SamplingLocation == "Lower reaches of estuary-winter", -34.080833, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Lower reaches of estuary-winter", 23.128611, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Lower reaches of estuary-winter", 270, uncertainty_radius_m)) %>% mutate(Latitude = if_else(SamplingLocation == "Main channel - summer", -34.078056, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Main channel - summer", 23.128333, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Main channel - summer", 20, uncertainty_radius_m)) %>% mutate(Latitude = if_else(SamplingLocation == "Main channel - winter", -34.078056, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Main channel - winter", 23.128333, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Main channel - winter", 20, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Middle and lower estuary - summer", -34.079722, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Middle and lower estuary - summer", 23.127778, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Middle and lower estuary _ summer", 110, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Middle and lower estuary - winter", -34.079722, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Middle and lower estuary - winter ", 23.127778, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Middle and lower estuary - winter", 110, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Pondoland MPA", -31.513889, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Pondoland MPA", 29.832222, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Pondoland MPA", 10500, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Mtamvuna River to Port St Johns", -31.631111, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Mtamvuna River to Port St Johns", 29.555833, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Mtamvuna River to Port St Johns", 42000, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Elands sandy deep 0.35km offshore", -34.05, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Elands sandy deep 0.35km offshore", 24.071111, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Elands sandy deep 0.35km offshore", 180, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Elands sandy deep 1.26km offshore", -34.057778, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Elands sandy deep 1.26km offshore", 24.068611, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Elands sandy deep 1.26km offshore", 530, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Elands sandy surface 0.35km offshore", -34.05, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Elands sandy surface 0.35km offshore", 24.071111, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Elands sandy surface 0.35km offshore", 180, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Elands sandy surface 3.83km offshore", -34.080556, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Elands sandy surface 3.83km offshore", 24.066389, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Elands sandy surface 3.83km offshore", 1720, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Sanddrif sand and reef deep 0.35km offshore", -34.043056, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Sanddrif sand and reef deep 0.35km offshore", 23.996389, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Sanddrif sand and reef deep 0.35km offshore", 180, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Sanddrif sand and reef deep 1.26km offshore", -34.048611, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Sanddrif sand and reef deep 1.26km offshore", 23.995833, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Sanddrif sand and reef deep 1.26km offshore", 730, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Sanddrif sand and reef surface 0.35km offshore", -34.043056, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Sanddrif sand and reef surface 0.35km offshore", 23.996389, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Sanddrif sand and reef surface 0.35km offshore", 180, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Sundays 12km lower", -33.281944, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Sundays 12km lower", 25.241389, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Sundays 12km lower", 6000, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "Tsitsikamma", -34.0394757, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Tsitsikamma", 23.8748845, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Tsitsikamma", 25000, uncertainty_radius_m)) %>%
  mutate(Latitude = if_else(SamplingLocation == "Kosi Estuary 200m from mouth", -26.9080626, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Kosi Estuary 200m from mouth", 32.8570938, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Kosi Estuary 200m from mouth", 200, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "St Lucia Estuary 4km from mouth", -28.3647379, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "St Lucia Estuary 4km from mouth", 32.4218157, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "St Lucia Estuary 4km from mouth", 4000, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "Slangbaai to Port Alfred", -33.6361921, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Slangbaai to Port Alfred", 27.0981754, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Slangbaai to Port Alfred", 4000, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "Sundays River lower 12km", -33.281944, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Sundays River lower 12km", 25.241389, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Sundays River lower 12km", 6000, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "Plettenberg Bay to Port Alfred", -33.9081748, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Plettenberg Bay to Port Alfred", 24.9643857, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Plettenberg Bay to Port Alfred", 175000, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "Langebaan in MPA", -33.1196335, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Langebaan in MPA", 18.0872889, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Langebaan in MPA", 250, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "SW Cape", -34.0681997, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "SW Cape", 18.952448, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "SW Cape", 25000, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "Noetsie Estuary", -33.9256005, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Noetsie Estuary", 18.4050195, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Noetsie Estuary", 2500, uncertainty_radius_m))%>%
  mutate(Latitude = if_else(SamplingLocation == "Pondoland", -31.5995171, Latitude)) %>%
  mutate(Longitude = if_else(SamplingLocation == "Pondoland", 29.4817428, Longitude)) %>%
  mutate(uncertainty_radius_m = if_else(SamplingLocation == "Pondoland", 25000, uncertainty_radius_m))


# ------------------------------------------------------------------------------
## Step 2: check for NA records 
# ------------------------------------------------------------------------------

#View catch records and remove NAs
na_count_records <- final_data_cleaned %>%
  filter(is.na(Count))
#three records removed
final_data_cleaned <- final_data_cleaned %>%
  filter(!is.na(Count))
#check
sum(is.na(final_data_cleaned$Count)) 


# ------------------------------------------------------------------------------
## Step 3: check for records with unknown years
# ------------------------------------------------------------------------------

no_date_records <- final_data_cleaned %>%
  filter(
    is.na(ObservationYear) & 
      is.na(StartDate) & 
      is.na(EndDate) & 
      is.na(DatasetYear)
  )

# View or inspect
View(no_date_records)  
nrow(no_date_records)

#DataSetID: 122,123,124,125,126,127,128,129 no dates go back and check 
#comes from ref 81< Whitfield et al 1989, (dates between 1983 and 1989)

final_data_cleaned <- final_data_cleaned %>%
  mutate(
    StartDate = if_else(
      is.na(StartDate) & is.na(EndDate) & is.na(ObservationYear) & is.na(DatasetYear),
      "1983", StartDate
    ),
    EndDate = if_else(
      is.na(StartDate) & is.na(EndDate) & is.na(ObservationYear) & is.na(DatasetYear),
      "1989", EndDate
    )
  )

# ------------------------------------------------------------------------------
## Step 4: standardize record names (spelling/variations)
# ------------------------------------------------------------------------------

unique_methods <- final_data_cleaned %>%
  distinct(Method) %>%
  arrange(Method)

print(unique_methods, n = Inf)


#fix names
final_data_cleaned <- final_data_cleaned %>%
  mutate(Method = case_when(
    str_detect(Method, regex("BeachSeine|Seine", ignore_case = TRUE)) ~ "Seine Net",
    str_detect(Method, regex("GillNet", ignore_case = TRUE)) ~ "Gill Net",
    str_detect(Method, regex("Trap", ignore_case = TRUE)) ~ "Trap",
    str_detect(Method, regex("BRUV", ignore_case = TRUE)) ~ "BRUV",
    str_detect(Method, regex("ROV", ignore_case = TRUE)) ~ "ROV",
    str_detect(Method, regex("Trawl", ignore_case = TRUE)) & !str_detect(Method, regex("Plankton", ignore_case = TRUE)) ~ "Trawl",
    str_detect(Method, regex("Angling|Boat angling|ShoreAngling|Ski-boat", ignore_case = TRUE)) ~ "Angling",
    str_detect(Method, regex("SCUBA|UVC|VisualEstimate|Line transect", ignore_case = TRUE)) ~ "UVC",
    str_detect(Method, regex("CoverNet|Net", ignore_case = TRUE)) ~ "Net (Unspecified)",
    str_detect(Method, regex("CloveOil|Rotenone", ignore_case = TRUE)) ~ "Chemical Sampling",
    str_detect(Method, regex("DeadFish|DeadCollection", ignore_case = TRUE)) ~ "Dead Fish Collection",
    str_detect(Method, regex("Fyke", ignore_case = TRUE)) ~ "Fyke Net",
    str_detect(Method, regex("Multiple", ignore_case = TRUE)) ~ "Multiple",
    str_detect(Method, regex("PersComm|Records", ignore_case = TRUE)) ~ "Other (Records/Personal Communication)",
    TRUE ~ NA_character_  # remove plankton and unmatched methods
  )) %>%
  filter(!is.na(Method))

# ------------------------------------------------------------------------------
## Step 5: remove any columns with only NA Values 
# ------------------------------------------------------------------------------
#clean up columns:
colnames(final_data_cleaned)

#check if necesssary, if all NA then remove (double ups from original access)

obsyear <-  final_data_cleaned %>%
  filter(is.na(ObservationYear))
nrow(obsyear)
#remove (all NA)

common <-  final_data_cleaned %>%
  filter(is.na(CommonName))
nrow(common)
#remove (All NA)

english <-  final_data_cleaned %>%
  filter(is.na(English))
nrow(english)
#remove (All NA)

season <-  final_data_cleaned %>%
  filter(is.na(Season))
nrow(season)
#remove (All NA)

#remove
final_data_cleaned <- final_data_cleaned %>%
  select(-CommonName, -English, -ObservationYear, -Season)

datayear <-  final_data_cleaned %>%
  filter(is.na(DatasetYear))
nrow(datayear)
#keep

uncertainty <-  final_data_cleaned %>%
  filter(is.na(uncertainty_radius_m))
nrow(uncertainty)
#keep

# ------------------------------------------------------------------------------
# SAVE #
# ------------------------------------------------------------------------------

final_FM <- final_data_cleaned
write.csv(final_FM, file = "final_FM.csv", row.names = FALSE)

library(readxl)
FMdat <- read_excel("final_FM.xlsx")

