setwd("/Users/savannahanderson/Desktop/wd/masters")
getwd()

# title: "Load the FishMaster database and its tables"
# author: "Savannah Anderson"
# date: "2025-02-17"

#first script in FM series 
##install packages
install.packages("RSQLite")
install.packages("DBI")
install.packages("RPostgres")
install.packages("tidyverse")
install.packages("readxl")

##load libraries
library(RPostgres)
library(DBI)
library(tidyverse)
library(RSQLite)
library(readxl)
library(dplyr)

# set directory to tables 
excel_dir <- "~/Desktop/FishTables"
# make SQLite database
db_path <- "~/Desktop/FishMaster.sqlite"
con <- dbConnect(SQLite(), db_path)

# list files
excel_files <- list.files(excel_dir, pattern = "\\.xlsx$", full.names = TRUE)

# each file as a table
for (file in excel_files) {
  # Extract table name from filename
  table_name <- tools::file_path_sans_ext(basename(file))
  message("Importing: ", table_name)
  
  # read and write data
  df <- read_excel(file)
  dbWriteTable(con, table_name, df, overwrite = TRUE)
}

# verify
print(dbListTables(con))

# read in tables
tblFishAbundance <- dbReadTable(con, "tblFishAbundance")
tblFullFishList <- dbReadTable(con, "tblFullFishList")
tblLocation <- dbReadTable(con, "tblLocation")

tblReferences <- dbReadTable(con, "tblReferences")
tblDataSet <- dbReadTable(con, "tblDataSet")

#merge into dataset
final_data <- tblFishAbundance %>%
  mutate(
    Location  = as.character(Location),
    DataSetID = as.character(DataSetID)
  ) %>%
  # taxonomy info
  left_join(
    tblFullFishList %>%
      select(English, Species, Class, subClass, Superorder, Order, Family, Genus),
    by = "Species"
  ) %>%
  # location info
  left_join(
    tblLocation %>%
      mutate(SamplingLocationID = as.character(SamplingLocationID)) %>%
      select(SamplingLocationID, Location, SamplingLocation, Latitude, Longitude),
    by = c("Location" = "SamplingLocationID")
  ) %>%
  # dataset metadata (with Reference ID)
  left_join(
    tblDataSet %>%
      mutate(ID = as.character(ID),
             Reference = as.character(Reference)) %>%
      select(ID, Name, Year, StartDate, EndDate, Sampling.Frequency,
             Reference, Method, DataDescription, AbsoluteCounts, MaxN,
             ProportionalAbundance, CPUE, `CPUE.units`, PresenceAbsence, Origin),
    by = c("DataSetID" = "ID")
  ) %>%
  # full reference text
  left_join(
    tblReferences %>%
      mutate(ID = as.character(ID)) %>%
      rename(RefID = ID, RefText = Reference),
    by = c("Reference" = "RefID")
  )

# include extra variables
method_dataset_refs_expanded2 <- final_data %>%
  distinct(Method, DataSetID, Name, DataDescription, Reference, Reference.y) %>%
  arrange(Method, DataSetID) %>%
  rename(
    ReferenceID   = Reference,      
    ReferenceText = Reference.y   
  )

# export
writexl::write_xlsx(method_dataset_refs_expanded2, "method_dataset_references2.xlsx")


#READ IN 
data_scheme <- read_xlsx('/Users/savannahanderson/Desktop/wd/masters/method_dataset_references2.xlsx')

colnames(final_data)
colnames(data_scheme)
# Make sure both are character before joining
final_data_joined <- final_data %>%
  mutate(DataSetID = as.character(DataSetID)) %>%
  left_join(
    data_scheme %>%
      mutate(DataSetID = as.character(DataSetID)) %>%
      select(DataSetID, MethodNew, DataType, DataClassification),
    by = "DataSetID"
  )

#breakdown of DATATYPE
datatype_summary <- final_data_joined %>%
  group_by(DataType) %>%
  summarise(
    n_records = n(),
    n_datasets = n_distinct(DataSetID),  
    .groups = "drop"                 
  )

print(datatype_summary)
#  DataType    n_records   n_datasets
# D             2253         31
# I             5106        133

#BREAKDOWN OF DATA CLASSIFICATION
DATAclass_summary <- final_data_joined %>%
  group_by(DataClassification) %>%
  summarise(
    n_records = n(),
    n_datasets = n_distinct(DataSetID),  
    .groups = "drop"                 
  )

print(DATAclass_summary)
#  DataClassification    n_records    n_datasets
# C                       1136         14
# O                       1117         17
# S                       5106        133

#No. Of references
length(unique(tblReferences$ID)) #123
#No. Of datasets
length(unique(final_data_joined$DataSetID)) #164

names(final_data_joined)
head(final_data_joined)
#% independent (survey)
(131/164)*100 
#79.87805

#next script is FM_CLEAN.R and follows on directly from this, without reading in anything new


#SCALE

colnames(final_data_joined)
head(final_data_joined)


lit_events <- final_data_joined %>%
  mutate(
    start_year = as.numeric(StartDate),
    end_year   = as.numeric(EndDate),
    latitude   = as.numeric(Latitude),
    longitude  = as.numeric(Longitude),
    method     = MethodNew
  ) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  distinct(DataSetID, SamplingLocation, latitude, longitude, method,
           start_year, end_year)

lit_events2 <- final_data_joined %>%
  mutate(
    latitude  = as.numeric(Latitude),
    longitude = as.numeric(Longitude),
    method    = tolower(MethodNew)   # or keep as MethodNew if you prefer
  ) %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(method)) %>%
  distinct(DataSetID, method, SamplingLocation, latitude, longitude)

lit_temporal <- lit_events %>%
  summarise(
    dataset = "LITERATURE",
    n_events = n(),
    start_year = min(start_year, na.rm = TRUE),
    end_year   = max(end_year, na.rm = TRUE),
    n_years    = n_distinct(unlist(map2(start_year, end_year, `:`)))
  )

lit_temporal_by_method <- lit_events %>%
  group_by(method) %>%
  summarise(
    n_events = n(),
    start_year = min(start_year, na.rm = TRUE),
    end_year   = max(end_year, na.rm = TRUE),
    .groups = "drop"
  )
library(geosphere)
lit_spatial <- lit_events %>%
  arrange(DataSetID, latitude, longitude) %>%
  group_by(DataSetID) %>%
  mutate(
    step_km = distHaversine(
      cbind(longitude, latitude),
      cbind(lag(longitude), lag(latitude))
    ) / 1000
  ) %>%
  ungroup() %>%
  filter(!is.na(step_km), step_km > 0, step_km < 500)

lit_spatial2 <- lit_events2 %>%
  arrange(DataSetID, method, latitude, longitude) %>%
  group_by(DataSetID, method) %>%
  mutate(
    step_km = geosphere::distHaversine(
      cbind(longitude, latitude),
      cbind(lag(longitude), lag(latitude))
    ) / 1000
  ) %>%
  ungroup() %>%
  filter(!is.na(step_km), step_km > 0, step_km < 500)

lit_spatial_summary2 <- lit_spatial2 %>%
  summarise(
    n_pairs = n(),
    median_spacing_km = median(step_km),
    q25 = quantile(step_km, 0.25),
    q75 = quantile(step_km, 0.75),
    max_spacing_km = max(step_km)
  )

lit_spatial_by_method2 <- lit_spatial2 %>%
  group_by(method) %>%
  summarise(
    n_pairs = n(),
    median_spacing_km = median(step_km),
    q25 = quantile(step_km, 0.25),
    q75 = quantile(step_km, 0.75),
    max_spacing_km = max(step_km),
    .groups = "drop"
  ) %>%
  arrange(desc(n_pairs))


