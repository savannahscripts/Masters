
setwd("/Users/savannahanderson/Desktop/wd/masters")
getwd()

# title: "Load the FishMaster database and its tables"
# author: "Savannah Anderson"
# date: "2025-02-17"


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


# Set directory containing Excel files
excel_dir <- "~/Desktop/FishTables"

# Create SQLite database
db_path <- "~/Desktop/FishMaster.sqlite"
con <- dbConnect(SQLite(), db_path)

# List all Excel files
excel_files <- list.files(excel_dir, pattern = "\\.xlsx$", full.names = TRUE)

# Import each file as a table
for (file in excel_files) {
  # Extract table name from filename
  table_name <- tools::file_path_sans_ext(basename(file))
  message("Importing: ", table_name)
  
  # Read and write data
  df <- read_excel(file)
  dbWriteTable(con, table_name, df, overwrite = TRUE)
}

# Verify tables
tables <- dbListTables(con)
print(tables)

# Check tables
dbListTables(con)

# Read in tables
tblFishAbundance <- dbReadTable(con, "tblFishAbundance")
tblFullFishList <- dbReadTable(con, "tblFullFishList")
tblLocation <- dbReadTable(con, "tblLocation")
tblReferences <- dbReadTable(con, "tblReferences")
tblDataSet <- dbReadTable(con, "tblDataSet")

#test
head(tblFishAbundance)

# Check the column names of each table
names(tblFishAbundance)
names(tblFullFishList)
names(tblLocation)
names(tblReferences)
names(tblDataSet)


# Join tables
final_data <- tblFishAbundance %>%
  mutate(Location = as.character(Location), 
         DataSetID = as.character(DataSetID)) %>%
  left_join(
    tblFullFishList %>%
      select(English, Species, Class, subClass, Superorder, Order, Family, Genus), 
    by = "Species"
  ) %>%
  # Convert SamplingLocationID to character for the join
  left_join(
    tblLocation %>%
      mutate(SamplingLocationID = as.character(SamplingLocationID)) %>%
      select(SamplingLocationID, Location, SamplingLocation, Latitude, Longitude), 
    by = c("Location" = "SamplingLocationID")
  ) %>%
  left_join(tblReferences, by = c("DataSetID" = "Reference")) %>%
  left_join(
    tblDataSet %>%
      mutate(ID = as.character(ID)) %>%
      select(ID, Name, Year, StartDate, EndDate, Sampling.Frequency, 
             Method, DataDescription, AbsoluteCounts, MaxN, ProportionalAbundance, 
             CPUE, `CPUE.units`, PresenceAbsence, Origin), 
    by = c("DataSetID" = "ID")
  )

# View and check the final data set
View(final_data)
head(final_data)
names(final_data)

# Always Disconnect
dbDisconnect(con)
