#-------------------------------------------------------------------------------
# NMLS data load and standardise
#-------------------------------------------------------------------------------
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
library(tidyverse)
library(tibble)
library(janitor)

# Set working directory
setwd("/Users/savannahanderson/Desktop/wd/masters")

# Set your folder path
path <- "~/Desktop/ANGLING"

#-------------------------------------------------------------------------------
# Load and clean each table, then combine each kind
#-------------------------------------------------------------------------------
COM <- read_excel(file.path(path, "COMMERCIAL 1985-2023.xlsx")) %>% clean_names()
OBS1 <- read_excel(file.path(path, "OBSERVER DATA 1985-2000.xlsx")) %>% clean_names()
OBS2 <- read_excel(file.path(path, "OBSERVER DATA 2001-2005.xlsx")) %>% clean_names()
OBS3 <- read_excel(file.path(path, "OBSERVER DATA 2006.xlsx")) %>% clean_names()
OBS4 <- read_excel(file.path(path, "OBSERVER DATA 2007-2010.xlsx")) %>% clean_names()
species_info <- read_excel(file.path(path, "Species_info.xlsx")) %>% clean_names()
coords <- read_excel(file.path(path, "tblNewGridLocality.xlsx")) %>% clean_names()

# Tag COM dataset
COM <- COM %>%
  mutate(data_source = "COM_1985_2023") %>%
  mutate(catch_date = as.Date(paste(catch_year, catch_month, catch_day, sep = "-"), format = "%Y/%m/%d"))

COM <- COM %>%
  mutate(
    date = as.Date(paste(catch_year, catch_month, catch_day, sep = "-")),
    year = catch_year,
    date = format(date, "%d-%m-%Y")
  )

head(OBS1$obs_date)
head(OBS2$obs_date)
head(OBS3$obs_date)
head(OBS4$obs_date)
head(COM$date)

#fix dates
OBS1 <- OBS1 %>%
  mutate(
    obs_date = mdy(obs_date),
    year = year(obs_date),
    date = format(obs_date, "%d/%m/%Y")
  )
OBS2 <- OBS2 %>%
  mutate(
    obs_date = as.Date(mdy_hms(obs_date)),
    year = year(obs_date),
    date = format(obs_date, "%d/%m/%Y")
  )
OBS3 <- OBS3 %>%
  mutate(
    obs_date = as.Date(mdy_hms(obs_date)),
    year = year(obs_date),
    date = format(obs_date, "%d/%m/%Y")
  )
OBS4 <- OBS4 %>%
  mutate(
    obs_date = as.Date(dmy_hms(obs_date)),
    year = year(obs_date),
    date = format(obs_date, "%d/%m/%Y")
  )
# Tag and clean observer data
OBS1 <- OBS1 %>% mutate(data_source = "OBS1_1985_2000")
OBS2 <- OBS2 %>% mutate(data_source = "OBS2_2001_2005")
OBS3 <- OBS3 %>% mutate(data_source = "OBS3_2006")
OBS4 <- OBS4 %>% mutate(data_source = "OBS4_2007_2010")
# Combine observer data
OBS_all <- bind_rows(OBS1, OBS2, OBS3, OBS4)

#-------------------------------------------------------------------------------
# #Standardise gear type
#-------------------------------------------------------------------------------
standardise_gear <- function(gear) {
  case_when(
    gear == "LINE"  ~ "LINE",
    gear == "POLE"  ~ "POLE",
    gear == "LONGL"  ~ "LONG_LINE",
    gear == "SHROD"  ~ "SHORTHAND_ROD",
    gear == "SHSPR"  ~ "SHORT_SPEAR",
    gear == "LINE"  ~ "LINE",
    gear == "SPEAR"  ~ "SPEAR",
    gear == "TRAWL"  ~ "TRAWL",
    gear == "PURSE"  ~ "PURSE",
    gear == "RES"    ~ "RESEARCH",
    gear == "BEACH"  ~ "BEACH_SEINE",
    gear == "SHORE"  ~ "SHORE",
    TRUE ~ gear
  )
}

OBS_all <- OBS_all %>% mutate(gear_std = standardise_gear(gear_type))
COM     <- COM     %>% mutate(gear_std = standardise_gear(gear_type))

#-------------------------------------------------------------------------------
#  Deduplicate coords and Join species info and cleaned coordinates
#-------------------------------------------------------------------------------
coords_dedup <- coords %>%
  group_by(locality) %>%
  summarise(
    grid_id = first(grid_id),
    g_long = mean(g_long),
    g_lat = mean(g_lat),
    locality_name = first(locality_name),
    .groups = "drop"
  )

# COMMERICAL DATA
COMdat <- COM %>%
  left_join(species_info, by = c("species" = "species_code")) %>%
  left_join(coords_dedup, by = "locality")

# OBSERVER DATA
OBSdat <- OBS_all %>%
  left_join(species_info, by = c("species" = "species_code")) %>%
  left_join(coords_dedup, by = "locality")

#-------------------------------------------------------------------------------
#select and rename columns
#-------------------------------------------------------------------------------
colnames(OBSdat)
colnames(COMdat)
#OBSERVER DATA
OBSdat <- OBSdat %>%
  rename(species_code = species) %>%
  mutate(scientific_name = paste(genus, species_name, sep = " ")) %>%
  select(
    obs_date,
    samp_locality,
    area_code,
    vessel_type,
    shore_dist,
    depth,
    species_code,
    number,
    data_source,
    gear_std,
    id,
    phylum,
    class,
    tax_order,
    family,
    genus,
    species_name,
    common_name,
    scientific_name,  
    grid_id,
    g_long,
    g_lat,
    locality_name,
    year,
    date
  )
#COMMERCIAL DATA
COMdat <- COMdat %>%
  rename(species_code = species) %>%
  mutate(scientific_name = paste(genus, species_name, sep = " ")) %>%
  select(
    catch_year,
    locality,
    shore_dist,
    species_code,
    data_source,
    gear_std,
    id,
    phylum,
    class,
    tax_order,
    family,
    genus,
    species_name,
    common_name,
    scientific_name,
    grid_id,
    g_long,
    g_lat,
    locality_name,
    date,
    year
  )
#-------------------------------------------------------------------------------
#next script follows on with OBSdat and COMdat
#-------------------------------------------------------------------------------


#SCALE
library(dplyr)
library(lubridate)
library(geosphere)

# COM: robust Date
COM_scale <- COMdat %>%
  mutate(
    sample_date = as.Date(paste(catch_year, catch_month, catch_day, sep = "-")),
    year = year(sample_date),
    month = month(sample_date)
  )

# OBS: already has obs_date as Date
OBS_scale <- OBSdat %>%
  mutate(
    sample_date = as.Date(obs_date),
    year = year(sample_date),
    month = month(sample_date)
  )

temporal_summary <- function(df, label) {
  df %>%
    mutate(event_id = paste(sample_date, locality, gear_std, sep = "_")) %>%
    summarise(
      Source = label,
      n_records = n(),
      n_events = n_distinct(event_id),
      first_year = min(year, na.rm = TRUE),
      last_year  = max(year, na.rm = TRUE),
      n_years    = n_distinct(year),
      start_date = min(sample_date, na.rm = TRUE),
      end_date   = max(sample_date, na.rm = TRUE),
      n_days_sampled = n_distinct(sample_date),
      n_months_sampled = n_distinct(paste(year, month))
    )
}

temporal_COM <- temporal_summary(COM_scale, "COM")
temporal_OBS <- temporal_summary(OBS_scale, "OBS")

bind_rows(temporal_COM, temporal_OBS)

annual_effort_linefish <- function(df, label) {
  df %>%
    mutate(event_id = paste(sample_date, locality, gear_std, sep = "_")) %>%
    group_by(year) %>%
    summarise(
      Source = label,
      n_events = n_distinct(event_id),
      n_records = n(),
      .groups = "drop"
    ) %>%
    arrange(year)
}

annual_COM <- annual_effort_linefish(COM_scale, "COM")
annual_OBS <- annual_effort_linefish(OBS_scale, "OBS")

seasonal_linefish <- function(df, label) {
  df %>%
    mutate(
      season = case_when(
        month %in% c(12,1,2) ~ "Summer",
        month %in% c(3,4,5)  ~ "Autumn",
        month %in% c(6,7,8)  ~ "Winter",
        month %in% c(9,10,11)~ "Spring"
      )
    ) %>%
    count(season) %>%
    mutate(Source = label)
}

season_COM <- seasonal_linefish(COM_scale, "COM")
season_OBS <- seasonal_linefish(OBS_scale, "OBS")
bind_rows(season_COM, season_OBS)


depth_OBS <- OBS_scale %>%
  mutate(depth_m = as.numeric(depth)) %>%
  summarise(
    Source = "OBS",
    n_records = n(),
    n_with_depth = sum(!is.na(depth_m)),
    mean_depth_m = mean(depth_m, na.rm = TRUE),
    median_depth_m = median(depth_m, na.rm = TRUE),
    sd_depth_m = sd(depth_m, na.rm = TRUE),
    min_depth_m = min(depth_m, na.rm = TRUE),
    max_depth_m = max(depth_m, na.rm = TRUE),
    q25 = quantile(depth_m, 0.25, na.rm = TRUE),
    q75 = quantile(depth_m, 0.75, na.rm = TRUE)
  )

depth_OBS

OBS_scale %>%
  mutate(depth_m = as.numeric(depth)) %>%
  group_by(gear_std) %>%
  summarise(
    n = n(),
    n_with_depth = sum(!is.na(depth_m)),
    median_depth_m = median(depth_m, na.rm = TRUE),
    q25 = quantile(depth_m, 0.25, na.rm = TRUE),
    q75 = quantile(depth_m, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

#Depth information not recorded in commercial linefishing data

spatial_spacing <- function(df, label) {
  df2 <- df %>%
    mutate(
      latitude = as.numeric(g_lat),
      longitude = as.numeric(g_long)
    ) %>%
    filter(!is.na(latitude), !is.na(longitude), !is.na(sample_date)) %>%
    arrange(sample_date)
  
  df2 %>%
    mutate(
      spacing_km = geosphere::distHaversine(
        cbind(longitude, latitude),
        cbind(lag(longitude), lag(latitude))
      ) / 1000
    ) %>%
    filter(!is.na(spacing_km), spacing_km > 0, spacing_km < 500) %>%  # drop jumps
    summarise(
      Source = label,
      n_pairs = n(),
      mean_spacing_km = mean(spacing_km),
      median_spacing_km = median(spacing_km),
      q25 = quantile(spacing_km, 0.25),
      q75 = quantile(spacing_km, 0.75),
      max_spacing_km = max(spacing_km)
    )
}

spatial_COM <- spatial_spacing(COM_scale, "COM")
spatial_OBS <- spatial_spacing(OBS_scale, "OBS")

bind_rows(spatial_COM, spatial_OBS)

spatial_OBS_by_vessel <- OBS_scale %>%
  mutate(
    latitude = as.numeric(g_lat),
    longitude = as.numeric(g_long)
  ) %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(sample_date), !is.na(vessel_num)) %>%
  arrange(vessel_num, sample_date) %>%
  group_by(vessel_num) %>%
  mutate(
    spacing_km = geosphere::distHaversine(
      cbind(longitude, latitude),
      cbind(lag(longitude), lag(latitude))
    ) / 1000
  ) %>%
  ungroup() %>%
  filter(!is.na(spacing_km), spacing_km > 0, spacing_km < 500) %>%
  summarise(
    n_pairs = n(),
    median_spacing_km = median(spacing_km),
    q25 = quantile(spacing_km, 0.25),
    q75 = quantile(spacing_km, 0.75),
    max_spacing_km = max(spacing_km)
  )

spatial_OBS_by_vessel






