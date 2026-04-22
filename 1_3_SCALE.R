#SCALE2

#FM
names(final_data_joined)

library(dplyr)
library(stringr)
library(lubridate)

#----------------------------
# 0) Start from your joined data
#----------------------------
df <- final_data_joined %>%
  mutate(
    DataSetID = as.character(DataSetID),
    MethodNew = na_if(str_trim(MethodNew), ""),
    SamplingLocation = na_if(str_trim(SamplingLocation), ""),
    Location = na_if(str_trim(Location), "")
  )

#----------------------------
# 1) Build a method-centric event key
#    - Prefer SamplingLocation; fallback to Location
#    - Use StartDate/EndDate if available; else Year/Season
#    - Add coarse lat/long rounding when present (optional but helpful)
#----------------------------
library(dplyr)
library(stringr)
library(lubridate)

library(stringr)
library(lubridate)

safe_parse_date <- function(x) {
  
  # If already Date
  if (inherits(x, "Date")) return(x)
  
  x_chr_all <- as.character(x)
  x_chr_all <- str_trim(x_chr_all)
  
  # Normalise common missing tokens
  x_chr_all[x_chr_all %in% c("", "NA", "N/A", "Unknown", "unknown")] <- NA_character_
  
  # Try numeric for Excel-style dates
  suppressWarnings({
    x_num <- as.numeric(x_chr_all)
  })
  
  out <- rep(as.Date(NA), length(x_chr_all))
  
  # Excel-style numeric dates (roughly year 1954+)
  is_excel <- !is.na(x_num) & x_num > 20000
  out[is_excel] <- as.Date(x_num[is_excel], origin = "1899-12-30")
  
  # Year-only strings like "1984"
  is_year <- !is_excel & !is.na(x_chr_all) & str_detect(x_chr_all, "^[12][0-9]{3}$")
  out[is_year] <- as.Date(paste0(x_chr_all[is_year], "-01-01"))
  
  # Parse remaining date strings
  is_other <- !(is_excel | is_year) & !is.na(x_chr_all)
  
  parsed <- suppressWarnings(parse_date_time(
    x_chr_all[is_other],
    orders = c(
      "Ymd", "Y-m-d", "Y/m/d",
      "dmY", "dmy", "d-m-Y", "d/m/Y",
      "mdY", "mdy", "m/d/Y", "m-d-Y",
      "Ymd HMS", "Y-m-d HMS", "Y/m/d HMS"
    ),
    tz = "UTC"
  ))
  
  out[is_other] <- as.Date(parsed)
  
  out
}

df <- df %>%
  mutate(
    site_key = coalesce(na_if(str_trim(SamplingLocation), ""),
                        na_if(str_trim(Location), ""),
                        "UNKNOWN_SITE"),
    
    StartDate_date = safe_parse_date(StartDate),
    EndDate_date   = safe_parse_date(EndDate),
    
    time_key = case_when(
      !is.na(StartDate_date) & !is.na(EndDate_date) ~ paste0(StartDate_date, "_", EndDate_date),
      !is.na(StartDate_date) &  is.na(EndDate_date) ~ paste0(StartDate_date, "_NA"),
      is.na(StartDate_date)  & !is.na(EndDate_date) ~ paste0("NA_", EndDate_date),
      !is.na(Year.x) | !is.na(Season)               ~ paste0(
        coalesce(as.character(Year.x), as.character(Year.y), "NA"),
        "_",
        coalesce(as.character(Season), "NA")
      ),
      TRUE ~ "UNKNOWN_TIME"
    ),
    
    lat_r = ifelse(!is.na(Latitude),  round(as.numeric(Latitude),  3), NA_real_),
    lon_r = ifelse(!is.na(Longitude), round(as.numeric(Longitude), 3), NA_real_),
    coord_key = ifelse(!is.na(lat_r) & !is.na(lon_r), paste0(lat_r, "_", lon_r), "NA_COORD"),
    
    DataSetID = as.character(DataSetID),
    MethodNew = na_if(str_trim(MethodNew), ""),
    
    record_event_id = paste(DataSetID, MethodNew, site_key, time_key, coord_key, sep = "|")
  )

bad_start <- df %>%
  mutate(StartDate_chr = as.character(StartDate)) %>%
  filter(!is.na(StartDate_chr)) %>%
  filter(is.na(safe_parse_date(StartDate_chr))) %>%
  distinct(StartDate_chr) %>%
  slice_head(n = 30)

bad_end <- df %>%
  mutate(EndDate_chr = as.character(EndDate)) %>%
  filter(!is.na(EndDate_chr)) %>%
  filter(is.na(safe_parse_date(EndDate_chr))) %>%
  distinct(EndDate_chr) %>%
  slice_head(n = 30)

bad_start
bad_end

#----------------------------
# 2) Species-per-record (event)
#----------------------------
record_spp <- df %>%
  filter(!is.na(MethodNew), !is.na(Species), Species != "", !is.na(record_event_id)) %>%
  group_by(DataSetID, MethodNew, record_event_id) %>%
  summarise(
    n_species_record = n_distinct(Species),
    .groups = "drop"
  )

#----------------------------
# 3) Summaries you can use in thesis tables (by method)
#----------------------------
FM_method_summary <- record_spp %>%
  group_by(MethodNew) %>%
  summarise(
    n_datasets = n_distinct(DataSetID),
    n_records  = n_distinct(record_event_id),
    mean_species_per_record = mean(n_species_record, na.rm = TRUE),
    max_species_per_record  = max(n_species_record, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_records))

FM_method_summary

FM_type <- df %>%
  distinct(DataSetID, DataType) %>%
  mutate(
    DataType = recode(DataType,
                      "I" = "Fishery-independent",
                      "D" = "Fishery-dependent",
                      .default = "Unknown")
  ) %>%
  count(DataType) %>%
  mutate(pct = round(100 * n / sum(n), 1))

FM_class <- df %>%
  distinct(DataSetID, DataClassification) %>%
  mutate(
    DataClassification = recode(DataClassification,
                                "S" = "Survey (fishery-independent)",
                                "O" = "Observer (fishery-dependent)",
                                "C" = "Catch return / commercial (fishery-dependent)",
                                .default = "Unknown")
  ) %>%
  count(DataClassification) %>%
  mutate(pct = round(100 * n / sum(n), 1))

FM_type
FM_class


#NOW DO HEIRARCHY

library(geosphere)

calc_extent_km <- function(lon, lat) {
  ok <- !is.na(lon) & !is.na(lat)
  if (sum(ok) < 2) return(NA_real_)
  
  coords <- cbind(lon[ok], lat[ok])
  d <- distm(coords, fun = distHaversine)
  max(d, na.rm = TRUE) / 1000
}

dataset_spatial <- df %>%
  filter(!is.na(MethodNew),
         !is.na(Longitude),
         !is.na(Latitude)) %>%
  group_by(MethodNew, DataSetID) %>%
  summarise(
    spatial_extent_km = calc_extent_km(Longitude, Latitude),
    n_sites = n_distinct(site_key),
    .groups = "drop"
  )

dataset_temporal <- df %>%
  filter(!is.na(MethodNew)) %>%
  group_by(MethodNew, DataSetID) %>%
  summarise(
    start_date = if (all(is.na(StartDate_date))) NA else min(StartDate_date, na.rm = TRUE),
    end_date   = if (all(is.na(EndDate_date)))   NA else max(EndDate_date,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    temporal_years = as.numeric(difftime(end_date, start_date, units = "days")) / 365.25,
    temporal_years = ifelse(is.infinite(temporal_years), NA, temporal_years)
  )


dataset_scales <- dataset_spatial %>%
  full_join(dataset_temporal, by = c("MethodNew", "DataSetID"))

FM_method_spatial <- dataset_scales %>%
  group_by(MethodNew) %>%
  summarise(
    n_datasets = n_distinct(DataSetID),
    n_with_extent = sum(!is.na(spatial_extent_km)),
    
    mean_km = ifelse(n_with_extent > 0,
                     mean(spatial_extent_km, na.rm = TRUE), NA_real_),
    median_km = ifelse(n_with_extent > 0,
                     median(spatial_extent_km, na.rm = TRUE), NA_real_),
    sd_km   = ifelse(n_with_extent > 1,
                     sd(spatial_extent_km, na.rm = TRUE), NA_real_),
    min_km  = ifelse(n_with_extent > 0,
                     min(spatial_extent_km, na.rm = TRUE), NA_real_),
    max_km  = ifelse(n_with_extent > 0,
                     max(spatial_extent_km, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

FM_method_temporal <- dataset_scales %>%
  group_by(MethodNew) %>%
  summarise(
    n_with_time = sum(!is.na(temporal_years)),
    
    mean_years = ifelse(n_with_time > 0,
                        mean(temporal_years, na.rm = TRUE), NA_real_),
    sd_years   = ifelse(n_with_time > 1,
                        sd(temporal_years, na.rm = TRUE), NA_real_),
    min_years  = ifelse(n_with_time > 0,
                        min(temporal_years, na.rm = TRUE), NA_real_),
    max_years  = ifelse(n_with_time > 0,
                        max(temporal_years, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )


FM_method_scale_summary <- FM_method_spatial %>%
  left_join(FM_method_temporal, by = "MethodNew") %>%
  arrange(desc(mean_km))


FM_method_scale_summary


#qc against werid bugs here



#-------------------------------------------------------------------------------#-------------------------------------------------------------------------------
#museum
#-------------------------------------------------------------------------------#-------------------------------------------------------------------------------
library(dplyr)
library(stringr)
library(lubridate)
library(geosphere)

# --- helper: safe numeric parse for lat/long fields
as_num <- function(x) suppressWarnings(as.numeric(x))

names(museumdat)

#--------------------------------------------------
# CLEAN DATA
#--------------------------------------------------

museum_clean <- museumdat %>%
  mutate(
    record_id = as.character(gbifID),
    sp  = species,
    
    lat = as_num(decimalLatitude),
    lon = as_num(decimalLongitude),
    
    year_num = suppressWarnings(as.integer(year)),
    event_year = suppressWarnings(year(ymd(eventDate))),
    
    year_final = coalesce(year_num, event_year),
    
    has_coords = !is.na(lat) & !is.na(lon),
    has_year   = !is.na(year_final)
  ) %>%
  filter(!is.na(record_id), !is.na(sp)) %>%
  distinct(record_id, .keep_all = TRUE)

#--------------------------------------------------
# TEMPORAL SUMMARY
#--------------------------------------------------

museum_temporal_summary <- museum_clean %>%
  filter(has_year) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(sp),
    
    min_year  = min(year_final, na.rm = TRUE),
    max_year  = max(year_final, na.rm = TRUE),
    
    range_years = max_year - min_year,
    
    mean_year = mean(year_final, na.rm = TRUE),
    median_year = median(year_final, na.rm = TRUE),
    iqr_year = IQR(year_final, na.rm = TRUE)
  )

museum_temporal_summary

#--------------------------------------------------
# YEAR DISTRIBUTION
#--------------------------------------------------

museum_year_dist <- museum_clean %>%
  filter(has_year) %>%
  summarise(
    mean_year  = mean(year_final),
    sd_year    = sd(year_final),
    median_year = median(year_final),
    q25_year   = quantile(year_final, 0.25),
    q75_year   = quantile(year_final, 0.75),
    iqr_year   = IQR(year_final),
    min_year   = min(year_final),
    max_year   = max(year_final)
  )

museum_year_dist

#--------------------------------------------------
# SPATIAL SUMMARY
#--------------------------------------------------

museum_spatial_summary <- museum_clean %>%
  filter(has_coords) %>%
  summarise(
    n_records_coords = n(),
    n_species_coords = n_distinct(sp),
    
    min_lat = min(lat),
    max_lat = max(lat),
    
    min_lon = min(lon),
    max_lon = max(lon),
    
    # extent proxy: distance between bbox corners
    extent_km = distHaversine(
      c(min_lon, min_lat),
      c(max_lon, max_lat)
    ) / 1000
  )

museum_spatial_summary

#--------------------------------------------------
# EVENT APPROXIMATION (using institution + year)
#--------------------------------------------------

museum_eventish <- museum_clean %>%
  mutate(
    event_key = str_c(institutionCode, year_final, sep = "_")
  )

museum_event_summary <- museum_eventish %>%
  summarise(
    n_events = n_distinct(event_key),
    mean_records_per_event = n() / n_events
  )

museum_event_summary

#--------------------------------------------------
# SOUTH AFRICA FILTER (bbox approximation)
#--------------------------------------------------

museum_SA <- museum_clean %>%
  filter(
    lon >= 10 & lon <= 40,
    lat >= -40 & lat <= -20
  )

#--------------------------------------------------
# FINAL SUMMARY TABLE
#--------------------------------------------------

museum_summary_all <- museum_SA %>%
  summarise(
    n_records_total = n(),
    n_species_total = n_distinct(sp),
    
    n_records_year = sum(has_year),
    n_species_year = n_distinct(sp[has_year]),
    
    n_records_coords = sum(has_coords),
    n_species_coords = n_distinct(sp[has_coords]),
    
    min_year = min(year_final, na.rm = TRUE),
    q25_year = quantile(year_final, 0.25, na.rm = TRUE),
    median_year = median(year_final, na.rm = TRUE),
    q75_year = quantile(year_final, 0.75, na.rm = TRUE),
    max_year = max(year_final, na.rm = TRUE),
    
    iqr_year = IQR(year_final, na.rm = TRUE)
  )

museum_summary_all
#-------------------------------------------------------------------------------#-------------------------------------------------------------------------------
#INAT 
#-------------------------------------------------------------------------------#-------------------------------------------------------------------------------
as_num <- function(x) suppressWarnings(as.numeric(x))
names(inatdat)


library(dplyr)
library(stringr)
library(lubridate)
library(geosphere)

#-------------------------------------------------------------------------------
# iNaturalist
#-------------------------------------------------------------------------------

# helper for numeric parsing
as_num <- function(x) suppressWarnings(as.numeric(x))

names(inatdat)

#--------------------------------------------------
# CLEAN DATA
#--------------------------------------------------

inat_clean <- inatdat %>%
  mutate(
    record_id = as.character(gbifID),
    sp = species,
    
    lat = as_num(decimalLatitude),
    lon = as_num(decimalLongitude),
    
    year_num = suppressWarnings(as.integer(year)),
    event_year = suppressWarnings(year(ymd(eventDate))),
    
    year_final = coalesce(year_num, event_year),
    
    has_coords = !is.na(lat) & !is.na(lon),
    has_year   = !is.na(year_final)
  ) %>%
  filter(!is.na(record_id), !is.na(sp)) %>%
  distinct(record_id, .keep_all = TRUE)

#--------------------------------------------------
# SOUTH AFRICA FILTER (bbox approximation)
#--------------------------------------------------

inat_SA <- inat_clean %>%
  filter(
    lon >= 10 & lon <= 40,
    lat >= -40 & lat <= -20
  )

#--------------------------------------------------
# OVERALL SUMMARY
#--------------------------------------------------

inat_summary_all <- inat_SA %>%
  summarise(
    n_records_total = n(),
    n_species_total = n_distinct(sp),
    
    n_records_year  = sum(has_year),
    n_species_year  = n_distinct(sp[has_year]),
    
    n_records_coords = sum(has_coords),
    n_species_coords = n_distinct(sp[has_coords]),
    
    min_year = min(year_final, na.rm = TRUE),
    q25_year = quantile(year_final, 0.25, na.rm = TRUE),
    median_year = median(year_final, na.rm = TRUE),
    q75_year = quantile(year_final, 0.75, na.rm = TRUE),
    max_year = max(year_final, na.rm = TRUE),
    
    iqr_year = IQR(year_final, na.rm = TRUE)
  )

inat_summary_all

#--------------------------------------------------
# SPATIAL SUMMARY
#--------------------------------------------------

inat_spatial_summary <- inat_SA %>%
  filter(has_coords) %>%
  summarise(
    n_records_coords = n(),
    n_species_coords = n_distinct(sp),
    
    min_lat = min(lat),
    max_lat = max(lat),
    
    min_lon = min(lon),
    max_lon = max(lon),
    
    extent_km = distHaversine(
      c(min_lon, min_lat),
      c(max_lon, max_lat)
    ) / 1000
  )

inat_spatial_summary

#--------------------------------------------------
# SPATIAL SAMPLING BIAS DIAGNOSTICS
#--------------------------------------------------

inat_bias_diag <- inat_SA %>%
  filter(has_coords) %>%
  summarise(
    n_unique_points = n_distinct(
      paste(round(lat,4), round(lon,4))
    ),
    
    n_records = n(),
    
    mean_records_per_point = n_records / n_unique_points
  )

inat_bias_diag

#--------------------------------------------------
# MOST SAMPLED LOCATIONS
#--------------------------------------------------

inat_top_sites <- inat_SA %>%
  filter(has_coords) %>%
  mutate(
    site_key = paste(round(lat,3), round(lon,3))
  ) %>%
  count(site_key, sort = TRUE) %>%
  slice_head(n = 10)

inat_top_sites

