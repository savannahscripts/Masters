### LOOKING AT SCALE VARIANCE AND RECORDS
# ------------------------------------------------------------------------------
#START WITH MWT
# ------------------------------------------------------------------------------

library(geosphere)
path <- "~/Desktop/MW TRAWL"

#Load tables (DD)
#DD is a combo of directed DFFE surveys and DD observer program
#so for source must say this is survey
DD_SPCOMP <- read_excel(file.path(path, "DD_SPCOMP.xlsx")) %>% clean_names()
DD_SPECIES <- read_excel(file.path(path, "DD_SPECIES.xlsx")) %>% clean_names()
DD_TRAWL <- read_excel(file.path(path, "DD_TRAWL.xlsx")) %>% clean_names()
DD_TRIPP <- read_excel(file.path(path, "DD_TRIPP.xlsx")) %>% clean_names()

#Load tables (OROP)
#OROP is historical data CAPFISH TRAWL 
#this is observer
OROP_Trawl_LF <- read_excel(file.path(path, "OROP_Trawl_LF.xlsx")) %>% clean_names()
OROP_Trawl_Trawl <- read_excel(file.path(path, "OROP_Trawl_Trawl.xlsx")) %>% clean_names()
OROP_Trip <- read_excel(file.path(path, "OROP_Trip.xlsx")) %>% clean_names()
OROP_zref_Species <- read_excel(file.path(path, "OROP_zref_Species.xlsx")) %>% clean_names()

#perform join 
# Join DD_SPCOMP to DD_SPECIES by species_id
DD_join1 <- DD_SPCOMP %>%
  left_join(DD_SPECIES, by = "species_id")
# join to DD_TRAWL by trip_no and trawl_no
DD_join2 <- DD_join1 %>%
  left_join(DD_TRAWL, by = c("trip_no", "trawl_no"))
# join to DD_TRIPP by trip_no
DD_final <- DD_join2 %>%
  left_join(DD_TRIPP, by = "trip_no")
#Join tables 
# Join length-frequency to species by spec_id
OROP_join1 <- OROP_Trawl_LF %>%
  left_join(OROP_zref_Species, by = "spec_id")
OROP_join1_fixed <- OROP_join1 %>%
  select(-trawl_id.y, -trawl) %>%     # remove unwanted columns
  rename(trawl_id = trawl_id.x)       # restore expected name
# Join trawl metadata
OROP_join2 <- OROP_join1_fixed %>%
  left_join(OROP_Trawl_Trawl, by = c("dd_trip_id", "trawl_id"))
# Join trip metadata
OROP_final <- OROP_join2 %>%
  left_join(OROP_Trip, by = "dd_trip_id")

###
DD_clean <- DD_final %>%
  mutate(
    source = "DD",
    species_id = as.character(species_id),
    scientific_name = tolower(scientific_name)
  ) %>%
  select(
    source, trip_no, trawl_no, species_id, scientific_name, weight, number, start_lat_deg, start_lat_min,
    start_long_deg, start_long_min,
    everything()
  )

OROP_clean <- OROP_final %>%
  select(-trawl_no) %>%  # Drop any pre-existing trawl_no
  mutate(
    source = "OROP",
    species_id = as.character(spec_id),
    scientific_name = tolower(paste(genus, specie)),
    number = rowSums(select(., starts_with("x")), na.rm = TRUE)
  ) %>%
  rename(
    trip_no = dd_trip_id,
    trawl_no = trawl_id
  ) %>%
  select(
    source, trip_no, trawl_no, species_id, scientific_name, weight, number, start_lat_deg, start_lat_min,
    start_long_deg, start_long_min,
    everything()
  )

#check spatial and temporal scale
names(DD_clean)
names(OROP_clean)
head(DD_clean)
head(OROP_clean)

#SPARIAL SCALE
degmin_to_dec <- function(deg, min) {
  deg + (min / 60)
}

DD_geo <- DD_clean %>%
  mutate(
    start_lat = degmin_to_dec(start_lat_deg, start_lat_min),
    start_lon = degmin_to_dec(start_long_deg, start_long_min),
    end_lat   = degmin_to_dec(end_lat_deg, end_lat_min),
    end_lon   = degmin_to_dec(end_long_deg, end_long_min)
  )

OROP_geo <- OROP_clean %>%
  mutate(
    start_lat = degmin_to_dec(start_lat_deg, start_lat_min),
    start_lon = degmin_to_dec(start_long_deg, start_long_min),
    end_lat   = degmin_to_dec(end_lat_deg, end_lat_min),
    end_lon   = degmin_to_dec(end_long_deg, end_long_min)
  )

trawl_scale <- function(df) {
  df %>%
    filter(
      !is.na(start_lat), !is.na(start_lon),
      !is.na(end_lat), !is.na(end_lon)
    ) %>%
    distinct(source, trip_no, trawl_no,
             start_lat, start_lon, end_lat, end_lon) %>%
    rowwise() %>%
    mutate(
      trawl_dist_km = distHaversine(
        c(start_lon, start_lat),
        c(end_lon, end_lat)
      ) / 1000
    ) %>%
    ungroup()
}

DD_trawl_scale   <- trawl_scale(DD_geo)
OROP_trawl_scale <- trawl_scale(OROP_geo)

bind_rows(DD_trawl_scale, OROP_trawl_scale) %>%
  group_by(source) %>%
  summarise(
    mean_km = mean(trawl_dist_km, na.rm = TRUE),
    min_km  = min(trawl_dist_km, na.rm = TRUE),
    max_km  = max(trawl_dist_km, na.rm = TRUE),
    sd_km   = sd(trawl_dist_km, na.rm = TRUE),
    n_trawls = n()
  )

trip_scale <- function(df) {
  df %>%
    filter(!is.na(start_lat), !is.na(start_lon)) %>%
    distinct(source, trip_no, trawl_no, start_lat, start_lon) %>%
    group_by(source, trip_no) %>%
    summarise(
      trip_extent_km = if (n() > 1) {
        coords <- cbind(start_lon, start_lat)
        max(distm(coords, fun = distHaversine)) / 1000
      } else {
        NA_real_
      },
      n_trawls = n(),
      .groups = "drop"
    )
}


DD_trip_scale   <- trip_scale(DD_geo)
OROP_trip_scale <- trip_scale(OROP_geo)

bind_rows(DD_trip_scale, OROP_trip_scale) %>%
  group_by(source) %>%
  summarise(
    mean_trip_extent_km = mean(trip_extent_km, na.rm = TRUE),
    min_trip_extent_km  = min(trip_extent_km, na.rm = TRUE),
    max_trip_extent_km  = max(trip_extent_km, na.rm = TRUE),
    sd_trip_extent_km   = sd(trip_extent_km, na.rm = TRUE),
    n_trips = n()
  )


trawl_div <- function(df) {
  df %>%
    filter(!is.na(scientific_name)) %>%
    group_by(source, trip_no, trawl_no) %>%
    summarise(
      richness = n_distinct(scientific_name),
      .groups = "drop"
    )
}

DD_trawl_div   <- trawl_div(DD_clean)
OROP_trawl_div <- trawl_div(OROP_clean)

bind_rows(DD_trawl_div, OROP_trawl_div) %>%
  group_by(source) %>%
  summarise(
    mean_richness = mean(richness),
    sd_richness   = sd(richness),
    min_richness  = min(richness),
    max_richness  = max(richness)
  )

#TEMPORAL
trawl_temporal_scale_DD <- function(df) {
  df %>%
    filter(!is.na(net_deployed), !is.na(net_on_board)) %>%
    distinct(source, trip_no, trawl_no, net_deployed, net_on_board) %>%
    mutate(
      trawl_duration_h = as.numeric(difftime(net_on_board, net_deployed, units = "hours")),
      trawl_duration_h = ifelse(trawl_duration_h < 0, trawl_duration_h + 24, trawl_duration_h)
    )
}

DD_trawl_time   <- trawl_temporal_scale_DD(DD_clean)

trawl_temporal_scale_OROP <- function(df) {
  df %>%
    filter(!is.na(time_net_deployed), !is.na(time_net_on_bot)) %>%
    distinct(source, trip_no, trawl_no, time_net_deployed, time_net_on_bot) %>%
    mutate(
      trawl_duration_h = as.numeric(difftime(time_net_on_bot, time_net_deployed, units = "hours")),
      # If negative, assume it crossed midnight -> add 24 hours
      trawl_duration_h = ifelse(trawl_duration_h < 0, trawl_duration_h + 24, trawl_duration_h)
    ) %>%
    # optional sanity filter (e.g., remove anything > 12h if that’s impossible for your trawls)
    # filter(trawl_duration_h <= 12)
    identity()
}
OROP_trawl_time <- trawl_temporal_scale_OROP(OROP_clean)

DD_trawl_time %>% summarise(n_23plus = sum(trawl_duration_h >= 20, na.rm = TRUE))
OROP_trawl_time %>% summarise(n_23plus = sum(trawl_duration_h >= 20, na.rm = TRUE))


bind_rows(DD_trawl_time, OROP_trawl_time) %>%
  group_by(source) %>%
  summarise(
    median_h = median(trawl_duration_h, na.rm = TRUE),
    IQR_h    = IQR(trawl_duration_h, na.rm = TRUE),
    mean_h   = mean(trawl_duration_h, na.rm = TRUE),
    sd_h     = sd(trawl_duration_h, na.rm = TRUE),
    max_h    = max(trawl_duration_h, na.rm = TRUE),
    n_trawls = n()
  )

#exclude outliers (2 for OROP and 4 for DD)
DD_trawl_time_qc   <- DD_trawl_time %>% filter(trawl_duration_h < 20)
OROP_trawl_time_qc <- OROP_trawl_time %>% filter(trawl_duration_h < 20)

bind_rows(DD_trawl_time_qc, OROP_trawl_time_qc) %>%
  group_by(source) %>%
  summarise(
    median_h = median(trawl_duration_h, na.rm = TRUE),
    IQR_h    = IQR(trawl_duration_h, na.rm = TRUE),
    mean_h   = mean(trawl_duration_h, na.rm = TRUE),
    sd_h     = sd(trawl_duration_h, na.rm = TRUE),
    max_h    = max(trawl_duration_h, na.rm = TRUE),
    n_trawls = n()
  )

# ------------------------------------------------------------
# MWTRAWL haul-level temporal scale (DD + OROP combined)
# ------------------------------------------------------------

MW_trawl_time_all <- bind_rows(
  DD_trawl_time_qc  %>% select(source, trip_no, trawl_no, trawl_duration_h),
  OROP_trawl_time_qc %>% select(source, trip_no, trawl_no, trawl_duration_h)
) %>%
  # in case any duplicates exist after joins
  distinct(source, trip_no, trawl_no, .keep_all = TRUE) %>%
  filter(!is.na(trawl_duration_h), trawl_duration_h > 0)

MW_trawl_time_summary <- MW_trawl_time_all %>%
  summarise(
    dataset = "MWTRAWL (DD + OROP)",
    n_hauls = n(),
    median_h = median(trawl_duration_h, na.rm = TRUE),
    q25_h    = quantile(trawl_duration_h, 0.25, na.rm = TRUE),
    q75_h    = quantile(trawl_duration_h, 0.75, na.rm = TRUE),
    iqr_h    = IQR(trawl_duration_h, na.rm = TRUE),
    mean_h   = mean(trawl_duration_h, na.rm = TRUE),
    sd_h     = sd(trawl_duration_h, na.rm = TRUE),
    min_h    = min(trawl_duration_h, na.rm = TRUE),
    max_h    = max(trawl_duration_h, na.rm = TRUE)
  )

MW_trawl_time_summary %>%
  transmute(
    dataset,
    n_hauls,
    median_h = round(median_h, 2),
    IQR_h = paste0(round(q25_h, 2), "–", round(q75_h, 2))
  )



DD_removed <- nrow(DD_trawl_time) - nrow(DD_trawl_time_qc)
OROP_removed <- nrow(OROP_trawl_time) - nrow(OROP_trawl_time_qc)

DD_removed / nrow(DD_trawl_time) * 100
OROP_removed / nrow(OROP_trawl_time) * 100

#NO OF RECORDS AND SPP PER TRAWL AND PER TRIP
trawl_effort <- function(df) {
  df %>%
    filter(!is.na(scientific_name)) %>%
    group_by(source, trip_no, trawl_no) %>%
    summarise(
      n_records = n(),
      n_species = n_distinct(scientific_name),
      .groups = "drop"
    )
}

DD_trawl_effort   <- trawl_effort(DD_clean)
OROP_trawl_effort <- trawl_effort(OROP_clean)

bind_rows(DD_trawl_effort, OROP_trawl_effort) %>%
  group_by(source) %>%
  summarise(
    mean_records = mean(n_records),
    mean_species = mean(n_species),
    min_species  = min(n_species),
    max_species  = max(n_species),
    sd_species   = sd(n_species)
  )

trip_effort <- function(df) {
  df %>%
    filter(!is.na(scientific_name)) %>%
    group_by(source, trip_no) %>%
    summarise(
      n_records = n(),
      n_species = n_distinct(scientific_name),
      n_trawls  = n_distinct(trawl_no),
      .groups = "drop"
    )
}

DD_trip_effort   <- trip_effort(DD_clean)
OROP_trip_effort <- trip_effort(OROP_clean)

bind_rows(DD_trip_effort, OROP_trip_effort) %>%
  group_by(source) %>%
  summarise(
    mean_records = mean(n_records),
    mean_species = mean(n_species),
    mean_trawls  = mean(n_trawls),
    sd_species   = sd(n_species),
    max_species  = max(n_species)
  )

# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#DEM TRAWL DFFE
# ------------------------------------------------------------------------------
setwd("/Users/savannahanderson/Desktop/wd/masters")
# Set your folder path
path <- "~/Desktop/DEMTRAWL"

# Load each table
trawl <- read_excel(file.path(path, "trawl.xlsx")) %>% clean_names()
cruise <- read_excel(file.path(path, "cruise.xlsx")) %>% clean_names()
catch <- read_excel(file.path(path, "catch.xlsx")) %>% clean_names()
worms <- read_excel(file.path(path, "WoRMS match.xlsx")) %>% clean_names()
ref_species <- read_excel(file.path(path, "REF_species.xlsx")) %>% clean_names()
ref_trawl <- read_excel(file.path(path, "REF_trawl_no.xlsx")) %>% clean_names()
ref_grid <- read_excel(file.path(path, "REF_grid.xlsx")) %>% clean_names()

#load REF grid table as well #join by coast to separate south and west coast
#join using ref grid on grid ID #gridID gives option of coast old and coast new

#check column names
colnames(catch)
colnames(cruise)
colnames(ref_species)
colnames(ref_trawl)
colnames(trawl)
colnames(worms)
names(ref_grid)

#join tables (Catch by trawl)
catch_joined <- catch %>%
  left_join(ref_species, by = "mnemonic")
catch_trawl_joined <- catch_joined %>%
  left_join(trawl, by = c("cruise", "station"))
ref_grid_dedup <- ref_grid %>%
  distinct(grid, .keep_all = TRUE)
catch_full <- catch_trawl_joined %>%
  left_join(ref_grid_dedup, by = "grid")
#keeping ones with cords (using start)
catch_with_coords <- catch_full%>%
  filter(!is.na(start_latitude) & !is.na(start_longitude))
demtrawldata <- catch_with_coords
#CLEANING
demtrawldat <- janitor::clean_names(demtrawldata)

#DEMTRAWL, a trip = cruise.
dem_trawl_scale <- demtrawldat %>%
  filter(!is.na(start_latitude), !is.na(start_longitude),
         !is.na(end_latitude), !is.na(end_longitude)) %>%
  distinct(cruise, trawl_no,
           start_latitude, start_longitude,
           end_latitude, end_longitude) %>%
  rowwise() %>%
  mutate(
    trawl_dist_km = distHaversine(
      c(start_longitude, start_latitude),
      c(end_longitude, end_latitude)
    ) / 1000
  ) %>%
  ungroup()


dem_trawl_summary <- dem_trawl_scale %>%
  group_by(trawl_no) %>%
  summarise(
    n_trawls   = n(),
    mean_dist  = mean(trawl_dist_km, na.rm = TRUE)
  )

dem_trip_scale <- demtrawldat %>%
  filter(!is.na(start_latitude), !is.na(start_longitude)) %>%
  distinct(cruise, trawl_no, start_latitude, start_longitude) %>%
  group_by(cruise) %>%
  summarise(
    trip_extent_km = if (n() > 1) {
      coords <- cbind(start_longitude, start_latitude)
      max(distm(coords, fun = distHaversine)) / 1000
    } else NA_real_,
    n_trawls = n(),
    .groups = "drop"
  )
dem_trip_scale

dem_trawl_time <- demtrawldat %>%
  filter(!is.na(fishing_duration_min)) %>%
  group_by(cruise, trawl_no) %>%
  summarise(
    fishing_duration_min = median(fishing_duration_min, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(trawl_duration_h = fishing_duration_min / 60) %>%
  filter(trawl_duration_h >= 0, trawl_duration_h <= 10)  
summary(dem_trawl_time$trawl_duration_h)

dem_trip_time <- demtrawldat %>%
  mutate(
    date = as.Date(paste(year, month, day, sep = "-"))
  ) %>%
  distinct(cruise, date) %>%
  group_by(cruise) %>%
  summarise(
    trip_duration_days = as.numeric(max(date) - min(date)) + 1,
    .groups = "drop"
  )

dem_trawl_richness <- demtrawldat %>%
  filter(!is.na(scientific_name)) %>%
  group_by(cruise, trawl_no) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(scientific_name),
    .groups = "drop"
  )

dem_trip_richness <- demtrawldat %>%
  filter(!is.na(scientific_name)) %>%
  group_by(cruise) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(scientific_name),
    n_trawls  = n_distinct(trawl_no),
    .groups = "drop"
  )

dem_trawl_effort <- demtrawldat %>%
  filter(!is.na(scientific_name)) %>%
  group_by(cruise, trawl_no) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(scientific_name),
    .groups = "drop"
  )

dem_trip_effort <- demtrawldat %>%
  filter(!is.na(scientific_name)) %>%
  group_by(cruise) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(scientific_name),
    n_trawls  = n_distinct(trawl_no),
    .groups = "drop"
  )

dem_trawl_summary <- dem_trawl_scale %>%
  left_join(dem_trawl_time, by = c("cruise","trawl_no")) %>%
  left_join(dem_trawl_richness, by = c("cruise","trawl_no")) %>%
  summarise(
    mean_dist_km = mean(trawl_dist_km, na.rm = TRUE),
    mean_duration_h = mean(trawl_duration_h, na.rm = TRUE),
    mean_species = mean(n_species, na.rm = TRUE),
    n_trawls = n()
  )

dem_trawl_summary

dem_trawl_full <- dem_trawl_scale %>%
  left_join(dem_trawl_time,   by = c("cruise", "trawl_no")) %>%
  left_join(dem_trawl_effort, by = c("cruise", "trawl_no"))


glimpse(dem_trawl_full)

dem_trawl_summary <- dem_trawl_full %>%
  summarise(
    # Spatial (trawl)
    mean_dist_km = mean(trawl_dist_km, na.rm = TRUE),
    min_dist_km  = min(trawl_dist_km, na.rm = TRUE),
    max_dist_km  = max(trawl_dist_km, na.rm = TRUE),
    sd_dist_km   = sd(trawl_dist_km, na.rm = TRUE),
    
    # Temporal (trawl)
    median_h     = median(trawl_duration_h, na.rm = TRUE),
    IQR_h        = IQR(trawl_duration_h, na.rm = TRUE),
    mean_h       = mean(trawl_duration_h, na.rm = TRUE),
    sd_h         = sd(trawl_duration_h, na.rm = TRUE),
    max_h        = max(trawl_duration_h, na.rm = TRUE),
    
    # Effort + richness (trawl)
    mean_records = mean(n_records, na.rm = TRUE),
    median_records = median(n_records, na.rm = TRUE),
    mean_species = mean(n_species, na.rm = TRUE),
    sd_species   = sd(n_species, na.rm = TRUE),
    min_species  = min(n_species, na.rm = TRUE),
    max_species  = max(n_species, na.rm = TRUE),
    
    n_trawls     = n()
  )


dem_trip_summary <- dem_trip_scale %>%
  left_join(dem_trip_effort, by = "cruise") %>%
  summarise(
    # Spatial (trip)
    mean_trip_extent_km = mean(trip_extent_km, na.rm = TRUE),
    min_trip_extent_km  = min(trip_extent_km, na.rm = TRUE),
    max_trip_extent_km  = max(trip_extent_km, na.rm = TRUE),
    sd_trip_extent_km   = sd(trip_extent_km, na.rm = TRUE),
    
    # Effort/richness (trip)
    mean_records        = mean(n_records, na.rm = TRUE),
    median_records     = median(n_records, na.rm = TRUE),
    mean_species        = mean(n_species, na.rm = TRUE),
    
    # Two versions of trawls-per-trip:
    mean_trawls_coords  = mean(n_trawls.x, na.rm = TRUE),  # from trip_scale
    mean_trawls_bio     = mean(n_trawls.y, na.rm = TRUE),  # from trip_effort
    
    max_species         = max(n_species, na.rm = TRUE),
    n_trips             = n()
  )

dem_trip_summary

dem_trawl_full %>% summarise(n_200plus = sum(trawl_dist_km > 200, na.rm = TRUE))
dem_trip_scale %>% summarise(n_1500plus = sum(trip_extent_km > 1500, na.rm = TRUE))

dem_trawl_full %>%
  summarise(
    median_dist_km = median(trawl_dist_km, na.rm = TRUE),
    IQR_dist_km    = IQR(trawl_dist_km, na.rm = TRUE)
  )

dem_trip_scale %>%
  summarise(
    median_trip_extent_km = median(trip_extent_km, na.rm = TRUE),
    IQR_trip_extent_km    = IQR(trip_extent_km, na.rm = TRUE)
  )
# ------------------------------------------------------------
# DEMTRAWL (DFFE) haul-level temporal scale
# one haul = one (cruise, trawl_no)
# ------------------------------------------------------------

dem_trawl_time_haul <- demtrawldat %>%
  filter(!is.na(fishing_duration_min)) %>%
  group_by(cruise, trawl_no) %>%
  summarise(
    fishing_duration_min = median(fishing_duration_min, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(trawl_duration_h = fishing_duration_min / 60) %>%
  filter(!is.na(trawl_duration_h), trawl_duration_h > 0, trawl_duration_h <= 10) %>%
  distinct(cruise, trawl_no, .keep_all = TRUE)

dem_trawl_time_summary <- dem_trawl_time_haul %>%
  summarise(
    dataset = "DEMTRAWL (DFFE)",
    n_hauls  = n(),
    median_h = median(trawl_duration_h, na.rm = TRUE),
    q25_h    = quantile(trawl_duration_h, 0.25, na.rm = TRUE),
    q75_h    = quantile(trawl_duration_h, 0.75, na.rm = TRUE),
    iqr_h    = IQR(trawl_duration_h, na.rm = TRUE),
    min_h    = min(trawl_duration_h, na.rm = TRUE),
    max_h    = max(trawl_duration_h, na.rm = TRUE)
  )

dem_trawl_time_summary %>%
  transmute(
    dataset,
    n_hauls,
    median_h = round(median_h, 2),
    IQR_h = paste0(round(q25_h, 2), "–", round(q75_h, 2))
  )


# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------------
#NMLS
path <- "~/Desktop/ANGLING"
# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------------

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

colnames(OBSdat)
colnames(COMdat)

OBS_event <- OBSdat %>%
  filter(!is.na(species_name), !is.na(grid_id)) %>%
  mutate(event_id = paste(vessel_num, date, grid_id, sep = "_"))
COM_event <- COMdat %>%
  filter(!is.na(species_name), !is.na(grid_id)) %>%
  mutate(event_id = paste(id, date, grid_id, sep = "_"))

library(geosphere)

OBS_trip_scale <- OBS_event %>%
  distinct(vessel_num, year, grid_id, g_long, g_lat) %>%
  group_by(vessel_num, year) %>%
  summarise(
    trip_extent_km = if (n() > 1) {
      coords <- cbind(g_long, g_lat)
      max(distm(coords, fun = distHaversine)) / 1000
    } else NA_real_,
    n_events = n(),
    .groups = "drop"
  )
COM_trip_scale <- COM_event %>%
  distinct(id, year, grid_id, g_long, g_lat) %>%
  group_by(id, year) %>%
  summarise(
    trip_extent_km = if (n() > 1) {
      coords <- cbind(g_long, g_lat)
      max(distm(coords, fun = distHaversine)) / 1000
    } else NA_real_,
    n_events = n(),
    .groups = "drop"
  )



OBS_event_time <- OBS_event %>%
  distinct(event_id, hours_fish) %>%
  filter(!is.na(hours_fish))

COM_event_time <- COM_event %>%
  distinct(event_id, hours_fish) %>%
  filter(!is.na(hours_fish))

OBS_trip_time <- OBS_event %>%
  distinct(vessel_num, year, date) %>%
  group_by(vessel_num, year) %>%
  summarise(
    trip_days = n(),
    .groups = "drop"
  )

COM_trip_time <- COM_event %>%
  distinct(id, year, date) %>%
  group_by(id, year) %>%
  summarise(
    trip_days = n(),
    .groups = "drop"
  )

OBS_event_effort <- OBS_event %>%
  group_by(event_id) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species_name),
    hours_fish = first(hours_fish),
    .groups = "drop"
  )


OBS_trip_effort <- OBS_event %>%
  group_by(vessel_num, year) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species_name),
    n_events  = n_distinct(event_id),
    .groups = "drop"
  )
COM_event_effort <- COM_event %>%
  group_by(event_id) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species_name),
    hours_fish = first(hours_fish),
    .groups = "drop"
  )

COM_trip_effort <- COM_event %>%
  group_by(id, year) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species_name),
    n_events  = n_distinct(event_id),
    .groups = "drop"
  )


OBS_event_summary <- OBS_event_effort %>%
  summarise(
    # Temporal (event)
    mean_hours      = mean(hours_fish, na.rm = TRUE),
    median_hours    = median(hours_fish, na.rm = TRUE),
    sd_hours        = sd(hours_fish, na.rm = TRUE),
    max_hours       = max(hours_fish, na.rm = TRUE),
    
    # Effort / richness (event)
    mean_records    = mean(n_records, na.rm = TRUE),
    mean_species    = mean(n_species, na.rm = TRUE),
    sd_species      = sd(n_species, na.rm = TRUE),
    max_species     = max(n_species, na.rm = TRUE),
    
    n_events        = n()
  )

COM_event_summary <- COM_event_effort %>%
  summarise(
    # Temporal (event)
    mean_hours      = mean(hours_fish, na.rm = TRUE),
    median_hours    = median(hours_fish, na.rm = TRUE),
    sd_hours        = sd(hours_fish, na.rm = TRUE),
    max_hours       = max(hours_fish, na.rm = TRUE),
    
    # Effort / richness (event)
    mean_records    = mean(n_records, na.rm = TRUE),
    mean_species    = mean(n_species, na.rm = TRUE),
    sd_species      = sd(n_species, na.rm = TRUE),
    max_species     = max(n_species, na.rm = TRUE),
    
    n_events        = n()
  )

COM_event_summary

OBS_event_summary

OBS_trip_summary <- OBS_trip_scale %>%
  left_join(OBS_trip_effort, by = c("vessel_num", "year")) %>%
  left_join(OBS_trip_time,   by = c("vessel_num", "year")) %>%
  summarise(
    # Spatial (trip)
    mean_trip_extent_km = mean(trip_extent_km, na.rm = TRUE),
    min_trip_extent_km  = min(trip_extent_km, na.rm = TRUE),
    max_trip_extent_km  = max(trip_extent_km, na.rm = TRUE),
    sd_trip_extent_km   = sd(trip_extent_km, na.rm = TRUE),
    
    # Temporal (trip)
    mean_trip_days      = mean(trip_days, na.rm = TRUE),
    max_trip_days       = max(trip_days, na.rm = TRUE),
    
    # Effort / richness (trip)
    mean_records        = mean(n_records, na.rm = TRUE),
    mean_species        = mean(n_species, na.rm = TRUE),
    
    # Two versions of events-per-trip (coords vs bio)
    mean_events_coords  = mean(n_events.x, na.rm = TRUE),  # from OBS_trip_scale
    mean_events_bio     = mean(n_events.y, na.rm = TRUE),  # from OBS_trip_effort
    
    max_species         = max(n_species, na.rm = TRUE),
    n_trips             = n()
  )

COM_trip_summary <- COM_trip_scale %>%
  left_join(COM_trip_effort, by = c("id", "year")) %>%
  left_join(COM_trip_time,   by = c("id", "year")) %>%
  summarise(
    # Spatial (trip)
    mean_trip_extent_km = mean(trip_extent_km, na.rm = TRUE),
    min_trip_extent_km  = min(trip_extent_km, na.rm = TRUE),
    max_trip_extent_km  = max(trip_extent_km, na.rm = TRUE),
    sd_trip_extent_km   = sd(trip_extent_km, na.rm = TRUE),
    
    # Temporal (trip)
    mean_trip_days      = mean(trip_days, na.rm = TRUE),
    max_trip_days       = max(trip_days, na.rm = TRUE),
    
    # Effort / richness (trip)
    mean_records        = mean(n_records, na.rm = TRUE),
    mean_species        = mean(n_species, na.rm = TRUE),
    
    # Two versions of events-per-trip (coords vs bio)
    mean_events_coords  = mean(n_events.x, na.rm = TRUE),  # from COM_trip_scale
    mean_events_bio     = mean(n_events.y, na.rm = TRUE),  # from COM_trip_effort
    
    max_species         = max(n_species, na.rm = TRUE),
    n_trips             = n()
  )

COM_trip_summary
OBS_trip_summary

OBS_trip_robust <- OBS_trip_scale %>%
  summarise(
    median_trip_extent_km = median(trip_extent_km, na.rm = TRUE),
    IQR_trip_extent_km    = IQR(trip_extent_km, na.rm = TRUE)
  )

COM_trip_robust <- COM_trip_scale %>%
  summarise(
    median_trip_extent_km = median(trip_extent_km, na.rm = TRUE),
    IQR_trip_extent_km    = IQR(trip_extent_km, na.rm = TRUE)
  )


library(dplyr)
library(geosphere)
library(lubridate)

# Build event-location table for spacing
nmls_events <- bind_rows(
  OBS_event %>%
    transmute(
      Source  = "LINEFISH_OBS",
      trip_id = paste(vessel_num, year, sep = "_"),
      event_id = as.character(event_id),
      date = dmy(date),          # your date is "dd/mm/YYYY"
      lon = as.numeric(g_long),
      lat = as.numeric(g_lat)
    ),
  
  COM_event %>%
    transmute(
      Source  = "LINEFISH_COM",
      trip_id = paste(id, year, sep = "_"),
      event_id = as.character(event_id),
      date = dmy(date),          # your date is "dd/mm/YYYY"
      lon = as.numeric(g_long),
      lat = as.numeric(g_lat)
    )
) %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  distinct(Source, trip_id, event_id, date, lat, lon)

# spacing between successive events within each trip
nmls_event_spacing <- nmls_events %>%
  arrange(Source, trip_id, date, event_id) %>%
  group_by(Source, trip_id) %>%
  mutate(
    next_lat = lead(lat),
    next_lon = lead(lon),
    spacing_km = ifelse(
      !is.na(next_lat),
      geosphere::distHaversine(cbind(lon, lat), cbind(next_lon, next_lat)) / 1000,
      NA_real_
    )
  ) %>%
  ungroup()

# Summarise spacing by dataset (OBS vs COM)
nmls_event_spacing_summary <- nmls_event_spacing %>%
  group_by(Source) %>%
  summarise(
    median_event_spacing_km = median(spacing_km, na.rm = TRUE),
    q25_event_spacing_km    = quantile(spacing_km, 0.25, na.rm = TRUE),
    q75_event_spacing_km    = quantile(spacing_km, 0.75, na.rm = TRUE),
    IQR_event_spacing_km    = IQR(spacing_km, na.rm = TRUE),
    max_event_spacing_km    = max(spacing_km, na.rm = TRUE),
    n_spacings              = sum(!is.na(spacing_km)),
    .groups = "drop"
  )

nmls_event_spacing_summary

# ------------------------------------------------------------
# NMLS: temporal scale = one EVENT duration (hours_fish)
# OBS events
# ------------------------------------------------------------
OBS_event_time <- OBS_event %>%
  transmute(
    Source   = "NMLS_OBS",
    event_id = as.character(event_id),
    hours_fish = as.numeric(hours_fish)
  ) %>%
  filter(!is.na(hours_fish), hours_fish > 0) %>%
  group_by(Source, event_id) %>%
  summarise(hours_fish = median(hours_fish, na.rm = TRUE), .groups = "drop")

# ------------------------------------------------------------
# COM events
# ------------------------------------------------------------
COM_event_time <- COM_event %>%
  transmute(
    Source   = "NMLS_COM",
    event_id = as.character(event_id),
    hours_fish = as.numeric(hours_fish)
  ) %>%
  filter(!is.na(hours_fish), hours_fish > 0) %>%
  group_by(Source, event_id) %>%
  summarise(hours_fish = median(hours_fish, na.rm = TRUE), .groups = "drop")

nmls_event_time_summary <- bind_rows(OBS_event_time, COM_event_time) %>%
  group_by(Source) %>%
  summarise(
    n_events = n(),
    median_h = median(hours_fish, na.rm = TRUE),
    q25_h    = quantile(hours_fish, 0.25, na.rm = TRUE),
    q75_h    = quantile(hours_fish, 0.75, na.rm = TRUE),
    iqr_h    = IQR(hours_fish, na.rm = TRUE),
    min_h    = min(hours_fish, na.rm = TRUE),
    max_h    = max(hours_fish, na.rm = TRUE),
    .groups = "drop"
  )

nmls_event_time_summary %>%
  transmute(
    Source,
    n_events,
    median_h = round(median_h, 2),
    IQR_h = paste0(round(q25_h, 2), "–", round(q75_h, 2))
  )


# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------------

#CAPFISH
#first load in CAPFISH_LOAD SCRIPT go up to steo 6

# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------------
PELAGICPS_std     <- standardize_dataset(PELAGICPS, map_pelagicps) %>% mutate(Source = "SAPFIA")
SMALLPELAGICS_std <- standardize_dataset(SMALLPELAGICS, map_smallpelagics) %>% mutate(Source = "SAPFIA")
HAKELL_std        <- standardize_dataset(HAKELL, map_hakell) %>% mutate(Source = "SAHLLA")
SADSTIA_std       <- standardize_dataset(SADSTIA, map_sadstia) %>% mutate(Source = "SADSTIA")
DEMERSALWFT_std   <- standardize_dataset(DEMERSALWFT, map_sadstia) %>% mutate(Source = "SADSTIA")
INSHORE_std       <- standardize_dataset(INSHORE, map_sadstia) %>% mutate(Source = "SECIFA")
TUNALL_std  <- standardize_dataset(TUNALL, map_tuna)  %>% mutate(Source = "SAHLLA")
SHARKLL_std <- standardize_dataset(SHARKLL, map_tuna) %>% mutate(Source = "SAHLLA")

#(follow on from capfish load)
names(PELAGICPS_std)
names(SMALLPELAGICS_std)
names(HAKELL_std)
names(SADSTIA_std)
names(DEMERSALWFT_std)
names(INSHORE_std)
names(TUNALL_std)
names(SHARKLL_std)

capfish_all <- bind_rows(
  PELAGICPS_std,
  SMALLPELAGICS_std,
  HAKELL_std,
  SADSTIA_std,
  DEMERSALWFT_std,
  INSHORE_std,
  TUNALL_std,
  SHARKLL_std
)

capfish_all <- capfish_all %>%
  mutate(
    lat = suppressWarnings(as.numeric(LatDeg) + as.numeric(LatMin) / 60),
    lon = suppressWarnings(as.numeric(LongDeg) + as.numeric(LongMin) / 60),
    scientific_name = case_when(
      !is.na(Genus) & !is.na(Specie) ~ paste(Genus, Specie),
      TRUE ~ NA_character_
    ),
    
    Date = as.Date(Date)   # important for trip temporal scale
  )

capfish_all %>%
  summarise(
    lat_min = min(lat, na.rm = TRUE),
    lat_max = max(lat, na.rm = TRUE),
    lon_min = min(lon, na.rm = TRUE),
    lon_max = max(lon, na.rm = TRUE)
  )

capfish_all <- capfish_all %>%
  mutate(
    lat = -abs(lat)
  )

capfish_event_summary <- capfish_all %>%
  filter(!is.na(Source), !is.na(Set_ID), !is.na(scientific_name)) %>%
  group_by(Source, Set_ID) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(scientific_name),
    .groups = "drop"
  ) %>%
  group_by(Source) %>%
  summarise(
    mean_records = mean(n_records, na.rm = TRUE),
    median_records =median(n_records, na.rm = TRUE),
    mean_species = mean(n_species, na.rm = TRUE),
    sd_species   = sd(n_species, na.rm = TRUE),
    max_species  = max(n_species, na.rm = TRUE),
    n_sets       = n(),
    .groups = "drop"
  )

cap_trip_scale <- capfish_all %>%
  filter(!is.na(Source), !is.na(Trip_ID), !is.na(lat), !is.na(lon)) %>%
  distinct(Source, Trip_ID, Set_ID, lat, lon) %>%
  group_by(Source, Trip_ID) %>%
  summarise(
    trip_extent_km = if (n() > 1) {
      coords <- cbind(lon, lat)
      max(geosphere::distm(coords, fun = geosphere::distHaversine), na.rm = TRUE) / 1000
    } else NA_real_,
    n_sets_coords = n_distinct(Set_ID),
    .groups = "drop"
  )

cap_trip_time <- capfish_all %>%
  filter(!is.na(Source), !is.na(Trip_ID), !is.na(Date)) %>%
  distinct(Source, Trip_ID, Set_ID, Date) %>%
  group_by(Source, Trip_ID) %>%
  summarise(
    trip_days = as.numeric(max(Date) - min(Date)) + 1,
    n_sets_time = n_distinct(Set_ID),
    .groups = "drop"
  )

cap_trip_effort <- capfish_all %>%
  filter(!is.na(Source), !is.na(Trip_ID), !is.na(Set_ID), !is.na(scientific_name)) %>%
  group_by(Source, Trip_ID) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(scientific_name),
    n_sets_bio = n_distinct(Set_ID),
    .groups = "drop"
  )

capfish_trip_summary <- cap_trip_scale %>%
  left_join(cap_trip_effort, by = c("Source","Trip_ID")) %>%
  left_join(cap_trip_time,   by = c("Source","Trip_ID")) %>%
  group_by(Source) %>%
  summarise(
    mean_trip_extent_km   = mean(trip_extent_km, na.rm = TRUE),
    min_trip_extent_km    = min(trip_extent_km, na.rm = TRUE),
    max_trip_extent_km    = max(trip_extent_km, na.rm = TRUE),
    sd_trip_extent_km     = sd(trip_extent_km, na.rm = TRUE),
    median_trip_extent_km = median(trip_extent_km, na.rm = TRUE),
    IQR_trip_extent_km    = IQR(trip_extent_km, na.rm = TRUE),
    
    mean_trip_days        = mean(trip_days, na.rm = TRUE),
    max_trip_days         = max(trip_days, na.rm = TRUE),
    
    mean_records          = mean(n_records, na.rm = TRUE),
    mean_species          = mean(n_species, na.rm = TRUE),
    
    mean_sets_coords      = mean(n_sets_coords, na.rm = TRUE),
    mean_sets_bio         = mean(n_sets_bio, na.rm = TRUE),
    
    max_species           = max(n_species, na.rm = TRUE),
    n_trips               = n(),
    .groups = "drop"
  )

capfish_summary_table <- capfish_event_summary %>%
  left_join(capfish_trip_summary, by = "Source") %>%
  arrange(Source)

capfish_all %>%
  filter(!is.na(lon) & (lon < -180 | lon > 180)) %>%
  count(Source) %>%
  arrange(desc(n))

capfish_geo_qc <- capfish_all %>%
  filter(
    !is.na(lat), !is.na(lon),
    between(lat, -40, 0),
    between(lon, 0, 60)
  )

capfish_trip_summary

cap_trip_scale_qc <- capfish_geo_qc %>%
  distinct(Source, Trip_ID, Set_ID, lat, lon) %>%
  group_by(Source, Trip_ID) %>%
  summarise(
    trip_extent_km = if (n() > 1) {
      coords <- cbind(lon, lat)
      max(geosphere::distm(coords, fun = geosphere::distHaversine), na.rm = TRUE) / 1000
    } else NA_real_,
    n_sets_coords = n_distinct(Set_ID),
    .groups = "drop"
  )


n_before <- capfish_all %>% filter(!is.na(lat), !is.na(lon)) %>% nrow()
n_after  <- capfish_geo_qc %>% nrow()

(data.frame(
  coord_rows_before = n_before,
  coord_rows_after  = n_after,
  pct_removed       = 100 * (n_before - n_after) / n_before
))

capfish_summary_table_clean <- capfish_summary_table %>%
  rename(
    mean_records_set = mean_records.x,
    mean_species_set = mean_species.x,
    max_species_set  = max_species.x,
    mean_records_trip = mean_records.y,
    mean_species_trip = mean_species.y,
    max_species_trip  = max_species.y
  )
capfish_summary_table_clean

print(capfish_summary_table, width =Inf)
print(capfish_summary_table_clean, width =Inf)

cap_trip_scale %>%
  filter(trip_extent_km > 2000) %>%
  count(Source)

cap_trip_scale_qc <- cap_trip_scale %>%
  filter(trip_extent_km <= 2000)

capfish_trip_spatial_qc <- cap_trip_scale_qc %>%
  group_by(Source) %>%
  summarise(
    mean_trip_extent_km   = mean(trip_extent_km, na.rm = TRUE),
    min_trip_extent_km    = min(trip_extent_km, na.rm = TRUE),
    max_trip_extent_km    = max(trip_extent_km, na.rm = TRUE),
    sd_trip_extent_km     = sd(trip_extent_km, na.rm = TRUE),
    median_trip_extent_km = median(trip_extent_km, na.rm = TRUE),
    IQR_trip_extent_km    = IQR(trip_extent_km, na.rm = TRUE),
    .groups = "drop"
  )

capfish_summary_table_qc <- capfish_summary_table_clean %>%
  select(
    -mean_trip_extent_km,
    -min_trip_extent_km,
    -max_trip_extent_km,
    -sd_trip_extent_km,
    -median_trip_extent_km,
    -IQR_trip_extent_km
  ) %>%
  left_join(capfish_trip_spatial_qc, by = "Source")

cap_trip_scale %>%
  mutate(flag_outlier = trip_extent_km > 2000) %>%
  count(Source, flag_outlier) %>%
  tidyr::pivot_wider(names_from = flag_outlier,
                     values_from = n,
                     values_fill = 0) %>%
  dplyr::rename(n_kept = `FALSE`, n_removed = `TRUE`) %>%
  mutate(pct_removed = 100 * n_removed / (n_kept + n_removed))
print(capfish_summary_table_qc, width = Inf)

capfish_summary_table_qc %>%
  arrange(Source) %>%
  mutate(
    Source = recode(Source,
                    "SADSTIA" = "SADSTIA (trawl / wetfish)",
                    "SAHLLA"  = "SAHLLA (hake & tuna longline)",
                    "SAPFIA"  = "SAPFIA (small pelagics / purse seine)",
                    "SECIFA"  = "SECIFA (inshore)"
    )
  )

capfish_event_scale <- capfish_all %>%
  filter(
    !is.na(Set_ID),
    !is.na(lat),
    !is.na(lon)
  ) %>%
  group_by(Source, Set_ID) %>%
  summarise(
    set_extent_km = if (n() > 1) {
      max(
        distm(
          cbind(lon, lat),
          fun = distHaversine
        )
      ) / 1000
    } else {
      0
    },
    .groups = "drop"
  )


capfish_event_spacing <- capfish_all %>%
  filter(!is.na(Set_ID), !is.na(lat), !is.na(lon), !is.na(Trip_ID)) %>%
  arrange(Source, Trip_ID, Date) %>%
  group_by(Source, Trip_ID) %>%
  mutate(
    next_lat = lead(lat),
    next_lon = lead(lon),
    event_spacing_km = ifelse(
      !is.na(next_lat),
      distHaversine(
        cbind(lon, lat),
        cbind(next_lon, next_lat)
      ) / 1000,
      NA
    )
  ) %>%
  ungroup() %>%
  group_by(Source, Set_ID) %>%
  summarise(
    event_spacing_km = median(event_spacing_km, na.rm = TRUE),
    .groups = "drop"
  )

capfish_event_spacing_summary <- capfish_event_spacing %>%
  group_by(Source) %>%
  summarise(
    mean_event_spacing_km   = mean(event_spacing_km, na.rm = TRUE),
    median_event_spacing_km = median(event_spacing_km, na.rm = TRUE),
    IQR_event_spacing_km    = IQR(event_spacing_km, na.rm = TRUE),
    max_event_spacing_km    = max(event_spacing_km, na.rm = TRUE),
    n_events                = n(),
    .groups = "drop"
  )
# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------------

#BRUV
# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(lubridate)
library(geosphere)
library(stringr)

# --- 1) Clean metadata: parse date/time into a deployment datetime ---
BRUVmeta2 <- BRUVmetadat %>%
  mutate(
    # date_format looks like the date field; name is confusing but keep as your date string
    date_raw = date_format,
    time_raw = time,
    
    # parse date (try multiple formats safely)
    date_parsed = suppressWarnings(parse_date_time(
      as.character(date_raw),
      orders = c("Ymd", "dmY", "dmy", "Y-m-d", "d/m/Y", "m/d/Y"),
      tz = "UTC"
    )),
    
    # parse time (HH:MM / HH:MM:SS / etc.)
    time_parsed = suppressWarnings(parse_date_time(
      as.character(time_raw),
      orders = c("HMS", "HM", "H:M", "H"),
      tz = "UTC"
    )),
    
    # combine into deployment datetime if both exist
    deploy_dt = case_when(
      !is.na(date_parsed) & !is.na(time_parsed) ~ as.POSIXct(
        paste(as.Date(date_parsed),
              format(time_parsed, "%H:%M:%S")),
        tz = "UTC"
      ),
      !is.na(date_parsed) ~ as.POSIXct(as.Date(date_parsed), tz = "UTC"),
      TRUE ~ as.POSIXct(NA)
    ),
    
    # BRUV coordinates should be negative lat for SA; fix if needed
    latitude  = as.numeric(latitude),
    longitude = as.numeric(longitude),
    latitude  = ifelse(!is.na(latitude), -abs(latitude), NA_real_),
    
    period_duration = as.numeric(period_duration),
    depth = as.numeric(depth)
  )

# --- 2) Clean BRUV occurrence data ---
BRUVdat2 <- BRUVdat %>%
  rename(sample_status = `sample status`) %>%  # fix name with space
  mutate(
    period = suppressWarnings(as.numeric(period)),
    maxn   = suppressWarnings(as.numeric(maxn))
  )

# --- 3) Final merged table (keep key fields) ---
finalBRUV <- BRUVdat2 %>%
  left_join(
    BRUVmeta2 %>%
      select(
        opcode, fieldtrip_code,
        project_name, location, management, mpa, sampaz_zones,
        bait_type, depth, latitude, longitude,
        period_duration, deploy_dt,
        sample_status, sample_status_comments
      ),
    by = "opcode"
  )

#richness and effort
BRUV_event_effort <- finalBRUV %>%
  filter(!is.na(opcode), !is.na(scientific_name)) %>%
  group_by(opcode) %>%
  summarise(
    n_records = n(),                          # species × period rows
    n_species = n_distinct(scientific_name),  # richness per deployment
    .groups = "drop"
  )
#time
BRUV_event_time <- BRUVmeta2 %>%
  filter(!is.na(opcode), !is.na(period_duration)) %>%
  distinct(opcode, period_duration) %>%
  mutate(duration_min = as.numeric(period_duration),
         duration_h   = duration_min / 60)

BRUV_event_scale <- BRUVmeta2 %>%
  filter(!is.na(opcode), !is.na(latitude), !is.na(longitude)) %>%
  distinct(opcode, latitude, longitude) %>%
  mutate(event_dist_km = 0)  # point deployment

#full summary
BRUV_event_full <- BRUV_event_scale %>%
  left_join(BRUV_event_time,   by = "opcode") %>%
  left_join(BRUV_event_effort, by = "opcode")

BRUV_event_summary <- BRUV_event_full %>%
  summarise(
    # spatial
    mean_dist_km = mean(event_dist_km, na.rm = TRUE),
    min_dist_km  = min(event_dist_km, na.rm = TRUE),
    max_dist_km  = max(event_dist_km, na.rm = TRUE),
    
    # temporal
    mean_duration_h = mean(duration_h, na.rm = TRUE),
    median_duration_h = median(duration_h, na.rm = TRUE),
    IQR_duration_h = IQR(duration_h, na.rm = TRUE),
    max_duration_h = max(duration_h, na.rm = TRUE),
    
    # effort / richness
    mean_records = mean(n_records, na.rm = TRUE),
    mean_species = mean(n_species, na.rm = TRUE),
    sd_species   = sd(n_species, na.rm = TRUE),
    max_species  = max(n_species, na.rm = TRUE),
    
    n_events = n()
  )

BRUV_event_summary

BRUV_trip_scale <- BRUVmeta2 %>%
  filter(!is.na(fieldtrip_code), !is.na(opcode),
         !is.na(latitude), !is.na(longitude)) %>%
  distinct(fieldtrip_code, opcode, latitude, longitude) %>%
  group_by(fieldtrip_code) %>%
  summarise(
    trip_extent_km = if (n() > 1) {
      coords <- cbind(longitude, latitude)
      max(distm(coords, fun = distHaversine), na.rm = TRUE) / 1000
    } else NA_real_,
    n_events_coords = n_distinct(opcode),
    .groups = "drop"
  )

BRUV_trip_time <- BRUVmeta2 %>%
  filter(!is.na(fieldtrip_code), !is.na(opcode), !is.na(deploy_dt)) %>%
  distinct(fieldtrip_code, opcode, deploy_dt) %>%
  group_by(fieldtrip_code) %>%
  summarise(
    trip_days = as.numeric(as.Date(max(deploy_dt)) - as.Date(min(deploy_dt))) + 1,
    n_events_time = n_distinct(opcode),
    .groups = "drop"
  )

BRUV_trip_effort <- finalBRUV %>%
  filter(!is.na(fieldtrip_code), !is.na(opcode), !is.na(scientific_name)) %>%
  group_by(fieldtrip_code) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(scientific_name),
    n_events_bio = n_distinct(opcode),
    .groups = "drop"
  )


BRUV_trip_summary <- BRUV_trip_scale %>%
  left_join(BRUV_trip_effort, by = "fieldtrip_code") %>%
  left_join(BRUV_trip_time,   by = "fieldtrip_code") %>%
  summarise(
    # spatial
    mean_trip_extent_km = mean(trip_extent_km, na.rm = TRUE),
    min_trip_extent_km  = min(trip_extent_km, na.rm = TRUE),
    max_trip_extent_km  = max(trip_extent_km, na.rm = TRUE),
    sd_trip_extent_km   = sd(trip_extent_km, na.rm = TRUE),
    median_trip_extent_km = median(trip_extent_km, na.rm = TRUE),
    IQR_trip_extent_km    = IQR(trip_extent_km, na.rm = TRUE),
    
    # temporal
    mean_trip_days = mean(trip_days, na.rm = TRUE),
    max_trip_days  = max(trip_days, na.rm = TRUE),
    
    # effort / richness
    mean_records = mean(n_records, na.rm = TRUE),
    mean_species = mean(n_species, na.rm = TRUE),
    
    mean_events_coords = mean(n_events_coords, na.rm = TRUE),
    mean_events_bio    = mean(n_events_bio, na.rm = TRUE),
    
    max_species = max(n_species, na.rm = TRUE),
    n_trips = n()
  )

BRUV_event_summary_table <- BRUV_event_summary %>%
  mutate(unit = "event (opcode)") %>%
  select(unit, everything())

BRUV_trip_summary_table <- BRUV_trip_summary %>%
  mutate(unit = "trip (fieldtrip_code)") %>%
  select(unit, everything())

BRUV_consolidated_summary <- bind_rows(
  BRUV_event_summary_table,
  BRUV_trip_summary_table
)
print(BRUV_consolidated_summary, width=Inf)
# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------------

#FMDAT
FM_records <- final_data_joined %>%
  filter(!is.na(DataSetID),
         !is.na(SamplingLocation),
         !is.na(Latitude),
         !is.na(Longitude)) %>%
  distinct(
    DataSetID,
    SamplingLocation,
    StartDate,
    EndDate,
    Latitude,
    Longitude,
    Species
  )
FM_record_richness <- FM_records %>%
  group_by(DataSetID, SamplingLocation, StartDate, EndDate) %>%
  summarise(
    n_species = n_distinct(Species),
    .groups = "drop"
  )

FM_study_spatial <- FM_records %>%
  distinct(DataSetID, Latitude, Longitude) %>%
  group_by(DataSetID) %>%
  summarise(
    study_extent_km = if (n() > 1) {
      coords <- cbind(Longitude, Latitude)
      max(distm(coords, fun = distHaversine), na.rm = TRUE) / 1000
    } else {
      NA_real_
    },
    n_locations = n(),
    .groups = "drop"
  )


str(final_data_joined$StartDate)
str(final_data_joined$EndDate)

final_data_joined <- final_data_joined %>%
  mutate(
    StartDate_year = suppressWarnings(as.integer(StartDate)),
    EndDate_year   = suppressWarnings(as.integer(EndDate)),
    StartDate_date = as.Date(paste0(StartDate_year, "-01-01")),
    EndDate_date   = as.Date(paste0(EndDate_year, "-12-31"))
  ) %>%
  filter(!is.na(StartDate_date), !is.na(EndDate_date))



FM_study_temporal <- final_data_joined %>%
  distinct(DataSetID, StartDate_date, EndDate_date) %>%
  mutate(
    study_days  = as.integer(EndDate_date - StartDate_date) + 1,
    study_years = study_days / 365.25
  )

summary(FM_study_temporal$study_days)
FM_study_temporal %>% filter(study_days < 0)

FM_summary <- FM_study_spatial %>%
  left_join(FM_study_temporal, by = "DataSetID") %>%
  left_join(
    FM_record_richness %>%
      group_by(DataSetID) %>%
      summarise(
        mean_species_per_record = mean(n_species, na.rm = TRUE),
        max_species_per_record  = max(n_species, na.rm = TRUE),
        n_records               = n(),
        .groups = "drop"
      ),
    by = "DataSetID"
  )


names(final_data_joined)

FM_methods <- final_data_joined %>%
  filter(!is.na(DataSetID)) %>%
  group_by(DataSetID) %>%
  summarise(
    Method    = paste(sort(unique(Method)), collapse = "; "),
    MethodNew = paste(sort(unique(MethodNew)), collapse = "; "),
    DataType = paste(sort(unique(DataType)), collapse = "; "),
    DataClassification = paste(sort(unique(DataClassification)), collapse = "; "),
    SamplingLocation  = paste(sort(unique(SamplingLocation)), collapse = "; "),
    Sampling.Frequency = paste(sort(unique(Sampling.Frequency)), collapse = "; "),
    .groups = "drop"
  )

FM_summary <- FM_summary %>%
  left_join(FM_methods, by = "DataSetID")

names(FM_summary)

FM_summary_clean <- FM_summary %>%
  select(
    DataSetID,
    study_extent_km, n_locations,
    StartDate_date, EndDate_date, study_days, study_years,
    mean_species_per_record, max_species_per_record, n_records,
    Method, MethodNew,
    DataType,
    DataClassification,
    SamplingLocation,
    Sampling.Frequency
  ) %>%
  mutate(
    SamplingLocation = ifelse(
      is.na(SamplingLocation), NA_character_,
      ifelse(
        nchar(SamplingLocation) > 80,
        paste0(substr(SamplingLocation, 1, 77), "..."),
        SamplingLocation
      )
    )
  )

print(FM_summary_clean, n = Inf, width = Inf)


FM_method_qc <- final_data_joined %>%
  distinct(DataSetID, Method, MethodNew) %>%
  group_by(DataSetID) %>%
  summarise(
    methods_raw = paste(sort(unique(Method)), collapse = "; "),
    methods_new = paste(sort(unique(MethodNew)), collapse = "; "),
    n_methods_raw = n_distinct(Method),
    n_methods_new = n_distinct(MethodNew),
    .groups = "drop"
  ) %>%
  filter(n_methods_raw > 1 | n_methods_new > 1 | is.na(methods_new) | is.na(methods_raw))

print(FM_method_qc, n = 50, width = Inf)

FM_method_majority <- final_data_joined %>%
  group_by(DataSetID, MethodNew) %>%
  summarise(n = n(), .groups = "drop") %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  select(DataSetID, MethodNew_majority = MethodNew)

FM_summary_clean <- FM_summary_clean %>%
  left_join(FM_method_majority, by = "DataSetID") %>%
  mutate(MethodNew = MethodNew_majority) %>%
  select(-MethodNew_majority)

FM_summary_clean <- FM_summary_clean %>%
  mutate(MethodNew_orig = MethodNew)

# 2) Majority method per DataSetID, but EXCLUDING NA/blank
FM_method_majority <- final_data_joined %>%
  mutate(MethodNew = na_if(str_trim(MethodNew), "")) %>%
  filter(!is.na(MethodNew)) %>%                       # <-- key fix
  count(DataSetID, MethodNew, name = "n") %>%
  group_by(DataSetID) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(DataSetID, MethodNew_majority = MethodNew)

# 3) Join, but don't overwrite with NA
FM_summary_clean <- FM_summary_clean %>%
  left_join(FM_method_majority, by = "DataSetID") %>%
  mutate(MethodNew = coalesce(MethodNew_majority, MethodNew_orig)) %>%
  select(-MethodNew_majority, -MethodNew_orig)

# Quick check
FM_summary_clean %>%
  count(MethodNew, sort = TRUE)

FM_method_n <- final_data_joined %>%
  mutate(MethodNew = na_if(str_trim(MethodNew), "")) %>%
  filter(!is.na(MethodNew)) %>%
  distinct(DataSetID, MethodNew) %>%
  count(DataSetID, name = "n_methods")

FM_summary_clean <- FM_summary_clean %>%
  left_join(FM_method_n, by = "DataSetID") %>%
  mutate(MethodNew = ifelse(!is.na(n_methods) & n_methods > 1, "mixedmethod", MethodNew)) %>%
  select(-n_methods)


library(dplyr)

# Helper for median [IQR]
med_iqr <- function(x) {
  x <- x[!is.na(x)]
  if(length(x) == 0) return(NA_character_)
  paste0(
    round(median(x), 2), " [",
    round(quantile(x, 0.25), 2), "–",
    round(quantile(x, 0.75), 2), "]"
  )
}

range2 <- function(x) {
  x <- x[!is.na(x)]
  if(length(x) == 0) return(NA_character_)
  paste0(round(min(x), 2), "–", round(max(x), 2))
}

FM_table1 <- tibble::tibble(
  Metric = c(
    "Number of datasets (n)",
    "Total records (sum n_records)",
    "Study duration (years), median [IQR]",
    "Study duration (years), range",
    "Spatial extent (km), median [IQR]",
    "Spatial extent (km), range",
    "Number of locations, median [IQR]",
    "Number of locations, range"
  ),
  Value = c(
    nrow(FM_summary_clean),
    sum(FM_summary_clean$n_records, na.rm = TRUE),
    med_iqr(FM_summary_clean$study_years),
    range2(FM_summary_clean$study_years),
    med_iqr(FM_summary_clean$study_extent_km),
    range2(FM_summary_clean$study_extent_km),
    med_iqr(FM_summary_clean$n_locations),
    range2(FM_summary_clean$n_locations)
  )
)

FM_methods_top <- FM_summary_clean %>%
  count(MethodNew, sort = TRUE) %>%
  mutate(pct = round(100*n/sum(n), 1)) %>%
  slice_head(n = 8)

FM_type <- FM_summary_clean %>%
  count(DataType, sort = TRUE) %>%
  mutate(pct = round(100*n/sum(n), 1))

FM_class <- FM_summary_clean %>%
  count(DataClassification, sort = TRUE) %>%
  mutate(pct = round(100*n/sum(n), 1))

FM_table1

FM_methods_top
FM_type
FM_class

FM_appendix <- FM_summary_clean %>%
  transmute(
    DataSetID,
    StartDate = StartDate_date,
    EndDate = EndDate_date,
    Study_years = study_years,
    Study_extent_km = study_extent_km,
    N_locations = n_locations,
    Method = MethodNew,
    DataType,
    DataClass = DataClassification,
    N_records = n_records,
    Mean_spp_per_record = mean_species_per_record,
    Max_spp_per_record = max_species_per_record,
    Sampling_frequency = Sampling.Frequency,
    Sampling_location = SamplingLocation
  )

FM_appendix_print <- FM_appendix %>%
  mutate(
    Sampling_location = ifelse(nchar(Sampling_location) > 120,
                               paste0(substr(Sampling_location, 1, 120), "..."),
                               Sampling_location)
  )

FM_appendix_print

min_start <- min(FM_summary_clean$StartDate_date, na.rm = TRUE)
max_end   <- max(FM_summary_clean$EndDate_date, na.rm = TRUE)

summary_years <- quantile(FM_summary_clean$study_years, probs = c(0, .25, .5, .75, 1), na.rm = TRUE)
summary_extent <- quantile(FM_summary_clean$study_extent_km, probs = c(0, .25, .5, .75, 1), na.rm = TRUE)
summary_records <- quantile(FM_summary_clean$n_records, probs = c(0, .25, .5, .75, 1), na.rm = TRUE)
summary_spprec <- quantile(FM_summary_clean$mean_species_per_record, probs = c(0, .25, .5, .75, 1), na.rm = TRUE)

top_methods <- FM_summary_clean %>% count(MethodNew, sort = TRUE) %>% slice_head(n = 5)
min_start; max_end; summary_years; summary_extent; summary_records; summary_spprec; top_methods


FM_methods <- final_data_joined %>%
  filter(!is.na(DataSetID)) %>%
  group_by(DataSetID) %>%
  summarise(
    Method_collapsed    = paste(sort(unique(Method)), collapse = "; "),
    MethodNew_collapsed = paste(sort(unique(MethodNew)), collapse = "; "),
    DataType_collapsed  = paste(sort(unique(DataType)), collapse = "; "),
    DataClass_collapsed = paste(sort(unique(DataClassification)), collapse = "; "),
    SamplingLocation_collapsed  = paste(sort(unique(SamplingLocation)), collapse = "; "),
    SamplingFrequency_collapsed = paste(sort(unique(Sampling.Frequency)), collapse = "; "),
    .groups = "drop"
  )
FM_summary <- FM_summary %>%
  left_join(FM_methods, by = "DataSetID")
FM_summary_clean <- FM_summary %>%
  transmute(
    DataSetID,
    study_extent_km, n_locations,
    StartDate_date, EndDate_date, study_days, study_years,
    mean_species_per_record, max_species_per_record, n_records,
    Method    = Method_collapsed,
    MethodNew = MethodNew_collapsed,
    DataType  = DataType_collapsed,
    DataClassification = DataClass_collapsed,
    SamplingLocation  = SamplingLocation_collapsed,
    Sampling.Frequency = SamplingFrequency_collapsed
  )
names(FM_records)

library(dplyr)
library(geosphere)

FM_event_spacing <- FM_records %>%
  mutate(
    start_year = suppressWarnings(as.integer(StartDate)),
    end_year   = suppressWarnings(as.integer(EndDate))
  ) %>%
  distinct(
    DataSetID,
    SamplingLocation,
    start_year,
    end_year,
    Latitude,
    Longitude
  ) %>%
  arrange(DataSetID, start_year, end_year, SamplingLocation) %>%
  group_by(DataSetID) %>%
  mutate(
    next_lat = lead(Latitude),
    next_lon = lead(Longitude),
    event_spacing_km = ifelse(
      !is.na(next_lat),
      distHaversine(
        cbind(Longitude, Latitude),
        cbind(next_lon, next_lat)
      ) / 1000,
      NA_real_
    )
  ) %>%
  ungroup()



library(dplyr)
library(stringr)
library(lubridate)

#--- 0) Helper: pick first existing column from a list
pick_col <- function(df, candidates) {
  candidates <- candidates[candidates %in% names(df)]
  if (length(candidates) == 0) return(NULL)
  candidates[1]
}

#--- 1) Define method -> preferred ID columns (best-case)
method_key <- tibble::tribble(
  ~MethodNew,         ~id_candidates,
  "BRUV",             c("DeploymentID", "BRUVID", "EventID", "SampleID"),
  "UVC",              c("TransectID", "SurveyID", "DiveID", "EventID", "SampleID"),
  "demtrawl",         c("HaulID", "TowID", "StationID", "EventID", "SampleID"),
  "beamtrawl",        c("HaulID", "TowID", "StationID", "EventID", "SampleID"),
  "dnettrawl",        c("HaulID", "TowID", "StationID", "EventID", "SampleID"),
  "shallowtrawl",     c("HaulID", "TowID", "StationID", "EventID", "SampleID"),
  "gillnet",          c("SetID", "NetSetID", "HaulID", "StationID", "EventID", "SampleID"),
  "seinenet",         c("HaulID", "SeineID", "StationID", "EventID", "SampleID"),
  "planktonnet",      c("TowID", "SampleID", "StationID", "EventID"),
  "chemical",         c("SampleID", "StationID", "EventID"),
  "deadcollection",   c("CollectionID", "EventID", "SampleID"),
  "shoreangling",     c("TripID", "EventID", "SampleID"),
  "boatangling",      c("TripID", "EventID", "SampleID"),
  "mixedangling",     c("TripID", "EventID", "SampleID"),
  "visualestimate",   c("SurveyID", "DiveID", "EventID", "SampleID"),
  "records",          c("OccurrenceID", "CatalogNumber", "EventID", "SampleID"),
  "mixedmethod",      c("EventID", "SampleID", "StationID") # by definition inconsistent
) %>%
  mutate(id_candidates = lapply(id_candidates, as.character))

#--- 2) Standardise MethodNew and enforce "mixedmethod" where >1 method per DataSetID
final_data_recoded <- final_data_joined %>%
  mutate(
    MethodNew = na_if(str_trim(MethodNew), "")
  )

FM_method_n <- final_data_recoded %>%
  filter(!is.na(MethodNew)) %>%
  distinct(DataSetID, MethodNew) %>%
  count(DataSetID, name = "n_methods")

final_data_recoded <- final_data_recoded %>%
  left_join(FM_method_n, by = "DataSetID") %>%
  mutate(
    MethodNew = ifelse(!is.na(n_methods) & n_methods > 1, "mixedmethod", MethodNew)
  ) %>%
  select(-n_methods)

#--- 3) Build a method-aware record_id
# Strategy:
#  - If a method-specific ID column exists, use it (best case)
#  - Else create an event key from: DataSetID + SamplingLocation + date/year/season (+ coordinates if present)

# Candidate "fallback event" fields
date_col   <- pick_col(final_data_recoded, c("StartDate_date", "Date", "SampleDate", "StartDate", "EventDate"))
year_col   <- pick_col(final_data_recoded, c("Year", "Year.x", "Year.y"))
season_col <- pick_col(final_data_recoded, c("Season"))
loc_col    <- pick_col(final_data_recoded, c("SamplingLocation", "Location", "Site", "Station", "Locality"))
lat_col    <- pick_col(final_data_recoded, c("Latitude", "Lat"))
lon_col    <- pick_col(final_data_recoded, c("Longitude", "Lon"))



# Join in the id candidates per method
final_data_with_key <- final_data_recoded %>%
  left_join(method_key, by = "MethodNew") %>%
  rowwise() %>%
  mutate(
    # choose the first valid ID column for this row's method
    method_id_col = {
      cols <- id_candidates[[1]]
      cols <- cols[cols %in% names(.)]
      if (length(cols) == 0) NA_character_ else cols[1]
    },
    method_id_val = if (!is.na(method_id_col)) as.character(get(method_id_col)) else NA_character_,
    # Build fallback components safely
    fb_date   = if (!is.null(date_col))   as.character(get(date_col))   else NA_character_,
    fb_year   = if (!is.null(year_col))   as.character(get(year_col))   else NA_character_,
    fb_season = if (!is.null(season_col)) as.character(get(season_col)) else NA_character_,
    fb_loc    = if (!is.null(loc_col))    as.character(get(loc_col))    else NA_character_,
    fb_lat    = if (!is.null(lat_col))    as.character(get(lat_col))    else NA_character_,
    fb_lon    = if (!is.null(lon_col))    as.character(get(lon_col))    else NA_character_,
    # Method-aware record ID
    record_id = ifelse(
      !is.na(method_id_val) & method_id_val != "",
      paste(DataSetID, MethodNew, method_id_val, sep = "|"),
      paste(DataSetID, MethodNew, fb_loc, fb_date, fb_year, fb_season, fb_lat, fb_lon, sep = "|")
    )
  ) %>%
  ungroup() %>%
  select(-id_candidates, -method_id_col, -method_id_val, -fb_date, -fb_year, -fb_season, -fb_loc, -fb_lat, -fb_lon)

#--- 4) Recalculate record-based metrics per DataSetID
# species-per-record = number of DISTINCT species observed within that record_id (sampling event)
record_spp <- final_data_with_key %>%
  filter(!is.na(record_id), record_id != "") %>%
  group_by(DataSetID, MethodNew, record_id) %>%
  summarise(
    n_species_record = n_distinct(Species),
    .groups = "drop"
  )

FM_record_metrics <- record_spp %>%
  group_by(DataSetID) %>%
  summarise(
    n_records = n_distinct(record_id),
    mean_species_per_record = mean(n_species_record, na.rm = TRUE),
    max_species_per_record  = max(n_species_record, na.rm = TRUE),
    .groups = "drop"
  )

#--- 5) Join back into your FM_summary_clean (or rebuild it)
FM_summary_clean2 <- FM_summary_clean %>%
  select(-n_records, -mean_species_per_record, -max_species_per_record) %>%
  left_join(FM_record_metrics, by = "DataSetID")

#--- 6) Update DataType / DataClassification labels to your meanings
FM_summary_clean2 <- FM_summary_clean2 %>%
  mutate(
    DataType = recode(
      DataType,
      "I" = "Fishery-independent",
      "D" = "Fishery-dependent",
      .default = DataType
    ),
    DataClassification = recode(
      DataClassification,
      "S" = "Survey (fishery-independent)",
      "O" = "Observer (fishery-dependent)",
      "C" = "Catch return / commercial (fishery-dependent)",
      .default = DataClassification
    )
  )

# View
print(FM_summary_clean2, n = 20, width = Inf)

names(final_data_joined)

library(dplyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(scales)

#--- helper: parse "a-b" OR "min a max b" (also handles comma decimals)
parse_range <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, ",", ".")
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_trim(x)
  
  # treat blanks and "/" as missing
  ifelse(x %in% c("", "/", "NA", "na"), NA_character_, x) -> x
  
  # if "min ... max ..."
  is_minmax <- str_detect(x, regex("^min\\s*", ignore_case = TRUE)) |
    str_detect(x, regex("max\\s*", ignore_case = TRUE))
  
  out <- lapply(x, function(s) {
    if (is.na(s)) return(c(NA_real_, NA_real_))
    
    s_lowhigh <- s
    
    if (str_detect(s, regex("min\\s*\\d", ignore_case = TRUE))) {
      lo <- str_match(s, regex("min\\s*([0-9.]+)", ignore_case = TRUE))[,2]
      hi <- str_match(s, regex("max\\s*([0-9.]+)", ignore_case = TRUE))[,2]
      return(c(as.numeric(lo), as.numeric(hi)))
    }
    
    # default: "a-b"
    nums <- str_split(s_lowhigh, "-", simplify = TRUE)
    if (ncol(nums) >= 2) {
      return(c(as.numeric(str_trim(nums[1])), as.numeric(str_trim(nums[2]))))
    }
    
    c(NA_real_, NA_real_)
  })
  
  do.call(rbind, out)
}

df_raw <- tibble::tribble(
  ~dataset,  ~group,    ~time_iqr,            ~time_median_hr, ~space_iqr,        ~space_median_km, ~note,
  "NMLS",    "OBS",     "6-8",          7,               "15-127",          46,               NA,
  "NMLS",    "COM",     "5-8,5",              7,               "40-181",          102,              NA,
  "MWTRAWL", NA,        "0,33-2,92",          1.25,            "6,28-19,4",       11.5,             NA,
  "DEMTRAWL",NA,        "0,5-0,5",            0.5,             "3,05-3,46",       3.25,             NA,
  "CAPFISH", "SADSTIA",  "0,5-2,5",   1,        "13,6-32,1",       20.6,             "*proxy (FAO)",
  "CAPFISH", "SAHLLA",   "4-24",      8,        "4,61-29,4",       10.9,             "*proxy (SAHLLA)",
  "CAPFISH", "SAPFIA",   "0,75-1",    0.875,        "1,99-10,7",       4.03,             "*proxy",
  "CAPFISH", "SECIFA",   "0,5-2,5",   1.5,        "11,9-24,5",       17.4,             "*proxy",
  "BRUV",    NA,        "1-1,0000017",        1,               "0,54-1,93",       0.94,             NA
)

# parse ranges
t_rng <- parse_range(df_raw$time_iqr)
s_rng <- parse_range(df_raw$space_iqr)

df <- df_raw %>%
  mutate(
    time_q25_hr = t_rng[,1],
    time_q75_hr = t_rng[,2],
    space_q25_km = s_rng[,1],
    space_q75_km = s_rng[,2],
    label = ifelse(is.na(group), dataset, paste(dataset, group, sep = " "))
  )

# Place "no-duration" datasets in a band at the top (so they still appear)
# choose a band safely above your max observed median time
max_time <- max(df$time_median_hr, df$time_q75_hr, na.rm = TRUE)
df <- df %>%
  mutate(
    has_time = !is.na(time_median_hr) & !is.na(time_q25_hr) & !is.na(time_q75_hr),
    y_plot = ifelse(has_time, time_median_hr, max_time * 1.8)
  )

?geom_text_repel()
p_time <- ggplot(df, aes(x = space_median_km, y = y_plot)) +
  
  # horizontal IQR (space)
  geom_errorbarh(
    aes(xmin = space_q25_km, xmax = space_q75_km),
    height = 0, linewidth = 0.6, alpha = 0.7
  ) +
  
  # vertical IQR (time) only where time exists
  geom_errorbar(
    data = df %>% filter(has_time),
    aes(ymin = time_q25_hr, ymax = time_q75_hr),
    width = 0, linewidth = 0.6, alpha = 0.7
  ) +
  
  # points
  geom_point(
    aes(shape = dataset),
    size = 3.3, stroke = 0.4, color = "navyblue", fill = "blue"
  ) +
  
  # labels with WHITE leader lines (segments)
  geom_text_repel(
    aes(label = label),
    size = 4,
    family = "serif",
    fontface = "italic",
    min.segment.length = 0,
    box.padding = 0.4,
    point.padding = 0.25,
    max.overlaps = Inf,
    segment.color = "white",
    segment.size = 0.5
  ) +
  
  # scales (log helps because your ranges span orders of magnitude)
  scale_x_log10(
    breaks = c(0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 200),
    labels = label_number()
  ) +
  scale_y_log10(
    breaks = c(0.1, 0.25, 0.5, 1, 2, 4, 8, 16, 32),
    labels = label_number()
  ) +
  
  labs(
    x = "Spatial scale (km) median with IQR)",
    y = "Time scale (hours) median with IQR)",
    shape = NULL
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10),
    text = element_text(family = "serif"),
    axis.title = element_text(face = "bold", size = 13),
    axis.text  = element_text(size = 11)
  )

p_time

ggsave("time_space_schema.png", p_time, width = 9, height = 5)
