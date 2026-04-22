#SPATIAL VS RECORDS SCHEMA

##DEM TRAWL

dem_trawl_schema <- dem_trawl_full %>%
  summarise(
    # Spatial (trawl)
    mean_dist_km = mean(trawl_dist_km, na.rm = TRUE),
    median_dist_km = median(trawl_dist_km, na.rm = TRUE),
    min_dist_km  = min(trawl_dist_km, na.rm = TRUE),
    max_dist_km  = max(trawl_dist_km, na.rm = TRUE),
    sd_dist_km   = sd(trawl_dist_km, na.rm = TRUE),
    IQR_dist   = IQR(trawl_dist_km, na.rm = TRUE),
    
    # records
    mean_records = mean(n_records, na.rm = TRUE),
    median_records = median(n_records, na.rm = TRUE),
    sd_records   = sd(n_records, na.rm = TRUE),
    min_records  = min(n_records, na.rm = TRUE),
    max_records  = max(n_records, na.rm = TRUE),
    IQR_records   = IQR(n_records, na.rm = TRUE),
    
    n_trawls     = n()
  )

dem_trawl_schema
# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------------

#MWT
dem_trawl_schema <- dem_trawl_full %>%
  summarise(
    # Spatial (trawl)
    mean_dist_km = mean(trawl_dist_km, na.rm = TRUE),
    median_dist_km = median(trawl_dist_km, na.rm = TRUE),
    min_dist_km  = min(trawl_dist_km, na.rm = TRUE),
    max_dist_km  = max(trawl_dist_km, na.rm = TRUE),
    sd_dist_km   = sd(trawl_dist_km, na.rm = TRUE),
    IQR_dist   = IQR(trawl_dist_km, na.rm = TRUE),
    
    # records
    mean_records = mean(n_records, na.rm = TRUE),
    median_records = median(n_records, na.rm = TRUE),
    sd_records   = sd(n_records, na.rm = TRUE),
    min_records  = min(n_records, na.rm = TRUE),
    max_records  = max(n_records, na.rm = TRUE),
    IQR_records   = IQR(n_records, na.rm = TRUE),
    
    n_trawls     = n()
  )

dem_trawl_schema

# A tibble: 1 × 13
#mean_dist_km median_dist_km min_dist_km max_dist_km sd_dist_km IQR_dist mean_records median_records sd_records min_records max_records IQR_records n_trawls
#<dbl>          <dbl>       <dbl>       <dbl>      <dbl>    <dbl>        <dbl>          <int>      <dbl>       <int>       <int>       <dbl>    <int>
#  1         4.38           3.20           0       2968.       44.5    0.547         12.8             13       5.72           1          36           7     8935


##MWT TRAWL

DD_trawl_scale   <- trawl_scale(DD_geo)
OROP_trawl_scale <- trawl_scale(OROP_geo)

bind_rows(DD_trawl_scale, OROP_trawl_scale) %>%
  group_by(source) %>%
  summarise(
    # Spatial (trawl)
    mean_dist_km = mean(trawl_dist_km, na.rm = TRUE),
    median_dist_km = median(trawl_dist_km, na.rm = TRUE),
    min_dist_km  = min(trawl_dist_km, na.rm = TRUE),
    max_dist_km  = max(trawl_dist_km, na.rm = TRUE),
    sd_dist_km   = sd(trawl_dist_km, na.rm = TRUE),
    IQR_dist   = IQR(trawl_dist_km, na.rm = TRUE),
    n_trawls = n()
  )

# A tibble: 2 × 8
#source mean_dist_km median_dist_km min_dist_km max_dist_km sd_dist_km IQR_dist n_trawls
#<chr>         <dbl>          <dbl>       <dbl>       <dbl>      <dbl>    <dbl>    <int>
# DD             12.8           9.37           0        348.       16.8     10.6     3721
# OROP           19.5          14.7            0        438.       20.3     16.4     3546


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
    # records
    mean_records = mean(n_records, na.rm = TRUE),
    median_records = median(n_records, na.rm = TRUE),
    sd_records   = sd(n_records, na.rm = TRUE),
    min_records  = min(n_records, na.rm = TRUE),
    max_records  = max(n_records, na.rm = TRUE),
    IQR_records   = IQR(n_records, na.rm = TRUE),
    n_trawls = n()
  )

## A tibble: 2 × 8
#source mean_records median_records sd_records min_records max_records IQR_records n_trawls
#<chr>         <dbl>          <dbl>      <dbl>       <int>       <int>       <dbl>    <int>
#  1 DD             2.84              3      1.44            1           9           2     3721
#2 OROP           1.15              1      0.549           1           7           0     3556


# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------------

##### NMLS

OBS_event_summary <- OBS_event_effort %>%
  summarise(
    
    # Effort / richness (event)
    mean_records    = mean(n_records, na.rm = TRUE),
    median_records = median(n_records, na.rm = TRUE),
    sd_records   = sd(n_records, na.rm = TRUE),
    min_records  = min(n_records, na.rm = TRUE),
    max_records  = max(n_records, na.rm = TRUE),
    IQR_records   = IQR(n_records, na.rm = TRUE),
    n_events        = n()
  )

COM_event_summary <- COM_event_effort %>%
  summarise(
    
    # Effort / richness (event)
    mean_records    = mean(n_records, na.rm = TRUE),
    median_records = median(n_records, na.rm = TRUE),
    sd_records   = sd(n_records, na.rm = TRUE),
    min_records  = min(n_records, na.rm = TRUE),
    max_records  = max(n_records, na.rm = TRUE),
    IQR_records   = IQR(n_records, na.rm = TRUE),
    n_events        = n()
  )

COM_event_summary
#> COM_event_summary
# A tibble: 1 × 7
#mean_records median_records sd_records min_records max_records IQR_records n_events
#<dbl>          <dbl>      <dbl>       <int>       <int>       <dbl>    <int>
# 1         1.81              1       2.88           1          88           1   451008

OBS_event_summary
#> OBS_event_summary
# A tibble: 1 × 7
#mean_records median_records sd_records min_records max_records IQR_records n_events
#<dbl>          <dbl>      <dbl>       <int>       <int>       <dbl>    <int>
#  1         1.89              1       1.50           1          21           1    32770


OBS_trip_summary <- OBS_trip_scale %>%
  left_join(OBS_trip_effort, by = c("vessel_num", "year")) %>%
  left_join(OBS_trip_time,   by = c("vessel_num", "year")) %>%
  summarise(
    # Spatial (trip)
    mean_trip_extent_km = mean(trip_extent_km, na.rm = TRUE),
    median_trip_extent_km = median(trip_extent_km, na.rm = TRUE),
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
    median_trip_extent_km = median(trip_extent_km, na.rm = TRUE),
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

COM_trip_summary
#> COM_trip_summary
# A tibble: 1 × 13
#mean_trip_extent_km median_trip_extent_km min_trip_extent_km max_trip_extent_km sd_trip_extent_km mean_trip_days max_trip_days mean_records mean_species mean_events_coords mean_events_bio max_species n_trips
#  765.                  713.               3.17              1732.              540.           114.           366         803.            1               30.2            444.           1    1016
OBS_trip_summary
# mean_trip_extent_km median_trip_extent_km min_trip_extent_km max_trip_extent_km sd_trip_extent_km mean_trip_days max_trip_days mean_records mean_species mean_events_coords mean_events_bio max_species n_trips
#  437.                  204.             0.0923              1643.              492.           6.81           162         13.0         3.94               2.50   6.87          26    4771

nmls_event_spacing <- nmls_events %>%   # <-- your event-level table with one row per event + lat/lon + trip_id
  filter(!is.na(lat), !is.na(lon), !is.na(trip_id)) %>%
  arrange(trip_id, date, event_id) %>%  # use whatever ordering makes sense
  group_by(trip_id) %>%
  mutate(
    next_lat = lead(lat),
    next_lon = lead(lon),
    spacing_km = ifelse(
      !is.na(next_lat),
      geosphere::distHaversine(cbind(lon, lat), cbind(next_lon, next_lat))/1000,
      NA_real_
    )
  ) %>%
  ungroup()
# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------------


#CAPFISH
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
    mean_records    = mean(n_records, na.rm = TRUE),
    median_records = median(n_records, na.rm = TRUE),
    sd_records   = sd(n_records, na.rm = TRUE),
    min_records  = min(n_records, na.rm = TRUE),
    max_records  = max(n_records, na.rm = TRUE),
    IQR_records   = IQR(n_records, na.rm = TRUE),
    .groups = "drop"
  )


#  Source  mean_records median_records sd_records min_records max_records IQR_records
# SADSTIA         5.72              6      2.21            1          18           3
# SAHLLA          7.25              7      3.88            1          20           6
# SAPFIA          1.82              2      0.938           1           7           1
# SECIFA          5.60              5      3.12            1          17           4

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
    n_trips               = n(),
    .groups = "drop"
  )

#Event spacing = effective spatial contribution per record
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

#Source  mean_event_spacing_km median_event_spacing_km IQR_event_spacing_km max_event_spacing_km n_events
#SADSTIA                  2.87                   0                     0                    500.    13451
# SAHLLA                   2.02                   0                     0                    502.     3330
# SAPFIA                   8.55                   0.745                 3.80                2763.     9236
# SECIFA                   3.41                   0                     0                    155.     3083

#For fisheries observer datasets, the individual fishing set (Set_ID) was treated as the primary sampling unit. 
#Because sets are typically recorded as point locations, event-level spatial scale was quantified using the distance between successive sets
#within the same trip, representing the effective spatial contribution of each sampling event. Median event spacing and interquartile ranges were 
#used to characterise within-dataset variability. Trip-level spatial extent was retained separately to 
#describe overall fishing movement but was not used as an independent sampling unit in spatial analyses.


# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------------

#BRUV
BRUV_event_summary <- BRUV_event_full %>%
  summarise(
    # spatial
    mean_dist_km = mean(event_dist_km, na.rm = TRUE),
    min_dist_km  = min(event_dist_km, na.rm = TRUE),
    max_dist_km  = max(event_dist_km, na.rm = TRUE),
    median_dist_km = median(event_dist_km, na.rm = TRUE),
    sd_dist_km = sd(event_dist_km, na.rm = TRUE),
    IQR_dist_km = IQR(event_dist_km, na.rm = TRUE),
    
    # effort / richness
    mean_records = mean(n_records, na.rm = TRUE),
  min_records = min(n_records, na.rm = TRUE),
  max_records = max(n_records, na.rm = TRUE),
  median_records = median(n_records, na.rm = TRUE),
  sd_records = sd(n_records, na.rm = TRUE),
  IQR_records = IQR(n_records, na.rm = TRUE),

    n_events = n()
  )

BRUV_event_summary

#mean_dist_km min_dist_km max_dist_km median_dist_km sd_dist_km IQR_dist_km mean_records min_records max_records median_records sd_records IQR_records n_events
#  0           0           0              0          0           0         12.6           1          86             10       11.0           9     1891


# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------------

#FM

FM_event_spacing_summary <- FM_event_spacing %>%
  group_by(DataSetID) %>%
  summarise(
    median_event_spacing_km = median(event_spacing_km, na.rm = TRUE),
    IQR_event_spacing_km    = IQR(event_spacing_km, na.rm = TRUE),
    mean_event_spacing_km   = mean(event_spacing_km, na.rm = TRUE),
    max_event_spacing_km    = max(event_spacing_km, na.rm = TRUE),
    min_event_spacing_km    = min(event_spacing_km, na.rm = TRUE),
    sd_event_spacing_km    = sd(event_spacing_km, na.rm = TRUE),
    n_events                = sum(!is.na(event_spacing_km)),
    .groups = "drop"
  )

FM_method_summary <- record_spp %>%
  group_by(MethodNew, DataSetID) %>%
  summarise(
    n_records = n_distinct(record_event_id),
    .groups = "drop"
  ) %>%
  group_by(MethodNew) %>%
  summarise(
    n_datasets = n_distinct(DataSetID),
    
    mean_records   = mean(n_records, na.rm = TRUE),
    median_records = median(n_records, na.rm = TRUE),
    sd_records     = sd(n_records, na.rm = TRUE),
    min_records    = min(n_records, na.rm = TRUE),
    max_records    = max(n_records, na.rm = TRUE),
    IQR_records    = IQR(n_records, na.rm = TRUE),
    
    total_records  = sum(n_records, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_records))

FM_method_summary

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
    iqr_km  = ifelse(n_with_extent > 0,
                     IQR(spatial_extent_km, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

names(record_spp)
# Records / richness per FM record-event, summarised to MethodNew names
FM_method_effort <- record_spp %>%
  group_by(MethodNew, DataSetID, record_event_id) %>%
  summarise(
    n_records = n(),
    .groups = "drop"
  ) %>%
  group_by(MethodNew) %>%
  summarise(
    # Records per record-event
    mean_records   = mean(n_records, na.rm = TRUE),
    median_records = median(n_records, na.rm = TRUE),
    sd_records     = sd(n_records, na.rm = TRUE),
    min_records    = min(n_records, na.rm = TRUE),
    max_records    = max(n_records, na.rm = TRUE),
    IQR_records    = IQR(n_records, na.rm = TRUE),
    
    n_events       = n(),  # number of record-events
    n_datasets     = n_distinct(DataSetID),
    .groups = "drop"
  )

names(final_data_joined)



FM_event_table <- final_data_joined %>%
  filter(!is.na(MethodNew), !is.na(DataSetID), !is.na(RecordID)) %>%
  group_by(MethodNew, DataSetID, RecordID) %>%
  summarise(
    n_records = n(),
    .groups = "drop"
  ) %>%
  left_join(
    record_spp %>%
      select(MethodNew, DataSetID, RecordID, n_species_record),
    by = c("MethodNew", "DataSetID", "RecordID")
  )

library(dplyr)
library(stringr)

final_data_joined2 <- final_data_joined %>%
  mutate(
    DataSetID = as.character(DataSetID),
    MethodNew = as.character(MethodNew),
    
    # coordinate key (match your earlier NA_COORD behaviour)
    coord_key = ifelse(is.na(Latitude) | is.na(Longitude),
                       "NA_COORD",
                       str_c(round(Latitude, 5), round(Longitude, 5), sep = "_")),
    
    # date key (you already have StartDate_date / EndDate_date)
    date_key = str_c(as.character(StartDate_date), as.character(EndDate_date), sep = "_"),
    
    # sampling location key (safe)
    loc_key = coalesce(as.character(SamplingLocation), "NA_LOC"),
    
    # FINAL event key
    record_event_id = str_c(DataSetID, MethodNew, loc_key, date_key, coord_key, sep = "|")
  )

record_spp2 <- record_spp %>%
  mutate(
    DataSetID = as.character(DataSetID),
    MethodNew = as.character(MethodNew)
  )

FM_event_table <- final_data_joined2 %>%
  filter(!is.na(MethodNew), !is.na(DataSetID), !is.na(record_event_id)) %>%
  group_by(MethodNew, DataSetID, record_event_id) %>%
  summarise(
    n_records = n(),
    .groups = "drop"
  ) %>%
  left_join(
    record_spp2 %>% select(MethodNew, DataSetID, record_event_id, n_species_record),
    by = c("MethodNew", "DataSetID", "record_event_id")
  )




#FM_method_summary
#   MethodNew      n_datasets mean_records median_records sd_records min_records max_records IQR_records total_records
#chemical                8        11.1             1.5     25.9             1          75        2               89
# gillnet                26         1.96            1        1.15            1           4        2               51
# seinenet               26         1.81            1        1.23            1           5        1               47
# planktonnet             9         3.67            1        5.32            1          16        1               33
# shoreangling           22         1.41            1        1.50            1           8        0               31
# UVC                     4         7.25            1.5     11.8             1          25        6.75            29
# records                14         1.21            1        0.426           1           2        0               17
# mixedmethod             8         1.75            1.5      1.04            1           4        1               14
# fykenet                 3         4               4        2               2           6        2               12
# BRUV                    3         2.67            3        1.53            1           4        1.5              8
# beamtrawl               8         1               1        0               1           1        0                8
# visualestimate          7         1.14            1        0.378           1           2        0                8
# boatangling             4         1               1        0               1           1        0                4
# demtrawl                4         1               1        0               1           1        0                4
# dnettrawl               4         1               1        0               1           1        0                4
# deadcollection          3         1               1        0               1           1        0                3
# mixedangling            1         1               1       NA               1           1        0                1
# shallowtrawl            1         1               1       NA               1           1        0                1
# trap                    1         1               1       NA               1           1        0                1


#FM_method_spatial
#   MethodNew      n_datasets n_with_extent mean_km median_km  sd_km min_km max_km iqr_km
# BRUV                    3             2   12.1      12.1    9.29   5.53   18.7  6.57
# UVC                     4             2   37.6      37.6   53.2    0      75.2  37.6
# beamtrawl               8             8    0         0      0      0       0    0
# boatangling             4             2    0         0      0      0       0   0
# chemical                8             6    5.36      0     10.1    0      25.2  5.23
# deadcollection          3             3    0         0      0      0       0    0
# demtrawl                4             3    0         0      0      0       0    0
# dnettrawl               4             3    0         0      0      0       0    0
# fykenet                 3             0   NA        NA     NA     NA      NA    NA
# gillnet                26            20   30.9       0    132.     0     592.  2.55
# mixedangling            1             1    0         0     NA      0       0  0
# mixedmethod             8             8   55.1       5.70 136.     0     390.  19.7
# planktonnet             9             6   70.7       0    173.     0     424. 0.208
# records                14            14    1.56      0      3.57   0      12.3. 0
# seinenet               26            21    3.68      0      8.23   0      27.5 0
# shallowtrawl            1             1    0         0     NA      0       0  0
# shoreangling           22            18   13.1       0     52.6    0     224. 0
# trap                    1             0   NA        NA     NA     NA      NA  NA
# visualestimate          7             6   23.2       0     56.9    0     139. 0
# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------------

#MUSEUM

names(museum_clean)
museum_SA <- museum_clean %>% filter(countryCode == "ZA")
museum_summary_all <-museum_clean %>%
  summarise(
    n_records_total = n(),
    n_species_total = n_distinct(sp),
    
    n_records_year  = sum(has_year),
    n_species_year  = n_distinct(sp[has_year]),
    
    n_records_coords = sum(has_coords),
    n_species_coords = n_distinct(sp[has_coords]),
    
    min_year    = min(year_final, na.rm = TRUE),
    q25_year    = quantile(year_final, 0.25, na.rm = TRUE),
    median_year = median(year_final, na.rm = TRUE),
    q75_year    = quantile(year_final, 0.75, na.rm = TRUE),
    max_year    = max(year_final, na.rm = TRUE),
    IQR_year    = IQR(year_final, na.rm = TRUE),
    
    n_uncert = sum(!is.na(uncert_m)),
    median_uncert_m = median(uncert_m, na.rm = TRUE),
    q25_uncert_m    = quantile(uncert_m, 0.25, na.rm = TRUE),
    q75_uncert_m    = quantile(uncert_m, 0.75, na.rm = TRUE),
    IQR_uncert_m    = IQR(uncert_m, na.rm = TRUE),
    max_uncert_m    = max(uncert_m, na.rm = TRUE)
  )

#> museum_summary_all
#n_records_total n_species_total n_records_year n_species_year n_records_coords n_species_coords min_year q25_year median_year q75_year max_year IQR_year n_uncert median_uncert_m
#           65329            2471          54915           2221            58371             2214     1803     1976        1988     2004     2022       28     2528            8679
#q25_uncert_m q75_uncert_m IQR_uncert_m max_uncert_m
#        3875       115000       111125      1414649
museum_eventish <- museum_SA %>%
  mutate(
    event_key = coalesce(
      na_if(eventID, ""),
      na_if(as.character(eventDate), ""),
      str_c(datasetKey, recordedBy, year_final, locality, sep = " | ")
    )
  )

museum_event_records <- museum_eventish %>%
  filter(!is.na(event_key)) %>%
  group_by(event_key) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(sp),
    .groups = "drop"
  )

museum_event_summary <- museum_event_records %>%
  summarise(
    n_events = n(),
    mean_records_per_event   = mean(n_records),
    median_records_per_event = median(n_records),
    IQR_records_per_event    = IQR(n_records),
    max_records_per_event    = max(n_records),
    min_records_per_event    = min(n_records)
   
  )

#   n_events mean_records_per_event median_records_per_event IQR_records_per_event max_records_per_event min_records_per_event
#    26411                   2.18                        1                     0                   305                     1

museum_spatial_uncert_summary <- museum_SA %>%
  filter(!is.na(uncert_m)) %>%
  summarise(
    median_footprint_km = median(uncert_m, na.rm = TRUE) / 1000,
    IQR_footprint_km    = IQR(uncert_m, na.rm = TRUE) / 1000,
    q25_footprint_km    = quantile(uncert_m, 0.25, na.rm = TRUE) / 1000,
    q75_footprint_km    = quantile(uncert_m, 0.75, na.rm = TRUE) / 1000,
    max_footprint_km    = max(uncert_m, na.rm = TRUE) / 1000,
    n_with_uncert       = n()
  )

#museum_spatial_uncert_summary
#median_footprint_km IQR_footprint_km q25_footprint_km q75_footprint_km max_footprint_km n_with_uncert
#  8.679          111.125            3.875              115         1414.649          2528
# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------------

#INAT
inat_event_records <- inat_eventish %>%
  filter(!is.na(event_key)) %>%
  group_by(event_key) %>%
  summarise(
    n_records = n(),                 # records per event
    n_species = n_distinct(sp),      # richness per event
    .groups = "drop"
  )
inat_event_summary <- inat_event_records %>%
  summarise(
    n_events = n(),
    
    mean_records_per_event   = mean(n_records),
    median_records_per_event = median(n_records),
    sd_records_per_event     = sd(n_records),
    IQR_records_per_event    = IQR(n_records),
    min_records_per_event    = min(n_records),
    max_records_per_event    = max(n_records)
  )

inat_event_summary

#n_events mean_records_per_event median_records_per_event sd_records_per_event IQR_records_per_event min_records_per_event max_records_per_event
#   5483                   2.19                        1                 4.54                     1                     1                    97

inat_footprint_summary <- inat_SA %>%
  filter(!is.na(uncert_m)) %>%
  summarise(
    n_with_uncert     = n(),
    median_footprint_km = median(uncert_m, na.rm = TRUE) / 1000,
    q25_footprint_km    = quantile(uncert_m, 0.25, na.rm = TRUE) / 1000,
    q75_footprint_km    = quantile(uncert_m, 0.75, na.rm = TRUE) / 1000,
    IQR_footprint_km    = IQR(uncert_m, na.rm = TRUE) / 1000,
    min_footprint_km    = min(uncert_m, na.rm = TRUE) / 1000,
    max_footprint_km    = max(uncert_m, na.rm = TRUE) / 1000
  )

inat_footprint_summary
#n_with_uncert median_footprint_km q25_footprint_km q75_footprint_km IQR_footprint_km min_footprint_km max_footprint_km
#    11235               0.067            0.008            0.891            0.883            0.001         1494.284


inat_event_spacing <- inat_eventish %>%
filter(has_coords, !is.na(event_key)) %>%
  arrange(event_key, lat, lon) %>%
  group_by(event_key) %>%
  mutate(
    next_lat = lead(lat),
    next_lon = lead(lon),
    spacing_km = ifelse(
      !is.na(next_lat),
      distHaversine(cbind(lon, lat), cbind(next_lon, next_lat)) / 1000,
      NA_real_
    )
  ) %>%
  ungroup()

inat_event_spacing_summary <- inat_event_spacing %>%
  summarise(
    median_event_spacing_km = median(spacing_km, na.rm = TRUE),
    IQR_event_spacing_km    = IQR(spacing_km, na.rm = TRUE),
    max_event_spacing_km    = max(spacing_km, na.rm = TRUE),
    n_spacings              = sum(!is.na(spacing_km))
  )

inat_event_spacing_summary

#median_event_spacing_km IQR_event_spacing_km max_event_spacing_km n_spacings
#      0              0.00524                 988.       6521

# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------------

schema_row <- function(source, record_unit,
                       x_med, x_q25, x_q75,
                       y_med, y_q25, y_q75,
                       n_units = NA_integer_) {
  tibble::tibble(
    source = source,
    record_unit = record_unit,
    x_med = x_med, x_q25 = x_q25, x_q75 = x_q75,
    y_med = y_med, y_q25 = y_q25, y_q75 = y_q75,
    n_units = n_units
  )
}

dem_schema <- schema_row(
  source = "DEMTRAWL",
  record_unit = "trawl",
  x_med = dem_trawl_schema$median_dist_km,
  x_q25 = dem_trawl_schema$median_dist_km - (dem_trawl_schema$IQR_dist/2),  # if you don't have q25/q75
  x_q75 = dem_trawl_schema$median_dist_km + (dem_trawl_schema$IQR_dist/2),
  y_med = dem_trawl_schema$median_records,
  y_q25 = dem_trawl_schema$median_records - (dem_trawl_schema$IQR_records/2),
  y_q75 = dem_trawl_schema$median_records + (dem_trawl_schema$IQR_records/2),
  n_units = dem_trawl_schema$n_trawls
)



mwt_dd_schema <- schema_row(
  "MWT_DD", "trawl",
  x_med = 9.37,
  x_q25 = 9.37 - (10.6/2),
  x_q75 = 9.37 + (10.6/2),
  y_med = 3,
  y_q25 = 3 - (2/2),
  y_q75 = 3 + (2/2),
  n_units = 3721
)

mwt_orop_schema <- schema_row(
  "MWT_OROP", "trawl",
  x_med = 14.7,
  x_q25 = 14.7 - (16.8/2),
  x_q75 = 14.7 + (16.8/2),
  y_med = 1,
  y_q25 = 1 - (0/2),
  y_q75 = 1 + (0/2),
  n_units = 3556
)

cap_schema <- capfish_event_summary %>%
  left_join(capfish_event_spacing_summary, by = "Source") %>%
  transmute(
    source      = paste0("CAPFISH_", Source),
    record_unit = "set",
    x_med = median_event_spacing_km,
    x_q25 = median_event_spacing_km - (IQR_event_spacing_km/2),
    x_q75 = median_event_spacing_km + (IQR_event_spacing_km/2),
    
    y_med = median_records,
    y_q25 = median_records - (IQR_records/2),
    y_q75 = median_records + (IQR_records/2),
    
    n_units = n_events
  )

bruv_schema <- schema_row(
  source = "BRUV",
  record_unit = "deployment",
  x_med = 0.001, x_q25 = 0.001, x_q75 = 0.001,   # epsilon km for log scale
  y_med = BRUV_event_summary$median_records,
  y_q25 = BRUV_event_summary$median_records - (BRUV_event_summary$IQR_records/2),
  y_q75 = BRUV_event_summary$median_records + (BRUV_event_summary$IQR_records/2),
  n_units = BRUV_event_summary$n_events
)

museum_schema <- schema_row(
  "MUSEUM_GBIF", "occurrence event",
  x_med = museum_spatial_uncert_summary$median_footprint_km,
  x_q25 = museum_spatial_uncert_summary$q25_footprint_km,
  x_q75 = museum_spatial_uncert_summary$q75_footprint_km,
  y_med = museum_event_summary$median_records_per_event,
  y_q25 = museum_event_summary$median_records_per_event - (museum_event_summary$IQR_records_per_event/2),
  y_q75 = museum_event_summary$median_records_per_event + (museum_event_summary$IQR_records_per_event/2),
  n_units = museum_event_summary$n_events
)

inat_schema <- schema_row(
  "INAT_GBIF", "observer-day event",
  x_med = inat_footprint_summary$median_footprint_km,
  x_q25 = inat_footprint_summary$q25_footprint_km,
  x_q75 = inat_footprint_summary$q75_footprint_km,
  y_med = inat_event_summary$median_records_per_event,
  y_q25 = inat_event_summary$median_records_per_event - (inat_event_summary$IQR_records_per_event/2),
  y_q75 = inat_event_summary$median_records_per_event + (inat_event_summary$IQR_records_per_event/2),
  n_units = inat_event_summary$n_events
)


FM_schema <- FM_method_summary %>%
  full_join(FM_method_spatial, by = "MethodNew", suffix = c("_records", "_spatial")) %>%
  mutate(
    n_datasets = coalesce(n_datasets_records, n_datasets_spatial),
    has_extent = !is.na(median_km) & !is.na(mean_km) & is.finite(median_km)
  ) %>%
  select(
    MethodNew,
    n_datasets,
    n_with_extent,
    mean_records, median_records, sd_records, min_records, max_records, IQR_records,
    total_records,
    mean_km, median_km, sd_km, min_km, max_km, iqr_km,
    has_extent
  ) %>%
  arrange(desc(total_records))

FM_method_table <- FM_method_summary %>%
  full_join(FM_method_spatial, by = "MethodNew", suffix = c("_records", "_spatial")) %>%
  mutate(
    n_datasets = coalesce(n_datasets_records, n_datasets_spatial),
    has_extent = !is.na(median_km) & is.finite(median_km)
  ) %>%
  arrange(desc(total_records))

FM_schema <- FM_method_summary %>%
  full_join(FM_method_spatial, by = "MethodNew", suffix = c("_records", "_spatial")) %>%
  transmute(
    source      = paste0("FM_", MethodNew),
    record_unit = MethodNew,   # keeps each method explicit (like CAPFISH_*). Or set "sampling event".
    
    # spatial axis (x): median + IQR/2
    x_med = median_km,
    x_q25 = median_km - (iqr_km / 2),
    x_q75 = median_km + (iqr_km / 2),
    
    # records axis (y): median + IQR/2
    y_med = median_records,
    y_q25 = median_records - (IQR_records / 2),
    y_q75 = median_records + (IQR_records / 2),
    
    # choose what "n_units" means for FM:
    # datasets is usually the most comparable across FM methods
    n_units = as.integer(n_datasets_records)
  ) %>%
  mutate(
    # if a method has no spatial extent, keep it but give epsilon for log-x plots
    x_med = if_else(is.na(x_med) | !is.finite(x_med) | x_med <= 0, 0.001, x_med),
    x_q25 = if_else(is.na(x_q25) | !is.finite(x_q25) | x_q25 <= 0, x_med, x_q25),
    x_q75 = if_else(is.na(x_q75) | !is.finite(x_q75) | x_q75 <= 0, x_med, x_q75)
  )



nmls_records_schema <- bind_rows(
  COM_event_effort %>% mutate(Source = "LINEFISH_COM"),
  OBS_event_effort %>% mutate(Source = "LINEFISH_OBS")
) %>%
  group_by(Source) %>%
  summarise(
    mean_records   = mean(n_records, na.rm = TRUE),
    median_records = median(n_records, na.rm = TRUE),
    sd_records     = sd(n_records, na.rm = TRUE),
    min_records    = min(n_records, na.rm = TRUE),
    max_records    = max(n_records, na.rm = TRUE),
    IQR_records    = IQR(n_records, na.rm = TRUE),
    n_events       = n(),
    .groups = "drop"
  )

nmls_spatial_schema <- nmls_event_spacing_summary %>%
  transmute(
    Source,
    median_km = median_event_spacing_km,
    q25_km    = q25_event_spacing_km,
    q75_km    = q75_event_spacing_km,
    iqr_km    = IQR_event_spacing_km,
    max_km    = max_event_spacing_km,
    n_spacings
  )

# 3) Combine into one schema table
nmls_schema <- nmls_records_schema %>%
  left_join(nmls_spatial_schema, by = "Source") %>%
  mutate(
    has_extent = !is.na(median_km) & is.finite(median_km)
  ) %>%
  select(
    Source,
    # records axis
    mean_records, median_records, sd_records, min_records, max_records, IQR_records, n_events,
    # spatial axis
    median_km, q25_km, q75_km, iqr_km, max_km, n_spacings,
    has_extent
  )


linefish_schema <- nmls_schema %>%
  transmute(
    source      = Source,
    record_unit = "trip-day event",   # or "observer-day event" if you prefer
    # spatial axis (x): prefer q25/q75 if present, else derive from median + iqr
    x_med = median_km,
    x_q25 = if_else(!is.na(q25_km), q25_km, median_km - (iqr_km / 2)),
    x_q75 = if_else(!is.na(q75_km), q75_km, median_km + (iqr_km / 2)),
    
    # records axis (y): median +/- IQR/2
    y_med = median_records,
    y_q25 = median_records - (IQR_records / 2),
    y_q75 = median_records + (IQR_records / 2),
    
    n_units = as.integer(n_events)
  ) %>%
  mutate(
    # guardrails for log scales / plotting
    x_med = if_else(is.na(x_med) | x_med <= 0, 0.001, x_med),
    x_q25 = if_else(is.na(x_q25) | x_q25 <= 0, x_med, x_q25),
    x_q75 = if_else(is.na(x_q75) | x_q75 <= 0, x_med, x_q75)
  )

schema_all <- bind_rows(
  dem_schema,
  mwt_dd_schema,
  mwt_orop_schema,
  cap_schema,
  FM_schema,        # now in correct shape
  bruv_schema,
  museum_schema,
  inat_schema,
  linefish_schema
) %>%
  mutate(
    x_med = if_else(x_med <= 0 | is.na(x_med), 0.001, x_med),
    x_q25 = if_else(x_q25 <= 0 | is.na(x_q25), x_med, x_q25),
    x_q75 = if_else(x_q75 <= 0 | is.na(x_q75), x_med, x_q75)
  )


schema_all <- schema_all %>%
  mutate(
    y_q25 = pmax(y_q25, 0),
    y_med = pmax(y_med, 0),
    y_q75 = pmax(y_q75, 0)
  )

schema_all <- schema_all %>%
  mutate(
    x_is_epsilon = x_med == 0.001 & x_q25 == 0.001 & x_q75 == 0.001
  )

print(schema_all, n =Inf)
    
library(ggplot2)
library(dplyr)

ggplot(schema_all, aes(x = x_med, y = y_med, label = source)) +
  geom_errorbar(aes(ymin = y_q25, ymax = y_q75), width = 0) +
  geom_errorbarh(aes(xmin = x_q25, xmax = x_q75), height = 0) +
  geom_point(aes(shape = x_is_epsilon), size = 3) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Spatial extent (km; median with IQR)",
    y = "Records per unit (median with IQR)",
    shape = "Extent unknown (epsilon)"
  ) +
  theme_classic()

schema_plot <- schema_all %>%
  mutate(
    group = case_when(
      str_detect(source, "^FM_") ~ "FM",
      str_detect(source, "^CAPFISH_") ~ "CAPFISH",
      str_detect(source, "^LINEFISH_") ~ "LINEFISH",
      source %in% c("DEMTRAWL","MWT_DD","MWT_OROP") ~ "TRAWL",
      source == "BRUV" ~ "BRUV",
      source %in% c("MUSEUM_GBIF","INAT_GBIF") ~ "GBIF",
      TRUE ~ "Other"
    )
  )

schema_plot <- schema_plot %>%
  mutate(
    y_q25 = if_else(y_q25 <= 0, 0.001, y_q25),
    y_med = if_else(y_med <= 0, 0.001, y_med),
    y_q75 = if_else(y_q75 <= 0, 0.001, y_q75)
  )

schema_plot <- schema_plot %>%
  mutate(x_is_epsilon = if_else(source == "BRUV", FALSE, x_is_epsilon))



ggplot(schema_plot, aes(x = x_med, y = y_med)) +
  geom_errorbar(aes(ymin = y_q25, ymax = y_q75), width = 0, alpha = 0.6) +
  geom_errorbarh(aes(xmin = x_q25, xmax = x_q75), height = 0, alpha = 0.6) +
  geom_point(aes(color = group, shape = x_is_epsilon), size = 3) +
  geom_text(aes(label = source), size = 2.6, vjust = -0.7, check_overlap = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Spatial extent (km; median with IQR)",
    y = "Records per sampling unit (median with IQR)",
    color = "Data source group",
    shape = "Extent unknown (epsilon)"
  ) +
  theme_classic()



# ============================================================
# SCHEMA FIGURE: total records (contribution) vs spatial extent
# X = sampling-unit spatial extent (km; median with IQR)
# Y = total records contributed by each source/method
# Both axes log10; includes IQR bars on X; point size = n_units (optional)
# ============================================================

library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(scales)

# ---- 0) Start from your existing schema_all (already built) ----
# schema_all columns: source, record_unit, x_med, x_q25, x_q75, y_med, y_q25, y_q75, n_units, x_is_epsilon

# ---- 1) Build TOTAL RECORDS per source/method ----
# (This uses the objects you confirmed exist via names(). For DEMTRAWL/BRUV/GBIF/CAPFISH
#  you must point to your record-level tables if you want exact totals; placeholders are marked.)

# LINEFISH totals (exact from event_effort tables)
linefish_totals <- bind_rows(
  COM_event_effort %>% mutate(source = "LINEFISH_COM"),
  OBS_event_effort %>% mutate(source = "LINEFISH_OBS")
) %>%
  group_by(source) %>%
  summarise(total_records = sum(n_records, na.rm = TRUE), .groups = "drop")

# MWT totals (exact from trawl_effort tables)
mwt_totals <- bind_rows(
  DD_trawl_effort   %>% mutate(source = "MWT_DD"),
  OROP_trawl_effort %>% mutate(source = "MWT_OROP")
) %>%
  group_by(source) %>%
  summarise(total_records = sum(n_records, na.rm = TRUE), .groups = "drop")

# FM totals (exact from FM_method_summary)
fm_totals <- FM_method_summary %>%
  transmute(source = paste0("FM_", MethodNew),
            total_records = total_records)

 capfish_totals <- capfish_all %>%
 count(Source, name = "total_records") %>%
   transmute(source = paste0("CAPFISH_", Source), total_records)


# DEMTRAWL totals
dem_totals <- tibble(source = "DEMTRAWL", total_records = nrow(demtrawldat))


# BRUV totals
bruv_totals <- tibble(source = "BRUV", total_records = nrow(finalBRUV))


# GBIF totals
 museum_totals <- tibble(source = "MUSEUM_GBIF", total_records = nrow(museum_clean))
inat_totals   <- tibble(source = "INAT_GBIF",   total_records = nrow(inat_clean))

totals_all <- bind_rows(
  linefish_totals,
  mwt_totals,
  fm_totals,
  capfish_totals,
  dem_totals,
  bruv_totals,
  museum_totals,
  inat_totals
)

# ---- 2) Join totals onto your schema_all x-axis (sampling-unit extent) ----
schema_tot <- schema_all %>%
  select(source, record_unit, x_med, x_q25, x_q75, n_units, x_is_epsilon) %>%
  left_join(totals_all, by = "source") %>%
  mutate(
    # groups for legend
    group = case_when(
      str_detect(source, "^FM_") ~ "FM",
      str_detect(source, "^CAPFISH_") ~ "CAPFISH",
      str_detect(source, "^LINEFISH_") ~ "LINEFISH",
      source %in% c("DEMTRAWL","MWT_DD","MWT_OROP") ~ "TRAWL",
      source == "BRUV" ~ "BRUV",
      source %in% c("MUSEUM_GBIF","INAT_GBIF") ~ "GBIF",
      TRUE ~ "Other"
    ),
    # nicer labels (optional)
    label = source
  ) %>%
  # log-safe x
  mutate(
    x_med = if_else(is.na(x_med) | x_med <= 0, 0.001, x_med),
    x_q25 = if_else(is.na(x_q25) | x_q25 <= 0, x_med, x_q25),
    x_q75 = if_else(is.na(x_q75) | x_q75 <= 0, x_med, x_q75)
  )

# ---- 3) Plot ----
#need log10
p <- ggplot(schema_tot, aes(x = x_med, y = total_records)) +
  geom_errorbarh(
    aes(xmin = x_q25, xmax = x_q75),
    height = 0,
    alpha = 0.55
  ) +
  geom_point(
    aes(
      color = group,
      shape = x_is_epsilon,
      size  = n_units,
      alpha = total_records > 20
    )
  ) +
  scale_alpha_manual(
    values = c(`TRUE` = 1, `FALSE` = 0.4),
    guide = "none"
  ) +
  geom_text(
    aes(label = label),
    size = 2.6,
    vjust = -0.7,
    check_overlap = TRUE
  ) +
  scale_x_log10(
    labels = scales::label_number(accuracy = 1)
  ) +
  scale_y_log10(
    labels = scales::label_number(big.mark = ",")
  ) +
  scale_size_continuous(
    labels = scales::label_number(big.mark = ",")
  ) +
  labs(
    x = "Spatial extent of sampling unit (km; median ± IQR)",
    y = "Total records contributed",
    color = "Data source group",
    shape = "Extent unknown (epsilon)",
    size  = "No. sampling units"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 10)
  )

p

#add more ticks
#log:-1 0 1 2 3
#km: 0.1 1 10 100 1000 etc

#make sure using log10

#apply radial area to point area for observer nmls could be 100km 
#find out from sven
#typical max 40/45 nm radius
#avrg 10m 
#median between 20nm and 100nm radial from point of obs

#Capfish sapfia less samples more species commercials will give less observers will give more and capfish trawl same spatial extent as dem trawl 
#SECIFA inshore trawl same sapatial extent as offshore only difference is depth, more species, shallower depth 
#capfish longline  spatial extent expect length of line 100km ish 
  
#dots do not cover scale of size, make graph dots bigger


#make a cross or elipsis where horizontal width is spatial range min to max and and the dot will fall in median or mean and the vertical distance will be records min max etc

#instead of total records, have average per year


#number of record per sampling unit = no of species

#one with spp maybe 
#and one with records most important

#total records COMMERCIAL NMLS y axis
# x axis spatial extent of one record (unit of sampling)

#what size of area can you attribute that record to

#TOTAL spatial extent of sampling vs total records 
#work out a footprint 

#abbreviate labels 
#can give eacha number and a key 

#spp per record 

#blob more important than lines, make lines les sprominent

# trawl should be larger sppatial extent for a record than linefish

#don't ue standard error, smallest range of sample



#linefishing trip deeply wrong??? need to find spatial extent of one event 


#looking at comdat unit for 1km so spatial extent same for observer nmls


# install.packages(c("tidyverse","ggrepel","scales"))
library(tidyverse)
library(ggrepel)
library(scales)

df <- tribble(
  ~dataset,  ~sub,     ~n_records, ~q25_km,  ~q75_km,  ~median_km,
  "NMLS",    "OBS",      63188,      15,      127,       46,
  "NMLS",    "COM",     759023,      40,      181,      102,
  
  "MWTRAWL", NA,         13481,     6.28,    19.4,     11.5,
  "DEMTRAWL",NA,        101756,     3.05,     3.46,     3.25,
  
  "CAPFISH", "SADSTIA",   75759,    13.6,    32.1,     20.6,
  "CAPFISH", "SAHLLA",    19395,     4.61,   29.4,     10.9,
  "CAPFISH", "SAPFIA",    16876,     1.99,   10.7,      4.03,
  "CAPFISH", "SECIFA",    16941,    11.9,    24.5,     17.4,
  
  "BRUV",    NA,         20164,     0.54,     1.93,     0.94,
  "MUSEUM",  NA,         36979,     7.71,    134,      40.6,
  "INAT",    NA,         10101,     0.060,    5.49,     0.568,
  "LIT",     NA,          6464,     2.40,    19.7,      4.62
) %>%
  mutate(
    label = if_else(is.na(sub), dataset, paste0(dataset, "_", sub)),
    # optional grouping for color (edit these if you want different buckets)
    group = case_when(
      dataset %in% c("BRUV","DEMTRAWL","MWTRAWL") ~ "Survey / research",
      dataset %in% c("NMLS","CAPFISH") ~ "Fisheries",
      dataset %in% c("MUSEUM") ~ "Museum specimens",
      dataset %in% c("INAT") ~ "Citizen science",
      dataset %in% c("LIT") ~ "Literature",
      TRUE ~ "Other"
    )
  )

p <- ggplot(df, aes(x = median_km, y = n_records)) +
  geom_errorbarh(aes(xmin = q25_km, xmax = q75_km), height = 0, linewidth = 0.6, alpha = 0.7) +
  geom_point(aes(shape = group, fill = group), size = 3.2, color = "black", stroke = 0.4) +
  geom_text_repel(
    aes(label = label),
    size = 3.3,
    min.segment.length = 0,
    box.padding = 0.35,
    point.padding = 0.25,
    color = "black",
    segment.color = "white",
    segment.size = 0.4,
    max.overlaps = Inf
  ) +

  scale_y_log10(
    breaks = c(1e3, 5e3, 1e4, 5e4, 1e5, 5e5, 1e6),
    labels = label_number(big.mark = ",")
  ) +
  scale_x_continuous(
    breaks = c(0, 1, 2, 5, 10, 20, 50, 100, 150, 200),
    labels = label_number()
  ) +
  labs(
    x = "Spatial scale (km; median with IQR)",
    y = "Number of records (log scale)",
    shape = NULL, fill = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(face = "bold", size = 13),
    axis.text  = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 11)
  )

p

# Export (nice and crisp for thesis)
ggsave("spatial_scale_schematic.png", p, width = 8.5, height = 5.5, dpi = 400)
ggsave("spatial_scale_schematic.pdf", p, width = 8.5, height = 5.5)

library(extrafont)
font_import(prompt = FALSE)   # run once only, may take a minute
loadfonts()








library(dplyr)
library(tibble)

#--------------------------------------------------
# Helper function
#--------------------------------------------------

schema_row <- function(source, record_unit,
                       x_med, x_q25, x_q75,
                       y_med, y_q25, y_q75,
                       n_units = NA_integer_) {
  
  tibble(
    source = source,
    record_unit = record_unit,
    
    x_med = x_med,
    x_q25 = x_q25,
    x_q75 = x_q75,
    
    y_med = y_med,
    y_q25 = y_q25,
    y_q75 = y_q75,
    
    n_units = n_units
  )
}

#--------------------------------------------------
# DEMERSAL TRAWL
#--------------------------------------------------

dem_schema <- schema_row(
  source = "DEMTRAWL",
  record_unit = "trawl",
  
  x_med = dem_trawl_schema$median_dist_km,
  x_q25 = dem_trawl_schema$median_dist_km - (dem_trawl_schema$IQR_dist/2),
  x_q75 = dem_trawl_schema$median_dist_km + (dem_trawl_schema$IQR_dist/2),
  
  y_med = dem_trawl_schema$median_records,
  y_q25 = dem_trawl_schema$median_records - (dem_trawl_schema$IQR_records/2),
  y_q75 = dem_trawl_schema$median_records + (dem_trawl_schema$IQR_records/2),
  
  n_units = dem_trawl_schema$n_trawls
)

#--------------------------------------------------
# MIDWATER TRAWL (DD / OROP)
#--------------------------------------------------

mwt_dd_schema <- schema_row(
  "MWT_DD", "trawl",
  x_med = 9.37,
  x_q25 = 9.37 - (10.6/2),
  x_q75 = 9.37 + (10.6/2),
  y_med = 3,
  y_q25 = 3 - (2/2),
  y_q75 = 3 + (2/2),
  n_units = 3721
)

mwt_orop_schema <- schema_row(
  "MWT_OROP", "trawl",
  x_med = 14.7,
  x_q25 = 14.7 - (16.8/2),
  x_q75 = 14.7 + (16.8/2),
  y_med = 1,
  y_q25 = 1,
  y_q75 = 1,
  n_units = 3556
)

#--------------------------------------------------
# CAPFISH
#--------------------------------------------------

cap_schema <- capfish_event_summary %>%
  left_join(capfish_event_spacing_summary, by = "Source") %>%
  transmute(
    source = paste0("CAPFISH_", Source),
    record_unit = "set",
    
    x_med = median_event_spacing_km,
    x_q25 = median_event_spacing_km - (IQR_event_spacing_km/2),
    x_q75 = median_event_spacing_km + (IQR_event_spacing_km/2),
    
    y_med = median_records,
    y_q25 = median_records - (IQR_records/2),
    y_q75 = median_records + (IQR_records/2),
    
    n_units = n_events
  )

#--------------------------------------------------
# BRUV
#--------------------------------------------------

bruv_schema <- schema_row(
  source = "BRUV",
  record_unit = "deployment",
  
  x_med = 0.001,
  x_q25 = 0.001,
  x_q75 = 0.001,
  
  y_med = BRUV_event_summary$median_records,
  y_q25 = BRUV_event_summary$median_records - (BRUV_event_summary$IQR_records/2),
  y_q75 = BRUV_event_summary$median_records + (BRUV_event_summary$IQR_records/2),
  
  n_units = BRUV_event_summary$n_events
)

#--------------------------------------------------
# FMDAT METHODS
#--------------------------------------------------

FM_schema <- FM_method_summary %>%
  full_join(FM_method_spatial, by = "MethodNew", suffix = c("_records","_spatial")) %>%
  mutate(
    n_datasets = coalesce(n_datasets_records, n_datasets_spatial)
  ) %>%
  transmute(
    source      = paste0("FM_", MethodNew),
    record_unit = MethodNew,
    
    # spatial axis
    x_med = median_km,
    x_q25 = median_km - (iqr_km / 2),
    x_q75 = median_km + (iqr_km / 2),
    
    # records axis
    y_med = median_records,
    y_q25 = median_records - (IQR_records / 2),
    y_q75 = median_records + (IQR_records / 2),
    
    n_units = as.integer(n_datasets)
  )

#--------------------------------------------------
# MUSEUM + iNAT (occurrence data)
#--------------------------------------------------

museum_schema <- schema_row(
  "MUSEUM_GBIF", "occurrence",
  x_med = 0.01, x_q25 = 0.01, x_q75 = 0.01,
  y_med = 1, y_q25 = 1, y_q75 = 1,
  n_units = nrow(museum_clean)
)

inat_schema <- schema_row(
  "INAT_GBIF", "occurrence",
  x_med = 0.01, x_q25 = 0.01, x_q75 = 0.01,
  y_med = 1, y_q25 = 1, y_q75 = 1,
  n_units = nrow(inat_clean)
)

#--------------------------------------------------
# LINEFISH
#--------------------------------------------------

linefish_schema <- nmls_schema %>%
  transmute(
    source = Source,
    record_unit = "trip-day",
    
    x_med = median_km,
    x_q25 = median_km - (iqr_km/2),
    x_q75 = median_km + (iqr_km/2),
    
    y_med = median_records,
    y_q25 = median_records - (IQR_records/2),
    y_q75 = median_records + (IQR_records/2),
    
    n_units = n_events
  )

#--------------------------------------------------
# FINAL SCHEMA TABLE
#--------------------------------------------------

schema_all <- bind_rows(
  dem_schema,
  mwt_dd_schema,
  mwt_orop_schema,
  cap_schema,
  FM_schema,
  bruv_schema,
  museum_schema,
  inat_schema,
  linefish_schema
) %>%
  mutate(
    x_med = if_else(x_med <= 0 | is.na(x_med), 0.001, x_med),
    x_q25 = if_else(x_q25 <= 0 | is.na(x_q25), x_med, x_q25),
    x_q75 = if_else(x_q75 <= 0 | is.na(x_q75), x_med, x_q75),
    
    y_q25 = pmax(y_q25, 0),
    y_med = pmax(y_med, 0),
    y_q75 = pmax(y_q75, 0)
  )

schema_all

schema_all <- bind_rows(
  dem_schema,
  mwt_dd_schema,
  mwt_orop_schema,
  cap_schema,
  FM_schema,
  bruv_schema,
  museum_schema,
  inat_schema,
  linefish_schema
) %>%
  mutate(
    # log-safe spatial values
    x_med = if_else(is.na(x_med) | x_med <= 0, 0.001, x_med),
    x_q25 = if_else(is.na(x_q25) | x_q25 <= 0, x_med, x_q25),
    x_q75 = if_else(is.na(x_q75) | x_q75 <= 0, x_med, x_q75),
    
    # log-safe records axis
    y_q25 = pmax(y_q25, 0.001),
    y_med = pmax(y_med, 0.001),
    y_q75 = pmax(y_q75, 0.001),
    
    # flag epsilon extents
    x_is_epsilon = x_med == 0.001 & x_q25 == 0.001 & x_q75 == 0.001
  )

print(schema_all, n = Inf)

schema_plot <- schema_all %>%
  mutate(
    group = case_when(
      str_detect(source, "^FM_") ~ "FM datasets",
      str_detect(source, "^CAPFISH_") ~ "CapFish",
      str_detect(source, "^LINEFISH_") ~ "Linefish",
      source %in% c("DEMTRAWL","MWT_DD","MWT_OROP") ~ "Trawl surveys",
      source == "BRUV" ~ "BRUV",
      source %in% c("MUSEUM_GBIF","INAT_GBIF") ~ "GBIF records",
      TRUE ~ "Other"
    ),
    
    # BRUV should not be treated as epsilon
    x_is_epsilon = if_else(source == "BRUV", FALSE, x_is_epsilon)
  )

ggplot(schema_plot, aes(x = x_med, y = y_med)) +
  geom_errorbar(aes(ymin = y_q25, ymax = y_q75),
                width = 0, alpha = 0.6) +
  
  geom_errorbarh(aes(xmin = x_q25, xmax = x_q75),
                 height = 0, alpha = 0.6) +
  
  geom_point(aes(color = group, shape = x_is_epsilon),
             size = 3) +
  
  geom_text(aes(label = source),
            size = 2.6,
            vjust = -0.7,
            check_overlap = TRUE) +
  
  scale_x_log10() +
  scale_y_log10() +
  
  labs(
    x = "Spatial extent of sampling unit (km; median ± IQR)",
    y = "Records per sampling unit (median ± IQR)",
    color = "Data source group",
    shape = "Extent unknown (epsilon)"
  ) +
  
  theme_classic()

# Linefish
linefish_totals <- bind_rows(
  COM_event_effort %>% mutate(source = "LINEFISH_COM"),
  OBS_event_effort %>% mutate(source = "LINEFISH_OBS")
) %>%
  group_by(source) %>%
  summarise(total_records = sum(n_records), .groups = "drop")

# Midwater trawl
mwt_totals <- bind_rows(
  DD_trawl_effort %>% mutate(source = "MWT_DD"),
  OROP_trawl_effort %>% mutate(source = "MWT_OROP")
) %>%
  group_by(source) %>%
  summarise(total_records = sum(n_records), .groups = "drop")

# FM datasets
fm_totals <- FM_method_summary %>%
  transmute(source = paste0("FM_", MethodNew),
            total_records = total_records)

# CapFish
capfish_totals <- capfish_all %>%
  count(Source, name = "total_records") %>%
  transmute(source = paste0("CAPFISH_", Source), total_records)

# Demersal trawl
dem_totals <- tibble(
  source = "DEMTRAWL",
  total_records = nrow(demtrawldat)
)

# BRUV
bruv_totals <- tibble(
  source = "BRUV",
  total_records = nrow(finalBRUV)
)

# GBIF
museum_totals <- tibble(
  source = "MUSEUM_GBIF",
  total_records = nrow(museum_clean)
)

inat_totals <- tibble(
  source = "INAT_GBIF",
  total_records = nrow(inat_clean)
)

totals_all <- bind_rows(
  linefish_totals,
  mwt_totals,
  fm_totals,
  capfish_totals,
  dem_totals,
  bruv_totals,
  museum_totals,
  inat_totals
)


library(ggrepel)



schema_tot <- schema_plot %>%
  select(source, record_unit, x_med, x_q25, x_q75, n_units, x_is_epsilon, group) %>%
  left_join(totals_all, by = "source")

scale_color_manual(
  values = c(
    "BRUV" = "#E64B35",
    "CapFish" = "#C49A00",
    "FM datasets" = "#00A087",
    "GBIF records" = "#4DBBD5",
    "Linefish" = "#3C5488",
    "Trawl surveys" = "#F39B7F"
  )
)

p_scale_contribution <- ggplot(schema_tot, aes(x = x_med, y = total_records)) +
  
  geom_errorbarh(
    aes(xmin = x_q25, xmax = x_q75),
    height = 0,
    alpha = 0.45,
    linewidth = 0.4
  ) +
  
  geom_point(
    aes(color = group, shape = x_is_epsilon),
    size = 3.8,
    stroke = 0.4
  ) +
  
  geom_text_repel(
    aes(label = source),
    size = 3.2,
    box.padding = 0.4,
    family = "serif",
    fontface = "italic",
    point.padding = 0.3,
    segment.color = "grey75",
    max.overlaps = Inf,
    segment.size = 0.3,
    min.segment.length = 0
  ) +
  
  scale_color_manual(
    values = c(
      "BRUV" = "#E64B35",
      "CapFish" = "#C49A00",
      "FM datasets" = "#00A087",
      "GBIF records" = "#4DBBD5",
      "Linefish" = "#3C5488",
      "Trawl surveys" = "#F39B7F"
    )
  ) +
  
  scale_x_log10(
    breaks = c(0.01, 0.1, 1, 10, 100),
    labels = scales::label_number()
  ) +
  
  scale_y_log10(
    labels = scales::label_number(big.mark = ",")
  ) +
  
  labs(
    x = "Spatial extent of sampling unit (km; median ± IQR)",
    y = "Total records contributed",
    color = "Data source group"
  ) +
  
  theme_classic(base_family = "serif") +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "right"
  ) +
  
  guides(shape = "none") +
  annotation_logticks(sides = "b") 


p_scale_contribution


ggsave("sampling_scale_schema.png", p_scale_contribution, width = 9, height = 5)
