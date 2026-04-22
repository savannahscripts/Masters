#TOTAL OBS
#dat_all = 1135983
#dat_species = 1022750

##SOURCE SPECIFIC CONTRIBUTIONS OF RECORDS

# -------- dat_all --------

source_summary <- dat_all %>%
  group_by(source) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species),
    .groups = "drop"
  ) %>%
  mutate(
    perc_records = round(100 * n_records / sum(n_records), 2)
  ) %>%
  arrange(desc(n_records))

#source     n_records n_species perc_records
# LINEFISH      822211       168        72.4 
# CAPFISH       128971       131        11.4 
# DEM_TRAWL     101756       143         8.96
# MUSEUM         32835      1953         2.89
# BRUV           20164       625         1.78
# MW_TRAWL       13481        43         1.19
# INAT           10101       695         0.89
# LITERATURE      6464       628         0.57

822211 + 128971 + 101756 + 32835 + 20164 + 13481 + 10101 +6464 
#1135983 total records (unfiltered)

# -------- dat_species --------
source_summary <- dat_species %>%
  group_by(source) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species),
    .groups = "drop"
  ) %>%
  mutate(
    perc_records = round(100 * n_records / sum(n_records), 2)
  ) %>%
  arrange(desc(n_records))

# source n_records  perc
#source     n_records n_species perc_records
# LINEFISH      714528       145        69.9 
# CAPFISH       125394       113        12.3 
# DEM_TRAWL     101527       133         9.93
# MUSEUM         32834      1952         3.21
# BRUV           19695       514         1.93
# MW_TRAWL       12639        38         1.24
# INAT           10101       695         0.99
# LITERATURE      6032       514         0.59

# -------- final_dat -------- Intersected with eez

source_summary <- final_dat %>%
  group_by(source) %>%
  summarise(
    n_records = n(),
    n_species = n_distinct(species),
    .groups = "drop"
  ) %>%
  mutate(
    perc_records = round(100 * n_records / sum(n_records), 2)
  ) %>%
  arrange(desc(n_records))

#source     n_records n_species perc_records
#LINEFISH      817090       167        73.9 
# CAPFISH       126929       130        11.5 
# DEM_TRAWL     100577       142         9.09
#BRUV           20164       625         1.82
#MUSEUM         19173      1728         1.73
# MW_TRAWL       13459        43         1.22
# INAT            6387       606         0.58
# LITERATURE      2557       464         0.23
