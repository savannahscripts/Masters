#TOTAL OBS

#final_dat = 1108096
# -------- final_dat -------- 

source_summary_final <- final_dat %>%
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
#LINEFISH      817090       167        73.7 
# CAPFISH       126929       130        11.5 
# DEM_TRAWL     100577       142         9.08
#BRUV           20164       625         1.82
#MUSEUM         19873       1742        1.79
# MW_TRAWL       13459        43        1.21
# INAT            6804       621         0.61
# LITERATURE      3190       512         0.29
