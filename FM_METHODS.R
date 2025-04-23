## 4th script in FM DATA series
# author: "Savannah Anderson"
# date: "2025-04-07"


#wd
setwd("/Users/savannahanderson/Desktop/wd/masters")
getwd()

#install packages
install.packages(c("ggplot2", "sf", "rnaturalearth", "rnaturalearthdata", "viridis"))
##load libraries
library(readxl)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis) 
library(ggspatial)
library(dplyr)


#Read in data from cleaned excel file
FMdat <- read_excel("final_FM.xlsx")
summary(FMdat)

# ------------------------------------------------------------------------------
## Step 1: Understand distribution of count metrics in records
# ------------------------------------------------------------------------------

FMdat <- FMdat %>%
  mutate(
    Count_Metric = case_when(
      AbsoluteCounts == 1 ~ "Absolute Counts",
      MaxN == 1 ~ "Max N",
      ProportionalAbundance == 1 ~ "Proportional Abundance",
      CPUE == 1 ~ "CPUE",
      PresenceAbsence == 1 ~ "Presence Absence",
      TRUE ~ "Unknown"  # If none of the labels are 1
    )
  )

table(FMdat$Count_Metric)

#AbsoluteCounts:3391 
#CPUE:743 
#MaxN:264 
#PresenceAbsence:633 
#ProportionalAbundance:1190 
#Unknown:93 
                                                                        

# Summarise counts
count_metric_summary <- FMdat %>%
  group_by(Count_Metric) %>%
  summarise(n_samples = n()) %>%
  arrange(desc(n_samples))

# Plot counts to get a visual representation of spread
ggplot(count_metric_summary, aes(x = reorder(Count_Metric, -n_samples), y = n_samples)) +
  geom_col(fill = "grey60", color = "black", width = 0.7) +
  geom_text(aes(label = n_samples), vjust = -0.5, size = 3, family = "Times") +
  labs(
    title = "Number of Records per Count Metric for Research Data",
    x = "Count Metric",
    y = "Number of Records"
  ) +
  theme_classic(base_family = "Times") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.7),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10), size = 11),
    axis.title.x = element_text(margin = margin(t = 10), size = 11),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )

# ------------------------------------------------------------------------------
## Step 2: Remove unknowns and remove higher order. classifications of taxonomy
# ------------------------------------------------------------------------------
#unknown metric makes up less than 1.5% of samples (remove)
#back to taxonomic resolution, keep only species level
#species level makes up >96% of data so fine to exclude others

FMdat <- FMdat %>%
  filter(
    Count_Metric != "Unknown",
    TaxonomicResolution == "Species-level"
  )

#went from 6314 to 5773 data points lost about 8.6% of data

#AbsoluteCounts:3107
#CPUE:723
#MaxN:247
#PresenceAbsence:600
#ProportionalAbundance:1096

#(no need to replot bar graph)

# ------------------------------------------------------------------------------
## Step 3: Look at spread of count metrics within different methods
# ------------------------------------------------------------------------------
#
#method types
unique(FMdat$Method)

#records per method
FMdat %>%
  count(Method, sort = TRUE)

FMdat %>%
  count(Method, Count_Metric) %>%
  arrange(Method)


# Define colour palette for count metrics
metric_colors <- c(
  "Absolute Counts" = "darkcyan",
  "CPUE" = "lightblue",
  "Max N" = "turquoise",
  "Presence Absence" = "deepskyblue3",
  "Proportional Abundance" = "navy"
)

# Summarise number of unique species per method and count metric
species_by_method <- FMdat %>%
  group_by(Method, Count_Metric) %>%
  summarise(n_species = n_distinct(Species), .groups = "drop")

# Sort methods by total species richness
species_by_method <- species_by_method %>%
  group_by(Method) %>%
  mutate(total = sum(n_species)) %>%
  ungroup() %>%
  mutate(Method = factor(Method, levels = unique(Method[order(-total)])))

#Rename Labels before Plotting
species_by_method <- species_by_method %>%
  mutate(Method = recode(Method,
                         "Other (Records/Personal Communication)" = "Records/Personal Communication",
                         "Net (Unspecified)" = "Net"
  ))

# Plot
ggplot(species_by_method, aes(x = Method, y = n_species, fill = Count_Metric)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = metric_colors, drop = FALSE) +
  labs(
    title = "Species Richness by Sampling Method and Count Metric",
    x = "Sampling Method",
    y = "Number of Unique Species",
    fill = "Count Metric"
  ) +
  theme_classic(base_size = 11, base_family = "Times") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8)
  )

# ------------------------------------------------------------------------------
## Step 4: Mapping methods in Multifaceted plot
# ------------------------------------------------------------------------------
#

# Recode method names in FMdat before filtering
FMdat <- FMdat %>%
  mutate(Method = recode(Method,
                         "Other (Records/Personal Communication)" = "Records/Personal Comms",
                         "Net (Unspecified)" = "Net"))

# Now define the updated method list
methods_to_map <- c(
  "BRUV", "Angling", "UVC", "Seine Net", "Net", "Gill Net",
  "Chemical Sampling", "Trawl", 
  "Trap", "Dead Fish Collection", "Multiple", "Records/Personal Comms"
)

# Filter and plot
FM_mapdata <- FMdat %>%
  filter(Method %in% methods_to_map, !is.na(Latitude), !is.na(Longitude))

sa_map <- ne_countries(scale = "medium", country = "South Africa", returnclass = "sf")

ggplot() +
  geom_sf(data = sa_map, fill = "grey95", color = "black") +
  geom_point(data = FM_mapdata, aes(x = Longitude, y = Latitude), 
             alpha = 0.6, size = 1.5, color = "steelblue") +
  facet_wrap(~ Method, ncol = 3) +
  coord_sf(xlim = c(14, 35), ylim = c(-37, -26), expand = FALSE) +
  labs(
    title = "Spatial Coverage of Sampling Methods",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_family = "Times") +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )


# ------------------------------------------------------------------------------
## Step 5: Mapping methods in single plot
# ------------------------------------------------------------------------------
#
library(stringr)
FMdat <- FMdat %>%
  mutate(Method = recode(Method,
                         "Other (Records/Personal Communication)" = "Records/Personal Comms",
                         "Net (Unspecified)" = "Net"
  ))
# Now define the updated method list
methods_to_map <- c(
  "BRUV", "Angling", "UVC", "Seine Net", "Net", "Gill Net",
  "Chemical Sampling", "Trawl", 
  "Trap", "Dead Fish Collection", "Multiple", "Records/Personal Comms"
)

# Filter for valid coordinates and selected methods
FM_mapdata <- FMdat %>%
  filter(Method %in% methods_to_map, !is.na(Latitude), !is.na(Longitude))

#exclude inland
FM_mapdata_marine <- FM_mapdata %>%
  filter(
    Latitude < -26 & Latitude > -38,     # South Africa’s coastline band
    Longitude > 15 & Longitude < 35      # Avoid inland/misaligned points
  )

# Get South Africa basemap
sa_map <- ne_countries(scale = "medium", country = "South Africa", returnclass = "sf")

# Load clean EEZ shapefile
sa_eez <- st_read("/Users/savannahanderson/Desktop/wd/masters/EEZ/eez_v12.shp", quiet = TRUE)

# Clean invalid geometries
sa_eez <- st_make_valid(sa_eez)

# Optionally keep only polygons or multipolygons
sa_eez <- sa_eez[st_geometry_type(sa_eez) %in% c("POLYGON", "MULTIPOLYGON"), ]
# Filter EEZ shapefile to only include South Africa
sa_eez <- sa_eez %>%
  filter(str_detect(TERRITORY1, "South Africa") | str_detect(SOVEREIGN1, "South Africa"))

# Convert to spatial points and keep only points within the EEZ
FM_mapdata_sf <- st_as_sf(FM_mapdata, coords = c("Longitude", "Latitude"), crs = 4326)
# Spatial join: retain only marine points within South Africa's EEZ
FM_mapdata_marine <- st_join(FM_mapdata_sf, sa_eez, join = st_within)

# Plot all methods in one map
ggplot() +
  geom_sf(data = sa_eez, fill = "grey95", color = "black") +
  geom_sf(data = FM_mapdata_marine, aes(color = Method), size = 1.6, alpha = 0.7) +
  scale_color_brewer(palette = "Paired") +
  coord_sf(xlim = c(14, 35), ylim = c(-37, -26), expand = FALSE) +
  labs(
    title = "Marine Sampling Locations by Method",
    x = "Longitude",
    y = "Latitude",
    color = "Sampling Method"
  ) +
  theme_minimal(base_size = 11, base_family = "Times") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )



