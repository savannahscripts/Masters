
## 5th script in FM DATA series
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

summary(FMdat)


# ------------------------------------------------------------------------------
## Step 1: Species richness MPAs
# ------------------------------------------------------------------------------

# Load and clean MPA shapefile
mpas <- st_read("/Users/savannahanderson/Desktop/wd/masters/SAMPAZ/SAMPAZ_OR_2024_Q3.shp", quiet = TRUE) %>%
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  filter(!st_is_empty(.)) %>%
  filter(!is.na(st_is_valid(.))) %>%
  st_make_valid()

# Calculate species richness per site
species_richness <- FMdat %>%
  filter(!is.na(Longitude), !is.na(Latitude)) %>%
  group_by(Longitude, Latitude) %>%
  summarise(n_species = n_distinct(Species), .groups = "drop")


# Convert to spatial object
species_richness_sf <- st_as_sf(species_richness, coords = c("Longitude", "Latitude"), crs = 4326)

species_richness_sf <- st_transform(species_richness_sf, crs = st_crs(mpas))

# Determine which points fall inside MPAs
species_richness_sf <- species_richness_sf %>%
  mutate(in_mpa = lengths(st_within(geometry, mpas)) > 0)

# Threshold for top 25% richest sites
threshold <- quantile(species_richness_sf$n_species, 0.75)

# Subset for high richness sites
high_richness <- species_richness_sf %>%
  filter(n_species >= threshold)

# Save unprotected high-richness sites
st_drop_geometry(high_richness %>% filter(!in_mpa)) %>%
  write.csv("unprotected_hotspots.csv", row.names = FALSE)

# Assign coast based on longitude
species_richness_sf <- species_richness_sf %>%
  mutate(Longitude = st_coordinates(.)[, 1],
         Latitude  = st_coordinates(.)[, 2]) %>%
  mutate(Coast = case_when(
    Longitude <= 19 ~ "West Coast",
    Longitude > 19 & Longitude < 26 ~ "South Coast",
    Longitude >= 26 ~ "East Coast"
  )) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Classify unprotected hotspots
high_unprotected <- species_richness_sf %>%
  filter(n_species >= threshold & !in_mpa)

species_richness_sf <- species_richness_sf %>%
  mutate(Highlight = ifelse(st_equals(geometry, high_unprotected$geometry),
                            "Unprotected hotspot", "Other"))

# Define bounding boxes for coastlines
bbox_west  <- c(xmin = 14, xmax = 20, ymin = -35.5, ymax = -28)
bbox_south <- c(xmin = 19, xmax = 27, ymin = -36.5, ymax = -33)
bbox_east  <- c(xmin = 26, xmax = 35, ymin = -35, ymax = -26)

# Filter by coast
west  <- species_richness_sf %>% filter(Coast == "West Coast")
south <- species_richness_sf %>% filter(Coast == "South Coast")
east  <- species_richness_sf %>% filter(Coast == "East Coast")

# Define function to generate coastal maps
plot_coast <- function(data, bbox, title) {
  ggplot() +
    geom_sf(data = sa_map, fill = "grey95", color = "black") +
    geom_sf(data = mpas, fill = "lightgreen", color = "darkgreen", alpha = 0.3) +
    geom_sf(data = data, aes(size = n_species, fill = Highlight), shape = 21, color = "black", alpha = 0.8) +
    scale_fill_manual(values = c("Unprotected hotspot" = "red", "Other" = "darkblue")) +
    scale_size_continuous(range = c(1, 6)) +
    coord_sf(xlim = bbox[c("xmin", "xmax")], ylim = bbox[c("ymin", "ymax")]) +
    labs(title = title, fill = "Sampling Site Type", size = "Species Richness") +
    theme_minimal(base_size = 11, base_family = "Times") +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.position = "right"
    )
}


library(ggrepel)
# Generate plots
west_plot <- plot_coast(west, bbox_west, "West Coast")
south_plot <- plot_coast(south, bbox_south, "South Coast")
east_plot <- plot_coast(east, bbox_east, "East Coast")


west_plot
south_plot
east_plot


# ------------------------------------------------------------------------------
## Step 2: Identify unprotected sites
# ------------------------------------------------------------------------------

FMdat_sf <- FMdat %>%
  filter(!is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Ensure matching CRS
high_unprotected <- st_transform(high_unprotected, crs = 4326)
FMdat_sf <- st_transform(FMdat_sf, crs = 4326)

# Perform spatial join to match sampling locations
unprotected_sites_detailed <- st_join(
  high_unprotected, 
  FMdat_sf %>% select(SamplingLocationID, SamplingLocation), 
  join = st_equals
)

# Extract the unique list with richness
unprotected_sites_list <- unprotected_sites_detailed %>%
  st_drop_geometry() %>%
  distinct(SamplingLocationID, SamplingLocation, n_species) %>%
  arrange(desc(n_species))

# (Optional) View or save
print(unprotected_sites_list, n = Inf)
write.csv(unprotected_sites_list, "unprotected_sites_with_richness25%.csv", row.names = FALSE)


table(high_richness$in_mpa)
prop.table(table(high_richness$in_mpa))
table(species_richness_sf$in_mpa)
prop.table(table(species_richness_sf$in_mpa))

# out of most species-rich sites (top 25% richest locations):
# 33 sites (75%) are outside MPAs
# 11 sites (25%) are inside MPAs

#So in entire dataset of sampling sites:
# 120 sites (75%) are outside MPAs
# 40 sites (25%) are inside MPAs


#When changed threshold to 10% most speciose:
#Of the top 10% most species-rich sampling sites (n = 16), 
#only 25% (n = 4) were located within South Africa’s Marine Protected Areas (MPAs). 
#Similarly, across all sampling sites (n = 160), only 25% (n = 40) fell within MPA boundaries. 
#This consistent pattern highlights a substantial mismatch between current MPA coverage 
#and areas of highest marine fish biodiversity.


# ------------------------------------------------------------------------------
## Step 3: Test the statistical significance
# ------------------------------------------------------------------------------


threshold_75 <- quantile(species_richness_sf$n_species, 0.75)

# Label high richness sites
species_richness_sf <- species_richness_sf %>%
  mutate(is_high = n_species >= threshold_75)

# Create contingency table
table_75 <- table(species_richness_sf$is_high, species_richness_sf$in_mpa)

# Run Fisher's Exact Test
fisher.test(table_75)

