#BASED ON MERGE INTO dat_all
getwd()
setwd("/Users/savannahanderson/Desktop/wd/masters")
#following on dat_all
#-------------------------------------------------------------------------------
# LOAD LIBRARIES
#-------------------------------------------------------------------------------
library(ragg)
library(rnaturalearth)
library(ggplot2)
library(sf)
library(dplyr)
library(ggrepel)
library(terra)
#-------------------------------------------------------------------------------
# BASE SPATIAL LAYERS
#------------------------------------------------------------------------------
# South Africa (context)
sa <- ne_countries(country = "South Africa", returnclass = "sf") %>%
  st_transform(4326)
# EEZ
eez <- st_read("MAPPING/EEZ/eez_v12.shp", quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(4326)

eez_sa <- eez %>% filter(TERRITORY1 == "South Africa")
#-------------------------------------------------------------------------------
# DEPTH (FIXED CRS ISSUE)
#-------------------------------------------------------------------------------
depth_rast <- rast("SA_bathymetry_100m_v1/SA_bathymetry_100m_v1.tif")
# Reproject raster to WGS84
depth_rast_4326 <- project(depth_rast, "EPSG:4326")
# Crop + mask to EEZ
depth <- depth_rast_4326 %>%
  crop(vect(eez_sa)) %>%
  mask(vect(eez_sa))
# Clean values
depth[depth > 0] <- NA
depth[depth < -6000] <- -6000
#-------------------------------------------------------------------------------
# BIOREGIONS (9 REGIONS)
#-------------------------------------------------------------------------------
bioregions <- st_read("~/Desktop/NSBA_layer6/biozones.shp") %>%
  st_set_crs(4326)

bioregions_9 <- bioregions %>%
  filter(!grepl("Supratidal", BIOREGION)) %>%
  mutate(region9 = case_when(
    BIOREGION == "Delagoa Bioregion" ~ "Delagoa",
    BIOREGION == "Natal Bioregion" ~ "Natal",
    BIOREGION == "South-western Cape Bioregion" ~ "South-Western Cape",
    BIOREGION == "Agulhas Bioregion" ~ "Agulhas",
    BIOREGION == "Namaqua Bioregion" ~ "Namaqua",
    BIOREGION == "South-west Indian Offshore Bioregion" ~ "South-west Indian Offshore",
    BIOREGION == "Atlantic Offshore Bioregion" ~ "Atlantic Offshore",
    BIOREGION == "Indo-Pacific Offshore Bioregion" ~ "Indo-Pacific Offshore",
    BIOREGION == "West Indian Offshore Bioregion" ~ "West Indian Offshore"
  )) %>%
  filter(!is.na(region9)) %>%
  group_by(region9) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")
#-------------------------------------------------------------------------------
# GRID (20 NM — CORRECT PROJECTION)
#-------------------------------------------------------------------------------
eez_proj <- st_transform(eez_sa, 32735)

cellsize_m <- 20 * 1852

grid <- st_make_grid(
  eez_proj,
  cellsize = cellsize_m,
  square = TRUE
)

grid_sa <- st_sf(
  grid_id = seq_along(grid),
  geometry = grid
) %>%
  st_intersection(eez_proj) %>%
  st_transform(4326)
#-------------------------------------------------------------------------------
# CONVERT DATA TO SF
#-------------------------------------------------------------------------------
dat_sf <- dat_all %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  mutate(
    lon_orig = longitude,
    lat_orig = latitude
  ) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

eez_sa      <- st_transform(eez_sa, 4326)
bioregions_9 <- st_transform(bioregions_9, 4326)
grid_sa     <- st_transform(grid_sa, 4326)
#-------------------------------------------------------------------------------
# SPATIAL JOINS
#-------------------------------------------------------------------------------
dat_sf <- st_join(dat_sf, eez_sa, left = TRUE)
dat_sf <- st_join(dat_sf, bioregions_9[, "region9"])
dat_sf <- st_join(dat_sf, grid_sa[, "grid_id"])
#-------------------------------------------------------------------------------
# ADD DEPTH
#-------------------------------------------------------------------------------
coords <- st_coordinates(dat_sf)
depth_vals <- terra::extract(depth, coords)
dat_sf$depth <- depth_vals[,1]
# Keep marine only
dat_sf <- dat_sf %>%
  filter(!is.na(depth), depth <= 0)
#-------------------------------------------------------------------------------
# FINAL DATASET
#-------------------------------------------------------------------------------
final_dat <- dat_sf %>%
  # keep only required columns
  dplyr::select(
    source,
    species,
    genus,
    family,
    order,
    method,
    datatype,
    taxonomic_resolution,
    year,
    coast,
    region9,
    grid_id,
    lon_orig,
    lat_orig,
    depth,
    geometry
  ) %>%
  # rename coordinates
  dplyr::rename(
    longitude = lon_orig,
    latitude  = lat_orig
  ) %>%
  # remove invalid spatial records
  dplyr::filter(
    !is.na(grid_id),
    !is.na(region9)
  ) %>%
  # clean variables
  dplyr::mutate(
    depth = abs(depth),                 # convert depth to positive
    grid_id = as.integer(grid_id),
    # define depth zone
    depth_zone = ifelse(depth > 200, "Offshore", "Inshore")
  )

#LASTLY CREATE GEAR_GROUPED COLUMN

final_dat %>%
  sf::st_drop_geometry() %>%
  dplyr::count(source, method, sort = TRUE) 


#    SOURCE                Method                n.           Gear Specific                        Gear grouped
#    DEM_TRAWL             Demersal trawl        100571       Demersal trawl offshore and inshoren   Bottom Trawl
#    LINEFISH              LINE                  783663       Shore and boat angling                 Angling
#    LINEFISH              POLE                  33320        Pole                                  Angling
#    LINEFISH              SHORTHAND_ROD         52           Shorthand rod                         Angling
#    LINEFISH              SPEAR                 49           Spear                             Spear
#    LINEFISH              SHORT_SPEAR            6           Spear                                 Spear
#    BRUV                  BRUV survey           20164        BRUV                                    UVC/Visual
#    MUSEUM                PRESERVED_SPECIMEN    19173        Specimen (No gear)                    NA
#    CAPFISH               SAHLLA                18125        Longline                             longline
#    CAPFISH               SECIFA                16782        Inshore demersal trawl                Bottom Trawl
#    CAPFISH               SAPFIA                16518        Purse seine (small pelagic)        Seine
#    CAPFISH               SADSTIA               75479        Offshore demersal trawl             Bottom Trawl
#    MW_TRAWL              Midwater trawl        13459        Midwater trawl                     Midwater Trawl
#    INAT                  HUMAN_OBSERVATION     6387         Citizen science                   UVC/Visual
#    LITERATURE            UVC                    940        UVC                                     UVC/Visual
#    LITERATURE            ShoreAngling           326         Shore angling                           Angling  
#    LITERATURE            BRUV                    261        BRUV reef                                   UVC/Visual
#    LITERATURE            GillNet                229         Gill net estuarine                    Gill net
#    LITERATURE            PlanktonNet            227         Plankton net surface and deep       Plankton net
#    LITERATURE            SeineNet             206           Beach and estuarine seine net         Seine
#    LITERATURE            BoatAngling           136          Boat angling                        Angling 
#    LITERATURE            Chemical             118           Chemical                          Chemical
#    LITERATURE            Trawl                38            Trawl mixed                       Trawl
#    LITERATURE            Records             33             No gear                         NA
#    LITERATURE            Mixed               28             No gear                          NA
#    LITERATURE            Angling              9             Shore and boat angling                         Angling           
#    LITERATURE            BeamTrawl              6           Trawl Mixed estuarine                         Trawl

gear_lookup <- tibble::tribble(
  ~source, ~method, ~gear_specific, ~gear_grouped,
  
  # DEMERSAL TRAWL
  "DEM_TRAWL", "Demersal trawl", "Demersal trawl (offshore & inshore)", "Bottom trawl",
  # LINEFISH
  "LINEFISH", "LINE", "Shore and boat angling", "Angling",
  "LINEFISH", "POLE", "Pole", "Angling",
  "LINEFISH", "SHORTHAND_ROD", "Shorthand rod", "Angling",
  "LINEFISH", "SPEAR", "Spear", "Spear",
  "LINEFISH", "SHORT_SPEAR", "Spear", "Spear",
  # BRUV
  "BRUV", "BRUV survey", "BRUV", "UVC/Visual",
  # MUSEUM
  "MUSEUM", "PRESERVED_SPECIMEN", "Specimen (no gear)", NA_character_,
  # CAPFISH
  "CAPFISH", "SAHLLA", "Longline", "Longline",
  "CAPFISH", "SECIFA", "Inshore demersal trawl", "Bottom trawl",
  "CAPFISH", "SAPFIA", "Purse seine (small pelagic)", "Seine",
  "CAPFISH", "SADSTIA", "Offshore demersal trawl", "Bottom trawl",
  # MIDWATER
  "MW_TRAWL", "Midwater trawl", "Midwater trawl", "Midwater trawl",
  # INAT
  "INAT", "HUMAN_OBSERVATION", "Citizen science", "UVC/Visual",
  # LITERATURE
  "LITERATURE", "UVC", "UVC", "UVC/Visual",
  "LITERATURE", "ShoreAngling", "Shore angling", "Angling",
  "LITERATURE", "BRUV", "BRUV reef", "UVC/Visual",
  "LITERATURE", "GillNet", "Gill net (estuarine)", "Gill net",
  "LITERATURE", "PlanktonNet", "Plankton net (surface & deep)", "Plankton net",
  "LITERATURE", "SeineNet", "Beach/estuarine seine", "Seine",
  "LITERATURE", "BoatAngling", "Boat angling", "Angling",
  "LITERATURE", "Chemical", "Chemical", "Chemical",
  "LITERATURE", "Trawl", "Trawl (mixed)", "Trawl",
  "LITERATURE", "Records", "No gear", NA_character_,
  "LITERATURE", "Mixed", "No gear", NA_character_,
  "LITERATURE", "Angling", "Shore and boat angling", "Angling",
  "LITERATURE", "BeamTrawl", "Trawl (estuarine)", "Trawl"
)

final_dat <- final_dat %>%
  dplyr::left_join(gear_lookup, by = c("source", "method"))

readr::write_csv(sf::st_drop_geometry(final_dat),"final_dat.csv")
readr::write_csv(dat_species, "dat_species.csv")
readr::write_csv(dat_all, "dat_raw.csv")

#-------------------------------------------------------------------------------
# CHECKS
#-------------------------------------------------------------------------------
summary(final_dat$depth)
table(final_dat$region9)
table(is.na(final_dat$grid_id))
table(final_dat$in_mpa)

#-------------------------------------------------------------------------------
#PLOTS
#-------------------------------------------------------------------------------
#1. depth
par(
  family = "serif",
  cex = 1.1,        # overall text size
  cex.axis = 1,     # axis tick labels
  cex.lab = 1.2,    # axis titles
  cex.main = 1.3    # main title
)

p_depth <- plot(depth)
agg_png("p_depth.png", width = 8, height = 6, units = "in", res = 600)
par(family = "serif")
plot(depth)
dev.off()

#2. bioregions
p_bioregions <- ggplot() +
  geom_sf(data = bioregions_9, aes(fill = region9), color = "black", size = 0.3) +
  scale_fill_brewer(palette = "Set3") +
  coord_sf(expand = FALSE) +
  theme_classic(base_family = "serif") +
  theme(
    panel.background = element_rect(fill = "#dbeaf2"),
    legend.position = "none", 
    axis.title = element_blank()
  )

ggsave("p_bioregions.png", p_bioregions,
       width = 8, height = 6, dpi = 600)
#3. study area (eez and grid with sa map)
p_study <- ggplot() +
  geom_sf(data = sa, fill = "grey95", colour = "black", linewidth = 0.3) +
  geom_sf(data = eez_sa, fill = NA, colour = "red", linewidth = 0.6) +
  geom_sf(data = grid_sa, fill = NA, colour = "grey20", linewidth = 0.1) +
  coord_sf(xlim = c(12, 37), ylim = c(-39, -27), expand = FALSE) +
  labs(
    x = "Longitude (°E)",
    y = "Latitude (°S)"
  ) +
  theme_classic(base_family = "Times") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
    plot.margin = margin(10, 10, 10, 10)
  )

p_study

ggsave("p_study.png", p_study,
       width = 8, height = 6, dpi = 600)

#4.mpa map
p_mpa <- ggplot() +
geom_sf(
  data = sa,
  fill = "grey90",
  colour = "black",
  linewidth = 0.3
) +
  geom_sf(
    data = eez_sa,
    fill = "#e8f2f7",
    colour = "black",
    linewidth = 0.5
  ) +
  geom_sf(
    data = mpas_sa,
    aes(fill = TYPE),
    colour = NA,
    alpha = 0.85
  ) +
  scale_fill_manual(
    values = c("Marine Protected Area" = "#1b7837"),
    name = NULL
  ) +
  coord_sf(
    xlim = c(12, 37),
    ylim = c(-39, -27),
    expand = FALSE
  ) +
  labs(
    x = "Longitude (°E)",
    y = "Latitude (°S)"
  ) +
  theme_classic(base_family = "Times", base_size = 12) +
theme(
  legend.position = c(0.85, 0.2),
  legend.background = element_rect(fill = "white", colour = "black"),
  legend.text = element_text(size = 10),
  panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6),
  plot.margin = margin(10, 15, 10, 10)
)

p_mpa

ggsave("p_mpa.png", p_mpa,
       width = 8, height = 6, dpi = 600)