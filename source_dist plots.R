
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis) 
library(ggspatial)
library(lubridate)
library(purrr)
library(tidyr)
library(stringr)
library(scales)
library(readr)
library(janitor)
library(patchwork)

setwd("/Users/savannahanderson/Desktop/wd/masters")

master_dat_march <- read.csv("~/Desktop/wd/masters/RESULTSPLOTS_NOVEMBER/masterdat_final.csv")

sa_map <- st_read("MAPPING/SA_COAST/sa_map.shp", quiet = TRUE)

eez <- st_read("MAPPING/EEZ/eez_v12.shp", quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(4326)

eez_sa <- eez %>% filter(SOVEREIGN1 == "South Africa")

# Convert master data to sf
masterdat_sf <- master_dat_march %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  mutate(
    longitude = as.numeric(longitude),
    latitude  = as.numeric(latitude)
  ) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# Remove land points
is_on_land <- st_intersects(masterdat_sf, sa_map, sparse = FALSE)[, 1]
masterdat_marine <- masterdat_sf[!is_on_land, ]

# Keep only points inside EEZ
in_eez <- st_intersects(masterdat_marine, eez_sa, sparse = FALSE)
masterdat_eez <- masterdat_marine[rowSums(in_eez) > 0, ]

# Now build plotting data from EEZ-filtered points
plot_dat <- masterdat_eez %>%
  st_drop_geometry() %>%
  transmute(
    longitude = longitude,
    latitude  = latitude,
    source    = as.character(source)
  )


# Recode sources for figure
plot_dat <- plot_dat %>%
  mutate(source = recode(source,
                         "CAPFISH"   = "CapMarine (O)",
                         "INAT"      = "iNaturalist (S)",
                         "MW_TRAWL"  = "MW Trawl (O)",
                         "MUSEUM"    = "Museum (S)",
                         "LITERATURE"= "Literature (S,C,O)",
                         "DEM_TRAWL" = "Demersal trawl (S)",
                         "BRUV"      = "BRUV (S)",
                         "LINEFISH"  = "Angling (C,O)"
  ))


# ----------------------------------------------------------
# Plot function with density overlay
# ----------------------------------------------------------

plot_source_map <- function(src){
  
  dat <- plot_dat %>% filter(source == src)
  
  ggplot() +
    
    geom_sf(data = sa_map,
            fill = "grey92",
            colour = "grey40",
            linewidth = 0.2) +
    
    # sampling points
    geom_point(
      data = dat,
      aes(x = longitude, y = latitude),
      size = 0.35,
      alpha = 0.35,
      colour = "#1f3b82"
    ) +
    
    coord_sf(
      xlim = c(10,35),
      ylim = c(-40,-25),
      expand = FALSE
    ) +
    
    labs(title = src,
         x = "Longitude",
         y = "Latitude") +
    
    theme_classic(base_family = "serif", base_size = 9) +
    
    theme(
      plot.title = element_text(
        size = 10,
        face = "bold",
        hjust = 0.5
      ),
      plot.margin = margin(2,4,2,4)
    )
}

# ----------------------------------------------------------
# Build panels
# ----------------------------------------------------------

pA <- plot_source_map("CapMarine (O)")
pB <- plot_source_map("iNaturalist (S)")
pC <- plot_source_map("MW Trawl (O)")
pD <- plot_source_map("Museum (S)")
pE <- plot_source_map("Literature (S,C,O)")
pF <- plot_source_map("Demersal trawl (S)")
pG <- plot_source_map("BRUV (S)")
pH <- plot_source_map("Angling (C,O)")

# ----------------------------------------------------------
# Assemble layout
# ----------------------------------------------------------

p_sources <- (pA | pB) /
  (pC | pD) /
  (pE | pF) /
  (pG | pH) +
  plot_annotation(
    title = "Distribution of marine teleost occurrence records by data source",
    tag_levels = "A",
    theme = theme(
      plot.title = element_text(
        size = 13,
        face = "bold",
        hjust = 0.5,
        margin = margin(b = 6)
      )
    )
  )

# ----------------------------------------------------------
# Save figure
# ----------------------------------------------------------

ggsave(
  "sampling_distribution_by_source.png",
  p_sources,
  width = 7.2,
  height = 8.8,
  units = "in"
)



















##OLD BELOW
theme_map_panel <- theme_classic(base_family = base_family, base_size = 10) +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0),
    axis.title = element_text(size = 9),
    axis.text  = element_text(size = 8),
    axis.ticks = element_line(linewidth = 0.25),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    plot.margin = margin(3, 3, 3, 3)
  )

make_panel <- function(df, panel_title) {
  ggplot() +
    geom_sf(data = sa_map, fill = "grey90", color = "black", linewidth = 0.2) +
    geom_sf(data = eez, fill = NA, color = "darkgreen", linewidth = 0.25) +
    geom_point(
      data = df,
      aes(x = longitude, y = latitude),
      colour = "navy",
      alpha = 0.6,
      size = 0.2
    ) +
    coord_sf(xlim = c(10, 35), ylim = c(-40, -25), expand = FALSE) +
    labs(title = panel_title, x = "Longitude", y = "Latitude") +
    theme_map_panel +
    theme(
      plot.title = element_text(size = 8),
      axis.title = element_text(size = 7),
      axis.text  = element_text(size = 6)
    )
}


plotA <- make_panel(filter(plot_dat, source == "CapMarine"),           "CapMarine (O)")
plotB <- make_panel(filter(plot_dat, source == "iNaturalist"),         "iNaturalist (S)")
plotC <- make_panel(filter(plot_dat, source == "Midwater trawl (obs)"),"MW Trawl (O)")
plotD <- make_panel(filter(plot_dat, source == "Museum specimens"),    "Museum (S)")
plotE <- make_panel(filter(plot_dat, source == "Literature"),          "Literature (S,C,O)")
plotF <- make_panel(filter(plot_dat, source == "Demersal trawl"),      "Demersal trawl (S)")
plotG <- make_panel(filter(plot_dat, source == "BRUV"),                "BRUV (S)")
plotH <- make_panel(filter(plot_dat, source == "NMLS angling"),       "Angling (C,O)")

drop_x <- theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.x = element_blank())

drop_y <- theme(axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank())



pA2 <- plotA + drop_x
pB2 <- plotB + drop_x + drop_y


pC2 <- plotC + drop_x
pD2 <- plotD + drop_x + drop_y

pE2 <- plotE + drop_x
pF2 <- plotF + drop_x + drop_y

pG2 <- plotG
pH2 <- plotH + drop_y

library(patchwork)

p_sources <- (
  pA | pB
) /
  (
    pC | pD
  ) /
  (
    pE | pF
  ) /
  (
    pG | pH
  ) +
  plot_layout(
    guides = "collect",
    heights = c(1,1,1,1)
  ) +
  plot_annotation(
    title = "Distribution of marine teleost occurrence records by data source",
    theme = theme(
      plot.title = element_text(
        family = "serif",
        size = 12,
        face = "bold",
        hjust = 0.5,
        margin = margin(b = 6)
      )
    )
  ) &
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5, family = base_family),
    plot.tag   = element_text(size = 8, face = "bold", family = base_family),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold", family = base_family),
    legend.text  = element_text(size = 9, family = base_family)
  )

combined

ggsave(
  "figure_sampling_sources.pdf",
  plot = combined,
  width = 16.5,
  height = 21,
  units = "cm"
)
