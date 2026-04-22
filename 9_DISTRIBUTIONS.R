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
library(writexl)
library(lubridate)
library(purrr)
library(tidyr)
library(stringr)
library(scales)
library(readr)
library(janitor)
library(patchwork)

master_dat_march <- read.csv("~/Desktop/wd/masters/RESULTSPLOTS_NOVEMBER/masterdat_final_march.csv")


#SPATIAL FILES
world <- ne_countries(scale = "medium", returnclass = "sf")
sa_map <- world %>% filter(admin == "South Africa")

species_list <- c(
  "Lithognathus lithognathus", #
  "Petrus rupestris", #
  "Austroglossus pectoralis", #
  "Polysteganus undulosus", #
  "Chrysoblephus gibbiceps", #
  "Argyrosomus inodorus", #
  "Cheilodactylus fasciatus", #
  "Clinus cottoides", #
  "Chorisochismus dentex", #
  "Spondyliosoma emarginatum", #
  "Sparodon durbanensis",
  "Pachymetopon aeneum",
  "Zeus faber",
  "Caranx ignobilis",
  "Gymnothorax favagineus",
  "Thalassoma lunare",
  "Pomacanthus imperator",
  "Epinephelus andersoni",
  "Chaetodon marleyi",
  "Cheilodipterus quinquelineatus",
  "Chelidonichthys queketti",
  "Cookeolus japonicus",
  "Sufflogobius bibarbatus",
  "Pristipomoides argyrogrammicus"
)
#_________________________________________________________________________________
#COMPARE RANGES OR PREDICTED  RANGES
my_ranges <- tibble::tribble(
  ~species, ~source, ~lon_min, ~lon_max, ~lat_min, ~lat_max,
  "Austroglossus pectoralis", "Observed", 16.9, 31.0, -36.4, -29.4, #C
  "Cheilodactylus fasciatus", "Observed", 15.0, 31.0, -36.4, -29.9, #CR
  "Chorisochismus dentex",    "Observed", 16.5, 30.8, -34.8, -28.6, #CR
  "Chrysoblephus gibbiceps",  "Observed", 17.2, 32.7, -35.8, -29.4, #T
  "Clinus cottoides",         "Observed", 17.1, 31.1, -34.8, -29.7, #CR
  "Lithognathus lithognathus","Observed", 16.7, 32.9, -36.6, -26.9, #C
  "Argyrosomus inodorus",      "Observed", 17.9, 27.9, -35.2, -31.7, #T
  "Pachymetopon aeneum",      "Observed", 15.1, 33.4, -35.8, -27.3, #E
  "Polysteganus undulosus",   "Observed", 18.8, 33.4, -35.7, -27.9, #T
  "Petrus rupestris",          "Observed", 17.5, 33.2, -35.7, -27.5, #C
  "Sparodon durbanensis",     "Observed", 17.8, 31.9, -35.7, -29.8, #E
  "Spondyliosoma emarginatum","Observed", 17.1, 32.1, -36.0, -29.0, 
  "Zeus faber",               "Observed", 11.5, 31.9, -36.8, 17.7,
  "Caranx ignobilis",         "Observed", 25.6, 33.2, -34.0, -26.9,
  "Gymnothorax favagineus",   "Observed", 25.6, 32.9, -34.0, -26.9,
  "Thalassoma lunare",        "Observed", 29, 32.9, -32.2, -26.9,
  "Pomacanthus imperator",    "Observed", 30.8,  32.9, -30.3, -26.9,
  "Epinephelus andersoni",    "Observed", 20.0, 33.4, -35.7, -26.9,
  "Chaetodon marleyi",         "Observed", 18.0,  32.7,  -34.6, -27.5,
  "Chelidonichthys queketti",    "Observed", 14.5, 49.8, -36.8, -26.8,
  "Cookeolus japonicus",    "Observed", 20.7,  30.7,  -34.7, -30.7,
  "Sufflogobius bibarbatus",    "Observed", 14.7, 20.8, -34.4, -28.8
)
#________________________________________________________________________


#Place name coords
#coastal coordinate at centre poinjt of place, with the line transect (if on coast) or area section around place for uncertainty
place_lookup <- tibble::tribble(
  ~place_std,            ~lat,     ~lon,     ~uncertainty_km,
  "cape point",          -34.3568,  18.4960,  5,
  "cape of good hope",   -34.3568,  18.4740,   5,  
  "mossel bay",          -34.1922,  22.1692,  15,
  "durban",              -29.8870,  31.0881,  30,
  "east london",         -33.0241,  27.9247,   30,
  "port nolloth",        -29.2566,  16.8629,  10,
  "saldanha bay",        -33.0570,  17.9504,  10,
  "st helena bay",       -32.7383,  18.0202,  15,
  "false bay",           -34.108075,  18.6440,  40, #central
  "st lucia",            -28.3937,  32.4349,   15,
  "sodwana bay",         -27.5380,  32.6798,  15,
  "maputo bay",          -26.0891,  32.7671,   30,
  "maputo",              -26.0891,  32.7671,   30,
  "swakopmund",          -22.6719,  14.5193,  5,
  "knysna",              -34.0872,  23.0629,   20,
  "margate",              -30.8528,  30.3936,  15,
  "north angola border",  -6.0901,  12.2323, 15,
  "south namibian border",  -28.6357,  16.4487,   15,
  "mozambican border",   -26.8584,  32.9122,   15,
  "mauritius",          -20.2707, 57.2955,  1000,
  "port alfred",      -33.6083, 26.8995, 5,
  "lamberts bay",        -32.0906, 18.2968, 5,
  "north namibian border", -28.6367,  16.4535,  15,
  "cape agulhas",-34.8328, 20.0167, 5,
  "algoa bay",-33.8196, 25.8779, 5,
  "st sebastian bay",-34.4094, 20.9899, 5,
  "table bay", -33.8458, 18.4731, 15,
  "port elizabeth", -33.9289, 25.6700, 60,
  # River mouths 
  "orange river mouth",  -28.6357,  16.4487,   5,
  "kei river mouth",     -32.6797,  28.3903,   5,
  "olifants river mouth", -31.7040,  18.2167,    5,
  # Regional proxies
  "kwazulu-natal",       -29.1509,  31.7520,   500,
  "northern kwazulu-natal",-28.1207,32.6110,  250,
  "cape",                -34.8671,  19.370,  1000,
  "cape peninsula",      -34.2000,  18.4500,  60,
  "southern mozambique", -21.8701,  35.7097,  900,
  "northern mozambique", -10.4419, 40.7459, 900,
  "madagascar",          -19.9213,  44.371,   2000,
  "southern madagascar", -25.6578,  45.1038,   1000
)

clean_range_text <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("\\[.*?\\]", " ") %>%       
    str_replace_all("\\(.*?\\)", " ") %>%         
    str_replace_all("ref\\.?\\s*\\d+", " ") %>%  
    str_replace_all("\\.", " ") %>%               
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

# Map synonyms 
syn_map <- c(
  "cape"              = "cape",
  "the cape"              = "cape",
  "cape peninsula"        = "cape peninsula",
  "cape of good hope"     = "cape of good hope",
  "cape point"            = "cape point",
  "false bay"             = "false bay",
  "kwazulu-natal"         = "kwazulu-natal",
  "northern kwazulu-natal"   = "northern kwazulu-natal",
  "north angola border"  = "north angola border",
  "south namibian border"       = "south namibian border",
  "mozambican border"     = "mozambican border",
  "southern mozambique"   = "southern mozambique",
  "maputo bay"            = "maputo bay",
  "maputo"                = "maputo",
  "mossel bay"            = "mossel bay",
  "durban"                = "durban",
  "east london"           = "east london",
  "port nolloth"          = "port nolloth",
  "orange river mouth"    = "orange river mouth",
  "kei river mouth"       = "kei river mouth",
  "olifants river"        = "olifants river mouth",
  "saldanha bay"          = "saldanha bay",
  "st helena bay"         = "st helena bay",
  "st lucia"              = "st lucia",
  "sodwana bay"           = "sodwana bay",
  "swakopmund"            = "swakopmund",
  "knysna"                = "knysna",
  "margate"               = "margate",
  "southern madagascar"   = "southern madagascar",
  "madagascar"            = "madagascar",
  "mauritius"            = "mauritius",
  "port alfred"         = "port alfred",
  "lamberts bay"        = "lamberts bay",
  "north namibian border" = "north namibian border",
  "cape agulhas" = "cape agulhas",
  "algoa bay"= "algoa bay",
  "st sebastian bay" = "st sebastian bay",
  "port elizabeth" = "port elizabeth",
  "table bay" = "table bay",
  "northern mozambique" = "northern mozambique"
)


apply_synonyms <- function(x) {
  out <- x
  keys <- names(syn_map)[order(nchar(names(syn_map)), decreasing = TRUE)]
  for (k in keys) {
    pattern <- paste0("\\b", stringr::str_replace_all(k, "([\\W])", "\\\\\\1"), "\\b")
    out <- stringr::str_replace_all(out, stringr::regex(pattern, ignore_case = TRUE), syn_map[[k]])
  }
  
  out
}

range_parse <- function(text) {
  txt0 <- clean_range_text(text) %>% apply_synonyms()
  
  # Case: "from A to B and to C"  -> keep C as end; keep B as extras
  m_multi <- str_match(txt0, "^from\\s+(.+?)\\s+and\\s+from\\s+(.+?)\\s+to\\s+(.+?)$")
  if (!is.na(m_multi[1,1])) {
    return(tibble(
      raw = text,
      clean = txt0,
      anchor_start = c(str_trim(m_multi[1,2]), str_trim(m_multi[1,3])),
      anchor_end   = c(NA_character_,              str_trim(m_multi[1,4])),
      parse_rule   = c("from_and_from_split", "from_to"),
      extras       = c("first segment has no explicit end (kept as start-only)", NA_character_)
    ))
  }
  m_to2 <- str_match(txt0, "^from\\s+(.+?)\\s+to\\s+(.+?)\\s+and\\s+to\\s+(.+?)$")
  if (!is.na(m_to2[1,1])) {
    return(tibble(
      raw = text,
      clean = txt0,
      anchor_start = str_trim(m_to2[1,2]),
      anchor_end   = str_trim(m_to2[1,4]),
      parse_rule   = "from_to_and_to",
      extras       = paste0("intermediate_to=", str_trim(m_to2[1,3]))
    ))
  }
  
  
  m1 <- str_match(txt0, "^from\\s+(.+?)\\s+to\\s+(.+?)$")
  if (!is.na(m1[1,1])) {
    # detect trailing extras like "... to maputo and madagascar and mauritius"
    # We'll treat anything after the end token as extras ONLY if end token is followed by "and ..."
    # But since regex is anchored, instead we do a second pattern:
    m1x <- str_match(txt0, "^from\\s+(.+?)\\s+to\\s+(.+?)(\\s+and\\s+.+)$")
    if (!is.na(m1x[1,1])) {
      return(tibble(
        raw = text,
        clean = txt0,
        anchor_start = str_trim(m1x[1,2]),
        anchor_end   = str_trim(m1x[1,3]),
        parse_rule   = "from_to",
        extras       = str_trim(m1x[1,4])
      ))
    }
    
    return(tibble(
      raw = text,
      clean = txt0,
      anchor_start = str_trim(m1[1,2]),
      anchor_end   = str_trim(m1[1,3]),
      parse_rule   = "from_to",
      extras       = NA_character_
    ))
  }
  
  # Case: "A to B" (no "from")
  m2 <- str_match(txt0, "^(.+?)\\s+to\\s+(.+?)$")
  if (!is.na(m2[1,1])) {
    return(tibble(
      raw = text,
      clean = txt0,
      anchor_start = str_trim(m2[1,2]),
      anchor_end   = str_trim(m2[1,3]),
      parse_rule   = "to",
      extras       = NA_character_
    ))
  }
  
  # Unparsed
  tibble(
    raw = text,
    clean = txt0,
    anchor_start = NA_character_,
    anchor_end   = NA_character_,
    parse_rule   = "unparsed",
    extras       = NA_character_
  )
}

extract_intermediate <- function(extras) {
  ifelse(
    !is.na(extras) & str_detect(extras, "intermediate_to="),
    str_trim(str_replace(extras, "^.*intermediate_to=([^\\s].*?)(\\s+and\\s+.*)?$", "\\1")),
    NA_character_
  )
}

resolve_anchors <- function(parsed_tbl, place_lookup) {
  stopifnot(all(c("place_std","lat","lon","uncertainty_km") %in% names(place_lookup)))
  
  out <- parsed_tbl %>%
    left_join(place_lookup %>% select(place_std, lat, lon, uncertainty_km),
              by = c("anchor_start" = "place_std")) %>%
    rename(
      start_lat = lat, start_lon = lon,
      start_unc_km = uncertainty_km
    ) %>%
    left_join(place_lookup %>% select(place_std, lat, lon, uncertainty_km),
              by = c("anchor_end" = "place_std")) %>%
    rename(
      end_lat = lat, end_lon = lon,
      end_unc_km = uncertainty_km
    ) %>%
    mutate(
      # midpoints if both ends exist; otherwise keep start
      mid_lat = ifelse(!is.na(start_lat) & !is.na(end_lat), (start_lat + end_lat)/2, start_lat),
      mid_lon = ifelse(!is.na(start_lon) & !is.na(end_lon), (start_lon + end_lon)/2, start_lon),
      # combine uncertainty as max of available endpoints
      uncertainty_km = pmax(start_unc_km, end_unc_km, na.rm = TRUE),
      uncertainty_km = ifelse(is.infinite(uncertainty_km), NA_real_, uncertainty_km)
    )
  
  # helpful QA flags
  out %>%
    mutate(
      start_missing = is.na(start_lat),
      end_missing   = !is.na(anchor_end) & is.na(end_lat)
    )
}


ranges_raw <- tibble::tribble(
  ~species, ~source,   ~range_text,
  "Austroglossus pectoralis", "FishBasee", "from the cape to kwazulu-natal",
  "Petrus rupestris",         "FishBase", "from mossel bay to kwazulu-natal",
  "Lithognathus lithognathus","FishBase", "from orange river mouth to the cape and to kwazulu-natal",
  "Polysteganus undulosus",   "FishBase", "from southern mozambique to durban",
  "Chrysoblephus gibbiceps",  "FishBase", "from the cape to kwazulu-natal",
  "Argyrosomus inodorus",     "FishBase", "from south namibian border to cape point and to kei river mouth",
  "Cheilodactylus fasciatus", "FishBase", "from south namibian border to cape point and to mozambican border",
  "Clinus cottoides",         "FishBase", "from south namibian border to cape point and to mozambican border", #borders for south africa
  "Chorisochismus dentex",    "FishBase", "from south namibian border to cape point and to northern kwazulu-natal",
  "Spondyliosoma emarginatum","FishBase", "from south namibian border to cape point and to southern madagascar", #borders for south africa
  "Sparodon durbanensis",     "FishBase", "from cape of good hope to southern mozambique", 
  "Pachymetopon aeneum",      "FishBase", "from sodwana bay to the cape",
  "Zeus faber",               "FishBase", "from south namibian border to cape point and to mozambican border",
  "Caranx ignobilis",         "FishBase", "from port elizabeth to mozambican border", #east coast of sa
  "Gymnothorax favagineus",   "FishBase", "from port elizabeth to mozambican border",#east coast of sa,
  "Thalassoma lunare",        "FishBase", "from port elizabeth to mozambican border",#east coast of sa,
  "Pomacanthus imperator",    "FishBase", "from port elizabeth to mozambican border",#east coast of sa,
  "Epinephelus andersoni",    "FishBase", "from cape agulhas to mozambican border",#southeast  coast of sa,
  "Chaetodon marleyi",        "FishBase", "from lamberts bay to southern mozambique",
  "Chelidonichthys queketti", "FishBase", "from table bay to southern mozambique",
  "Cookeolus japonicus",      "FishBase", "from south namibian border to cape point and to mozambican border", #borders for south africa
  "Sufflogobius bibarbatus",  "FishBase", "from north namibian border to cape point and to mozambican border", #borders for south africa #,
  
  "Austroglossus pectoralis", "Smiths", "from the cape to kwazulu-natal",
  "Petrus rupestris",         "Smiths", "from mossel bay to kwazulu-natal",
  "Lithognathus lithognathus","Smiths", "from orange river mouth to cape point and to mozambican border", #borders for south africa
  "Polysteganus undulosus",   "Smiths", "from the cape to maputo bay",
  "Chrysoblephus gibbiceps",  "Smiths", "from the cape to east london",
  "Argyrosomus inodorus",     "Smiths", "from orange river mouth to cape point and to mozambican border", 
  "Cheilodactylus fasciatus", "Smiths", "from port nolloth to cape point and to durban.",
  "Clinus cottoides",         "Smiths", "from port nolloth to cape point and to kei river mouth",
  "Chorisochismus dentex",    "Smiths", "from the cape to kwazulu-natal",
  "Spondyliosoma emarginatum","Smiths", "from saldanha bay to cape point and to madagascar",
  "Sparodon durbanensis",     "Smiths", "from the cape to kwazulu-natal",
  "Pachymetopon aeneum",      "Smiths", "from the cape to kwazulu-natal",
  "Zeus faber",               "Smiths", "not found",
  "Caranx ignobilis",         "Smiths", "from kwazulu-natal to maputo bay",
  "Gymnothorax favagineus",   "Smiths", "from port alfred to kwazulu-natal",
  "Thalassoma lunare",        "Smiths", "from east london to kwazulu-natal",
  "Pomacanthus imperator",    "Smiths", "from northern mozambique to maputo bay",
  "Epinephelus andersoni",    "Smiths", "from knysna to maputo bay",
  "Chaetodon marleyi",        "Smiths", "from the cape to lamberts bay and to madagascar",
  "Chelidonichthys queketti", "Smiths", "from the cape to kwazulu-natal",
  "Cookeolus japonicus",      "Smiths", "from algoa bay to southern mozambique",
  "Sufflogobius bibarbatus",  "Smiths", "from st helena bay to st sebastian bay",
  
  "Austroglossus pectoralis", "WIO", "from cape peninsula to southern mozambique",
  "Petrus rupestris",         "WIO", "from false bay to st lucia",
  "Lithognathus lithognathus","WIO", "from orange river mouth to cape point and to kwazulu-natal",
  "Polysteganus undulosus",   "WIO", "from knysna to southern mozambique",
  "Chrysoblephus gibbiceps",  "WIO", "from false bay to margate", #(most common southwest of East London)
  "Argyrosomus inodorus",     "WIO", "from south namibian border to cape point and to kei river mouth",
  "Cheilodactylus fasciatus", "WIO", "from swakopmund to cape point and to durban",
  "Clinus cottoides",         "WIO", "from olifants river to cape point and to kei river mouth", # particularly abundant east of False Bay.
  "Chorisochismus dentex",    "WIO", "from south namibian border to cape point and to northern kwazulu-natal",
  "Spondyliosoma emarginatum","WIO", "from saldanha bay to cape point and to kwazulu-natal", # (plentiful on the southern Cape and southeastern coasts).
  "Sparodon durbanensis",     "WIO", "from st helena bay to southern mozambique",
  "Pachymetopon aeneum",      "WIO", "from cape point to kwazulu-natal ",
  "Zeus faber",               "WIO", "from south namibian border to cape point and to northern mozambique",
  "Caranx ignobilis",       "WIO",   "from south namibian border to algoa bay and to northern mozambique",
  "Gymnothorax favagineus",   "WIO", "from algoa bay to northern mozambique",
  "Thalassoma lunare",       "WIO",  "from south namibian border to the cape and to mozambican border",
  "Pomacanthus imperator",  "WIO",   "from east london to madagascar",
  "Epinephelus andersoni",    "WIO", "from knysna to maputo bay",
  "Chaetodon marleyi",       "WIO",  "from lamberts bay to southern mozambique",
  "Chelidonichthys queketti", "WIO", "from south namibian border to cape point and to mozambican border",
  "Cookeolus japonicus", "WIO",      "from algoa bay to southern mozambique",
  "Sufflogobius bibarbatus", "WIO", "not found"
)

########
#PLOTTING
range_levels <- c("Observed","Smiths","WIO","FishBase") ##FIX TO INCLUDE

range_cols <- c(
  "Observed"       = "#000000",  
  "Smiths"         = "palegreen",  
  "WIO"            = "lightblue", 
  "FishBase" = "salmon"   
)

rect_offsets <- tibble::tibble(
  source = range_levels,
  dx = c(0.00, 0.20, 0.40, 0.60),
  dy = c(0.00,-0.15,-0.30,-0.45)
)


# ---------------------------
# Helpers: degree axis labels
# ---------------------------
deg_lon <- function(x) sprintf("%g\u00B0E", x)
deg_lat <- function(x) sprintf("%g\u00B0S", abs(x))

# ---------------------------
# Base map (if you don't already have sa_map)
# ---------------------------
if (!exists("sa_map")) {
  sa_map <- rnaturalearth::ne_countries(
    scale = "medium",
    returnclass = "sf"
  ) %>%
    dplyr::filter(admin == "South Africa") %>%
    st_make_valid()
}

# ---------------------------
# Parse prep (only parse range_ok rows)
# ---------------------------
range_text_clean <- ranges_raw %>%
  dplyr::mutate(
    range_ok = !is.na(range_text) &
      !stringr::str_detect(stringr::str_to_lower(range_text),
                           "not found|unknown|n/a|none|no data")
  )

parsed <- range_text_clean %>%
  dplyr::filter(range_ok) %>%
  mutate(parsed = map(range_text, range_parse)) %>%
  unnest(parsed)

resolved <- resolve_anchors(parsed, place_lookup)

unresolved <- resolved %>%
  filter(
    (!is.na(anchor_start) & is.na(start_lat)) |
      (!is.na(anchor_end) & is.na(end_lat)) |
      parse_rule == "unparsed"
  ) %>%
  distinct(source, species, parse_rule, anchor_start, anchor_end, clean)

print(unresolved, n = Inf)

# ---------------------------
# PLOTTING inputs
# ---------------------------
range_levels <- c("Observed","Smiths","WIO","FishBase")

range_cols <- c(
  "Observed"        = "#000000",
  "Smiths"          = "palegreen",
  "WIO"             = "lightblue",
  "FishBase" = "salmon"
)

# Observed lon/lat extents from your master_dat
obs_extent <- master_dat_march %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  group_by(species) %>%
  summarise(
    lon_min = min(longitude),
    lon_max = max(longitude),
    lat_min = min(latitude),
    lat_max = max(latitude),
    .groups = "drop"
  ) %>%
  mutate(source = "Observed")

# For text ranges: use parsed longitudes + observed lat span for bbox height
obs_lat_extent <- master_dat_march %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  group_by(species) %>%
  summarise(
    lat_min = min(latitude),
    lat_max = max(latitude),
    .groups = "drop"
  )

# pull intermediate place (if present)
resolved2 <- resolved %>%
  mutate(intermediate_place = extract_intermediate(extras))

# join coords for the intermediate_to place
resolved2 <- resolved2 %>%
  left_join(
    place_lookup %>% select(place_std, lat, lon),
    by = c("intermediate_place" = "place_std")
  ) %>%
  rename(int_lat = lat, int_lon = lon)


# ---------- helper ----------
bbox_to_poly_df <- function(bbox_df) {
  bbox_df %>%
    filter(!is.na(lon_min), !is.na(lon_max), !is.na(lat_min), !is.na(lat_max)) %>%
    mutate(range_id = paste(species, as.character(source), row_number(), sep = "||")) %>%
    tidyr::uncount(weights = 5, .id = "vertex") %>%
    mutate(
      lon = case_when(
        vertex == 1 ~ lon_min,
        vertex == 2 ~ lon_min,
        vertex == 3 ~ lon_max,
        vertex == 4 ~ lon_max,
        vertex == 5 ~ lon_min
      ),
      lat = case_when(
        vertex == 1 ~ lat_min,
        vertex == 2 ~ lat_max,
        vertex == 3 ~ lat_max,
        vertex == 4 ~ lat_min,
        vertex == 5 ~ lat_min
      )
    ) %>%
    select(species, source, range_id, vertex, lon, lat)
}

clip_ranges_bbox <- function(bbox, lon_lim = c(10, 45), lat_lim = c(-45, -15)) {
  bbox %>%
    mutate(
      source_chr = trimws(as.character(source)),
      lon_min = ifelse(source_chr == "Observed", lon_min, pmax(lon_min, lon_lim[1])),
      lon_max = ifelse(source_chr == "Observed", lon_max, pmin(lon_max, lon_lim[2])),
      lat_min = ifelse(source_chr == "Observed", lat_min, pmax(lat_min, lat_lim[1])),
      lat_max = ifelse(source_chr == "Observed", lat_max, pmin(lat_max, lat_lim[2]))
    ) %>%
    mutate(
      bad = is.na(lon_min) | is.na(lon_max) | is.na(lat_min) | is.na(lat_max) |
        lon_min >= lon_max | lat_min >= lat_max
    ) %>%
    filter(!bad) %>%
    select(-source_chr, -bad)
}


extreme_pts_long <- master_dat_march %>%
  dplyr::filter(!is.na(longitude), !is.na(latitude)) %>%
  dplyr::group_by(species) %>%
  dplyr::summarise(
    west_lon = min(longitude),
    east_lon = max(longitude),
    west_lat = latitude[which.min(longitude)][1],
    east_lat = latitude[which.max(longitude)][1],
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(
    cols = c(west_lon, west_lat, east_lon, east_lat),
    names_to = c("side", ".value"),
    names_pattern = "(west|east)_(lon|lat)"
  )

# ---------- text bbox (start/end/intermediate) ----------
text_bbox <- resolved2 %>%
  mutate(
    lon_min = pmin(start_lon, end_lon, int_lon, na.rm = TRUE),
    lon_max = pmax(start_lon, end_lon, int_lon, na.rm = TRUE),
    lat_min = pmin(start_lat, end_lat, int_lat, na.rm = TRUE),
    lat_max = pmax(start_lat, end_lat, int_lat, na.rm = TRUE)
  ) %>%
  mutate(
    lon_min = ifelse(is.infinite(lon_min), NA_real_, lon_min),
    lon_max = ifelse(is.infinite(lon_max), NA_real_, lon_max),
    lat_min = ifelse(is.infinite(lat_min), NA_real_, lat_min),
    lat_max = ifelse(is.infinite(lat_max), NA_real_, lat_max)
  ) %>%
  filter(!is.na(lon_min) & !is.na(lon_max) & !is.na(lat_min) & !is.na(lat_max)) %>%
  select(species, source, lon_min, lon_max, lat_min, lat_max)

text_bbox <- text_bbox %>%
  mutate(
    outside_lon = (lon_max < 10 | lon_min > 45),
    outside_lat = (lat_max < -45 | lat_min > -15),
    outside_all = outside_lon | outside_lat
  ) %>%
  filter(!outside_all) %>%
  select(-outside_lon, -outside_lat, -outside_all)

# ---------- combine with observed ----------
ranges_bbox <- bind_rows(obs_extent %>% select(species, source, lon_min, lon_max, lat_min, lat_max),
                           text_bbox) %>%
  mutate(source = factor(trimws(as.character(source)), levels = range_levels)) %>%
  mutate(
    lon_min0 = lon_min, lon_max0 = lon_max,
    lat_min0 = lat_min, lat_max0 = lat_max
  )

ranges_bbox <- clip_ranges_bbox(ranges_bbox, lon_lim = c(10,45), lat_lim = c(-45,-15)) %>%
  mutate(
    clipped = (as.character(source) != "Observed") &
      (lon_min != lon_min0 | lon_max != lon_max0 | lat_min != lat_min0 | lat_max != lat_max0)
  )

# polygons for bars (if you still use them)
ranges_poly <- bbox_to_poly_df(ranges_bbox)

# ---------- map rectangles should NOT shift N-S: force observed lat envelope ----------
ranges_bbox_map <- ranges_bbox %>%
  left_join(obs_lat_extent, by = "species", suffix = c("", "_obs")) %>%
  mutate(
    source_chr = trimws(as.character(source)),
    lat_min = ifelse(source_chr == "Observed", lat_min, lat_min_obs),
    lat_max = ifelse(source_chr == "Observed", lat_max, lat_max_obs)
  ) %>%
  select(-lat_min_obs, -lat_max_obs, -source_chr)

ranges_poly_map <- bbox_to_poly_df(ranges_bbox_map)


sa_map_df <- sa_map %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>%
  st_coordinates() %>%
  as_tibble() %>%
  mutate(group = interaction(L1, L2, L3, drop = TRUE))

# ---------------------------
# Plot function (robust if extreme_pts_long missing)
# ---------------------------
plot_species_rangebars_3panel <- function(
    sp_name,
    master_dat,
    ranges_bbox,
    ranges_poly_map,     # <-- NEW
    extreme_pts_long = NULL,
    sa_map_df,
    range_levels = c("Observed","Smiths","WIO","FishBase"),
    range_cols,
    lat_lim = c(-45, -15),
    lon_lim = c(10, 45),
    x_breaks = seq(10, 40, by = 5),
    y_breaks = seq(-45, -15, by = 5)
) {
  
  
  sp_pts <- master_dat %>%
    filter(species == sp_name) %>%
    filter(!is.na(longitude), !is.na(latitude))
  
  if (nrow(sp_pts) == 0) {
    message("No points for: ", sp_name)
    return(NULL)
  }
  
  sp_bbox <- ranges_bbox %>%
    filter(species == sp_name) %>%
    mutate(source = factor(as.character(source), levels = range_levels))
  
  sp_rect <- ranges_poly_map %>%
    filter(species == sp_name, as.character(source) == "Observed") %>%  # <- only observed
    mutate(source = factor(as.character(source), levels = range_levels))
  
  #sp_rect <- ranges_poly_map %>%          # <-- USE MAP VERSION
   # filter(species == sp_name) %>%
    #mutate(source = factor(as.character(source), levels = range_levels))
  
  sp_ext <- tibble()
  if (!is.null(extreme_pts_long) && nrow(extreme_pts_long) > 0) {
    sp_ext <- extreme_pts_long %>%
      filter(species == sp_name) %>%
      mutate(
        side = tolower(trimws(as.character(side))),
        side = case_when(
          side %in% c("w","west","western","westernmost","left") ~ "west",
          side %in% c("e","east","eastern","easternmost","right") ~ "east",
          TRUE ~ side
        ),
        side = factor(side, levels = c("west","east"))
      )
  }
  
  # LEFT: MAP
  p_map <- ggplot() +
    geom_polygon(
      data = sa_map_df,
      aes(x = X, y = Y, group = group),
      fill = "grey90",
      color = "black",
      linewidth = 0.1
    ) +
    geom_polygon(
      data = sp_rect,
      aes(x = lon, y = lat, group = range_id, colour = source),
      fill = NA,
      linewidth = 0.2,
      alpha = 0.9
    ) +
    geom_point(
      data = sp_pts,
      aes(x = longitude, y = latitude),
      color = "grey30",
      size = 0.6,
      alpha = 0.45
    ) +
    coord_cartesian(xlim = lon_lim, ylim = lat_lim, expand = FALSE) +
    scale_x_continuous(breaks = x_breaks, labels = deg_lon(x_breaks), name = "Longitude") +
    scale_y_continuous(breaks = y_breaks, labels = deg_lat(y_breaks), name = "Latitude") +
    scale_colour_manual(values = range_cols, limits = range_levels, drop = FALSE) +
    theme_classic(base_family = "Times") +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "italic", size = 12),
      axis.title = element_text(size = 11),
      axis.text  = element_text(size = 10),
      plot.margin = margin(5, 5, 5, 5)
    ) +
    labs(title = sp_name) +
    annotate(
      "text",
      x = lon_lim[1] + 0.2, y = lat_lim[1] + 0.8,
      label = "The rectangle shows an envelope of observed occurences for visual comparability",
      hjust = 0, size = 2.8, family = "Times"
    )
  
  if (nrow(sp_ext) > 0) {
    p_map <- p_map +
      geom_point(
        data = sp_ext,
        aes(x = lon, y = lat, shape = side),
        size = 3,
        stroke = 1.1,
        color = "black",
        fill = "white"
      ) +
      scale_shape_manual(
        values = c(west = 21, east = 24),
        breaks = c("west","east"),
        labels = c("Westernmost record", "Easternmost record")
      )
  }
  
  # MIDDLE: LAT RANGE (Observed only)
  # MIDDLE: LAT RANGE (per source)
  sp_bbox_lat <- sp_bbox %>%
    mutate(
      source_chr = factor(as.character(source), levels = range_levels),
      x = as.numeric(source_chr)
    ) %>%
    filter(!is.na(lat_min), !is.na(lat_max))
  
  p_lat <- ggplot(sp_bbox_lat) +
    geom_segment(
      aes(x = x, xend = x, y = lat_min, yend = lat_max, colour = source_chr,
          linewidth = source_chr == "Observed"),
      lineend = "round"
    ) +
    geom_point(aes(x = x, y = lat_min, colour = source_chr), size = 2.2) +
    geom_point(aes(x = x, y = lat_max, colour = source_chr), size = 2.2) +
    scale_linewidth_manual(values = c(`TRUE` = 1.8, `FALSE` = 1.2), guide = "none") +
    scale_colour_manual(values = range_cols, limits = range_levels, drop = FALSE) +
    coord_cartesian(ylim = lat_lim, expand = FALSE) +
    scale_y_continuous(breaks = y_breaks, labels = deg_lat(y_breaks), name = "Latitude") +
    scale_x_continuous(
      breaks = seq_along(range_levels),
      labels = range_levels,
      limits = c(0.5, length(range_levels) + 0.5),
      expand = c(0, 0),
      name = NULL
    ) +
    theme_classic(base_family = "Times") +
    theme(
      axis.title.y = element_text(size = 11),
      axis.text.y  = element_text(size = 10),
      axis.text.x  = element_text(size = 9, angle = 45, hjust = 1),
      axis.ticks.x = element_line(),
      legend.position = "none",
      plot.title   = element_text(size = 11),
      plot.margin  = margin(5, 5, 5, 5)
    ) +
    labs(title = "Latitudinal range comparison")
  
  
  # RIGHT: LON RANGES
  sp_bbox_bar <- sp_bbox %>%
    mutate(
      source_chr = factor(as.character(source), levels = range_levels),
      y = as.numeric(source_chr)
    )
  
  p_lon <- ggplot(sp_bbox_bar) +
    geom_segment(
      aes(x = lon_min, xend = lon_max, y = y, yend = y, colour = source_chr,
          linewidth = source_chr == "Observed"),
      lineend = "round"
    ) +
    geom_point(aes(x = lon_min, y = y, colour = source_chr), size = 2.2) +
    geom_point(aes(x = lon_max, y = y, colour = source_chr), size = 2.2) +
    scale_linewidth_manual(values = c(`TRUE` = 1.8, `FALSE` = 1.2), guide = "none") +
    scale_colour_manual(values = range_cols, limits = range_levels, drop = FALSE) +
    scale_x_continuous(breaks = x_breaks, labels = deg_lon(x_breaks), name = "Longitude") +
    scale_y_continuous(
      breaks = seq_along(range_levels),
      labels = range_levels,
      limits = c(0.5, length(range_levels) + 0.5),
      expand = c(0, 0),
      name = NULL
    ) +
    coord_cartesian(xlim = lon_lim, expand = FALSE) +
    theme_classic(base_family = "Times") +
    theme(
      axis.title.y = element_blank(),
      axis.text.y  = element_text(size = 10),
      axis.title.x = element_text(size = 11),
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text  = element_text(size = 9),
      plot.title   = element_text(size = 11),
      plot.margin  = margin(5, 5, 5, 5)
    ) +
    labs(colour = "Source range", title = "Longitudinal range comparison")
  
  # COMBINE
  (p_map | p_lat | p_lon) + patchwork::plot_layout(widths = c(1.25, 0.45, 1.05))
}

exists("extreme_pts_long")

# ---------------------------
run_rangebars_3panel <- function(sp) {
  plot_species_rangebars_3panel(
    sp_name = sp,
    master_dat = master_dat_march,
    ranges_bbox = ranges_bbox,            # bars still use source lat
    ranges_poly_map = ranges_poly_map,    # map uses observed lat
    extreme_pts_long = if (exists("extreme_pts_long")) extreme_pts_long else NULL,
    sa_map_df = sa_map_df,
    range_cols = range_cols
  )
}

p_Lithognathus_lithognathus <- run_rangebars_3panel("Lithognathus lithognathus")
p_Chrysoblephus_gibbiceps <- run_rangebars_3panel("Chrysoblephus gibbiceps")
p_Pachymetopon_aeneum <- run_rangebars_3panel("Pachymetopon aeneum")
p_Spondyliosoma_emarginatum <- run_rangebars_3panel("Spondyliosoma emarginatum")
p_Petrus_rupestris <- run_rangebars_3panel("Petrus rupestris")
p_Polysteganus_undulosus <- run_rangebars_3panel("Polysteganus undulosus")
p_Argyrosomus_inodorus <- run_rangebars_3panel("Argyrosomus inodorus")
p_Sparodon_durbanensis <- run_rangebars_3panel("Sparodon durbanensis")
p_Austroglossus_pectoralis <- run_rangebars_3panel("Austroglossus pectoralis")
p_Zeus_faber <- run_rangebars_3panel("Zeus faber")
p_Clinus_cottoides <- run_rangebars_3panel("Clinus cottoides")
p_Chorisochismus_dentex <- run_rangebars_3panel("Chorisochismus dentex")
p_Cheilodactylus_fasciatus <- run_rangebars_3panel("Cheilodactylus fasciatus")
p_Caranx_ignobilis <- run_rangebars_3panel("Caranx ignobilis")
p_Gymnothorax_favagineus <- run_rangebars_3panel("Gymnothorax favagineus")
p_Thalassoma_lunare <- run_rangebars_3panel("Thalassoma lunare")
p_Pomacanthus_imperator <- run_rangebars_3panel("Pomacanthus imperator")
p_Chaetodon_marleyi <- run_rangebars_3panel("Chaetodon marleyi")
p_Chelidonichthys_queketti <- run_rangebars_3panel("Chelidonichthys queketti")
p_Cookeolus_japonicus <- run_rangebars_3panel("Cookeolus japonicus")
p_Sufflogobius_bibarbatus <- run_rangebars_3panel("Sufflogobius bibarbatus")
p_Epinephelus_andersoni <- run_rangebars_3panel("Epinephelus andersoni")

p_Lithognathus_lithognathus
p_Chrysoblephus_gibbiceps
p_Pachymetopon_aeneum
p_Spondyliosoma_emarginatum
p_Petrus_rupestris
p_Polysteganus_undulosus
p_Argyrosomus_inodorus
p_Sparodon_durbanensis
p_Austroglossus_pectoralis
p_Zeus_faber
p_Clinus_cottoides
p_Chorisochismus_dentex
p_Cheilodactylus_fasciatus
p_Caranx_ignobilis
p_Gymnothorax_favagineus
p_Thalassoma_lunare
p_Pomacanthus_imperator
p_Chaetodon_marleyi
p_Chelidonichthys_queketti
p_Cookeolus_japonicus
p_Sufflogobius_bibarbatus
p_Epinephelus_andersoni


#EXTENSIONS

# ============================================================
# Range extension + overlap + variability vs Observed
# Works with your existing objects:
# - master_dat (points)
# - resolved2 (parsed anchors with start/end/int coords) OR text_bbox
# - ranges_bbox (already built in your script) is fine too
# ============================================================

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# ---------------------------
# 1) Observed range (from points)
# ---------------------------
obs_extent <- master_dat_march %>%
  filter(species %in% species_list) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  group_by(species) %>%
  summarise(
    obs_lon_min = min(longitude),
    obs_lon_max = max(longitude),
    obs_lat_min = min(latitude),
    obs_lat_max = max(latitude),
    obs_lon_span = obs_lon_max - obs_lon_min,
    obs_lat_span = obs_lat_max - obs_lat_min,
    .groups = "drop"
  )

# ---------------------------
# 2) Reference-source range boxes
#    Option A (recommended): use your computed text_bbox (from resolved2)
#    text_bbox must have: species, source, lon_min, lon_max, lat_min, lat_max
# ---------------------------

# If you haven't created text_bbox yet, here's a compact version using resolved2:
# (skip this block if text_bbox already exists)
if (!exists("text_bbox")) {
  # Ensure resolved2 exists (your script makes it)
  stopifnot(exists("resolved2"))
  
  text_bbox <- resolved2 %>%
    filter(!is.na(source), !is.na(species)) %>%
    mutate(
      lon_min = pmin(start_lon, end_lon, int_lon, na.rm = TRUE),
      lon_max = pmax(start_lon, end_lon, int_lon, na.rm = TRUE),
      lat_min = pmin(start_lat, end_lat, int_lat, na.rm = TRUE),
      lat_max = pmax(start_lat, end_lat, int_lat, na.rm = TRUE)
    ) %>%
    mutate(
      lon_min = ifelse(is.infinite(lon_min), NA_real_, lon_min),
      lon_max = ifelse(is.infinite(lon_max), NA_real_, lon_max),
      lat_min = ifelse(is.infinite(lat_min), NA_real_, lat_min),
      lat_max = ifelse(is.infinite(lat_max), NA_real_, lat_max)
    ) %>%
    filter(!is.na(lon_min) & !is.na(lon_max) & !is.na(lat_min) & !is.na(lat_max)) %>%
    transmute(
      species,
      source = trimws(as.character(source)),
      ref_lon_min = lon_min,
      ref_lon_max = lon_max,
      ref_lat_min = lat_min,
      ref_lat_max = lat_max
    )
}

# Keep only your target sources; fix any "FishBasee" typo
text_bbox <- text_bbox %>%
  mutate(source = recode(source, "FishBasee" = "FishBase"))

# If you want to include ONLY these sources:
ref_sources <- c("FishBase","Smiths","WIO")
text_bbox <- text_bbox %>% filter(source %in% ref_sources)

# Quick check (optional)
names(text_bbox)

# Standardise text_bbox column names to ref_lon_min/ref_lon_max/ref_lat_min/ref_lat_max
text_bbox2 <- text_bbox %>%
  mutate(source = recode(trimws(as.character(source)), "FishBasee" = "FishBase")) %>%
  # If your bbox columns are lon_min/lon_max/lat_min/lat_max, rename them:
  rename(
    ref_lon_min = any_of("lon_min"),
    ref_lon_max = any_of("lon_max"),
    ref_lat_min = any_of("lat_min"),
    ref_lat_max = any_of("lat_max")
  ) %>%
  # If they were already ref_* then this does nothing; if not, it fixes it
  filter(source %in% c("FishBase","Smiths","WIO"))

# Now summarise safely
ref_extent <- text_bbox2 %>%
  group_by(species, source) %>%
  summarise(
    ref_lon_min = min(ref_lon_min, na.rm = TRUE),
    ref_lon_max = max(ref_lon_max, na.rm = TRUE),
    ref_lat_min = min(ref_lat_min, na.rm = TRUE),
    ref_lat_max = max(ref_lat_max, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ref_lon_span = ref_lon_max - ref_lon_min,
    ref_lat_span = ref_lat_max - ref_lat_min
  )


# ---------------------------
# 3) Helper functions: overlap and extensions (1D)
# ---------------------------
overlap_1d <- function(a_min, a_max, b_min, b_max) {
  # length of intersection between [a_min,a_max] and [b_min,b_max]
  pmax(0, pmin(a_max, b_max) - pmax(a_min, b_min))
}

union_1d <- function(a_min, a_max, b_min, b_max) {
  # length of union between [a_min,a_max] and [b_min,b_max]
  pmax(a_max, b_max) - pmin(a_min, b_min)
}

# Extension of observed beyond reference:
# west/south = reference_min - observed_min (positive => observed extends further)
# east/north = observed_max - reference_max
ext_min_side <- function(obs_min, ref_min) pmax(0, ref_min - obs_min)
ext_max_side <- function(obs_max, ref_max) pmax(0, obs_max - ref_max)

# ---------------------------
# 4) Metrics vs observed (per species x source)
# ---------------------------
range_compare_tbl <- ref_extent %>%
  left_join(obs_extent, by = "species") %>%
  mutate(
    # ---- LONGITUDE metrics ----
    lon_overlap = overlap_1d(obs_lon_min, obs_lon_max, ref_lon_min, ref_lon_max),
    lon_union   = union_1d(obs_lon_min, obs_lon_max, ref_lon_min, ref_lon_max),
    lon_jaccard = ifelse(lon_union > 0, lon_overlap / lon_union, NA_real_),
    
    # coverage of reference by observed (how much of stated range is covered)
    lon_pct_ref_covered = ifelse(ref_lon_span > 0, 100 * lon_overlap / ref_lon_span, NA_real_),
    
    # observed extension beyond reference envelope
    lon_ext_west = ext_min_side(obs_lon_min, ref_lon_min),
    lon_ext_east = ext_max_side(obs_lon_max, ref_lon_max),
    lon_ext_total = lon_ext_west + lon_ext_east,
    
    # variability/difference
    lon_span_diff = obs_lon_span - ref_lon_span,
    lon_span_ratio = ifelse(ref_lon_span > 0, obs_lon_span / ref_lon_span, NA_real_),
    lon_mid_obs = (obs_lon_min + obs_lon_max)/2,
    lon_mid_ref = (ref_lon_min + ref_lon_max)/2,
    lon_mid_shift = lon_mid_obs - lon_mid_ref,
    
    # ---- LATITUDE metrics ----
    lat_overlap = overlap_1d(obs_lat_min, obs_lat_max, ref_lat_min, ref_lat_max),
    lat_union   = union_1d(obs_lat_min, obs_lat_max, ref_lat_min, ref_lat_max),
    lat_jaccard = ifelse(lat_union > 0, lat_overlap / lat_union, NA_real_),
    lat_pct_ref_covered = ifelse(ref_lat_span > 0, 100 * lat_overlap / ref_lat_span, NA_real_),
    
    # note: with negative south latitudes, "southward extension" is *more negative*
    # but extension in degrees works the same via min/max logic:
    lat_ext_south = ext_min_side(obs_lat_min, ref_lat_min),
    lat_ext_north = ext_max_side(obs_lat_max, ref_lat_max),
    lat_ext_total = lat_ext_south + lat_ext_north,
    
    lat_span_diff = obs_lat_span - ref_lat_span,
    lat_span_ratio = ifelse(ref_lat_span > 0, obs_lat_span / ref_lat_span, NA_real_),
    lat_mid_obs = (obs_lat_min + obs_lat_max)/2,
    lat_mid_ref = (ref_lat_min + ref_lat_max)/2,
    lat_mid_shift = lat_mid_obs - lat_mid_ref,
    
    # ---- Combined summary (simple) ----
    mean_jaccard = rowMeans(cbind(lon_jaccard, lat_jaccard), na.rm = TRUE),
    mean_ref_covered = rowMeans(cbind(lon_pct_ref_covered, lat_pct_ref_covered), na.rm = TRUE)
  ) %>%
  select(
    species, source,
    # observed
    obs_lon_min, obs_lon_max, obs_lat_min, obs_lat_max, obs_lon_span, obs_lat_span,
    # reference
    ref_lon_min, ref_lon_max, ref_lat_min, ref_lat_max, ref_lon_span, ref_lat_span,
    # longitude metrics
    lon_overlap, lon_jaccard, lon_pct_ref_covered, lon_ext_west, lon_ext_east, lon_ext_total,
    lon_span_diff, lon_span_ratio, lon_mid_shift,
    # latitude metrics
    lat_overlap, lat_jaccard, lat_pct_ref_covered, lat_ext_south, lat_ext_north, lat_ext_total,
    lat_span_diff, lat_span_ratio, lat_mid_shift,
    # combined
    mean_jaccard, mean_ref_covered
  ) %>%
  arrange(species, source)

print(range_compare_tbl, n =Inf, width =Inf)

# ---------------------------
# 5) Optional: summarise patterns by source (median etc.)
# ---------------------------
range_compare_summary <- range_compare_tbl %>%
  group_by(source) %>%
  summarise(
    n_species = n_distinct(species),
    
    # coverage / overlap
    median_lon_covered = median(lon_pct_ref_covered, na.rm = TRUE),
    median_lat_covered = median(lat_pct_ref_covered, na.rm = TRUE),
    median_mean_covered = median(mean_ref_covered, na.rm = TRUE),
    median_mean_jaccard = median(mean_jaccard, na.rm = TRUE),
    
    # how often observed extends beyond reference
    pct_any_lon_extension = 100 * mean(lon_ext_total > 0, na.rm = TRUE),
    pct_any_lat_extension = 100 * mean(lat_ext_total > 0, na.rm = TRUE),
    
    # magnitude of extension (deg)
    median_lon_ext_total = median(lon_ext_total, na.rm = TRUE),
    median_lat_ext_total = median(lat_ext_total, na.rm = TRUE),
    
    # variability in span
    median_lon_span_diff = median(lon_span_diff, na.rm = TRUE),
    median_lat_span_diff = median(lat_span_diff, na.rm = TRUE),
    .groups = "drop"
  )


print(range_compare_summary, n =Inf, width =Inf)


# ---- 1) Flag obvious QC artefacts (edit thresholds as needed) ----
# Use domain logic: SA coast is roughly lon 14–33E and lat -37 to -22 (very rough),
# so anything far outside that is likely not your "biological" SA signal.
qc_filtered <- range_compare_tbl %>%
  mutate(
    qc_flag =
      obs_lon_max > 36 | obs_lon_min < 10 |     # extreme longitudes
      obs_lat_max > -20 | obs_lat_min < -40 |   # extreme latitudes
      lon_ext_total > 10 | lat_ext_total > 10   # huge single-axis expansions
  ) %>%
  filter(!qc_flag)

# ---- 2) Directional bias metrics ----
dir_bias_tbl <- qc_filtered %>%
  mutate(
    lon_bias = lon_ext_east - lon_ext_west,      # + = eastward-biased expansion
    lat_bias = lat_ext_south - lat_ext_north,    # + = southward-biased expansion
    lon_bias_dir = case_when(lon_bias > 0 ~ "east", lon_bias < 0 ~ "west", TRUE ~ "none"),
    lat_bias_dir = case_when(lat_bias > 0 ~ "south", lat_bias < 0 ~ "north", TRUE ~ "none")
  )

# ---- 3) Summarise by source + overall ----
dir_bias_summary <- dir_bias_tbl %>%
  group_by(source) %>%
  summarise(
    n = n(),
    median_lon_bias = median(lon_bias, na.rm = TRUE),
    median_lat_bias = median(lat_bias, na.rm = TRUE),
    pct_east_bias = mean(lon_bias > 0, na.rm = TRUE) * 100,
    pct_west_bias = mean(lon_bias < 0, na.rm = TRUE) * 100,
    pct_south_bias = mean(lat_bias > 0, na.rm = TRUE) * 100,
    pct_north_bias = mean(lat_bias < 0, na.rm = TRUE) * 100,
    .groups = "drop"
  )

print(dir_bias_summary, n =Inf, width =Inf)

# ---- 4) Simple non-parametric tests: is median bias different from 0? ----
# (Directional bias = systematic sign away from 0)
bias_tests <- dir_bias_tbl %>%
  group_by(source) %>%
  summarise(
    p_lon_bias = wilcox.test(lon_bias, mu = 0, exact = FALSE)$p.value,
    p_lat_bias = wilcox.test(lat_bias, mu = 0, exact = FALSE)$p.value,
    .groups = "drop"
  )

print(bias_tests, n =Inf, width =Inf)


midshift_tbl <- qc_filtered %>%
  mutate(
    obs_lon_mid = (obs_lon_min + obs_lon_max) / 2,
    ref_lon_mid = (ref_lon_min + ref_lon_max) / 2,
    obs_lat_mid = (obs_lat_min + obs_lat_max) / 2,
    ref_lat_mid = (ref_lat_min + ref_lat_max) / 2,
    
    lon_mid_shift2 = obs_lon_mid - ref_lon_mid,   # + = eastward shift
    lat_mid_shift2 = obs_lat_mid - ref_lat_mid    # - = southward shift (more negative)
  )

midshift_summary <- midshift_tbl %>%
  group_by(source) %>%
  summarise(
    n = n(),
    median_lon_mid_shift = median(lon_mid_shift2, na.rm = TRUE),
    median_lat_mid_shift = median(lat_mid_shift2, na.rm = TRUE),
    pct_east_shift = mean(lon_mid_shift2 > 0, na.rm = TRUE) * 100,
    pct_west_shift = mean(lon_mid_shift2 < 0, na.rm = TRUE) * 100,
    pct_south_shift = mean(lat_mid_shift2 < 0, na.rm = TRUE) * 100, # NOTE: south is negative
    pct_north_shift = mean(lat_mid_shift2 > 0, na.rm = TRUE) * 100,
    .groups = "drop"
  )

print(midshift_summary, n =Inf, width =Inf)

midshift_tests <- midshift_tbl %>%
  group_by(source) %>%
  summarise(
    p_lon_shift = wilcox.test(lon_mid_shift2, mu = 0, exact = FALSE)$p.value,
    p_lat_shift = wilcox.test(lat_mid_shift2, mu = 0, exact = FALSE)$p.value,
    .groups = "drop"
  )

print(midshift_tests, n =Inf, width =Inf)

library(dplyr)
library(ggplot2)
library(forcats)

plot_shift_dat <- midshift_tbl %>%
  filter(species %in% species_list) %>%
  mutate(
    source = factor(source, levels = c("FishBase", "Smiths", "WIO"))
  ) %>%
  group_by(species) %>%
  mutate(species_order = median(lon_mid_shift2, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(species = fct_reorder(species, species_order))

p_shift <- ggplot(plot_shift_dat,
                  aes(x = lon_mid_shift2, y = species, colour = source)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_point(size = 2) +
  scale_colour_manual(values = c(
    "FishBase" = "navy",
    "Smiths"   = "green",
    "WIO"      = "lightblue"
  )) +
  labs(
    x = "Longitude midpoint shift (observed - reference, degrees)",
    y = NULL,
    colour = NULL
  ) +
  theme_minimal(base_family = "serif", base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

p_shift

#a midpoint shift or directional bias plot, with a dashed line at 0.
#Option A: longitude midpoint shift
#negative = observed range shifted west of reference
#positive = observed range shifted east of reference

plot_cover_dat <- range_compare_tbl %>%
  filter(species %in% species_list) %>%
  mutate(
    source = factor(source, levels = c("FishBase", "Smiths", "WIO"))
  ) %>%
  group_by(species) %>%
  mutate(species_order = median(mean_ref_covered, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(species = fct_reorder(species, species_order))

p_cover <- ggplot(plot_cover_dat,
                  aes(x = mean_ref_covered, y = species, colour = source)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_point(size = 2) +
  scale_colour_manual(values = c(
    "FishBase" = "navy",
    "Smiths"   = "green",
    "WIO"      = "lightblue"
  )) +
  labs(
    x = "Reference range covered by observed records (%)",
    y = NULL,
    colour = NULL
  ) +
  theme_minimal(base_family = "serif", base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

p_cover

p_shift + p_cover + plot_layout(ncol = 2)

ggsave(
  "p_cover.png",
  p_cover,
  width = 7,
  height = 6,
  units = "in"
)

ggsave(
  "p_shift.png",
  p_shift,
  width = 7,
  height = 6,
  units = "in"
)

# ============================================================
# FUNCTION TO FLAG GROSS SPATIAL OUTLIERS PER SPECIES
# ============================================================

flag_species_outliers <- function(dat,
                                  species_col = "species",
                                  lon_col = "longitude",
                                  lat_col = "latitude",
                                  min_n = 8,
                                  mad_mult = 4,
                                  lon_mad_mult = 4,
                                  lat_mad_mult = 4) {
  
  sp_sym  <- rlang::sym(species_col)
  lon_sym <- rlang::sym(lon_col)
  lat_sym <- rlang::sym(lat_col)
  
  dat %>%
    filter(!is.na(!!sp_sym), !is.na(!!lon_sym), !is.na(!!lat_sym)) %>%
    group_by(!!sp_sym) %>%
    group_modify(~{
      x <- .x
      
      # too few records: do not auto-remove, just return unflagged
      if (nrow(x) < min_n) {
        return(
          x %>%
            mutate(
              centroid_lon = median(!!lon_sym, na.rm = TRUE),
              centroid_lat = median(!!lat_sym, na.rm = TRUE),
              dist_centroid = sqrt((!!lon_sym - centroid_lon)^2 + (!!lat_sym - centroid_lat)^2),
              outlier_dist = FALSE,
              outlier_lon  = FALSE,
              outlier_lat  = FALSE,
              gross_outlier = FALSE,
              outlier_reason = NA_character_
            )
        )
      }
      
      # robust centroid
      cen_lon <- median(x[[lon_col]], na.rm = TRUE)
      cen_lat <- median(x[[lat_col]], na.rm = TRUE)
      
      # euclidean distance in degree space
      dist_vec <- sqrt((x[[lon_col]] - cen_lon)^2 + (x[[lat_col]] - cen_lat)^2)
      
      # robust thresholds
      dist_med <- median(dist_vec, na.rm = TRUE)
      dist_mad <- mad(dist_vec, constant = 1, na.rm = TRUE)
      lon_med  <- median(x[[lon_col]], na.rm = TRUE)
      lon_mad  <- mad(x[[lon_col]], constant = 1, na.rm = TRUE)
      lat_med  <- median(x[[lat_col]], na.rm = TRUE)
      lat_mad  <- mad(x[[lat_col]], constant = 1, na.rm = TRUE)
      
      # fallback if MAD = 0
      if (is.na(dist_mad) || dist_mad == 0) dist_mad <- sd(dist_vec, na.rm = TRUE) / 1.4826
      if (is.na(lon_mad)  || lon_mad  == 0) lon_mad  <- sd(x[[lon_col]], na.rm = TRUE) / 1.4826
      if (is.na(lat_mad)  || lat_mad  == 0) lat_mad  <- sd(x[[lat_col]], na.rm = TRUE) / 1.4826
      
      outlier_dist <- dist_vec > (dist_med + mad_mult * dist_mad)
      outlier_lon  <- abs(x[[lon_col]] - lon_med) > (lon_mad_mult * lon_mad)
      outlier_lat  <- abs(x[[lat_col]] - lat_med) > (lat_mad_mult * lat_mad)
      
      # require distance outlier + at least lon or lat extreme
      gross_outlier <- outlier_dist & (outlier_lon | outlier_lat)
      
      reason <- case_when(
        gross_outlier & outlier_lon & outlier_lat ~ "distance + lon + lat",
        gross_outlier & outlier_lon ~ "distance + lon",
        gross_outlier & outlier_lat ~ "distance + lat",
        TRUE ~ NA_character_
      )
      
      x %>%
        mutate(
          centroid_lon = cen_lon,
          centroid_lat = cen_lat,
          dist_centroid = dist_vec,
          outlier_dist = outlier_dist,
          outlier_lon = outlier_lon,
          outlier_lat = outlier_lat,
          gross_outlier = gross_outlier,
          outlier_reason = reason
        )
    }) %>%
    ungroup()
}

master_flagged <- flag_species_outliers(
  dat = master_dat_march,
  species_col = "species",
  lon_col = "longitude",
  lat_col = "latitude",
  min_n = 8,
  mad_mult = 4,
  lon_mad_mult = 4,
  lat_mad_mult = 4
)

gross_outliers_tbl <- master_flagged %>%
  filter(gross_outlier) %>%
  select(species, longitude, latitude, dist_centroid, outlier_reason)

print(gross_outliers_tbl, n = Inf)

outlier_summary <- master_flagged %>%
  group_by(species) %>%
  summarise(
    n_records = n(),
    n_outliers = sum(gross_outlier, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_outliers > 0) %>%
  arrange(desc(n_outliers), species)

print(outlier_summary, n = Inf)


plot_species_outlier_check <- function(dat_flagged, sp_name) {
  dat_flagged %>%
    filter(species == sp_name, !is.na(longitude), !is.na(latitude)) %>%
    ggplot(aes(x = longitude, y = latitude)) +
    geom_point(aes(colour = gross_outlier), size = 2, alpha = 0.8) +
    scale_colour_manual(values = c(`FALSE` = "grey40", `TRUE` = "red")) +
    coord_cartesian(xlim = c(10, 45), ylim = c(-40, -20), expand = FALSE) +
    theme_classic() +
    labs(
      title = sp_name,
      colour = "Gross outlier"
    )
}

plot_species_outlier_check(master_flagged, "Chelidonichthys queketti")
