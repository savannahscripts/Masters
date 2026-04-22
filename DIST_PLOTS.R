
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

#SPATIAL FILES
world <- ne_countries(scale = "medium", returnclass = "sf")
sa_map_proj <- world %>% filter(admin == "South Africa")
#load shp files
eez <- st_read("MAPPING/EEZ/eez_v12.shp", quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(4326)
summary(eez)
names(eez)

sa_eez <- eez %>%
  filter(ISO_SOV1 == "ZAF" | ISO_SOV2 == "ZAF" | ISO_SOV3 == "ZAF") %>%
  st_make_valid()

st_bbox(sa_eez)

#zeidae: zeus faber p270
#Carangidae: Caranx ignobilis p525
#muraenidae: Gymnothorax favagineus p 398. 
#labridae: Thalassoma lunare p287
#Pomacanthidae: Pomacanthus imperator p235

#Epinephilidae: epinephelus andersoni p195
#Chaetodontidae: chaetodon marleyi p236
#Apogonidae; Cheilodipterus quinquelineatus p205
#Triglidae; Chelidonichthys queketti (Trigla queketti) p383
#Priacanthidae; cookeolus japonicus (priacanthus boops) p184
#Gobiidae: Sufflogobius bibarbatus (gobius bibarbatus) p337
#lutjanidae: Pristipomoides argyrogrammicus p 252

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

range_summary <- master_dat %>%
  filter(species %in% species_list) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  group_by(species) %>%
  summarise(
    n_records = n(),
    lon_min = min(longitude),
    lon_max = max(longitude),
    lat_min = min(latitude),
    lat_max = max(latitude),
    .groups = "drop"
  )

range_summary


gap_summary <- master_dat %>%
  filter(species %in% species_list) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  arrange(species, longitude) %>%
  group_by(species) %>%
  mutate(
    lon_gap_km = c(NA, diff(longitude)) * 111  # approx km per degree
  ) %>%
  summarise(
    max_gap_km = max(lon_gap_km, na.rm = TRUE),
    mean_gap_km = mean(lon_gap_km, na.rm = TRUE),
    n_large_gaps = sum(lon_gap_km > 100, na.rm = TRUE),
    .groups = "drop"
  )

gap_summary


distribution_class <- gap_summary %>%
  mutate(
    distribution_type = case_when(
      max_gap_km < 100 ~ "Continuous",
      n_large_gaps <= 2 ~ "Weakly fragmented",
      TRUE ~ "Fragmented"
    )
  )

distribution_class

final_dist_summary <- range_summary %>%
  left_join(distribution_class, by = "species") %>%
  arrange(species)

print(final_dist_summary, n= Inf)

#most westward and eastward point

extreme_points <- master_dat %>%
  filter(species %in% species_list) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  group_by(species) %>%
  summarise(
    # Westernmost point
    west_lon = longitude[which.min(longitude)],
    west_lat = latitude[which.min(longitude)],
    
    # Easternmost point
    east_lon = longitude[which.max(longitude)],
    east_lat = latitude[which.max(longitude)],
    
    n_records = n(),
    .groups = "drop"
  )
extreme_points_clean <- extreme_points %>%
  mutate(
    west_lon = round(west_lon, 2),
    west_lat = round(west_lat, 2),
    east_lon = round(east_lon, 2),
    east_lat = round(east_lat, 2)
  )

extreme_points_clean 
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


#_________________________________________________________________________________
#FISHBASE
#HSPEC modelled potential based on habitat suitability
fishbase_HSPEC_ranges <- tibble::tribble(
  ~species, ~source, ~lon_min, ~lon_max, ~lat_min, ~lat_max,
  "Austroglossus pectoralis", "Observed", 17.25, 32.75, -35.75, -25.75, #C n=110
  "Lithognathus lithognathus","Observed", 13.75, 33.75, -30.75, -22.25, #C n=66
  "Petrus rupestris",          "Observed", 14.75, 32.25, -33.75, -28.25, #C n=45
  "Cheilodactylus fasciatus", "Observed", 11.25, 31.25, -36.75, -13.75, #CR n=147
  "Chorisochismus dentex",    "Observed", 11.75, 58.75, -34.75, 20.75, #CR n =135
  "Clinus cottoides",         "Observed", -12.75, 29.75, -40.25, -9.75, #CR n =79 #implausible keep in mind
  "Chrysoblephus gibbiceps",  "Observed", 14.25, 33.75, -34.75, -22.25, #T n=94
  "Argyrosomus inodorus",      "Observed", 11.75, 27.75, -36.25, -17.25, #T n=76
  "Polysteganus undulosus",   "Observed", 31.02, 32.89, -29.88, -25.97, #T X (SAY NONE REMOVE THESE HERE)
  "Pachymetopon aeneum",      "Observed", 14.75, 43.75, -33.75, -28.25, #E n=35
  "Sparodon durbanensis",     "Observed", 11.75, 76.75, -36.25, -30.25, #E n=487
  "Spondyliosoma emarginatum","Observed", 17.25, 50.25, -34.75, -10.25 #E n = 226
)
#_________________________________________________________________________________
#fishbase native
#"Austroglossus pectoralis": Southeast Atlantic: known only from the Cape to Natal in South Africa.
#"Petrus rupestris":Southeast Atlantic: Mossel Bay to Natal in South Africa.
#"Lithognathus lithognathus": Southeast Atlantic: known only from the Orange River mouth to Natal, South Africa.
 
#"Polysteganus undulosus" Western Indian Ocean: southern Mozambique to Durban, South Africa.
#"Chrysoblephus gibbiceps" Southeast Atlantic: known only from the Cape to Natal in South Africa.
#"Argyrosomus inodorus" Southeast Atlantic: Namibia southwards around the Cape of Good Hope and northwards at least as far as the Kei River in South Africa.

#Cheilodactylus fasciatus: Southeast Atlantic: Namibia to Natal, South Africa.
#Clinus cottoides: Southeast Atlantic: South Africa.
#"Chorisochismus dentex":Southeast Atlantic: Namibia (Ref. 27121), and from Port Nolloth to northern Natal in South Africa
#(REF= Bianchi, G., K.E. Carpenter, J.-P. Roux, F.J. Molloy, D. Boyer and H.J. Boyer, 1999. FAO species identification guide for fishery purposes. Field guide to the living marine resources of Namibia. FAO, Rome. 265p.)

#"Spondyliosoma emarginatum":Western Indian Ocean: South Africa and southern Madagascar.
#"Sparodon durbanensis", Southeast Atlantic: Cape of Good Hope to Natal in South Africa. Also recorded from Djibouti (Ref. 5450) and from Mozambique (Ref. 5213)..
#REF 5450 Bouhlel, M., 1988. Poissons de Djibouti. Placerville (California, USA): RDA International, Inc. 416 p.
#REF 5213 Fischer, W., I. Sousa, C. Silva, A. de Freitas, J.M. Poutiers, W. Schneider, T.C. Borges, J.P. Feral and A. Massinga, 1990. Fichas FAO de identificaçao de espécies para actividades de pesca. Guia de campo das espécies comerciais marinhas e de águas salobras de Moçambique. Publicaçao preparada em collaboraçao com o Instituto de Investigaçao Pesquiera de Moçambique, com financiamento do Projecto PNUD/FAO MOZ/86/030 e de NORAD. Roma, FAO. 1990. 424 p.
#"Pachymetopon aeneum": Western Indian Ocean: known only from Sodwana Bay to the Cape in South Africa.
#_________________________________________________________________________________
#SMITHS

#Smiths' Sea fishes by Smith, J. L. B. (James Leonard Brierley), 1897-1968 (online copy) and my copy
#"Austroglossus pectoralis": Found only in South Africafrom the Cape to Natal
#"Petrus rupestris": from mossel bay to natal
#"Lithognathus lithognathus" found only in south africa, on all our coasts, prefers sandyareas. 

#"Polysteganus undulosus" found only in south africa, from the cape to delagoa bay
#"Chrysoblephus gibbiceps"found in south africa from the cape to east london, rarely in natal waters,
#"Argyrosomus inodorus" (referred to in Smith's as Argyrosomus hololepidotus): West coast of Africa, (south of equator) to the cape and along our coast to at least maputo, also reported from madagascar, mauritius. india and australia

#"Cheilodactylus fasciatus: Found only in our land,from Port Nolloth to Durban, 
#"Clinus cottoides": from port nolloth to the kei river, in shallow water,
#"Chorisochismus dentex": found only in south africa, cape to natal

#"Spondyliosoma emarginatum": found only in south africa from saldanha bay round to madagascar ,
#"Sparodon durbanensis",: found only in south africa. From the cape to natal. 
#"Pachymetopon aeneum": found only in south africa,in rather deep water, from the cape to natal, 
#_________________________________________________________________________________
#WIO
#"Austroglossus pectoralis": Endemic to southern Africa: South Africa (Cape Peninsula) to southern Mozambique in WIO.
#"Petrus rupestris": WIO endemic to south africa (st Lucia to false Bay)
#"Lithognathus lithognathus": South Africa: Orange River mouth in southeastern Atlantic, to KwaZulu-Natal in WIO. 

#"Polysteganus undulosus":WIO: southern Mozambique to South Africa (Knysna, Western cape) 
#"Chrysoblephus gibbiceps" WIO: endemic to South Africa (Margate, KwaZulu-Natal [rarely] to False Bay; most common southwest of East London).
#"Argyrosomus inodorus": Southern Africa: Namibia in southeastern Atlantic, to South Africa (south coast, at least to Kei River mouth, Eastern Cape) in WIO.

#"Cheilodactylus fasciatus: Southern Africa: Namibia (Swakopmund) in southeastern Atlantic, to South Africa (Durban, KwaZulu- Natal) in WIO. 
#"Clinus cottoides": South Africa: Olifants River (west coast) in southeastern Atlantic, to Kei River mouth (Eastern Cape) in WIO. particularly abundant east of False Bay. 
#"Chorisochismus dentex": Southern Africa: Namibia in southeastern Atlantic, to South Africa (northern KwaZulu-Natal) in WIO.

#"Spondyliosoma emarginatum": Endemic to South Africa: Saldanha Bay in southeastern Atlantic, to KwaZulu-Natal in WIO (plentiful on the southern Cape and southeastern coasts).
#"Sparodon durbanensis": Southern Africa: South Africa (St Helena Bay) in southeastern Atlantic, to southern Mozambique in WIO.
#"Pachymetopon aeneum":endemic to south africa, kwazulu natal to cape point, inhabits rocky areas in shallow waters
#_________________________________________________________________________________


#zeidae: zeus faber p144
#S not specified
#W Mozambique to South Africa (False Bay),Madagascar
#F namibian border to mozambican border (whole of south africa)

#Carangidae: Caranx ignobilis p217
#S natal and maputo bay
#W  South Africa (AlgoaBay), Madagascar
#F east coast of south africa

#muraenidae: Gymnothorax favagineus p 398. 
#S port alfred to natal
#W Gulf of Oman to South Africa (Algoa Bay),
#F east coast of south africa

#labridae: Thalassoma lunare p287
#S natal and east london
#W Tanzania to South Africa (Transkei region),
#F east coast of south africa

#Pomacanthidae: Pomacanthus imperator p235
#S delgoa bay
#W  South Africa (Aliwal Shoal; juveniles to East London, Eastern Cape), Madagascar,
#F east coast of south africa

#Epinephilidae: epinephelus andersoni p195
#S knysna to maputo bay
#W southern Mozambique (Quissico) to South Africa (Knysna, Western Cape)
#F southeast coast of Africa

#Chaetodontidae: chaetodon marleyi p236
#S cape to lamberts bay and madagascar
#W southern Mozambique (Maputo) to South Africa (Lambert’s Bay, Western Cape) and southeastern Madagascar.
#F lamberts bay to mozambique

#Triglidae; Chelidonichthys queketti (Trigla queketti) p383
#S cape to natal
#W Southern Africa: Namibia (Walvis Bay) in southeastern Atlantic, to South Africa (KwaZulu-Natal)
#F table bay southern mozambique

#Priacanthidae; cookeolus japonicus (priacanthus boops) p184
#S algoa bay to beira
#W Mozambique (Beira) to South Africa (Algoa Bay,Eastern Cape), 
#F namibian border to mozambican border (whole of south africa)

#Gobiidae: Sufflogobius bibarbatus 
#S st helena bay and st sebastian bay (gobius bibarbatus) p337
#W /
#F north namibian border to mozambican border (whole of south africa and namibia)


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

# 3) resolve anchors (NO type column) --------------------------------------------------

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
  "Austroglossus pectoralis", "FishBase_native", "from the cape to kwazulu-natal",
  "Petrus rupestris",         "FishBase_native", "from mossel bay to kwazulu-natal",
  "Lithognathus lithognathus","FishBase_native", "from orange river mouth to the cape and to kwazulu-natal",
  "Polysteganus undulosus",   "FishBase_native", "from southern mozambique to durban",
  "Chrysoblephus gibbiceps",  "FishBase_native", "from the cape to kwazulu-natal",
  "Argyrosomus inodorus",     "FishBase_native", "from south namibian border to cape point and to kei river mouth",
  "Cheilodactylus fasciatus", "FishBase_native", "from south namibian border to cape point and to mozambican border",
  "Clinus cottoides",         "FishBase_native", "from south namibian border to cape point and to mozambican border", #borders for south africa
  "Chorisochismus dentex",    "FishBase_native", "from south namibian border to cape point and to northern kwazulu-natal",
  "Spondyliosoma emarginatum","FishBase_native", "from south namibian border to cape point and to southern madagascar", #borders for south africa
  "Sparodon durbanensis",     "FishBase_native", "from cape of good hope to southern mozambique", 
  "Pachymetopon aeneum",      "FishBase_native", "from sodwana bay to the cape",
  "Zeus faber",               "FishBase_native", "from south namibian border to cape point and to mozambican border",
  "Caranx ignobilis",         "FishBase_native", "from port elizabeth to mozambican border", #east coast of sa
  "Gymnothorax favagineus",   "FishBase_native", "from port elizabeth to mozambican border",#east coast of sa,
  "Thalassoma lunare",        "FishBase_native", "from port elizabeth to mozambican border",#east coast of sa,
  "Pomacanthus imperator",    "FishBase_native", "from port elizabeth to mozambican border",#east coast of sa,
  "Epinephelus andersoni",    "FishBase_native", "from cape agulhas to mozambican border",#southeast  coast of sa,
  "Chaetodon marleyi",        "FishBase_native", "from lamberts bay to southern mozambique",
  "Chelidonichthys queketti", "FishBase_native", "from table bay to southern mozambique",
  "Cookeolus japonicus",      "FishBase_native", "from south namibian border to cape point and to mozambican border", #borders for south africa
  "Sufflogobius bibarbatus",  "FishBase_native", "from north namibian border to cape point and to mozambican border", #borders for south africa #,
  
  "Austroglossus pectoralis", "Smiths", "from the cape to kwazulu-natal",
  "Petrus rupestris",         "Smiths", "from mossel bay to kwazulu-natal",
  "Lithognathus lithognathus","Smiths", "from orange river mouth to cape point and to mozambican border", #borders for south africa
  "Polysteganus undulosus",   "Smiths", "from the cape to maputo bay",
  "Chrysoblephus gibbiceps",  "Smiths", "from the cape to east london",
  "Argyrosomus inodorus",     "Smiths", "from north angola border to mauritius", 
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

# example logic: before parsing text -> coords
range_text_clean <- ranges_raw %>%
  dplyr::mutate(
    range_ok = !is.na(range_text) &
      !stringr::str_detect(stringr::str_to_lower(range_text),
                           "not found|unknown|n/a|none|no data")
  )


parsed <- range_text_clean %>%
  mutate(parsed = map(range_text, range_parse)) %>%
  unnest(parsed)

resolved <- resolve_anchors(parsed, place_lookup)

print(resolved, n = Inf)


resolved %>%
  mutate(span_deg = abs(end_lon - start_lon)) %>%
  arrange(desc(span_deg)) %>%
  select(species, source, anchor_start, anchor_end, start_lon, end_lon, span_deg) %>%
  print(n = Inf)

unresolved <- resolved %>%
  filter(
    (!is.na(anchor_start) & is.na(start_lat)) |
      (!is.na(anchor_end) & is.na(end_lat)) |
      parse_rule == "unparsed"
  ) %>%
  distinct(source, species, parse_rule, anchor_start, anchor_end, clean)

print(unresolved, n = Inf)

#n2 unresolved

extreme_pts_long <- extreme_points_clean %>%
  pivot_longer(
    cols = c(west_lon, west_lat, east_lon, east_lat),
    names_to = c("side", ".value"),
    names_pattern = "(west|east)_(lon|lat)"
  ) %>%
  mutate(
    side = factor(side, levels = c("west", "east"))
  )

#PLOTTING
range_levels <- c("Observed","Smiths","WIO","FishBase_native") ##FIX TO INCLUDE

range_cols <- c(
  "Observed"       = "#000000",  
  "Smiths"         = "palegreen",  
  "WIO"            = "lightblue", 
  "FishBase_native" = "salmon"   
)

rect_offsets <- tibble::tibble(
  source = range_levels,
  dx = c(0.00, 0.20, 0.40, 0.60),
  dy = c(0.00,-0.15,-0.30,-0.45)
)

obs_lat_extent <- master_dat %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  group_by(species) %>%
  summarise(
    obs_lat_min = min(latitude),
    obs_lat_max = max(latitude),
    .groups = "drop"
  )

ranges_text_bbox <- resolved %>%
  filter(!is.na(start_lon), !is.na(end_lon)) %>%
  mutate(
    lon_min = pmin(start_lon, end_lon),
    lon_max = pmax(start_lon, end_lon)
  ) %>%
  left_join(obs_lat_extent, by = "species") %>%
  mutate(
    lat_min = obs_lat_min,
    lat_max = obs_lat_max
  ) %>%
  select(species, source, lon_min, lon_max, lat_min, lat_max)


ranges_all <- bind_rows(
  my_ranges,          # your Observed bbox table (already has lat_min/max)
  ranges_text_bbox
) %>%
  mutate(source = factor(trimws(source), levels = range_levels))


# Make 4-corner polygons from lon/lat bbox
bbox_to_poly_df <- function(ranges_bbox, rect_offsets = NULL) {
  out <- ranges_bbox %>%
    mutate(
      range_id = paste(species, source, row_number(), sep = "||")
    )
  
  # join offsets (optional)
  if (!is.null(rect_offsets)) {
    out <- out %>%
      left_join(rect_offsets, by = "source") %>%
      mutate(
        dx = coalesce(dx, 0),
        dy = coalesce(dy, 0)
      )
  } else {
    out <- out %>% mutate(dx = 0, dy = 0)
  }
  
  # Build polygon ring: (xmin,ymin) -> (xmin,ymax) -> (xmax,ymax) -> (xmax,ymin) -> back to start
  out %>%
    transmute(
      species, source, range_id,
      xmin = lon_min + dx, xmax = lon_max + dx,
      ymin = lat_min + dy, ymax = lat_max + dy
    ) %>%
    tidyr::uncount(weights = 5, .id = "vertex") %>%
    mutate(
      lon = case_when(
        vertex == 1 ~ xmin,
        vertex == 2 ~ xmin,
        vertex == 3 ~ xmax,
        vertex == 4 ~ xmax,
        vertex == 5 ~ xmin
      ),
      lat = case_when(
        vertex == 1 ~ ymin,
        vertex == 2 ~ ymax,
        vertex == 3 ~ ymax,
        vertex == 4 ~ ymin,
        vertex == 5 ~ ymin
      )
    ) %>%
    select(species, source, range_id, vertex, lon, lat)
}

# Build polygon ranges from your bbox table
ranges_poly <- bbox_to_poly_df(
  ranges_bbox = ranges_all %>% 
    filter(!is.na(lon_min), !is.na(lon_max), !is.na(lat_min), !is.na(lat_max)),
  rect_offsets = rect_offsets
)


deg_lon <- function(x) paste0(abs(x), "°", ifelse(x < 0, "W", "E"))
deg_lat <- function(x) paste0(abs(x), "°", ifelse(x < 0, "S", "N"))

sa_map_df <- sa_map %>%
  sf::st_make_valid() %>%
  sf::st_cast("MULTIPOLYGON") %>%
  sf::st_coordinates() %>%
  as_tibble() %>%
  mutate(group = interaction(L1, L2, L3, drop = TRUE))

plot_species_compare_sources_poly <- function(
    sp_name,
    map_df = sa_map_df,
    lat_lim = c(-45, -15),
    lon_lim = c(10, 45),
    x_breaks = seq(10, 40, by = 5),
    y_breaks = seq(-45, -15, by = 5)
) {
  
  sp_dat <- master_dat %>%
    filter(species == sp_name) %>%
    filter(!is.na(longitude), !is.na(latitude))
  
  sp_ext <- extreme_pts_long %>% filter(species == sp_name)
  
  sp_ext <- extreme_pts_long %>%
    dplyr::filter(species == sp_name) %>%
    dplyr::mutate(
      side = tolower(trimws(as.character(side))),
      side = dplyr::case_when(
        side %in% c("w", "west", "western", "westernmost", "left") ~ "west",
        side %in% c("e", "east", "eastern", "easternmost", "right") ~ "east",
        TRUE ~ side
      ),
      side = factor(side, levels = c("west", "east"))
    )
  
  sp_rng_poly <- ranges_poly %>%
    filter(species == sp_name) %>%
    mutate(source = factor(source, levels = range_levels))
  
  if (nrow(sp_dat) == 0) {
    message("No records found for: ", sp_name)
    return(NULL)
  }
  
  p <- ggplot() +
    geom_polygon(
      data = map_df,
      aes(x = X, y = Y, group = group),
      fill = "grey90",
      color = "black",
      linewidth = 0.2
    ) +
    geom_polygon(
      data = sp_rng_poly,
      aes(x = lon, y = lat, group = range_id, colour = source),
      fill = NA,
      linewidth = 1
    ) +
    scale_colour_manual(values = range_cols, limits = range_levels, drop = FALSE) +
    geom_point(
      data = sp_dat,
      aes(x = longitude, y = latitude),
      color = "grey30",
      size = 0.6,
      alpha = 0.45
    ) +
    coord_cartesian(xlim = lon_lim, ylim = lat_lim, expand = FALSE) +
    scale_x_continuous(breaks = x_breaks, labels = deg_lon(x_breaks), name = "Longitude") +
    scale_y_continuous(breaks = y_breaks, labels = deg_lat(y_breaks), name = "Latitude") +
    labs(title = sp_name, colour = "Source range") +
    theme_classic(base_family = "Times New Roman") +
    theme(
      text = element_text(family = "Times New Roman"),
      axis.title = element_text(size = 11),
      axis.text  = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.text  = element_text(size = 9),
      plot.title   = element_text(size = 12, face = "italic")
    )
  
  if (nrow(sp_rng_poly) == 0) message("No range polygons in ranges_poly for: ", sp_name)
  
  if (nrow(sp_rng_poly) > 0) {
    bad <- sp_rng_poly %>% dplyr::summarise(all_na = all(is.na(lon) | is.na(lat))) %>% dplyr::pull(all_na)
    if (isTRUE(bad)) message("Range polygons exist but lon/lat are NA for: ", sp_name)
  }
  
  # ONLY add extremes + shape scale if extremes exist
  if (nrow(sp_ext) > 0) {
    p <- p +
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
        breaks = c("west", "east"),
        labels = c("Westernmost record", "Easternmost record")
      ) +
      labs(shape = "Observed extremes")
  }
  
  p
}

#PLOTS

plot_species_compare_sources_poly("Lithognathus lithognathus") #sparidae
plot_species_compare_sources_poly("Chrysoblephus gibbiceps") #sparidae
plot_species_compare_sources_poly("Pachymetopon aeneum") #sparidae
plot_species_compare_sources_poly("Spondyliosoma emarginatum") #sparidae
plot_species_compare_sources_poly("Petrus rupestris") #sparidae
plot_species_compare_sources_poly("Polysteganus undulosus") #sparidae
plot_species_compare_sources_poly("Argyrosomus inodorus") #Sciaenidae
plot_species_compare_sources_poly("Sparodon durbanensis") #sparidae
plot_species_compare_sources_poly("Austroglossus pectoralis") #soleidae
plot_species_compare_sources_poly("Zeus faber")
plot_species_compare_sources_poly("Clinus cottoides") #Clinidae
plot_species_compare_sources_poly("Chorisochismus dentex") #Gobiesocidae
plot_species_compare_sources_poly("Cheilodactylus fasciatus") #Cheilodactylidae
plot_species_compare_sources_poly("Caranx ignobilis")
plot_species_compare_sources_poly("Gymnothorax favagineus")
plot_species_compare_sources_poly("Thalassoma lunare")
plot_species_compare_sources_poly("Pomacanthus imperator")
plot_species_compare_sources_poly("Chaetodon marleyi")
plot_species_compare_sources_poly("Chelidonichthys queketti")
plot_species_compare_sources_poly("Cookeolus japonicus")
plot_species_compare_sources_poly("Sufflogobius bibarbatus")
plot_species_compare_sources_poly("Epinephelus andersoni")

#####
library(dplyr)
library(tidyr)
library(sf)

# ---------------------------
# 1) Build final bbox table
# ---------------------------

ranges_bbox <- bind_rows(
  my_ranges,        # Observed (must include lon_min/lon_max/lat_min/lat_max)
  ranges_text_bbox  # Smiths/WIO/FishBase_native
) %>%
  mutate(source = trimws(source)) %>%
  mutate(source = factor(source, levels = range_levels))

# longitude degrees -> km at representative latitude
deg_lon_to_km <- function(deg, lat) {
  abs(deg) * 111.32 * cos(lat * pi/180)
}

# choose which sources you want to compare against Observed
compare_sources <- c("Smiths", "WIO", "FishBase_native")

# wide table: one row per species with Observed and each reference bbox
range_wide <- ranges_bbox %>%
  group_by(species) %>%
  mutate(
    # representative latitude for lon->km conversion (same approach as you used)
    lat_ref = mean(c(lat_min, lat_max), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(species, source, lon_min, lon_max, lat_ref) %>%
  pivot_wider(
    names_from  = source,
    values_from = c(lon_min, lon_max),
    names_sep   = "__"
  )

# compute extension metrics for one source vs Observed (degrees; km later)
compute_ext_lon <- function(df, src) {
  df %>%
    transmute(
      species,
      source_group = src,
      
      obs_min = .data[[paste0("lon_min__Observed")]],
      obs_max = .data[[paste0("lon_max__Observed")]],
      src_min = .data[[paste0("lon_min__", src)]],
      src_max = .data[[paste0("lon_max__", src)]],
      lat_ref,
      
      obs_span = obs_max - obs_min,
      src_span = src_max - src_min,
      
      overlap_span = pmax(0, pmin(obs_max, src_max) - pmax(obs_min, src_min))
    )
}

range_extension_tbl <- bind_rows(lapply(compare_sources, \(s) compute_ext_lon(range_wide, s))) %>%
  arrange(species, factor(source_group, levels = compare_sources))

# -----------------------------------------
# 2) Clip reference longitudes to SA EEZ bbox
# -----------------------------------------

sa_eez <- eez %>%
  filter(ISO_SOV1 == "ZAF" | ISO_SOV2 == "ZAF" | ISO_SOV3 == "ZAF") %>%
  st_make_valid()

bb <- st_bbox(sa_eez)
SA_LON_MIN <- bb["xmin"]
SA_LON_MAX <- bb["xmax"]

# -------------------------------------------------------
# 3) East–west extension metrics (Observed vs Reference)
#    using your updated “N/S and E/W” style:
#    - obs_ext_* : observed extends beyond (west/east)
#    - src_excess_* : reference exceeds observed (west/east)
# -------------------------------------------------------

range_extension_tbl_SA <- range_extension_tbl %>%
  mutate(
    # clip reference to SA bbox (lon only)
    src_min_SA  = pmax(src_min, SA_LON_MIN),
    src_max_SA  = pmin(src_max, SA_LON_MAX),
    src_span_SA = src_max_SA - src_min_SA,
    
    # overlap in SA-clipped frame
    overlap_span_SA = pmax(
      0,
      pmin(obs_max, src_max_SA) - pmax(obs_min, src_min_SA)
    ),
    
    # ---- observed extends beyond reference (EXTENSION) ----
    # west extension: observed goes further west (smaller lon) than reference start
    obs_ext_west_deg  = pmax(0, src_min_SA - obs_min),
    # east extension: observed goes further east (larger lon) than reference end
    obs_ext_east_deg  = pmax(0, obs_max - src_max_SA),
    obs_ext_total_deg = obs_ext_west_deg + obs_ext_east_deg,
    
    obs_ext_west_km   = deg_lon_to_km(obs_ext_west_deg,  lat_ref),
    obs_ext_east_km   = deg_lon_to_km(obs_ext_east_deg,  lat_ref),
    obs_ext_total_km  = deg_lon_to_km(obs_ext_total_deg, lat_ref),
    
    # ---- reference exceeds observed (EXCESS) ----
    src_excess_west_deg  = pmax(0, obs_min - src_min_SA),
    src_excess_east_deg  = pmax(0, src_max_SA - obs_max),
    src_excess_total_deg = src_excess_west_deg + src_excess_east_deg,
    
    # percent of SA-clipped reference span exceeded by observed
    pct_obs_ext_vs_source_SA =
      ifelse(src_span_SA > 0, 100 * obs_ext_total_deg / src_span_SA, NA_real_),
    
    # categorise the mismatch (and handle missing reference cleanly)
    mismatch_type = case_when(
      is.na(src_min) | is.na(src_max) ~ "Reference not stated / missing",
      obs_ext_total_deg > 0 & src_excess_total_deg == 0 ~ "Observed exceeds reference",
      obs_ext_total_deg == 0 & src_excess_total_deg > 0 ~ "Reference exceeds observed",
      obs_ext_total_deg == 0 & src_excess_total_deg == 0 ~ "Perfectly contained",
      TRUE ~ "Partial overlap (both exceed)"
    ),
    
    # direction label (E/W) for observed extension
    obs_ext_direction = case_when(
      obs_ext_west_deg > 0 & obs_ext_east_deg > 0 ~ "Both",
      obs_ext_west_deg > 0 ~ "West",
      obs_ext_east_deg > 0 ~ "East",
      TRUE ~ "None"
    )
  )


######

range_refinement_lat <- range_ext_lat %>%
  select(
    species,
    source_group,
    extension_km = obs_ext_total_km
  ) %>%
  mutate(axis = "North–south")


range_refinement_lon <- range_extension_tbl_SA %>%
  select(
    species,
    source_group,
    extension_km = obs_ext_total_km
  ) %>%
  mutate(axis = "East–west")

print(range_refinement_plotdat, n =Inf)
range_refinement_plotdat <- bind_rows(
  range_refinement_lon,
  range_refinement_lat
) %>%
  mutate(
    axis = factor(axis, levels = c("East–west", "North–south"))
  )


species_order <- range_refinement_plotdat %>%
  group_by(species) %>%
  summarise(max_ext = max(extension_km, na.rm = TRUE)) %>%
  arrange(max_ext) %>%
  pull(species)

range_refinement_plotdat <- range_refinement_plotdat %>%
  mutate(species = factor(species, levels = species_order))

zoom_limits <- tibble::tibble(
  axis = c("East–west", "North–south"),
  zoom_km = c(2000, 100)   # <-- change as needed
)

plotdat <- range_refinement_plotdat %>%
  left_join(zoom_limits, by = "axis") %>%
  mutate(
    is_outlier = !is.na(extension_km) & extension_km > zoom_km
  )

outliers <- plotdat %>%
  filter(is_outlier) %>%
  group_by(axis, species) %>%
  summarise(
    max_km = max(extension_km, na.rm = TRUE),
    zoom_km = first(zoom_km),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(">", zoom_km, " km (", round(max_km), ")"),
    x = zoom_km  # place label at the right edge of the zoomed axis
  )


################

deg_lon <- function(x) paste0(abs(x), "°", ifelse(x < 0, "W", "E"))
deg_lat <- function(x) paste0(abs(x), "°", ifelse(x < 0, "S", "N"))

range_levels <- c("Observed", "Smiths", "WIO", "FishBase_native")

range_cols <- c(
  "Observed"        = "#000000",
  "Smiths"          = "palegreen",
  "WIO"             = "lightblue",
  "FishBase_native" = "salmon"
)


# Observed lat/lon extents from your data
obs_extent <- master_dat %>%
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

# Text-derived lon extents + use observed lat extents for the bbox height
obs_lat_extent <- master_dat %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  group_by(species) %>%
  summarise(
    lat_min = min(latitude),
    lat_max = max(latitude),
    .groups = "drop"
  )

text_bbox <- resolved %>%
  filter(!is.na(start_lon), !is.na(end_lon)) %>%
  mutate(
    lon_min = pmin(start_lon, end_lon),
    lon_max = pmax(start_lon, end_lon)
  ) %>%
  left_join(obs_lat_extent, by = "species") %>%
  select(species, source, lon_min, lon_max, lat_min, lat_max)

# Combined bboxes
ranges_bbox <- bind_rows(
  obs_extent %>% select(species, source, lon_min, lon_max, lat_min, lat_max),
  text_bbox
) %>%
  mutate(
    source = factor(trimws(source), levels = range_levels)
  )

bbox_to_poly_df <- function(bbox_df) {
  bbox_df %>%
    filter(!is.na(lon_min), !is.na(lon_max), !is.na(lat_min), !is.na(lat_max)) %>%
    mutate(range_id = paste(species, source, row_number(), sep = "||")) %>%
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

ranges_poly <- bbox_to_poly_df(ranges_bbox)

sa_map_df <- sa_map %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>%
  st_coordinates() %>%
  as_tibble() %>%
  mutate(group = interaction(L1, L2, L3, drop = TRUE))


# ALLL SPECIES

plot_species_rangebars_3panel <- function(
    sp_name,
    master_dat,
    ranges_bbox,
    ranges_poly,
    extreme_pts_long,
    sa_map_df,
    range_levels = c("Observed","Smiths","WIO","FishBase_native"),
    range_cols,
    lat_lim = c(-45, -15),
    lon_lim = c(10, 45),
    x_breaks = seq(10, 40, by = 5),
    y_breaks = seq(-45, -15, by = 5)
) {
  
  # --- points
  sp_pts <- master_dat %>%
    filter(species == sp_name) %>%
    filter(!is.na(longitude), !is.na(latitude))
  
  if (nrow(sp_pts) == 0) {
    message("No points for: ", sp_name)
    return(NULL)
  }
  
  # --- bboxes / rectangles
  sp_bbox <- ranges_bbox %>%
    filter(species == sp_name) %>%
    mutate(source = factor(as.character(source), levels = range_levels))
  
  sp_rect <- ranges_poly %>%
    filter(species == sp_name) %>%
    mutate(source = factor(as.character(source), levels = range_levels))
  
  # --- extremes (E/W)
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
  
  # ---------------- LEFT: MAP ----------------
  p_map <- ggplot() +
    geom_polygon(
      data = sa_map_df,
      aes(x = X, y = Y, group = group),
      fill = "grey90",
      color = "black",
      linewidth = 0.2
    ) +
    geom_polygon(
      data = sp_rect,
      aes(x = lon, y = lat, group = range_id, colour = source),
      fill = NA,
      linewidth = 0.7,
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
    theme_classic(base_family = "Times New Roman") +
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
      label = "Rectangles are schematic envelopes\nbased on reported longitudinal limits",
      hjust = 0, size = 3, family = "Times New Roman"
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
  
  # ---------------- MIDDLE: LAT RANGE (Observed only) ----------------
  sp_lat <- sp_pts %>%
    summarise(lat_min = min(latitude), lat_max = max(latitude)) %>%
    mutate(x = 1)
  
  p_lat <- ggplot(sp_lat) +
    geom_segment(aes(x = 1, xend = 1, y = lat_min, yend = lat_max),
                 linewidth = 1.8, lineend = "round", colour = "black") +
    geom_point(aes(x = 1, y = lat_min), size = 2.4, colour = "black") +
    geom_point(aes(x = 1, y = lat_max), size = 2.4, colour = "black") +
    coord_cartesian(ylim = lat_lim, expand = FALSE) +
    scale_y_continuous(breaks = y_breaks, labels = deg_lat(y_breaks), name = "Latitude (observed)") +
    scale_x_continuous(breaks = NULL, name = NULL) +
    theme_classic(base_family = "Times New Roman") +
    theme(
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(size = 11),
      axis.text.y  = element_text(size = 10),
      plot.title   = element_text(size = 11),
      plot.margin  = margin(5, 5, 5, 5)
    ) +
    labs(title = "Latitudinal range")
  
  # ---------------- RIGHT: LON RANGES (numeric y to avoid squish) ----------------
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
    scale_x_continuous(breaks = x_breaks, labels = deg_lon(x_breaks), name = "Longitude (range envelope)") +
    scale_y_continuous(
      breaks = seq_along(range_levels),
      labels = range_levels,
      limits = c(0.5, length(range_levels) + 0.5),
      expand = c(0, 0),
      name = NULL
    ) +
    coord_cartesian(xlim = lon_lim, expand = FALSE) +
    theme_classic(base_family = "Times New Roman") +
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
  
  # ---------------- COMBINE ----------------
  (p_map | p_lat | p_lon) + plot_layout(widths = c(1.25, 0.45, 1.05))
}

species_list <- c(
  "Lithognathus lithognathus",
  "Chrysoblephus gibbiceps",
  "Pachymetopon aeneum",
  "Spondyliosoma emarginatum",
  "Petrus rupestris",
  "Polysteganus undulosus",
  "Sparodon durbanensis",
  "Austroglossus pectoralis",
  "Argyrosomus inodorus",
  "Cheilodactylus fasciatus",
  "Clinus cottoides",
  "Chorisochismus dentex",
  "Zeus faber",
  "Caranx ignobilis",
  "Gymnothorax favagineus",
  "Thalassoma lunare",
  "Pomacanthus imperator",
  "Epinephelus andersoni",
  "Chaetodon marleyi",
  "Chelidonichthys queketti",
  "Cookeolus japonicus",
  "Sufflogobius bibarbatus"
)

p <- plot_species_rangebars_3panel(
  sp_name = "Sufflogobius bibarbatus",
  master_dat = master_dat,
  ranges_bbox = ranges_bbox,
  ranges_poly = ranges_poly,
  extreme_pts_long = extreme_pts_long,
  sa_map_df = sa_map_df,
  range_cols = range_cols
)

p



# -----------------------------------------
# print
# -----------------------------------------

print(range_extension_tbl_SA, n = Inf, width = Inf)

#NORTH SOUTH FINAL
zoom_km <- 100 # pick what makes your “bulk” visible (300–800 works well)

ggplot(range_ext_lat,
       aes(x = obs_ext_total_km,
           y = reorder(species, obs_ext_total_km),
           colour = source_group)) +
  geom_point(position = position_jitter(width = 10, height = 0),
             size = 3, alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_x_continuous(
    limits = c(0, zoom_km),
    oob = squish,                       # <-- outliers get pinned to the edge
    breaks = seq(0, zoom_km, by = 100)
  ) +
  geom_text(
    data = subset(range_ext_lat, obs_ext_total_km > zoom_km),
    aes(label = paste0(">>>", zoom_km, " km (", round(obs_ext_total_km), ")")),
    hjust = -0.05, size = 3, colour = "black", inherit.aes = FALSE,
    x = zoom_km, y = reorder(subset(range_ext_lat, obs_ext_total_km > zoom_km)$species,
                             subset(range_ext_lat, obs_ext_total_km > zoom_km)$obs_ext_total_km)
  ) +
  labs(
    x = paste0("Latitudinal range extension beyond source (km)  (zoomed to 0–", zoom_km, " km; outliers squished)"),
    y = NULL,
    colour = "Reference source",
    title = "North–south range differences revealed by data integration"
  ) +
  theme_minimal(base_size = 11, base_family = "Times") +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )

range_ext_lat %>%
  dplyr::filter(obs_ext_total_km > zoom_km) %>%
  dplyr::select(species, source_group, obs_ext_total_km)


#EAST WEST FINAL
zoom_km_lon <- 900  # choose: 300–800 usually works; pick what reveals the “bulk”

range_ext_lon <- range_extension_tbl_SA %>%
  mutate(
    # labels etc (optional; you already do this)
    range_signal = case_when(
      obs_ext_total_deg > 0 ~ "Observed range exceeds source",
      obs_ext_total_deg == 0 & src_excess_total_deg > 0 ~ "Source exceeds observed",
      TRUE ~ "No difference"
    ),
    ext_direction = case_when(
      obs_ext_west_deg > 0 & obs_ext_east_deg > 0 ~ "Both",
      obs_ext_west_deg > 0 ~ "West",
      obs_ext_east_deg > 0 ~ "East",
      TRUE ~ "None"
    )
  )

ggplot(range_ext_lon,
       aes(x = obs_ext_total_km,
           y = reorder(species, obs_ext_total_km),
           colour = source_group)) +
  geom_point(
    position = position_jitter(width = 10, height = 0),
    size = 3,
    alpha = 0.85
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_x_continuous(
    limits = c(0, zoom_km_lon),
    oob = squish,
    breaks = seq(0, zoom_km_lon, by = 100)
  ) +
  geom_text(
    data = subset(range_ext_lon, obs_ext_total_km > zoom_km_lon),
    aes(label = paste0(">>", zoom_km_lon, " km (", round(obs_ext_total_km), ")")),
    hjust = -0.05,
    size = 3,
    colour = "black",
    inherit.aes = FALSE,
    x = zoom_km_lon,
    y = reorder(
      subset(range_ext_lon, obs_ext_total_km > zoom_km_lon)$species,
      subset(range_ext_lon, obs_ext_total_km > zoom_km_lon)$obs_ext_total_km
    )
  ) +
  labs(
    x = paste0("Longitudinal range extension beyond source (km) (zoomed to 0–", zoom_km_lon, " km; outliers squished)"),
    y = NULL,
    colour = "Reference source",
    title = "East–west range differences revealed by data integration"
  ) +
  theme_minimal(base_size = 11, base_family = "Times") +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )



##########next
# 1A) Observed bbox from points
obs_bbox <- master_dat %>%
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

# 1B) Source bboxes from resolved anchors (TRUE lat and lon)
src_bbox <- resolved %>%
  filter(source %in% c("Smiths", "WIO", "FishBase_native")) %>%
  filter(!is.na(start_lon), !is.na(end_lon), !is.na(start_lat), !is.na(end_lat)) %>%
  mutate(
    lon_min = pmin(start_lon, end_lon),
    lon_max = pmax(start_lon, end_lon),
    lat_min = pmin(start_lat, end_lat),
    lat_max = pmax(start_lat, end_lat)
  ) %>%
  select(species, source, lon_min, lon_max, lat_min, lat_max)

# 1C) Combine
ranges_bbox <- bind_rows(obs_bbox, src_bbox) %>%
  mutate(
    source = factor(trimws(as.character(source)), levels = range_levels)
  )

bbox_to_poly_df <- function(bbox_df) {
  bbox_df %>%
    filter(!is.na(lon_min), !is.na(lon_max), !is.na(lat_min), !is.na(lat_max)) %>%
    mutate(range_id = paste(species, source, row_number(), sep = "||")) %>%
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

ranges_poly <- bbox_to_poly_df(ranges_bbox) %>%
  mutate(source = factor(as.character(source), levels = range_levels))

lat_offsets <- tibble::tibble(
  source = factor(range_levels, levels = range_levels),
  x = c(0.85, 0.95, 1.05, 1.15)
)

plot_species_rangebars_3panel <- function(
    sp_name,
    master_dat,
    ranges_bbox,
    ranges_poly,
    extreme_pts_long,
    sa_map_df,
    range_levels = c("Observed","Smiths","WIO","FishBase_native"),
    range_cols,
    lat_lim = c(-45, -15),
    lon_lim = c(10, 45),
    x_breaks = seq(10, 40, by = 5),
    y_breaks = seq(-45, -15, by = 5)
) {
  
  # Points (observed)
  sp_pts <- master_dat %>%
    filter(species == sp_name) %>%
    filter(!is.na(longitude), !is.na(latitude))
  
  if (nrow(sp_pts) == 0) {
    message("No points for: ", sp_name)
    return(NULL)
  }
  
  # Source bboxes for this species
  sp_bbox <- ranges_bbox %>%
    filter(species == sp_name) %>%
    mutate(source = factor(as.character(source), levels = range_levels))
  
  # Rectangles for this species
  sp_rect <- ranges_poly %>%
    filter(species == sp_name) %>%
    mutate(source = factor(as.character(source), levels = range_levels))
  
  # Observed extremes
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
  
  # ---------------- LEFT: MAP ----------------
  p_map <- ggplot() +
    geom_polygon(
      data = sa_map_df,
      aes(x = X, y = Y, group = group),
      fill = "grey90",
      color = "black",
      linewidth = 0.2
    ) +
    geom_polygon(
      data = sp_rect,
      aes(x = lon, y = lat, group = range_id, colour = source),
      fill = NA,
      linewidth = 0.7,
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
    theme_classic(base_family = "Times New Roman") +
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
      label = "Rectangles are schematic envelopes\nfrom resolved place-name endpoints",
      hjust = 0, size = 3, family = "Times New Roman"
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
      scale_shape_manual(values = c(west = 21, east = 24))
  }
  
  # ---------------- MIDDLE: LAT RANGE BARS (numeric y) ----------------
  sp_latbar <- sp_bbox %>%
    mutate(
      source_chr = factor(as.character(source), levels = range_levels)
    ) %>%
    left_join(lat_offsets, by = c("source_chr" = "source"))
  
  p_lat <- ggplot(sp_latbar) +
    geom_segment(
      aes(x = x, xend = x, y = lat_min, yend = lat_max,
          colour = source_chr, linewidth = source_chr == "Observed"),
      lineend = "round"
    ) +
    geom_point(
      aes(x = x, y = lat_min, colour = source_chr),
      size = 2.2
    ) +
    geom_point(
      aes(x = x, y = lat_max, colour = source_chr),
      size = 2.2
    ) +
    scale_linewidth_manual(values = c(`TRUE` = 1.8, `FALSE` = 1.2), guide = "none") +
    scale_colour_manual(values = range_cols, limits = range_levels, drop = FALSE) +
    coord_cartesian(ylim = lat_lim, expand = FALSE) +
    scale_y_continuous(
      breaks = y_breaks,
      labels = deg_lat(y_breaks),
      name = "Latitude (range)"
    ) +
    scale_x_continuous(
      limits = c(0.75, 1.25),
      breaks = NULL,
      name = NULL
    ) +
    theme_classic(base_family = "Times New Roman") +
    theme(
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(size = 11),
      axis.text.y  = element_text(size = 10),
      legend.position = "none",
      plot.title = element_text(size = 11),
      plot.margin = margin(5, 5, 5, 5)
    ) +
    labs(title = "Latitudinal range")
  
  
  # ---------------- RIGHT: LON RANGE BARS (numeric y to avoid squish) ----------------
  sp_lonbar <- sp_bbox %>%
    mutate(
      source_chr = factor(as.character(source), levels = range_levels),
      y = as.numeric(source_chr)
    )
  
  p_lon <- ggplot(sp_lonbar) +
    geom_segment(
      aes(x = lon_min, xend = lon_max, y = y, yend = y, colour = source_chr,
          linewidth = source_chr == "Observed"),
      lineend = "round"
    ) +
    geom_point(aes(x = lon_min, y = y, colour = source_chr), size = 2.2) +
    geom_point(aes(x = lon_max, y = y, colour = source_chr), size = 2.2) +
    scale_linewidth_manual(values = c(`TRUE` = 1.8, `FALSE` = 1.2), guide = "none") +
    scale_colour_manual(values = range_cols, limits = range_levels, drop = FALSE) +
    scale_x_continuous(breaks = x_breaks, labels = deg_lon(x_breaks), name = "Longitude (range)") +
    scale_y_continuous(
      breaks = seq_along(range_levels),
      labels = range_levels,
      limits = c(0.5, length(range_levels) + 0.5),
      expand = c(0, 0),
      name = NULL
    ) +
    coord_cartesian(xlim = lon_lim, expand = FALSE) +
    theme_classic(base_family = "Times New Roman") +
    theme(
      axis.title.y = element_blank(),
      axis.text.y  = element_text(size = 10),
      axis.title.x = element_text(size = 11),
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text  = element_text(size = 9),
      plot.title = element_text(size = 11),
      plot.margin = margin(5, 5, 5, 5)
    ) +
    labs(colour = "Source range", title = "Longitudinal range")
  
  # ---------------- COMBINE ----------------
  (p_map | p_lat | p_lon) + plot_layout(widths = c(1.25, 0.55, 1.05))
}

# Single plot
p <- plot_species_rangebars_3panel(
  sp_name = "Epinephelus andersoni",
  master_dat = master_dat,
  ranges_bbox = ranges_bbox,
  ranges_poly = ranges_poly,
  extreme_pts_long = extreme_pts_long,
  sa_map_df = sa_map_df,
  range_cols = range_cols
)

p




####depth

install.packages("terra")  
library(terra)
library(sf)
getwd()

depth_rast <- rast("SA_bathymetry_100m_v1/SA_bathymetry_100m_v1.tif")
depth_rast

# Downsample for plotting: increase fact until it runs comfortably (10, 20, 30...)
depth_plot <- aggregate(depth_rast, fact = 10, fun = "mean", na.rm = TRUE)

depth_df <- as.data.frame(depth_plot, xy = TRUE, na.rm = TRUE)
names(depth_df) <- c("x", "y", "depth")

ggplot(depth_df, aes(x = x/1000, y = y/1000, fill = depth)) +
  geom_raster() +
  coord_equal(expand = FALSE) +
  scale_fill_viridis_c(name = "Depth (m)", direction = -1) +
  scale_x_continuous("Easting (km)", labels = label_number()) +
  scale_y_continuous("Northing (km)", labels = label_number()) +
  labs(title = "South Africa bathymetry (100 m)") +
  theme_minimal(base_family = "Times", base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )


library(dplyr)
library(tibble)

depth_lut <- tribble(
  ~species, ~zmin_m, ~zmax_m,
  "Lithognathus lithognathus", 10, 150,
  "Petrus rupestris",          10, 160,
  "Austroglossus pectoralis",  10, 120,
  "Polysteganus undulosus",    30, 160,
  "Chrysoblephus gibbiceps",   20, 150,
  "Argyrosomus inodorus",       1, 100,
  "Cheilodactylus fasciatus",   1, 120,
  "Clinus cottoides",           0,   8,
  "Chorisochismus dentex",      0,  15,
  "Spondyliosoma emarginatum",  1,  60,
  "Sparodon durbanensis",       0,  80,
  "Pachymetopon aeneum",       20,  80,
  "Zeus faber",                 5, 400,
  "Caranx ignobilis",          10, 188,
  "Gymnothorax favagineus",     1,  50,
  "Thalassoma lunare",          1,  20,
  "Pomacanthus imperator",      1, 100,
  "Epinephelus andersoni",      5,  70,
  "Chaetodon marleyi",          1, 120,
  "Chelidonichthys queketti",   0, 150,
  "Cookeolus japonicus",       40, 400,
  "Sufflogobius bibarbatus",    0, 340
)

depth_band_mask <- function(depth_rast, zmin, zmax) {
  # Convert to positive depth in meters (handles rasters where sea is negative)
  d <- depth_rast
  r <- global(d, "mean", na.rm = TRUE)[1,1]
  if (is.finite(r) && r < 0) d <- abs(d)
  
  # 1 where within band, NA elsewhere
  m <- ifel(d >= zmin & d <= zmax, 1, NA)
  names(m) <- "band"
  m
}

# ranges_poly is your long df with lon/lat vertices
# You already have range_id, source, species, lon, lat in ranges_poly

ranges_sf <- ranges_poly %>%
  arrange(species, source, range_id, vertex) %>%
  group_by(species, source, range_id) %>%
  summarise(geometry = sf::st_sfc(sf::st_polygon(list(as.matrix(cbind(lon, lat))))),
            .groups = "drop") %>%
  st_as_sf(crs = 4326)

# Use an equal-area CRS for area calculations.
# EPSG:6933 = World Cylindrical Equal Area (good general-purpose option)
ea_crs <- 6933

depth_area_by_source <- function(sp_name, depth_rast, ranges_sf, depth_lut, ea_crs = 6933) {
  
  dr <- depth_lut %>% filter(species == sp_name)
  if (nrow(dr) == 0) stop("No depth range for species: ", sp_name)
  
  polys <- ranges_sf %>% filter(species == sp_name)
  if (nrow(polys) == 0) stop("No range polygons for species: ", sp_name)
  
  # Build band mask once
  band <- depth_band_mask(depth_rast, dr$zmin_m[1], dr$zmax_m[1])
  
  # Project raster to equal-area for correct km2 cell areas
  band_ea  <- project(band, paste0("EPSG:", ea_crs), method = "near")
  # cell areas (m2); sum of cellSize where mask==1 inside polygon = area
  cell_m2  <- cellSize(band_ea, unit = "m")
  
  # Project polygons to same CRS
  polys_ea <- st_transform(polys, ea_crs)
  polys_v  <- vect(polys_ea)
  
  # Extract which cells fall inside each polygon
  # We multiply mask(1/NA) * cell area, then sum extracted values.
  band_area_r <- band_ea * cell_m2
  
  ex <- terra::extract(band_area_r, polys_v, fun = sum, na.rm = TRUE)
  
  out <- polys %>%
    st_drop_geometry() %>%
    mutate(area_km2 = ex[[2]] / 1e6) %>%   # m2 -> km2
    select(species, source, range_id, area_km2)
  
  out
}

# Example:
depth_area_by_source("Chrysoblephus gibbiceps", depth_rast, ranges_sf, depth_lut)

plot_species_depth_band <- function(sp_name, depth_rast, depth_lut, ranges_sf,
                                    lat_lim = c(-45, -15), lon_lim = c(10, 45)) {
  
  dr <- depth_lut %>% filter(species == sp_name)
  band <- depth_band_mask(depth_rast, dr$zmin_m[1], dr$zmax_m[1])
  
  # crop to your plotting window (fast)
  e <- ext(lon_lim[1], lon_lim[2], lat_lim[1], lat_lim[2])
  band_c <- crop(band, e)
  
  band_df <- as.data.frame(band_c, xy = TRUE, na.rm = TRUE)  # values are 1
  names(band_df)[3] <- "in_band"
  
  poly_sp <- ranges_sf %>%
    filter(species == sp_name) %>%
    st_drop_geometry() %>%
    left_join(
      ranges_poly %>% filter(species == sp_name),
      by = c("species","source","range_id")
    )
  
  ggplot() +
    geom_raster(data = band_df, aes(x = x, y = y), alpha = 0.25) +
    geom_polygon(
      data = poly_sp,
      aes(x = lon, y = lat, group = range_id, colour = source),
      fill = NA, linewidth = 1
    ) +
    coord_cartesian(xlim = lon_lim, ylim = lat_lim, expand = FALSE) +
    labs(
      title = sp_name,
      subtitle = paste0("Depth band: ", dr$zmin_m[1], "–", dr$zmax_m[1], " m"),
      colour = "Source range"
    ) +
    theme_classic(base_family = "Times New Roman")
}

plot_species_depth_band("Chrysoblephus gibbiceps", depth_rast, depth_lut, ranges_sf)


#####
#depth band fishbase and combined with 
#  "Lithognathus lithognathus" = 10-150m (https://www.fishbase.se/references/FBRefSummary.php?ID=27121), https://www.google.com/url?sa=t&source=web&rct=j&opi=89978449&url=https://speciesstatus.sanbi.org/assessment/last-assessment/2896/&ved=2ahUKEwiEiv2e_JeSAxVYV0EAHWytMcAQFnoECBsQAQ&usg=AOvVaw07T-gnieklBlmLOkn523CC
#  "Petrus rupestris" = 10-160m https://www.google.com/url?sa=i&source=web&rct=j&url=https://speciesstatus.sanbi.org/assessment/last-assessment/2951/%23:~:text%3DAdults%2520of%2520this%2520species%2520are,Deep%2520Sea%2520Anglers%2520Association%25202012).&ved=2ahUKEwj0nfH-_JeSAxXWUUEAHRHzObsQqYcPegQIBxAC&opi=89978449&cd&psig=AOvVaw2X8m4zS0TmvxH0qdLRik_i&ust=1768924923539000
#  "Austroglossus pectoralis"= 10-120m (https://www.fishbase.se/references/FBRefSummary.php?ID=3200)
#  "Polysteganus undulosus" = 30-160m https://www.fishbase.se/references/FBRefSummary.php?ID=5213
#  "Chrysoblephus gibbiceps" = 20-150m https://www.fishbase.se/references/FBRefSummary.php?ID=28016 https://www.google.com/url?sa=i&source=web&rct=j&url=https://speciesstatus.sanbi.org/assessment/last-assessment/2954/%23:~:text%3DChrysoblephus%2520gibbiceps%2520is%2520a%2520reef,%252C%2520Buxton%2520and%2520Smale%25201984).&ved=2ahUKEwiCmbOg_ZeSAxU3TkEAHQIqM4YQqYcPegQIBRAG&opi=89978449&cd&psig=AOvVaw0V0GpTSIvgf3X2tvGEGcdu&ust=1768924993828000, https://www.google.com/url?sa=i&source=web&rct=j&url=https://issuu.com/sheenacarnie/docs/ski-boat_november_2020/s/11209750%23:~:text%3DThis%2520species%2520can%2520be%2520caught,you%2520are%2520after%2520increases%2520exponentially.&ved=2ahUKEwiCmbOg_ZeSAxU3TkEAHQIqM4YQqYcPegQIBRAC&opi=89978449&cd&psig=AOvVaw0V0GpTSIvgf3X2tvGEGcdu&ust=1768924993828000
#  "Argyrosomus inodorus" = 1 - 100m https://www.fishbase.se/references/FBRefSummary.php?ID=11025, https://www.google.com/url?sa=i&source=web&rct=j&url=https://spo.nmfs.noaa.gov/sites/default/files/pdf-content/1997/951/griffiths.pdf&ved=2ahUKEwirjPi9_ZeSAxXmY0EAHWw6MNEQqYcPegQIBxAC&opi=89978449&cd&psig=AOvVaw0jeCRHuhWvjA7GKu5gebLn&ust=1768925055772000
#  "Cheilodactylus fasciatus" = 1-120m https://www.fishbase.se/references/FBRefSummary.php?ID=120445
#  "Clinus cottoides" = 0-8m https://www.google.com/url?sa=i&source=web&rct=j&url=https://www.inaturalist.org/taxa/445800-Clinus-cottoides%23:~:text%3D*Clinus%2520cottoides*%252C%2520also%2520known%2520as%2520the%2520bluntnose,15%2520centimeters%2520(5.9%2520in)%2520*%2520Varied%2520diet&ved=2ahUKEwiOl9DP_ZeSAxXdWkEAHVN4FQQQqYcPegQIBhAJ&opi=89978449&cd&psig=AOvVaw1qjKPhtHpC3XjsAlJa6Jwt&ust=1768925092867000, https://www.google.com/url?sa=i&source=web&rct=j&url=https://www.vliz.be/imisdocs/publications/391235.pdf&ved=2ahUKEwiOl9DP_ZeSAxXdWkEAHVN4FQQQqYcPegQIBhAC&opi=89978449&cd&psig=AOvVaw1qjKPhtHpC3XjsAlJa6Jwt&ust=1768925092867000
#  "Chorisochismus dentex" = 0-15m  Lubke, R., & I. De Mour (1998). Field Guide to the Eastern and Southern Cape Coasts, p. 144.
#  "Spondyliosoma emarginatum" = 1 - 60m https://www.fishbase.se/references/FBRefSummary.php?ID=4332 https://www.google.com/url?sa=i&source=web&rct=j&url=https://speciesstatus.sanbi.org/assessment/last-assessment/2907/%23:~:text%3DDistribution,2007).&ved=2ahUKEwjE69CQ_peSAxWWQkEAHToDGv4QqYcPegQIBRAC&opi=89978449&cd&psig=AOvVaw2W50LZ14tMKwQKdcUizA3J&ust=1768925229192000
#  "Sparodon durbanensis"= 0-80m https://www.google.com/url?sa=i&source=web&rct=j&url=https://speciesstatus.sanbi.org/assessment/last-assessment/2943/%23:~:text%3DSparodon%2520durbanensis%2520adults%2520are%2520found,2013).&ved=2ahUKEwiF-pGd_peSAxXIXEEAHYYuEnQQqYcPegQIBhAG&opi=89978449&cd&psig=AOvVaw3slIRpPHk8CCWVpZzU6Zgt&ust=1768925255425000
#  "Pachymetopon aeneum"= 20-80m https://www.google.com/url?sa=i&source=web&rct=j&url=https://speciesstatus.sanbi.org/assessment/last-assessment/2925/%23:~:text%3DPachymetopon%2520aeneum%2520adults%2520inhabit%2520inshore,(Buxton%2520and%2520Clarke%25201986).&ved=2ahUKEwj905il_peSAxV-QkEAHZpAJ8gQqYcPegQIBhAC&opi=89978449&cd&psig=AOvVaw3wDUn5rdeR2Q18dFVskAYP&ust=1768925272312000
#  "Zeus faber"= 5-400m https://www.fishbase.se/references/FBRefSummary.php?ID=9563
#  "Caranx ignobilis"= 10 -188m https://www.fishbase.se/references/FBRefSummary.php?ID=58302
#  "Gymnothorax favagineus"= 1 -50m https://www.fishbase.se/references/FBRefSummary.php?ID=90102
#  "Thalassoma lunare"= 1-20m https://www.fishbase.se/references/FBRefSummary.php?ID=27115
#  "Pomacanthus imperator"= 1-100m https://www.fishbase.se/references/FBRefSummary.php?ID=48391
#  "Epinephelus andersoni"= 5-70m https://www.google.com/url?sa=i&source=web&rct=j&url=https://saambr.org.za/wp-content/uploads/2023/03/ORI-Fish-Fact-Catface-Rockcod-ZC-LB.pdf%23:~:text%3DHabitat.%2520Rocky%2520reefs%2520from%2520the%2520surf%252Dzone%2520to,coral%2520reefs.%2520Crustaceans%252C%2520small%2520fish%2520and%2520squid.&ved=2ahUKEwiSv829_peSAxWaVUEAHdzHHkwQqYcPegQIBxAG&opi=89978449&cd&psig=AOvVaw2z8yEYKwkdOUYt1XChXgDb&ust=1768925323509000  https://www.fishbase.se/references/FBRefSummary.php?ID=89707
#  "Chaetodon marleyi"= 1-120m https://www.fishbase.se/references/FBRefSummary.php?ID=9710
#  "Chelidonichthys queketti"= 0-150m https://www.fishbase.se/references/FBRefSummary.php?ID=4316
#  "Cookeolus japonicus"= 40-400m https://www.fishbase.se/references/FBRefSummary.php?ID=9335
#  "Sufflogobius bibarbatus"= 0-340 https://www.fishbase.se/references/FBRefSummary.php?ID=27121

#######