
#install.packages("tidygeocoder","tidyverse","sf","leaflet")
require(tidygeocoder)
require(tidyverse)
require(leaflet)
require(sf)
require(reactable)

# Create the data frame
nces_mapping <- data.frame(
  Code = c(
    "NCESSCH", "SURVYEAR", "STABR", "LEAID", "ST_LEAID", "LEA_NAME", "SCH_NAME", 
    "LSTREET1", "LSTREET2", "LCITY", "LSTATE", "LZIP", "LZIP4", "PHONE", 
    "CHARTER_TEXT", "VIRTUAL", "GSLO", "GSHI", "SCHOOL_LEVEL", "STATUS", 
    "SCHOOL_TYPE_TEXT", "SY_STATUS_TEXT", "ULOCALE", "NMCNTY", "CNTY", 
    "TOTFRL", "FRELCH", "REDLCH", "DIRECTCERT", "PK", "KG", "G01", "G02", 
    "G03", "G04", "G05", "G06", "G07", "G08", "G09", "G10", "G11", "G12", 
    "G13", "UG", "AE", "TOTMENROL", "TOTFENROL", "TOTAL", "MEMBER", "FTE", 
    "STUTERATIO", "AMALM", "AMALF", "AM", "ASALM", "ASALF", "AS_", "BLALM", 
    "BLALF", "BL", "HPALM", "HPALF", "HP", "HIALM", "HIALF", "HI", "TRALM", 
    "TRALF", "TR", "WHALM", "WHALF", "WH", "LATCOD", "LONCOD"
  ),Description = c(
    "Unique School ID", "Year corresponding to survey record", "Postal state abbreviation code", 
    "NCES Agency ID", "State Local Education Number", "Education Agency Name", "School name", 
    "Location address, street 1", "Location address, street 2", "Location city", "Location state", 
    "Location 5 digit ZIP code", "Location Secondary ZIP code", "Telephone number", "Whether a Charter School", 
    "Virtual School Status", "Grades Offered - Lowest", "Grades Offered - Highest", "School level", 
    "Start of year status (code)", "School type (description)", "Start of year Status (description)", 
    "Locale Code", "County Name", "County FIPS", "Total of free lunch and reduced-price lunch eligible", 
    "Free Lunch Program", "Reduced-Lunch Program", "Direct Certification", "Prekindergarten students", 
    "Kindergarten students", "Grade 1 students", "Grade 2 students", "Grade 3 students", 
    "Grade 4 students", "Grade 5 students", "Grade 6 students", "Grade 7 students", 
    "Grade 8 students", "Grade 9 students", "Grade 10 students", "Grade 11 students", 
    "Grade 12 students", "Grade 13 students", "Ungraded students", "Adult Education Students", 
    "Total Male Enrollment", "Total Female Enrollment", "Total students, all grades (includes AE)", 
    "Total elementary/secondary students (excludes AE)", "Total Teachers", "Student teacher ratio", 
    "All Students - American Indian/Alaska Native - Male", "All Students - American Indian/Alaska Native - Female", 
    "All Students - American Indian/Alaska Native", "All Students - Asian - Male", "All Students - Asian - Female", 
    "All Students - Asian", "All Students - Black or African American - Male", "All Students - Black or African American - Female", 
    "All Students - Black or African American", "All Students - Native Hawai'ian or Other Pacific Islander - Male", 
    "All Students - Native Hawai'ian or Other Pacific Islander - Female", "All Students - Native Hawai'ian or Other Pacific Islander", 
    "All Students - Hispanic - Male", "All Students - Hispanic - Female", "All Students - Hispanic", 
    "All Students - Two or More Races - Male", "All Students - Two or More Races - Female", 
    "All Students - Two or More Races", "All Students - White - Male", "All Students - White - Female", 
    "All Students - White", "Latitude", "Longitude"
  )
)

reactable(
  nces_mapping,
  searchable = TRUE,
  striped = TRUE,
  highlight = TRUE,
  bordered = TRUE,
  defaultPageSize = 25,
  columns = list(
    Code = colDef(style = list(fontFamily = "monospace", fontWeight = "bold", color = "#2c3e50"))
  )
)

# Run this block once to generate RDS files needed by app.R
# (saves the geocoded + processed objects so the app doesn't re-geocode on startup)
if (FALSE) {
  saveRDS(df_normed,      "df_normed.rds")
  saveRDS(schools_final,  "schools_final.rds")
  saveRDS(sbhc_map,       "sbhc_map.rds")
  saveRDS(FQHC_sf,        "FQHC_sf.rds")
  saveRDS(va_counties_sf, "va_counties_sf.rds")
}

read.csv("SBHC VA.csv")->SBHC

read.csv("TTbyNCESID.csv")->TravelTimes

read.csv("AHA2023.csv")->hospitals

read.csv("FQHC2026.csv")->FQHC 

read.csv("CountyHealthRankingsDictionary.csv")->CHRdataDict

read.csv("County_ExportTable.csv")->CountyData

CountyData %>% 
  filter(state == "VA")->VACountyData

FQHC %>% 
  drop_na(Geocoding.Artifact.Address.Primary.X.Coordinate) %>% 
  filter(Site.State.Abbreviation == "VA")->FQHC
TravelTimes %>% 
  filter(STABR == "VA")->TTVA


CountyData$fipscode <- as.character(CountyData$fipscode)
TTVA$CNTY      <- as.character(TTVA$CNTY)

TTVA_w_CountyData<- TTVA %>%
  left_join(CountyData, by = c("CNTY" = "fipscode"))


df_clean <- SBHC %>%
  extract(Street.Address, 
          into = c("Street", "City", "State", "Zip"),
          # Regex logic: 
          # (.+?) matches street, then an optional comma/space
          # ([^,]+) matches city (everything until a comma or state)
          # ([A-Z]{2}) matches the state
          # (\\d{5}) matches the zip
          regex = "^(.+?)[,\\s]+([^,]+)[,\\s]+([A-Z]{2})\\s+(\\d{5})$")

df_ready <- df_clean %>%
  drop_na(Street) %>% 
  mutate(Street = str_replace_all(Street, c("\\bDr\\b" = "Drive", 
                                            "\\bLn\\b" = "Lane"))) %>%
  mutate(full_address = toupper(paste(Street, City, State, Zip, sep = ", ")))

# Perform the geocoding
df_geocoded <- df_ready %>%
  geocode(address = full_address, 
          method = 'census', 
          lat = latitude, 
          long = longitude)


normalize_street <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("\\bdrive\\b", "dr") |>
    str_replace_all("\\blane\\b",  "ln") |>
    str_replace_all("\\bstreet\\b", "st") |>
    str_replace_all("\\broad\\b",  "rd") |>
    str_squish()
}

# TTVA lookup: normalize street, keep first distinct match
ttva_lookup <- TTVA_w_CountyData |>
  mutate(Street_normalized = normalize_street(LSTREET1)) |>
  select(Street_normalized, LATCOD, LONCOD) |>
  distinct(Street_normalized, .keep_all = TRUE)

# Normalize df_normed street
df_normed <- df_geocoded |>
  mutate(Street_normalized = normalize_street(Street)) |>
  left_join(ttva_lookup, by = "Street_normalized") |>
  mutate(
    latitude  = coalesce(latitude,  LATCOD),
    longitude = coalesce(longitude, LONCOD)
  ) |>
  select(-LATCOD, -LONCOD, -Street_normalized)

df_normed |> summarise(missing_lat = sum(is.na(latitude)), missing_lon = sum(is.na(longitude)))

# 1. Convert TTVA to an sf object
# with LATCOD and LONCOD
ttva_sf <- TTVA |>
  st_as_sf(coords = c("LONCOD", "LATCOD"), crs = 4326) |>
  st_transform(2284) # Transform to VA State Plane (Feet)


FQHC_sf <- FQHC |>
  st_as_sf(coords = c("Geocoding.Artifact.Address.Primary.X.Coordinate","Geocoding.Artifact.Address.Primary.Y.Coordinate"),
           crs = 4326)
# 2. Convert df_normed to an sf object
# Assuming it has longitude and latitude
sbhc_sf <- df_normed |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(2284)
# 2. Identify schools within 1000ft of ANY SBHC
# We buffer the SBHCs and join them to the schools
sbhc_buffer <- st_buffer(sbhc_sf, dist = 1000)
# Join SBHCs to the school buffers
# 'left = TRUE' ensures every SBHC stays in the table
sbhc_joined <- st_join(sbhc_sf, sbhc_buffer, left = TRUE)

schools_final <- st_join(ttva_sf, sbhc_buffer, left = TRUE) |> 
  mutate(has_sbhc_nearby = !is.na(arc)) |> # Replace 'arc' with any SBHC-specific column
  st_transform(4326)

# 3. Prep SBHCs for mapping
sbhc_map <- sbhc_sf |> st_transform(4326)

# 4. Create a unique icon for SBHCs
sbhc_icon <- makeAwesomeIcon(
  icon = "plus-sign", # Medical plus
  iconColor = "white",
  markerColor = "blue",
  library = "glyphicon"
)


library(tigris)
options(tigris_use_cache = TRUE)

VACountyData$fipscode <- as.character(VACountyData$fipscode)


# Fetch VA county boundaries and join County Health Rankings data
va_counties_sf <- counties(state = "VA", cb = TRUE, year = 2022) |>
  left_join(VACountyData, by = c("GEOID" = "fipscode"))

# Per-county school counts from schools_final
county_school_counts <- schools_final |>
  st_drop_geometry() |>
  group_by(CNTY) |>
  summarise(
    total_schools   = n(),
    schools_w_sbhc  = sum(has_sbhc_nearby, na.rm = TRUE),
    .groups = "drop"
  )

va_counties_sf <- va_counties_sf |>
  left_join(county_school_counts, by = c("GEOID" = "CNTY"))->va_counties_sf_join


va_counties_sf_join %>% 
  mutate(
    total_schools  = replace_na(total_schools, 0),
    schools_w_sbhc = replace_na(schools_w_sbhc, 0)
  ) |>
  st_transform(4326)->va_counties_sf

# Color palette for Children in Poverty (v024_rawvalue)
county_pal <- colorNumeric("YlOrRd", domain = va_counties_sf$v024_rawvalue, na.color = "#cccccc")

leaflet() |>
  addTiles() |>
  
  # LAYER 0: County polygons with health data
  addPolygons(
    data = va_counties_sf,
    group = "Counties",
    fillColor = ~county_pal(v024_rawvalue),
    fillOpacity = 0.5,
    color = "white",
    weight = 1,
    label = ~county,
    popup = ~paste0(
      "<b>", county, "</b><br>",
      "<b>Total Schools:</b> ", total_schools, "<br>",
      "<b>Schools Near an SBHC:</b> ", schools_w_sbhc
    ),
    highlightOptions = highlightOptions(
      color = "#333333",
      weight = 2.5,
      fillOpacity = 0.8,
      bringToFront = TRUE
    )
  ) |>
  addLegend(
    pal = county_pal,
    values = va_counties_sf$v024_rawvalue,
    title = "Children in Poverty (%)",
    position = "bottomright"
  ) |>

  # LAYER 1: All Schools (Neutral base)
  addCircleMarkers(
    data = schools_final,
    group = "All Schools",
    radius = 4, color = "gray", fillOpacity = 0.5, stroke = FALSE,
    label = ~SCH_NAME,
    popup = ~paste0(
      "<b>School Name:</b> ", SCH_NAME, "<br>",
      "<b>Travel Time:</b> ", round(Total_TravelTime, 2), " min<br>",
      "<b>Travel Distance:</b> ", round(Total_Kilometers, 2), " km<br>",
      "<b>School Level:</b> ", SCHOOL_LEVEL
    )
  ) |>
  
  # LAYER 2: Highlighted Schools (Those near an SBHC)
  addCircleMarkers(
    data = schools_final |> filter(has_sbhc_nearby == TRUE),
    group = "Schools with SBHC (1000ft)",
    radius = 8, color = "red", weight = 3, fillOpacity = 0.2,
    label = ~SCH_NAME,
    popup = ~paste0(
      "<b>School Within 1000ft of SBHC</b>",
      "<b>School Name:</b> ", SCH_NAME, "<br>",
      "<b>Travel Time:</b> ", round(Total_TravelTime, 2), " min<br>",
      "<b>Travel Distance:</b> ", round(Total_Kilometers, 2), " km<br>",
      "<b>School Level:</b> ", SCHOOL_LEVEL
    )
  ) |>
  
  # LAYER 3: The SBHCs (Unique Symbol)
  addAwesomeMarkers(
    data = sbhc_map,
    group = "Health Centers (SBHC)",
    icon = sbhc_icon,
    label = ~paste0(Name),
    popup = ~paste0(
      "<b>SBHC Name:</b> ", Name, "<br>",
      "<b>Provider:</b> ", Operated.By, " min<br>",
      "<b>Services Offered:</b> ", School.Based.Health.Services, " km<br>",
      "<b>Delivery Model:</b> ", Delivery.Models
    )
  ) |># LAYER 3: The SBHCs (Unique Symbol)
  addCircleMarkers(
    data = FQHC_sf,
    group = "Health Centers (FQHC)",
    radius = 4, color = "lightblue", fillOpacity = 0.5, stroke = FALSE,
    label = ~paste0(Site.Name),
    popup = ~paste0(
      "<b>FQHC Name:</b> ", Site.Name, "<br>",
      "<b>Provider:</b> ", Health.Center.Name,
      "<b>Service Site:</b> ", Health.Center.Service.Delivery.Site.Location.Setting.Description
    )
  ) |>
  
  # Layer Toggle
  addLayersControl(
    overlayGroups = c("Counties", "All Schools", "Schools Near SBHC (1000ft)", "Health Centers (SBHC)", "Health Centers (FQHC)"),
    options = layersControlOptions(collapsed = FALSE)
  )

