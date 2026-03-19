library(shiny)
library(bslib)
library(tidyverse)
library(sf)
library(leaflet)

# Load pre-processed spatial objects.
# Generate these once from Working.R by running the saveRDS block near the top.
va_counties_sf <- readRDS("va_counties_sf.rds")
schools_final  <- readRDS("schools_final.rds")
sbhc_map       <- readRDS("sbhc_map.rds")
FQHC_sf        <- readRDS("FQHC_sf.rds")
CHRdataDict    <- read.csv("CountyHealthRankingsDictionary.csv")

# ---------------------------------------------------------------------------
# Build fill-variable choices from county data columns,
# excluding numerators, denominators, CIs, flags, and race breakouts
# ---------------------------------------------------------------------------
fill_cols <- names(va_counties_sf)[
  grepl("_rawvalue$|_other_data", names(va_counties_sf)) &
    !grepl("_numerator|_denominator|_ci_low|_ci_high|_flag|_race_",
           names(va_counties_sf), ignore.case = TRUE)
]

dict_lookup <- CHRdataDict |>
  mutate(code = str_extract(Variable.Name, "^v\\d+")) |>
  filter(!is.na(code)) |>
  distinct(code, .keep_all = TRUE) |>
  select(code, Measure)

desc_lookup <- CHRdataDict |>
  select(Variable.Name, Description) |>
  filter(Variable.Name %in% fill_cols) |>
  deframe()  # named vector: col -> description

col_labels_df <- tibble(col = fill_cols) |>
  mutate(code = str_extract(col, "^v\\d+")) |>
  left_join(dict_lookup, by = "code") |>
  mutate(label = str_remove(coalesce(Measure, col), "\\s*raw value$"))

fill_choices <- setNames(col_labels_df$col, col_labels_df$label)

# ---------------------------------------------------------------------------
# SBHC map icon
# ---------------------------------------------------------------------------
sbhc_icon <- makeAwesomeIcon(
  icon      = "plus-sign",
  iconColor = "white",
  markerColor = "blue",
  library   = "glyphicon"
)

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- page_sidebar(
  title   = "VA School-Based Health Centers",
  theme   = bs_theme(version = 5),
  fillable = TRUE,

  sidebar = sidebar(
    width = 300,
    selectInput(
      "fill_var",
      "Color counties by:",
      choices  = fill_choices,
      selected = "v024_rawvalue"
    ),
    uiOutput("var_desc")
  ),

  card(
    full_screen = TRUE,
    leafletOutput("map", height = "100%")
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # Base map — point layers only; polygons are managed via proxy
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      addMapPane("county_pane", zIndex = 300) |>

      addCircleMarkers(
        data = schools_final,
        group = "All Schools",
        radius = 5, color = "gray", fillOpacity = .75, stroke = FALSE,
        label = ~SCH_NAME,
        popup = ~paste0(
          "<b>School Name:</b> ", SCH_NAME, "<br>",
          "<b>Travel Time:</b> ", round(Total_TravelTime, 2), " min<br>",
          "<b>Travel Distance:</b> ", round(Total_Kilometers, 2), " km<br>",
          "<b>School Level:</b> ", SCHOOL_LEVEL
        )
      ) |>

      addCircleMarkers(
        data = schools_final |> filter(has_sbhc_nearby == TRUE),
        group = "Schools Near SBHC (1000ft)",
        radius = 8, color = "black", weight = 2, fillOpacity = 0.2,
        label = ~SCH_NAME,
        popup = ~paste0(
          "<b>School Within 1000ft of SBHC</b><br>",
          "<b>School Name:</b> ", SCH_NAME, "<br>",
          "<b>Travel Time:</b> ", round(Total_TravelTime, 2), " min<br>",
          "<b>Travel Distance:</b> ", round(Total_Kilometers, 2), " km<br>",
          "<b>School Level:</b> ", SCHOOL_LEVEL
        )
      ) |>

      addAwesomeMarkers(
        data  = sbhc_map,
        group = "Health Centers (SBHC)",
        icon  = sbhc_icon,
        label = ~Name,
        popup = ~paste0(
          "<b>SBHC Name:</b> ", Name, "<br>",
          "<b>Provider:</b> ", Operated.By, "<br>",
          "<b>Services Offered:</b> ", School.Based.Health.Services, "<br>",
          "<b>Delivery Model:</b> ", Delivery.Models
        )
      ) |>

      addCircleMarkers(
        data = FQHC_sf,
        group = "Health Centers (FQHC)",
        radius = 5, color = "lightblue", fillOpacity = .75, stroke = FALSE,
        label = ~Site.Name,
        popup = ~paste0(
          "<b>FQHC Name:</b> ", Site.Name, "<br>",
          "<b>Provider:</b> ", Health.Center.Name, "<br>",
          "<b>Service Site:</b> ", Health.Center.Service.Delivery.Site.Location.Setting.Description
        )
      ) |>

      addLayersControl(
        overlayGroups = c(
          "Counties", "All Schools", "Schools Near SBHC (1000ft)",
          "Health Centers (SBHC)", "Health Centers (FQHC)"
        ),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  output$var_desc <- renderUI({
    req(input$fill_var)
    desc <- desc_lookup[[input$fill_var]]
    if (!is.na(desc) && nzchar(trimws(desc))) {
      p(trimws(desc), style = "font-size: 0.85em; color: #555; margin-top: 4px;")
    }
  })

  # Swap county polygon fill and legend when the dropdown changes
  observe({
    req(input$fill_var)
    var   <- input$fill_var
    vals  <- va_counties_sf[[var]]
    pal   <- colorNumeric("viridis", domain = vals, na.color = "#cccccc")
    label <- names(fill_choices)[fill_choices == var]

    popup_html <- paste0(
      "<b>", va_counties_sf$county, "</b><br>",
      "<b>Total Schools:</b> ",       va_counties_sf$total_schools, "<br>",
      "<b>Schools Near an SBHC:</b> ", va_counties_sf$schools_w_sbhc, "<br>",
      "<b>", label, ":</b> ",         round(vals, 2)
    )

    leafletProxy("map") |>
      clearGroup("Counties") |>
      addPolygons(
        data        = va_counties_sf,
        group       = "Counties",
        fillColor   = pal(vals),
        fillOpacity = 0.5,
        color       = "white",
        weight      = 1,
        options     = pathOptions(pane = "county_pane"),
        label       = ~county,
        popup       = popup_html,
        highlightOptions = highlightOptions(
          color       = "#333333",
          weight      = 2.5,
          fillOpacity = 0.8,
          sendToBack = TRUE
        )
      ) |>
      removeControl("county_legend") |>
      addLegend(
        pal      = pal,
        values   = vals,
        title    = label,
        position = "bottomright",
        layerId  = "county_legend"
      )
  })
}

shinyApp(ui, server)
