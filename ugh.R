

library(tidyverse)
library(sf)
library(leaflet)
library(spdep)
library(leafem)
library(terra)
library(osmdata)
library(dplyr)

#1

bmps <- read.csv("./data/BMPreport2016.csv")

counties <- st_read("./data/County_Boundaries/County_Boundaries.shp") %>%
  st_make_valid()

bmps <- bmps %>%
  mutate(FIPS.trimmed = substr(GeographyName, 1, 5))

county_costs <- bmps %>%
  group_by(FIPS.trimmed) %>%
  summarize(total_cost = sum(Cost, na.rm = TRUE))

counties_data <- counties %>%
  left_join(county_costs, by = c("GEOID10" = "FIPS.trimmed")) %>%
  filter(!is.na(total_cost))

breaks <- seq(
  from = min(counties_data$total_cost, na.rm = TRUE),
  to = max(counties_data$total_cost, na.rm = TRUE),
  length.out = 6  
)

view(breaks)

my.colors <- c("#edf8e9", "#bae4b3", "#74c476", "#31a354", "#006d2c")

palette.fun <- colorBin(palette = my.colors, domain = counties_data$total_cost, bins = breaks)

leaflet(data = counties_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~palette.fun(total_cost),
    color = "white",
    weight = 1,
    fillOpacity = 0.7,
    label = ~paste0(NAME10, ": $", formatC(total_cost, big.mark = ",", digits = 0)),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#444",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = palette.fun,  
    values = ~total_cost,
    title = "Total BMP Cost ($)",
    position = "bottomright"
  )


#2

yellowstone_edu <- st_read("./data/yellowstone_edu_complete.gpkg")
yellowstone_edu <- st_transform(yellowstone_edu, crs = 4326)

county_sp <- as(yellowstone_edu, "Spatial")

nb <- poly2nb(county_sp, queen = TRUE)
lw <- nb2listw(nb, style = "W")

moran_values <- moran.test(yellowstone_edu$pct_bachelors, lw)

moran_result <- localmoran(yellowstone_edu$pct_bachelors, lw)

yellowstone_edu$lisa_quadrant <- factor(
  ifelse(moran_result[, 5] < 0.05 & moran_result[, 4] > 0, "High-High", 
         ifelse(moran_result[, 5] < 0.05 & moran_result[, 4] < 0, "Low-Low", 
                ifelse(moran_result[, 5] >= 0.05 & moran_result[, 4] > 0, "High-Low", "Low-High"))), 
  levels = c("High-High", "Low-Low", "High-Low", "Low-High")
)

palette.fun2 <- colorFactor(palette = c("darkred", "blue", "green", "yellow"), domain = yellowstone_edu$lisa_quadrant)

popup_labels <- paste0("<strong>County: </strong>", yellowstone_edu$NAMELSAD, 
                       "<br><strong>p-value: </strong>", round(moran_values$p.value, 4))

leaflet(data = yellowstone_edu) %>%
  addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>%
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
  addPolygons(
    fillColor = ~palette.fun2(lisa_quadrant),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    label = ~NAMELSAD,
    popup = popup_labels,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = palette.fun2,
    values = ~lisa_quadrant,
    title = "Moran's I Quadrants",
    position = "bottomright"
  ) %>%
  addLayersControl(
    baseGroups = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery"),
    options = layersControlOptions(collapsed = FALSE)
  )


#3

gallatin <- st_read("./data/montana/MontanaCounties2021/MontanaCounties.gdb") %>%
  st_make_valid() %>%
  filter(NAME == "GALLATIN")

yellowstone <- st_read("./data/montana/Yellowstone/YellowstonePark1995.shp") %>%
  st_make_valid() %>%
  st_transform(st_crs(gallatin)) %>%
  st_intersection(gallatin)

g.forest <- st_read("./data/montana/NationalForests/NationalForest2002.shp") %>%
  st_make_valid() %>%
  st_transform(st_crs(gallatin)) %>%
  st_intersection(gallatin)

bozeman <- st_read("./data/montana/City_Limits/City_Limits.shp") %>%
  st_transform(st_crs(gallatin))

elev_files <- list.files(path = "./data/montana/elevation_data", pattern = "\\.tif$", full.names = TRUE)
elev_rasters <- lapply(elev_files, rast)
elev_combined <- merge(sprc(elev_rasters))
elev_final <- elev_combined %>%
  project(crs(gallatin)) %>%
  crop(vect(gallatin)) %>%
  mask(vect(gallatin))
elev_simple <- aggregate(elev_final, fact = 5)

elev_pal <- colorNumeric(
  palette = "YlOrBr",
  domain = values(elev_simple),
  na.color = "transparent"
)

msu <- opq(bbox = "Bozeman, Montana") %>%
  add_osm_feature(key = "amenity", value = "university") %>%
  add_osm_feature(key = "name", value = "Montana State University") %>%
  osmdata_sf()

msu.point <- st_centroid(msu$osm_polygons)

msu.point <- st_transform(msu.point, st_crs(gallatin))

gallatin <- st_transform(gallatin, crs = 4326)
yellowstone <- st_transform(yellowstone, crs = 4326)
g.forest <- st_transform(g.forest, crs = 4326)
bozeman <- st_transform(bozeman, crs = 4326)
msu.point <- st_transform(msu.point, crs = 4326)

bozeman <- st_zm(bozeman, drop = TRUE, what = "ZM") #this took me forever to fix

locations <- data.frame(
  name = c("Hyalite Canyon", "Fairy Lake", "Ousel Falls", "Mount Blackmore", "Palisade Falls"),
  lat = c(45.5797, 45.9044, 45.2391, 45.4441, 45.4716),
  lon = c(-111.0791, -110.9578, -111.3416, -111.0030, -110.9363)
)


map <- leaflet() %>%
  addTiles() %>%
  setView(lng = -110.9, lat = 45.5, zoom = 9) %>%
  addRasterImage(elev_simple, colors = elev_pal, opacity = 0.7, group = "Elevation") %>%
  addPolygons(data = gallatin, 
              color = "black", 
              weight = 2, 
              fillOpacity = 0, 
              group = "Gallatin County") %>%
  addPolygons(data = yellowstone, 
              color = "darkgoldenrod", 
              fillColor = "yellow", 
              weight = 1, 
              fillOpacity = 0.5, 
              group = "Yellowstone NP") %>%
  addPolygons(data = g.forest, 
              color = "darkgreen", 
              fillColor = "green", 
              weight = 1, 
              fillOpacity = 0.5, 
              group = "Gallatin Forest") %>%
  addPolygons(data = bozeman, 
              color = "darkblue", 
              fillColor = "blue", 
              weight = 1, 
              fillOpacity = 0.7, 
              group = "Bozeman City") %>%
  addCircleMarkers(data = msu.point, 
                   color = "maroon", 
                   fillColor = "maroon", 
                   radius = 8, 
                   fillOpacity = 0.8, 
                   group = "MSU Campus") %>%
  addMarkers(data = locations, 
             ~lon, ~lat, 
             popup = ~name) %>%
  addLegend(position = "bottomright", 
            pal = elev_pal, 
            values = values(elev_simple), 
            title = "Elevation (m)") %>%
  addLayersControl(
    overlayGroups = c("Elevation", "Gallatin County", "Yellowstone NP", 
                      "Gallatin Forest", "Bozeman City", "MSU Campus"),
    position = "topright"
  ) %>%
  addControl(
    html = "<strong style='font-size:16px;'>ECOLOGICAL RESEARCH SITES<br>GALLATIN COUNTY, MONTANA</strong>",
    position = "bottomleft"
  ) %>%
  addScaleBar(position = "bottomleft") %>%
  addLegend(
    position = "bottomright",
    colors = c("black", "yellow", "green", "blue", "maroon"),
    labels = c("Gallatin County", "Yellowstone NP", "Gallatin Forest", "Bozeman City", "MSU Campus"),
    title = "Map Features",
    opacity = 1
  )

map


