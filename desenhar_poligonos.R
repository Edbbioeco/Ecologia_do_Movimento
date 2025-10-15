# Pacotes ----

library(geobr)

library(tidyverse)

library(leaflet)

library(leaflet.extras)

library(terra)

library(mapedit)

# Dados ----

## Importando ----

slm <- geobr::read_municipality(year = 2019) |>
  dplyr::filter(name_muni == "São Lourenço Da Mata")

## Visualizando ----

ggplot() +
  geom_sf(data = slm)

# Mapa de satélite ----

mapa <- leaflet::leaflet() |>
  leaflet::addProviderTiles(providers$Esri.WorldImagery) |>
  leaflet.extras::addDrawToolbar(targetGroup = "draw",
                                 polylineOptions = TRUE,
                                 polygonOptions = TRUE,
                                 circleOptions = TRUE,
                                 rectangleOptions = TRUE,
                                 markerOptions = TRUE,
                                 editOptions = leaflet.extras::editToolbarOptions()) |>
  leaflet::addPolygons(data = slm |>
                         sf::st_as_sf() |>
                         sf::st_transform(4326))


mapa

# Edição interativa ----

shapes <- mapa |>
  mapedit::editMap(targetGroup = "draw",
                   polylineOptions = TRUE,
                   polygonOptions = TRUE,
                   circleOptions = TRUE,
                   rectangleOptions = TRUE,
                   markerOptions = TRUE,
                   editOptions = leaflet.extras::editToolbarOptions())

# Extraindo as feições ----

sfs <- shapes$drawn

ggplot() +
  geom_sf(data = sfs)
