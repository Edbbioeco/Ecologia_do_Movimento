# Pacotes ----

library(leaflet)

library(tidyverse)

library(leaflet.extras)

library(mapedit)

# Mapa de satélite ----

mapa <- leaflet::leaflet() |>
  leaflet::addProviderTiles(providers$Esri.WorldImagery) |>
  leaflet.extras::addDrawToolbar(targetGroup = "draw",
                                 polylineOptions = TRUE,
                                 polygonOptions = TRUE,
                                 circleOptions = TRUE,
                                 rectangleOptions = TRUE,
                                 markerOptions = TRUE,
                                 editOptions = leaflet.extras::editToolbarOptions())

mapa

# Edição interativa ----

shapes <- mapa |>
  mapedit::editMap()

# Extraindo as feições ----

sfs <- shapes$drawn

ggplot() +
  geom_sf(data = sfs)
