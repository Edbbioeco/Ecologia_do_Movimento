# Pacotes ----

library(geobr)

library(tidyverse)

library(maptiles)

library(tidyterra)

library(terra)

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

# Imagem de satélite ----

## Carregando ----

imagem <- maptiles::get_tiles(x = sfs,
                              provider = "Esri.WorldImagery",
                              zoom = 17)

imagem

## Visualizando ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = imagem) +
  geom_sf(data = sfs, fill = "transparent", color = "gold", linewidth = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

## Exportando ----

imagem |>
  terra::writeRaster("tapacura_rec.tif")
