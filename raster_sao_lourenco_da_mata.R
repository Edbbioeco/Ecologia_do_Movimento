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

# Imagem de satélite ----

## Carregando ----

imagem <- maptiles::get_tiles(x = slm,
                              provider = "Esri.WorldImagery",
                              zoom = 16)

imagem

## Visualizando ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = imagem) +
  geom_sf(data = slm, fill = "transparent", color = "gold", linewidth = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

## Exportando ----

imagem |>
  terra::writeRaster("sao_lourenco_da_mata.tif")
