# Pacotes ----

library(tidyverse)

library(parzer)

library(sf)

library(terra)

library(tidyterra)

library(adehabitatHR)

library(sp)

# Dados ----

## Coordenadas ----

### Importando ----

dados <- read.csv("dados_gps.csv")

### Visualizando ----

dados

dados |> dplyr::glimpse()

### Tratando ----

dados <- dados |>
  dplyr::mutate(long = long |> parzer::parse_lon(),
                lat = lat |> parzer::parse_lat())

dados

### Criando shapefile ----

sf_dados <- dados |>
  sf::st_as_sf(coords = c("long", "lat"),
               crs = 4674) |>
  sf::st_transform(crs = 32725)

sf_dados

ggplot() +
  geom_sf(data = sf_dados |>
            sf::st_as_sf() |>
            sf::st_set_crs(4674)) +
  theme_bw()

## Imagem de satélite ----

### Importando ----

tapacura_sat <- terra::rast("tapacura.tif")

### Visualizando ----

tapacura_sat

ggplot() +
  tidyterra::geom_spatraster_rgb(data = tapacura_sat) +
  coord_sf(expand = FALSE) +
  theme_minimal()

### Tratando ----

terra::crs(tapacura_sat) <- "EPSG:4674"

# MCP ----

## 95% ----

mcp_95 <- sf_dados |>
  adehabitatHR::mcp(percent = 95) |>
  sf::st_as_sf() |>
  dplyr::mutate(`% de ocorrências` = "95%")

mcp_95

## 100% ----

mcp_100 <- sf_dados |>
  adehabitatHR::mcp(percent = 100) |>
  sf::st_as_sf() |>
  dplyr::mutate(`% de ocorrências` = "100%")

mcp_100

## Unindo os shapefiles ----

unido_mcp <- ls(pattern = "mcp_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

unido_mcp

## Gráfico ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = tapacura_sat) +
  geom_sf(data = unido_mcp |>
            sf::st_set_crs(4674),
          aes(color = `% de ocorrências`,
                                fill = `% de ocorrências`),
          alpha = 0.3) +
  coord_sf(expand = FALSE) +
  geom_sf(data = sf_dados, aes(color = "Pontos de registro")) +
  scale_fill_manual(values = c("orange",
                               "gold",
                               "red")) +
  theme_minimal()

ggsave(filename = "mapa_mcp.png", height = 10, width = 12)

## Área ----

unido_mcp |> sf::st_area() / 1e6

# Área de vida ----

## KDE ----

kde <- sf_dados |>
  adehabitatHR::kernelUD(h = "href")

kde

## 95% ----

kde_contour_95 <- kde |>
  adehabitatHR::getverticeshr(percent = 95) |>
  sf::st_as_sf(crs = 32725) |>
  dplyr::mutate(`Área de vida` = "95%")

kde_contour_95

## 50%

kde_contour_50 <-  kde |>
  adehabitatHR::getverticeshr(percent = 50) |>
  sf::st_as_sf(crs = 32725) |>
  dplyr::mutate(`Área de vida` = "55%")

kde_contour_50

## Unindo os shapefiles ----

unido_kde <- ls(pattern = "kde_contour_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

unido_kde

## Gráfico ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = tapacura_sat) +
  geom_sf(data = unido_kde |>
            sf::st_set_crs(4674),
          aes(color = `Área de vida`,
              fill = `Área de vida`),
          alpha = 0.3) +
  coord_sf(expand = FALSE) +
  geom_sf(data = sf_dados, aes(color = "Pontos de registro")) +
  scale_fill_manual(values = c("orange",
                               "gold",
                               "red")) +
  theme_minimal()

ggsave(filename = "mapa_area_vida.png", height = 10, width = 12)

## Área ----

unido_kde |> sf::st_area() / 1e6

# BBMM ----
