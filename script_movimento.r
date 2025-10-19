# Pacotes ----

library(tidyverse)

library(magrittr)

library(adehabitatHR)

library(sf)

library(terra)

library(tidyterra)

library(sp)

# Dados ----

## Importando ----

ecomov <- read.csv("planilha_movimento.csv",
                   stringsAsFactors = FALSE)

## Visualizando ----

ecomov

ecomov |> dplyr::glimpse()

## Tratando ----

ecomov %<>%
  dplyr::mutate(datetime = paste0(date, time) |>
                  lubridate::mdy_hms()) |>
  dplyr::arrange(datetime)

ecomov

# Shapefile das coordenadas ----

## Criando o shapefile

ecomov_sf <- ecomov |>
  sf::st_as_sf(coords = c("long", "lat"),
               crs = 32725)

ecomov_sf

ggplot() +
  geom_sf(data = ecomov_sf |>
            sf::st_set_crs(4674)) +
  theme_bw()

## Criando um shapefile na classe SpatialPointsDataFrame ----

ecomov_sf2 <- ecomov_sf |>
  sf::as_Spatial()

ecomov_sf2

plot(ecomov_sf2)

# Minimun Convex Polygon ----

## MCP 95% ----

mcp_95 <- ecomov_sf2 |>
  adehabitatHR::mcp(percent = 95) |>
  sf::st_as_sf()

mcp_95

## MCP 100% ----

mcp_100 <- ecomov_sf2 |>
  adehabitatHR::mcp(percent = 100) |>
  sf::st_as_sf()

mcp_100

## Visualizando ----

ggplot() +
  geom_sf(data = mcp_95 |>
            sf::st_set_crs(4674),
          fill = NA, color = "blue", linewidth = 1.5) +
  geom_sf(data = mcp_100 |>
            sf::st_set_crs(4674),
          fill = NA, color = "green", linewidth = 1) +
  geom_sf(data = ecomov_sf |>
            sf::st_set_crs(4674),
          size = 1, color = "red", alpha = 1) +
  theme_bw()

# Calcular área em m² ----

mcp_95 |>
  sf::st_area()

mcp_100 |>
  sf::st_area()

# Calcular área km² ----

mcp_95 |>
  sf::st_area() / 1e6

mcp_100 |>
  sf::st_area() / 1e6

# Kernel Density Estimator ----

## KDE com parâmetros de suavização

### Criando o KDE ----

kde <- ecomov_sf2 |>
  adehabitatHR::kernelUD(h = "href")

### Visualizando ----

kde_rast <- kde |>
  terra::rast()

terra::crs(kde_rast) <- "EPSG:4674"

kde_rast

ggplot() +
  tidyterra::geom_spatraster(data = kde_rast) +
  scale_fill_viridis_c() +
  coord_sf(expand = FALSE) +
  theme_bw()

## Obter polígonos de contorno (95% e 50%)

### 95% ----

kde_contour_95 <- kde |>
  adehabitatHR::getverticeshr(percent = 95) |>
  sf::st_as_sf(crs = 32725)

kde_contour_95

### 50%

kde_contour_50 <-  kde |>
  adehabitatHR::getverticeshr(percent = 50) |>
  sf::st_as_sf(crs = 32725)

kde_contour_50

### Visualizando ----

ggplot() +
  geom_sf(data = kde_contour_95 |>
            sf::st_set_crs(4674),
          fill = "orange", color = "orange", linewidth = 1.5) +
  geom_sf(data = kde_contour_50 |>
            sf::st_set_crs(4674),
          fill = "gold", color = "gold", linewidth = 1) +
  geom_sf(data = ecomov_sf |>
            sf::st_set_crs(4674), size = 1, color = "black", alpha = 1) +
  theme_bw()

### Calcular área em m² ----

kde_contour_95 |> st_area()

kde_contour_50 |> st_area()

### Calcular área em km²

kde_contour_95 |> st_area() / 1e6


kde_contour_50 |> st_area() / 1e6

# Brownian Bridge Moviment Model ----

## Transformar em um objeto de trajetória ----

ecomov_utm <- ecomov |>
  sf::st_as_sf(coords = c("long", "lat"),
               crs = 4674) |>
  sf::st_transform(crs = 32725)

ecomov_utm

ecomov.ltraj <- adehabitatLT::as.ltraj(xy = ecomov_utm |>
                                         sf::st_coordinates() |>
                                         as.data.frame(),
                                       date = ecomov$datetime,
                                       id = ecomov$id,
                                       proj4string = sp::CRS("+init=epsg:32725"))

ecomov.ltraj

plot(ecomov.ltraj)

## Cálculo para a variância do movimento animal (sig 1) dado pelo ponto de inflexão - likelihood ----

adehabitatHR::liker(ecomov.ltraj,
                    rangesig1 = c(1, 1000),
                    sig2 = 5,
                    byburst = FALSE,
                    plotit = TRUE)

## Calcular o modelo de utilização da área ----

bb_ecomov <- adehabitatHR::kernelbb(ecomov.ltraj,
                                    sig1 = 2,
                                    sig2 = 5,
                                    grid = 500,
                                    extent = 5,
                                    nalpha = 25)

bb_ecomov

bb_ecomov_rast <- bb_ecomov |>
  terra::rast()

terra::crs(bb_ecomov_rast) <- "EPSG:4674"

terra::ext(bb_ecomov_rast) <- kde_rast |> terra::ext()

ggplot() +
  tidyterra::geom_spatraster(data = bb_ecomov_rast) +
  scale_fill_viridis_c() +
  coord_sf(expand = FALSE) +
  theme_bw()

## Polígonos de contorno (95% e 50%) ----

### 95% ----

bb_contour_95 <- bb_ecomov |>
  adehabitatHR::getverticeshr(percent = 95, unout = "km2")

bb_contour_95

### 50% ----

bb_contour_50 <- bb_ecomov |>
  adehabitatHR::getverticeshr(percent = 50, unout = "km2")

bb_contour_50

### Visualizando 95% e 50% ----

ggplot() +
  geom_sf(data = bb_contour_95 |>
            sf::st_as_sf(crs = 4674)) +
  geom_sf(data = bb_contour_50 |>
            sf::st_as_sf(crs = 4674))

## Corrigindo os valores de contorno ----

### 60 % ----

bb_contour_60 <- getverticeshr(bb_ecomov, percent = 60, unout = "km2")

### 30% ----

bb_contour_30 <- getverticeshr(bb_ecomov, percent = 30, unout = "km2")

### Visualizando ----

bb_contour_95 |> sf::st_set_crs(4674)

ggplot() +
  geom_sf(data = bb_contour_95 |>
            sf::st_as_sf() |>
            sf::st_set_crs(32725) |>
            sf::st_transform(crs = 4674),
          fill = "blue", color = "blue") +
  geom_sf(data = bb_contour_60 |>
            sf::st_as_sf() |>
            sf::st_set_crs(32725) |>
            sf::st_transform(crs = 4674),
          fill = "forestgreen", color = "forestgreen") +
  geom_sf(data = bb_contour_50 |>
            sf::st_as_sf() |>
            sf::st_set_crs(32725) |>
            sf::st_transform(crs = 4674),
          fill = "yellowgreen", color = "yellowgreen") +
  geom_sf(data = bb_contour_30 |>
            sf::st_as_sf() |>
            sf::st_set_crs(32725) |>
            sf::st_transform(crs = 4674),
          fill = "yellow", color = "yellow")

# Áreas ----

ls(pattern = "bb_contour") |>
  mget(envir = globalenv())
