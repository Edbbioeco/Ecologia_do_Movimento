# Pacotes ----

library(geobr)

library(tidyverse)

library(dismo)

library(sf)

library(spatstat)

library(spatstat.geom)

library(spatstat.explore)

library(tidyterra)

# Dados -----

## Brasil ----

### Importando ----

br <- geobr::read_country(year = 2019)

### Visualizando ----

br |>
  ggplot() +
  geom_sf(color = "black", fill = "gray")

## Pontos de ocorrência ----

### Importando ----

pontos <- dismo::gbif(genus = "Scinax", species = "x-signatus") |>
  dplyr::select(lon, lat) |>
  tidyr::drop_na() |>
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = 4674) |>
  sf::st_intersection(br)

### Visualizando ----

pontos

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = pontos)

# Calcular KDE -----

## Calculando a janela ----

w <- spatstat.geom::as.owin(br |>
                              sf::st_transform(crs = 31984) |>
                              sf::st_bbox() |>
                              sf::st_as_sfc())

w

## Coordenadas dos pontos -----

coords <- pontos |>
  sf::st_transform(crs = 31984) |>
  sf::st_coordinates()

coords

## Objeto ppp ----

ppp_obj <- spatstat.geom::ppp(x = coords[,1],
                              y = coords[,2],
                              window = w)

ppp_obj

## Criando o Kernel ----

dens <- density(ppp_obj, kernel = "gaussian")

dens

plot(dens)

## Rasterizando o kernel ----

raster_kernel <- dens |>
  terra::rast()

terra::crs(raster_kernel) <- br |> terra::crs()

terra::ext(raster_kernel) <- br |> terra::ext()

raster_kernel <- raster_kernel |>
  terra::crop(br) |>
  terra::mask(br)

ggplot() +
  tidyterra::geom_spatraster(data = raster_kernel) +
  geom_sf(data = br, color = "black", fill = "transparent") +
  geom_sf(data = pontos) +
  scale_fill_viridis_c(na.value = "transparent")

## Perfil de verossimelhaça ----

h_ppl <- spatstat.explore::bw.ppl(ppp_obj)

h_ppl

dens_ppl <- density(ppp_obj, kernel = "gaussian", sigma = h_ppl)

plot(dens_ppl)

raster_kernel_ppl <- dens_ppl |>
  terra::rast()

terra::crs(raster_kernel_ppl) <- br |> terra::crs()

terra::ext(raster_kernel_ppl) <- br |> terra::ext()

raster_kernel_ppl <- raster_kernel_ppl |>
  terra::crop(br) |>
  terra::mask(br)

ggplot() +
  tidyterra::geom_spatraster(data = raster_kernel_ppl) +
  geom_sf(data = br, color = "black", fill = "transparent") +
  geom_sf(data = pontos) +
  scale_fill_viridis_c(na.value = "transparent")

## Regra prática de Diggle ----

h_dgg <- spatstat.explore::bw.diggle(ppp_obj)

h_dgg

dens_dgg <- density(ppp_obj, kernel = "gaussian", sigma = h_dgg)

plot(dens_dgg)

raster_kernel_dgg <- dens_dgg |>
  terra::rast()

terra::crs(raster_kernel_dgg) <- br |> terra::crs()

terra::ext(raster_kernel_dgg) <- br |> terra::ext()

raster_kernel_dgg <- raster_kernel_dgg |>
  terra::crop(br) |>
  terra::mask(br)

ggplot() +
  tidyterra::geom_spatraster(data = raster_kernel_dgg) +
  geom_sf(data = br, color = "black", fill = "transparent") +
  geom_sf(data = pontos) +
  scale_fill_viridis_c(na.value = "transparent")

## Validação cruzada dos mínimos quadrados ----

h_cvl <- spatstat.explore::bw.CvL(ppp_obj)

h_cvl

dens_cvl <- density(ppp_obj, kernel = "gaussian", sigma = h_cvl)

plot(dens_cvl)

raster_kernel_cvl <- dens_cvl |>
  terra::rast()

terra::crs(raster_kernel_cvl) <- br |> terra::crs()

terra::ext(raster_kernel_cvl) <- br |> terra::ext()

raster_kernel_cvl <- raster_kernel_cvl |>
  terra::crop(br) |>
  terra::mask(br)

ggplot() +
  tidyterra::geom_spatraster(data = raster_kernel_cvl) +
  geom_sf(data = br, color = "black", fill = "transparent") +
  geom_sf(data = pontos) +
  scale_fill_viridis_c(na.value = "transparent")

## Regra empírica de Scott ----

h_sct <- spatstat.explore::bw.scott(ppp_obj)

h_sct

dens_sct <- density(ppp_obj, kernel = "gaussian", sigma = h_sct)

plot(dens_sct)

raster_kernel_sct <- dens_sct |>
  terra::rast()

terra::crs(raster_kernel_sct) <- br |> terra::crs()

terra::ext(raster_kernel_sct) <- br |> terra::ext()

raster_kernel_sct <- raster_kernel_sct |>
  terra::crop(br) |>
  terra::mask(br)

ggplot() +
  tidyterra::geom_spatraster(data = raster_kernel_sct) +
  geom_sf(data = br, color = "black", fill = "transparent") +
  geom_sf(data = pontos) +
  scale_fill_viridis_c(na.value = "transparent")
