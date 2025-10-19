# Pacotes ----

library(tidyverse)

library(magrittr)

library(parzer)

library(sf)

library(terra)

library(tidyterra)

library(adehabitatHR)

library(ggnewscale)

library(ggspatial)

library(sp)

# Dados ----

## Coordenadas ----

### Importando ----

dados <- read.csv("dados_gps.csv",
                  sep = ";")

### Visualizando ----

dados

dados |> dplyr::glimpse()

### Tratando ----

dados %<>%
  dplyr::mutate(long = long |> parzer::parse_lon(),
                lat = lat |> parzer::parse_lat(),
                datetime = paste0(date, time) |>
                  lubridate::mdy_hm()) |>
  tidyr::drop_na()

dados |> dplyr::glimpse()

### Criando shapefile ----

sf_dados <- dados |>
  sf::st_as_sf(coords = c("long", "lat"),
               crs = 4674) |>
  sf::st_transform(crs = 32725)

sf_dados

ggplot() +
  geom_sf(data = sf_dados |>
            sf::st_transform(crs = 4674)) +
  geom_path(data = dados, aes(long, lat)) +
  geom_sf(data = sf_dados |>
              dplyr::slice_head(),
            color = "blue") +
  geom_sf(data = sf_dados |>
              dplyr::slice_tail(),
            color = "red") +
  theme_bw()

## Imagem de satélite ----

### Importando ----

tapacura_sat <- terra::rast("tapacura_rec.tif")

### Visualizando ----

tapacura_sat

ggplot() +
  tidyterra::geom_spatraster_rgb(data = tapacura_sat) +
  geom_sf(data = sf_dados |>
            sf::st_transform(crs = 4674),
          color = "gold") +
  geom_path(data = dados, aes(long, lat),
            color = "gold") +
  coord_sf(expand = FALSE) +
  theme_minimal()

### Tratando ----

terra::crs(tapacura_sat) <- "EPSG:4674"

# MCP ----

## 95% ----

mcp_95 <- sf_dados |>
  sf::as_Spatial() |>
  adehabitatHR::mcp(percent = 95) |>
  sf::st_as_sf() |>
  dplyr::mutate(`% de ocorrências` = "95%")

mcp_95

## 100% ----

mcp_100 <- sf_dados |>
  sf::as_Spatial() |>
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
            sf::st_transform(crs = 4674),
          aes(color = `% de ocorrências`,
                                fill = `% de ocorrências`),
          alpha = 0.3, linewidth = 1) +
  coord_sf(expand = FALSE) +
  scale_fill_manual(values = c("orange",
                               "royalblue")) +
  scale_color_manual(values = c("orange",
                               "royalblue")) +
  guides(fill = guide_legend(title.position = "top"),
         color = guide_legend(title.position = "top")) +
  ggnewscale::new_scale_color() +
  geom_sf(data = sf_dados, aes(color = "Pontos de registro")) +
  scale_color_manual(values = c("red")) +
  labs(colour = NULL,
       x = NULL,
       y = NULL) +
  scale_x_continuous(limits = c(-35.203, -35.19)) +
  scale_y_continuous(limits = c(-8.045, -8.031)) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              height = unit(0.5, "cm")) +
  coord_sf(expand = FALSE) +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black"),
        legend.position = "bottom")

ggsave(filename = "mapa_mcp.png", height = 10, width = 12)

## Área ----

unido_mcp |> sf::st_area() / 1e6

# Área de vida ----

## KDE ----

kde <- sf_dados |>
  sf::as_Spatial() |>
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
  dplyr::mutate(`Área de vida` = "50%")

kde_contour_50

## Unindo os shapefiles ----

unido_kde <- ls(pattern = "kde_contour_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::arrange(`Área de vida` |> dplyr::desc())

unido_kde

## Gráfico ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = tapacura_sat) +
  geom_sf(data = unido_kde |>
            sf::st_transform(crs = 4674),
          aes(color = `Área de vida`,
              fill = `Área de vida`),
          alpha = 0.3,
          linewidth = 1) +
  scale_color_manual(values = c("royalblue",
                                "orange")) +
  scale_fill_manual(values = c("royalblue",
                               "orange")) +
  guides(fill = guide_legend(title.position = "top"),
         color = guide_legend(title.position = "top")) +
  ggnewscale::new_scale_color() +
  geom_sf(data = sf_dados, aes(color = "Pontos de registro")) +
  scale_color_manual(values = "red") +
  labs(color = NULL) +
  scale_x_continuous(limits = c(-35.203, -35.19)) +
  scale_y_continuous(limits = c(-8.045, -8.031)) +
  coord_sf(expand = FALSE) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              height = unit(0.5, "cm")) +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black"),
        legend.position = "bottom")

ggsave(filename = "mapa_area_vida.png", height = 10, width = 12)

## Área ----

unido_kde |> sf::st_area() / 1e6

# BBMM ----

## Transformar em um objeto de trajetória ----

traj <- adehabitatLT::as.ltraj(xy = sf_dados |>
                                         sf::st_coordinates() |>
                                         as.data.frame(),
                                       date = dados$datetime,
                                       id = dados$id,
                                       proj4string = sp::CRS("+init=epsg:32725"))

traj

plot(traj)

## Cálculo para a variância do movimento animal (sig 1) dado pelo ponto de inflexão - likelihood ----

adehabitatHR::liker(traj,
                    rangesig1 = c(1, 1000),
                    sig2 = 5,
                    byburst = FALSE,
                    plotit = TRUE)

## Calcular o modelo de utilização da área ----

bb_traj <- adehabitatHR::kernelbb(traj,
                                    sig1 = 4,
                                    sig2 = 5,
                                    grid = 1000,
                                    extent = 5,
                                    nalpha = 25)

bb_traj

bb_traj_rast <- bb_traj |>
  terra::rast()

terra::crs(bb_traj_rast) <- "EPSG:4674"

ggplot() +
  tidyterra::geom_spatraster(data = bb_traj_rast) +
  scale_fill_viridis_c() +
  coord_sf(expand = FALSE) +
  theme_bw()

## Polígonos de contorno (95% e 50%) ----

### 95% ----

bb_contour_95 <- bb_traj |>
  adehabitatHR::getverticeshr(percent = 95, unout = "km2")

bb_contour_95

### 50% ----

bb_contour_50 <- bb_traj |>
  adehabitatHR::getverticeshr(percent = 50, unout = "km2")

bb_contour_50

## Unindo ----

unido_bb <- dplyr::bind_rows(bb_contour_95 |>
                               sf::st_as_sf(crs = 32725) |>
                               sf::st_set_crs(32725) |>
                               sf::st_transform(crs = 4674) |>
                               dplyr::mutate(`Brownian Bridge Moviment Model` = "95%"),
                             bb_contour_50 |>
                               sf::st_as_sf(crs = 32725) |>
                               sf::st_set_crs(32725) |>
                               sf::st_transform(crs = 4674) |>
                               dplyr::mutate(`Brownian Bridge Moviment Model` = "50%")) |>
  dplyr::arrange(`Brownian Bridge Moviment Model` |> dplyr::desc())

unido_bb


## Visualizando ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = tapacura_sat) +
  geom_sf(data = unido_bb,
          aes(color = `Brownian Bridge Moviment Model`,
              fill = `Brownian Bridge Moviment Model`),
          alpha = 0.3,
          linewidth = 1) +
  scale_color_manual(values = c("royalblue",
                                "orange")) +
  scale_fill_manual(values = c("royalblue",
                               "orange")) +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5),
         color = guide_legend(title.position = "top",
                              title.hjust = 0.5)) +
  ggnewscale::new_scale_color() +
  geom_path(data = dados,
            aes(long, lat, color = "Trajetória")) +
  geom_sf(data = sf_dados, aes(color = "Pontos de registro")) +
  scale_color_manual(values = c("red",
                                "black")) +
  labs(color = NULL,
       x = NULL,
       y = NULL) +
  scale_x_continuous(limits = c(-35.203, -35.19)) +
  scale_y_continuous(limits = c(-8.045, -8.031)) +
  coord_sf(expand = FALSE) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              height = unit(0.5, "cm")) +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black"),
        legend.position = "bottom")

ggsave(filename = "mapa_bbmm.png", height = 10, width = 12)

## Áreas ----

ls(pattern = "bb_contour") |>
  mget(envir = globalenv())
