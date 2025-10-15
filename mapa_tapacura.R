# Pacotes ----

library(geobr)

library(tidyverse)

library(terra)

library(tidyterra)

library(ggview)

library(patchwork)

# Dados ----

## Brasil ----

### Importando ----

br <- geobr::read_state(year = 2019)

### Visualizando -----

ggplot() +
  geom_sf(data = br, color = "black")

## Pernambuco ----

### Importando ----

pe <- br |>
  dplyr::filter(abbrev_state == "PE")

### Visualizando ----

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = pe, color = "black", fill = "goldenrod")

## imagem de satélite ----

### Importando ----

sat <- terra::rast("tapacura.tif")

### Visualizando ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = sat) +
  coord_sf(expand = FALSE) +
  theme_bw()

# Mapa ----

## Escalas de diferença ----

escala_x <- (35.23 - 35.15) * 200

escala_x

escala_y <- (8.07 - 8.02) * 200

escala_y

## inset map ----

inset_map <- ggplot(data = br) +
  geom_sf(color = "black", fill = "white", linewidth = 0.5) +
  geom_sf(data = pe, color = "black", fill = "goldenrod", linewidth = 0.5) +
  scale_x_continuous(limits = c(-74, -16)) +
  scale_y_continuous(limits = c(-33.76, 6.2)) +
  theme_void() +
  tidyterra::geom_spatraster_rgb(data = sat) +
  ggmagnify::geom_magnify(from = c(-35.23, -35.15,
                                   -8.07, -8.02),
                          to = c(-33, -33 + escala_x, -4, -4 + escala_y),
                          shadow = TRUE,
                          colour = "darkred",
                          proj.fill = alpha("red", 0.5),
                          linewidth = 1)

inset_map

## Mapa principal ----

mapa_principal <- ggplot(data = br) +
  geom_sf(color = "black", aes(fill = "Brasil"), linewidth = 1) +
  geom_sf(data = pe, color = "black", aes(fill = "Pernambuco"), linewidth = 1) +
  tidyterra::geom_spatraster_rgb(data = sat) +
  coord_sf(label_graticule = "NSWE",
           xlim = c(-35.23, -35.15),
           ylim = c(-8.07, -8.02)) +
  scale_x_continuous(breaks = seq(-35.22, -35.15, 0.025)) +
  scale_y_continuous(breaks = seq(-8.08, -8.02, 0.02)) +
  scale_fill_manual(values = c("white",
                               "goldenrod")) +
  labs(fill = NULL) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15, face = "bold"),
        panel.border = element_rect(color = "darkred", linewidth = 2),
        legend.text = element_text(color = "black", size = 15, face = "bold"),
        legend.position = "bottom") +
  ggspatial::annotation_scale(location = "bl",
                              text_face = "bold",
                              text_cex = 2,
                              text_col = "black",
                              unit_category = "metric",
                              bar_cols = c("black", "gold")) +
  ggspatial::annotation_north_arrow(location = "tl",
                                    height = unit(4, "cm"),
                                    width = unit(4, "cm"),
                                    style = ggspatial::north_arrow_nautical(fill = c("black", "gold"),
                                                                            text_face = "bold",
                                                                            text_size = 20)) +
  ggview::canvas(height = 10, width = 12)

mapa_principal

## Unindo os mapas ----

cowplot::ggdraw(mapa_principal) +
  cowplot::draw_plot(inset_map,
                     x = 0.66,
                     y = 0.57,
                     height = 0.35,
                     width = 0.35) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_tapacura.png", height = 10, width = 12)
