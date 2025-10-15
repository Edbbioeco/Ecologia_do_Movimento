install.packages(c("adehabitatHR", "sf", "lubridate", "ggspatial", "tidyverse", "raster"))

library(adehabitatHR)
library(sf)
library(lubridate)
library(tidyverse)
library(ggspatial)
library(raster)



ecomov <- read.csv("planilha_movimento.csv",
                   stringsAsFactors = FALSE) #diz ao R para não converter automaticamente colunas de texto (strings) em fatores (factors, dados categóricos) quando você lê um arquivo

# Visualizar estrutura dos dados
head(ecomov)
str(ecomov)


# Converter data e hora para formato datetime corretamente
ecomov$datetime <- as.POSIXct(
  paste(ecomov$date, ecomov$time),
  format = "%m/%d/%Y %I:%M:%S %p"
)

# Ordenar por data/hora
ecomov <- ecomov[order(ecomov$datetime), ]

# Verificar se a conversão funcionou
sum(is.na(ecomov$datetime))  # Deve ser 0 se tudo converteu corretamente
head(ecomov$datetime)


# 2. CONVERTER PARA OBJETO ESPACIAL
# Converter para objeto espacial sf (simple features)
ecomov_sf <- st_as_sf(ecomov, coords = c("long", "lat"), crs = 4326) #WGS84

# Reprojetar para UTM 25S
ecomov_utm <- st_transform(ecomov_sf, crs = 32725)

# Extraindo as coordenadas para colunas separadas
coords <- st_coordinates(ecomov_utm)

# Adicionando as colunas X (long) e Y (lat) ao data frame original
ecomov$long <- coords[, "X"]
ecomov$lat <- coords[, "Y"]

#confere
ecomov

#exportar (vai ser importante no Qgis)
write.csv(ecomov, file = "C:/Users/paulo/OneDrive/Documentos/Paulo Braga/Documentos/MEU PENDRIVE (F1)/UFPE/Docência/PPGBA/Ecologia do Movimento disciplina/Aula prática/ecomov_utm.csv")

#MASSA.. AGORA VAMOS CRIAR O OBJETO ESPACIAL EM UTM
ecomov_sf_2 <- st_as_sf(ecomov, coords = c("long", "lat"), crs = 32725) #WGS 84 UTM ZONE 25 S


# Visualizar pontos no mapa

# Plot com mapa de fundo
ggplot() +
  geom_sf(data = ecomov_sf_2, color = "red", size = 1.5, alpha = 0.7) +
  labs(title = "Pontos de Movimento - Dados EcoMov",
       subtitle = paste("Total de pontos:", nrow(ecomov_sf_2))) +
  theme_minimal()

# Converter para SpatialPointsDataFrame
ecomov_sp <- as(ecomov_sf_2, "Spatial")

#######################################################################
### TUDO CERTO COM OS DADOS. BORA BRINCAR DE CALCULAR AS ÁREAS DE VIDA#
#######################################################################


#MINIMUM CONVEX POLYGON

# Criar Minimum Convex Polygon (MCP)
mcp_95 <- mcp(ecomov_sp, percent = 95)
mcp_100 <- mcp(ecomov_sp, percent = 100)

# Visualizar MCP
ggplot() +
  # MCP 95%
  geom_sf(data = st_as_sf(mcp_95), fill = NA, color = "blue", linewidth = 1.5) +
  # MCP 100%
  geom_sf(data = st_as_sf(mcp_100), fill = NA, color = "green", linewidth = 1) +
  # Pontos originais
  geom_sf(data = ecomov_sf, size = 1, color = "red", alpha = 1)+
  # Título e tema
  labs(title = "Mínimo Polígono Convexo") +
  theme_minimal()

# Converter o MCP para sf para calcular a área
mcp_95_sf <- st_as_sf(mcp_95)
mcp_100_sf <- st_as_sf(mcp_100)


# Reprojetar para UTM (exemplo: zona 25S para nossa área de estudo)
mcp_95_utm <- st_transform(mcp_95_sf, crs = 32725)
mcp_100_utm <- st_transform(mcp_100_sf, crs = 32725)

# Calcular área em m²
mcp_95_utm$area_m2 <- st_area(mcp_95_utm)
mcp_100_utm$area_m2 <- st_area(mcp_100_utm)

# Converter para km²
mcp_95_utm$area_km2 <- as.numeric(mcp_95_utm$area_m2) / 1e6
mcp_100_utm$area_km2 <- as.numeric(mcp_100_utm$area_m2) / 1e6

# Resultados
mcp_95_utm$area_km2
mcp_100_utm$area_km2

#####################
######DEU BOM!########
#######################

#KERNEL DENSITY ESTIMATOR

## Criar KDE com parâmetros de suavização
kde <- kernelUD(ecomov_sp, h = "href") #pode ser href, LSCV ou conhecimento biológico
plot(kde)

# Obter polígonos de contorno (95% e 50%)
kde_contour_95 <- getverticeshr(kde, percent = 95)
kde_contour_50 <- getverticeshr(kde, percent = 50)

# Visualizar KDE
ggplot() +
  # KDE 95%
  geom_sf(data = st_as_sf(kde_contour_95), fill = NA, color = "blue", linewidth = 1.5) +
  # KDE 50%
  geom_sf(data = st_as_sf(kde_contour_50), fill = NA, color = "green", linewidth = 1) +
  # Pontos originais
  geom_sf(data = ecomov_sf, size = 1, color = "red", alpha = 1)+
  # Título e tema
  labs(title = "Kernel Density Estimator") +
  theme_minimal()

# Converter o KDE para sf para calcular a área
kde_95_sf <- st_as_sf(kde_contour_95)
kde_50_sf <- st_as_sf(kde_contour_50)


# Reprojetar para UTM (exemplo: zona 25S para nossa área de estudo)
kde_95_utm <- st_transform(kde_95_sf, crs = 32725)
kde_50_utm <- st_transform(kde_50_sf, crs = 32725)

# Calcular área em m²
kde_95_utm$area_m2 <- st_area(kde_95_utm)
kde_50_utm$area_m2 <- st_area(kde_50_utm)

# Converter para km²
kde_95_utm$area_km2 <- as.numeric(kde_95_utm$area_m2) / 1e6
kde_50_utm$area_km2 <- as.numeric(kde_50_utm$area_m2) / 1e6

# Resultados
kde_95_utm$area_km2
kde_50_utm$area_km2

#####################
######DEU BOM!########
#######################

# BROWNIAN BRIDGE MOVEMENT MODEL


# Transformar em um objeto de trajetória
ecomov.ltraj <- as.ltraj(xy = ecomov[,c("long", "lat")],
                         date = ecomov$datetime,
                         id = ecomov$id,
                         proj4string = CRS("+init=epsg:32725"))
plot(ecomov.ltraj)

#Cálculo para a variância do movimento animal (sig 1) dado pelo ponto de inflexão - likelihood
liker(ecomov.ltraj,
      rangesig1 = c(1,1000),
      sig2 = 5,
      byburst = FALSE,
      plotit = TRUE)

#sig 1 deu 2. Precisa testar vários ranges para chegar no parâmetro adequado

#Vamos calcular o modelo de utilização da área
bb_ecomov <- kernelbb(ecomov.ltraj,
                      sig1 = 2,
                      sig2 = 5,
                      grid = 500, #resolução da grade. Quanto maior, mais suave o resultado (e mais poder computacional)
                      extent = 5, #extensão da área de estudo. 1 = 100% dos pontos
                      nalpha = 25) #numeros de parâmetros para suavização que são utilizados.

plot(bb_ecomov)

# Obter polígonos de contorno (95% e 50%)
bb_contour_95 <- getverticeshr(bb_ecomov, percent = 95, unout = "km2")
bb_contour_50 <- getverticeshr(bb_ecomov, percent = 50, unout = "km2")
plot(bb_contour_95)
plot(bb_contour_50)

#Às vezes o modelo fica hiperajustado (principalmente em áreas pequenas) e, para corrigir áreas irreais, você pode alterar os valores dos contornos
bb_contour_60<- getverticeshr(bb_ecomov, percent = 60, unout = "km2")
bb_contour_30 <- getverticeshr(bb_ecomov, percent = 30, unout = "km2")
plot(bb_contour_30)
plot(bb_contour_60)

#ÁREA:
bb_contour_95
bb_contour_50
bb_contour_30


#AGORA VAMOS CONVERTER TODOS NOSSOS AQUIVOS DE INTERESSE EM SHAPEFILE, SALVAR E EXPORTAR PARA O QGIS

setwd("C:/Users/paulo/OneDrive/Documentos/Paulo Braga/Documentos/MEU PENDRIVE (F1)/UFPE/Docência/PPGBA/Ecologia do Movimento disciplina/Aula prática")

#MCPs
#shapefiles
write_sf(st_as_sf(mcp_95), "mcp_95.shp")
write_sf(st_as_sf(mcp_100), "mcp_100.shp")

#KDEs
#shapefile
write_sf(st_as_sf(kde_contour_95), "kde_95.shp")
write_sf(st_as_sf(kde_contour_50), "kde_50.shp")
#raster
kde_raster <- raster(kde)
crs(kde_raster) <- CRS("+init=epsg:32725")
writeRaster(kde_raster, "kde.tif", format = "GTiff", overwrite = TRUE)

#BBMMs
#shapefiles
write_sf(st_as_sf(bb_contour_95), "bbmm_95.shp")
write_sf(st_as_sf(bb_contour_50), "bbmm_50.shp")
write_sf(st_as_sf(bb_contour_30), "bbmm_30.shp")
#raster
bb_raster <- raster(bb_ecomov)
crs(bb_raster) <- CRS("+init=epsg:32725")
writeRaster(bb_raster, "bb.tif", format = "GTiff", overwrite = TRUE)

#TRAJETÓRIAS
trajetorias<-ltraj2sldf(ecomov.ltraj, byid = FALSE)
plot(trajetorias)
write_sf(st_as_sf(trajetorias), "trajetorias.shp")

##### FIM  ######

