# Packages ----
library(tidyverse)
library(raster)
library(ggforce)
library(ggrepel)
library(scales)
library(rworldxtra)
library(sf)
library(forcats)
library(broom)

setwd("~/Casanova/Universidad/Master/Tesis/Datos")

# Límites administrativos ----
# Regionales
download.file("https://www.bcn.cl/obtienearchivo?id=repositorio/10221/10398/2/Regiones.zip",
              destfile = "Regiones.zip")

unzip("Regiones.zip")

# Nacional
download.file("https://www.ide.cl/descargas/capas/DIFROL/v3_1_limite_internacional_50k.rar",
              destfile = "v3_1_limite_internacional_50k.rar")

regiones <- read_sf("~/Casanova/Universidad/Master/Tesis/Datos_Giancarlo/Originales/divisiones territoriales/Regional.shp") %>% 
  st_transform(crs = 32718) %>% st_crop(contorno_chile)

contorno_chile <- read_sf("chile.kml") %>% 
  st_transform(crs = 32718)

ggplot() + geom_sf(data = regiones)

scales::show_col(c("#9DBF9E", "#A84268", "#FCB97D", "#C0BCB5", "#4A6C6F", "#FF5E5B"))

# 1. Datos_Giancarlo ----
# Área de estudio
cau <- read_sf("CAU_desembocadura.shp") %>% 
  st_transform(crs = 32718)

  # 1.a Base de datos de suelo ----
bd_cau <- read_csv("BD_soilprof_20SEP.csv") %>% 
  st_as_sf(coords = c(9,10),
           crs = 32719) %>%
  st_transform(crs = 32718) %>% 
  # Agregar la columna field_1
  mutate(field_1 = seq(length.out = 18958)) %>% 
  relocate(field_1, .before = ID) %>% 
  # Cortar y maskear
  st_crop(cau) %>% st_intersection(cau)
  
# ¿Cuantas observaciones hay de cada fuente? R: CHLSOC (108 ~ 22.9%), CIREN (11 ~ 0.2%), UChile (353 ~ 75.8%)
bd_cau %>% group_by(Fuente) %>% 
  summarize(N = n()) %>% 
  mutate(Percent = N/472) %>% 
  relocate(Percent, .after = N)

# Gráfico de las distintas observaciones por fuente
ggplot() + geom_sf(data = cau) +
  geom_sf(data = bd_cau, aes(color = Fuente)) + 
  facet_wrap(~Fuente) + 
  labs(title = "Muestreo por Fuente",
       subtitle = "Cuenca Cauquenes",
       caption = "Elaboración propia, 2021.",
       y = NULL,
       x = NULL) +
  theme(legend.position = "bottom",
        text = element_text(size = 16,
                            family = "TT Times New Roman"))

# Transformar las observaciones a puntos
bd_cau_pts <- distinct(bd_cau,
              ID,
              .keep_all = T)

# ¿Cuantos puntos corresponden a cada Fuente? R: CHLSOC (108 ~ 56.2%), CIREN (3 ~ 1.56%) y UChile (81 ~ 42.2%)
bd_cau_pts %>% group_by(Fuente) %>% 
  summarize(N = n()) %>% 
  mutate(Percent = N/192) %>% 
  relocate(Percent, .after = N)

  # 1.b Biomasa----
# Carga
biom_cau <- raster("./Biomasa_FONDECYT_1171560/tif/final_bm_map_luc.tif") %>% 
  projectRaster(crs = 32718) %>% 
  crop(cau) %>% mask(cau)

# Conversión a df
biom_cau_df <- biom_cau %>% as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>% rename(Biomasa = final_bm_map_luc)

biom_cau_df$Biomasa %>% summary()

# Gráfico de biomasa
ggplot() + geom_raster(data = biom_cau_df, 
                       aes(x = x, y = y, fill = Biomasa)) +
  scale_fill_gradient2(mid = "white",
                       low = "#ccb6a3",
                       high = "#486325",
                       midpoint = median(biom_cau_df$Biomasa)) +
  geom_sf(data = cau,
          alpha = 0) +
  labs(title = "Biomasa",
       subtitle = "Cuenca Cauquenes",
       caption = "Elaboración propia, 2021.",
       y = NULL,
       x = NULL) +
  theme(legend.position = "bottom",
        text = element_text(size = 16,
                            family = "TT Times New Roman"))
  
  # 1.c Landcover ----
lc <- raster("./Biomasa_FONDECYT_1171560/tif/LandCover_rf_2019.tif")
projection(lc) <- projection(cau)

st_bbox(biom);st_bbox(cau)

# Extent
lc <- lc %>% setExtent(ext = extent(cau))

extent(lc);extent(cau)

# crop() y mask()
lc_cau <- lc %>% crop(cau) %>% mask(cau)

# Conversión a df

lc_cau_df <- lc_cau %>% as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>% rename(LandCover = LandCover_rf_2019)

lc_cau_df$LandCover %>% table()
lc_cau_df %>% head(10)
lc_cau_df$LandCover

# Graficar
ggplot() + geom_tile(data = lc_cau_df,
                     aes(x = x, y = y, fill = LandCover)) + 
  scale_fill_gradientn(colours = rainbow(12))

# 2. Manejo ----
  # 2.a Profundidad frente meteorización
prof_fr.meteo <- bd_cau %>% group_by(ID) %>% filter(bottom == max(bottom))

prof_fr.meteo %>% filter(Fuente == "UChile")

  # Prof efectiva
prof_fr.meteo %>% filter(Fuente == "UChile") %>% pull(field_1)
