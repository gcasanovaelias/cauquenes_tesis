#Paquetes----
library(sf)
library(tidyverse)

#Datos----
setwd("~/Casanova/Universidad/Master/Tesis/Datos_Giancarlo/Trabajados/vector")
list.files(pattern = '.shp')
ae = read_sf("Cau_18s.shp")

setwd("~/Casanova/Universidad/Master/Tesis/Datos_Giancarlo/Trabajados/vector/version 3/Observaciones")
list.files(pattern = '.shp')
soilData = read_sf("soilData.shp")

#Filtros 'Fuente'----
soil_uchile = soilData %>% filter(Fuente == 'UChile')

soil_ciren = soilData %>% filter(Fuente == 'CIREN')

soil_chlsoc = soilData %>% filter(Fuente == 'CHLSOC')

#Gráfico----
ggplot() + geom_sf(data = ae) + 
  geom_sf(data = soil_chlsoc, color = 'light green') + 
  geom_sf(data = soil_ciren, color = 'red') +
  geom_sf(data = soil_uchile, color = 'light blue') +
  geom_sf(data = novo_pts, color = "navy") +
  ggtitle('Muestreo por fuente')

ggplot() + geom_sf(data = ae) + 
  geom_sf(data = soilData, aes(color = Fuente)) +
  ggtitle('Puntos de muestreo según la fuente', 
          subtitle = "Cuenca de Cauquenes") + 
  labs(tag = " ", caption = "Datos del 20 de septiembre, 2021.") +
  xlab(NULL) + ylab(NULL)

# Profundidad efectiva----
ggplot() + geom_sf(data = ae) +
  geom_sf(data = prof_efectiva, color = 'dark green') + 
  geom_sf(data = pts_interes, color = "orange") +
  ggtitle('Profundidad efectiva', 
          subtitle = "Cuenca de Cauquenes") + 
  labs(tag = " ", caption = "Octubre, 2021.")

ggplot() + geom_sf(data = ae) +
  geom_sf(data = prof_pts2)

# 13 puntos s/ descripción
s_descr <- prof_pts2 %>% filter(ID %in% c(13240,13241,13246,13664,13665,
                                          13666,13667,13779,13780,13781,
                                          13782,13783,13784))

saveRDS(s_descr,'s_descr.rds')

ggplot() + geom_sf(data = ae) + 
  geom_sf(data =s_descr, color = "dark red")

# DSM profundidad efectiva ----
# Bootstrap mean
prof_efect_mean = map.metrics$mean %>% as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>% rename(Prof.efect = mean)

ggplot() + geom_tile(data = prof_efect_mean, aes (x = x, y = y, fill = Prof.efect)) +
  scale_fill_viridis_c(direction = -1) + 
  ggtitle('Profundidad efectiva (promedio de 10 modelos bootstrap)', 
          subtitle = "Cuenca de Cauquenes") +  
  labs(tag = " ", caption = "Octubre, 2021.") +
  xlab('') + ylab('') + 
  theme_bw()

# Bootstrap overvar
prof_efect_overvar = map.metrics$overVar %>% as("SpatialPixelsDataFrame") %>% 
  as.data.frame()

ggplot() + geom_tile(data = prof_efect_overvar, aes (x = x, y = y, fill = overVar)) +
  scale_fill_viridis_c(direction = -1,
                       option = "magma") + 
  ggtitle('Profundidad efectiva (varianza total de 10 modelos bootstrap)', 
          subtitle = "Cuenca de Cauquenes") +  
  labs(tag = " ", caption = "Octubre, 2021.") +
  xlab('') + ylab('') + 
  theme_bw()

# Profundidad efectiva

ggplot() + geom_sf(data = ae) + 
  geom_sf(data = prof_efect, color = "dark red") +
  ggtitle('Mediciones de profundidad', 
          subtitle = "Cuenca de Cauquenes") +  
  labs(tag = " ", caption = "Noviembre, 2021.") +
  xlab('') + ylab('') + 
  theme_bw()

# MOLCO

molco <- pts_soilData2 %>% filter(grepl("MOLCO", Nombre))
molco <- pts_soilData2 %>% filter(ID %in% c(13242,13260,13262,13261,13243,13244,13245))

ggplot() + geom_sf(data = ae) +
  geom_sf(data = molco, color = "dark red") +
  ggtitle('Mediciones en la microcuenca MOLCO', 
          subtitle = "Cuenca de Cauquenes") +  
  labs(tag = " ", caption = "Noviembre, 2021.") +
  xlab('') + ylab('') + 
  theme_bw()

ggplot() + geom_sf(data = molco)

st_coordinates(molco)
molco$Nombre

molco_df <- data.frame(molco$Nombre, st_coordinates(molco))
molco_df <- molco_df[-8,]

# Serie de mapas

ggplot() + geom_sf(data = ae) + 
  geom_sf(data = soilData, aes(color = Fuente)) + 
  facet_wrap(~Fuente)
