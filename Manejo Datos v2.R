# Packages ----
library(sf)
library(tidyverse)
library(readr)
library(raster)

# Datos ----
setwd("~/Casanova/Universidad/Master/Tesis1/Datos")
list.files(pattern = ".csv")

  # AE: cuenca Cauquenes
cau <- read_sf("CAU_desembocadura.shp") %>% 
  st_transform(crs = 32718)

  # Base de datos de suelo
bd_cau <- read_csv("BD_soilprof_20SEP.csv") %>% 
  st_as_sf(coords = c(9,10),
           crs = 32718) %>%
  st_transform(crs = 32718) %>% 
  # Agregar la columna N_obs
  mutate(N_obs = seq(length.out = 18958)) %>% 
  relocate(N_obs, .before = ID) %>% 
  # Cortar y maskear
  st_crop(cau) %>% st_intersection(cau)

# ¿Cuantas observaciones hay de cada fuente? R: CHLSOC (108 ~ 22.9%), CIREN (11 ~ 0.2%), UChile (353 ~ 75.8%)
bd_cau %>% group_by(Fuente) %>% 
  summarize(N = n()) %>% 
  mutate(Percent = N/472) %>% 
  relocate(Percent, .after = N)

# Transformar las observaciones a puntos
bd_cau_pts <- distinct(bd_cau,
                       ID,
                       .keep_all = T)

# ¿Cuantos puntos corresponden a cada Fuente? R: CHLSOC (108 ~ 56.2%), CIREN (3 ~ 1.56%) y UChile (81 ~ 42.2%)
bd_cau_pts %>% group_by(Fuente) %>% 
  summarize(N = n()) %>% 
  mutate(Percent = N/192) %>% 
  relocate(Percent, .after = N)

# Nuevos puntos ----
#Coronel de maule, pilen y tequel (loma y valle) + PINO-LA-GRANJA
novo_pts <- bd_cau %>% filter(N_obs %in% c('18935', '18939', '18944', '18949', '18954', '18958', '17212'))

#* ¿Dónde se realizaron estos puntos?
lc <- raster("~/Casanova/Universidad/Master/Tesis1/Datos/Biomasa_FONDECYT_1171560/tif/LandCover_rf_2019.tif")

novo_pts %>% mutate(lc = raster::extract(lc, novo_pts)) %>% 
  relocate(lc, .before = N_obs) %>% dplyr::select(lc, N_obs, ID, Nombre, Fuente2)

# Variables ----

  # Observaciones (mediciones)
ala <- bd_cau %>% filter(Fuente == "UChile", L != -999.00)

da <- bd_cau %>% filter(Fuente == "UChile", Da != -999.00)

pdr_pdl <- bd_cau %>% filter(Fuente == "UChile", PDR != -999.00)

cc_pmp <- bd_cau %>% filter(Fuente == "UChile", PMP != -999.00)

n <- bd_cau %>% filter(Fuente == "UChile", N != -999.00)

p <- bd_cau %>% filter(Fuente == "UChile", P != -999.00)

ph <- bd_cau %>% filter(Fuente == "UChile", pH != -999.00)

mo <- bd_cau %>% filter(MO != -999.00)

  # Puntos
ala_pts <- bd_cau %>% filter(Fuente == "UChile", L != -999.00) %>% 
  group_by(ID) %>% filter(top == min(top)) 

da_pts <- bd_cau %>% filter(Fuente == "UChile", Da != -999.00) %>% 
  group_by(ID) %>% filter(top == min(top))

pdr_pdl_pts <- bd_cau %>% filter(Fuente == "UChile", PDR != -999.00) %>% 
  group_by(ID) %>% filter(top == min(top))

pau_pts <- bd_cau %>% filter(Fuente == "UChile", PAU != -999.00) %>% 
  group_by(ID) %>% filter(top == min(top))

cc_pmp_pts <- bd_cau %>% filter(Fuente == "UChile", CC != -999.00) %>% 
  group_by(ID) %>% filter(top == min(top))

n_pts <- bd_cau %>% filter(Fuente == "UChile", N != -999.00) %>% 
  group_by(ID) %>% filter(top == min(top))

p_pts <- bd_cau %>% filter(Fuente == "UChile", P != -999.00) %>% 
  group_by(ID) %>% filter(top == min(top))

ph_pts <- bd_cau %>% filter(Fuente == "UChile", pH != -999.00) %>% 
  group_by(ID) %>% filter(top == min(top))

mo_pts <- bd_cau %>% filter(MO != -999.00) %>% 
  group_by(ID) %>% filter(top == min(top))

  # Profundidad frente meteorización
prof_fr.meteo <- bd_cau %>% group_by(ID) %>% 
  filter(bottom == max(bottom))

prof_fr.meteo %>% filter(Fuente == "UChile")

  # Prof efectiva
pos_index <- c(17109, 17114, 17119, 17125, 17131, 
               17136, 17140, 17146, 17151, 17156, 
               17159, 17164, 17170, 17114, 17174, 
               17178, 17184, 17189, 17194, 17200, 
               17203, 17207, 17212, 17217, 17221, 
               17225, 17230, 17237, 17242, 17247, 
               17252, 17257, 17262, 17266, 17272, 
               17276, 17281, 17287, 17291, 17296, 
               17302, 17307, 17311, 17317, 17321, 
               17971, 17999, 18002, 18006, 18009, 
               18692, 18698, 18704, 18710, 18935,
               18939, 18944, 18949, 18954, 18958)

prof_efect <- prof_fr.meteo %>% 
  filter(Fuente == "UChile",
         N_obs %in% pos_index)

# Exportación ----
setwd("~/Casanova/Universidad/Master/Tesis1/Datos/T1 -Densidad")

  #Observaciones
write_sf(ala,'ala.shp')
write_sf(cc_pmp,'cc_pmp.shp')
write_sf(da,'da.shp')
write_sf(mo,'mo.shp')
write_sf(n,'n.shp')
write_sf(p,'p.shp')
write_sf(pau,'pau.shp')
write_sf(pdr_pdl,'pdr_pdl.shp')
write_sf(ph,'ph.shp')

  #Puntos
write_sf(ala_pts,'ala_pts.shp')
write_sf(cc_pmp_pts,'cc_pmp_pts.shp')
write_sf(da_pts,'da_pts.shp')
write_sf(mo_pts,'mo_pts.shp')
write_sf(n_pts,'n_pts.shp')
write_sf(p_pts,'p_pts.shp')
write_sf(pau_pts,'pau_pts.shp')
write_sf(pdr_pdl_pts,'pdr_pdl_pts.shp')
write_sf(ph_pts,'ph_pts.shp')

write_sf(prof_efect, 'prof_efect.shp')
