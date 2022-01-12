# Packages ----
library(sf)
library(tidyverse)
library(readr)
library(raster)

# Datos ----
setwd("~/Casanova/Universidad/Master/Tesis/Datos")
list.files(pattern = ".csv")

  # AE: cuenca Cauquenes
cau <- read_sf("CAU_desembocadura.shp") %>% 
  st_transform(crs = 32718)

  # Base de datos suelo (30NOV)
bd_cau <- read_csv("BD_soilprof_17DIC.csv") %>% 
  st_as_sf(coords = c(9,10),
           # Sistema de proyección latitud-longitud (grados decimales)
           crs = 4326) %>% 
  # Reproyectar a sistema de proyección UTM zone 18S
  st_transform(crs = 32718) %>% 
  # distinct() %>% : Eliminar observaciones repetidas. tambien puede ser unique()
  # Agregar columna N_obs
  mutate(N_obs = seq(length.out = 19315)) %>% 
  relocate(N_obs, .before = ID) %>% 
  # Cortar e intersectar con el AE
  st_crop(cau) %>% 
  st_intersection(cau)

# ¿Cuantas observaciones hay de cada fuente? R: CHLSOC (108 ~ 21.1%), CIREN (11 ~ 2.15%), UChile (392 ~ 76.7%)
bd_cau %>% 
  group_by(Fuente) %>% 
  summarize(f = n()) %>% 
  mutate(h = f/sum(f)*100) %>% 
  relocate(h, .after = f)

# Transformar las observaciones a puntos
bd_cau_pts <- distinct(.data = bd_cau,
                       ID,
                       .keep_all = T)

# ¿Cuantos puntos corresponden a cada Fuente? R: CHLSOC (108 ~ 54.3%), CIREN (3 ~ 1.51%) y UChile (88 ~ 44.2%)
bd_cau_pts %>% 
  group_by(Fuente) %>% 
  summarize(f = n()) %>% 
  mutate(h = f/sum(f)*100) %>% 
  relocate(h, .after = f)

# Nuevos puntos ----
# Identificación nuevos puntos: Deben estar en formato data frame, no en sf

# anti_join(): returns all rows from x without a match in y
novo_pts <- anti_join(
  # BD actualizada
  x = tibble(bd_cau),
  # BD anterior 
  y = tibble(bd_cau30NOV),
  # La comparación será según la columna ID. Si se hace en función de N_obs entrega información de desde cuando empieza a variar esta columna
  by = "N_obs") %>% 
  # Conversión del tibble de vuelta a un sf
  st_as_sf(crs = 32718)

#* ¿Dónde se realizaron estos puntos?
lc <- raster("~/Casanova/Universidad/Master/Tesis/Datos/Biomasa_FONDECYT_1171560/tif/LandCover_rf_2019.tif") %>% 
  projectRaster(crs = 32718) %>% 
  crop(cau) %>% 
  mask(cau)

novo_pts %>% 
  mutate(lc = raster::extract(lc, novo_pts)) %>% 
  relocate(lc, .before = N_obs) %>% 
  dplyr::select(lc, N_obs, ID, Nombre, Fuente2)

# Identificación de observaciones duplicadas ----
obs_dup <- bd_cau %>%
  # Se remueve la columna N_obs ya que varía en todas las observaciones
  dplyr::select(-N_obs) %>% 
  # duplicated(): función base de R que entrega un objeto lógico que indica si la observación se ha repetido
  duplicated()

bd_cau[obs_dup,] %>% View()

# Variables ----

  # Observaciones (mediciones)
ala <- bd_cau %>% filter(Fuente == "UChile", L != -999.00)

da <- bd_cau %>% filter(Fuente == "UChile", Da != -999.00)

pdr_pdl <- bd_cau %>% filter(Fuente == "UChile", PDR != -999.00)

cc_pmp <- bd_cau %>% filter(Fuente == "UChile", PMP != -999.00)

pau <- bd_cau %>% filter(Fuente == "UChile", PAU != -999.00)

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
pts_uchile <- bd_cau %>% 
  filter(Fuente == "UChile") %>% 
  group_by(ID) %>% 
  filter(bottom == max(bottom))

  # Prof efectiva
prof_efect <- bd_cau %>% 
  filter(N_obs %in% {c(17109, 17114, 17131, 17135, 17140, 17145, 17156, 17159, 17164, 17170, 
                       17174, 17178, 17184, 17189, 17194, 17200, 17203, 17207, 17212, 17217, 
                       17221, 17225, 17230, 17237, 17242, 17247, 17252, 17257, 17261, 17266, 
                       17272, 17276, 17280, 17286, 17291, 17296, 17301, 17307, 17310, 17317, 
                       17320, 17663, 18003, 18006, 18009, 18013, 17119, 17125, 17151, 18696, 
                       18706, 18713, 18719, 18944, 18948, 18953, 18958, 18963, 18967, 19154, 
                       19159, 19164, 19169, 19307, 19310, 19315)})

# Exportación ----
setwd("~/Casanova/Universidad/Master/Tesis/Datos/T1 -Densidad")

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

write_sf(pts_uchile, "pts_uchile.shp")
