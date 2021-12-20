# Packages ----
library(tidyverse)
library(readr)
library(sf)

# Datos ----
setwd("~/Casanova/Universidad/Master/Tesis/Datos_Giancarlo/Originales/septiembre")
list.files(pattern = ".csv")
data <- read_csv("BD_soilprof_20SEP.csv") %>%
  st_as_sf(coords=c(9,10),
           crs = "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")  %>% 
  st_transform(crs = "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# data2 <- left_join(data, read_csv("BD_soilprof_20SEP.csv")) #agregarle las columnas x e y

# FORMA ALTERNATIVA - FUNCIONA: 
data <- read_sf("BD_soilprof_20SEP_18s.shp") %>% relocate(field_1) #relocate(): mueve una columna
data1 <- read_sf("BD_soilprof_18.shp")

# otros argumentos útiles: ".after =" y ".before = "

cau <- read_sf("~/Casanova/Universidad/Master/Tesis/Datos_Giancarlo/Originales/septiembre/Cau_desem_18.shp")

soilData2 <- data %>% st_intersection(cau)
soilData1 <- data1 %>% st_intersection(cau)

soil_uchile2 <- soilData2 %>% filter(Fuente == "UChile")

# Entendiendo las nuevas mediciones ----
  #Coronel de maule, pilen y tequel (loma y valle) + PINO-LA-GRANJA
novo_obs <- soilData2 %>% filter(Nombre %in% c('CORONEL DE MAULE- LOMA', 'CORONEL DE MAULE-VALLE', 'PILEN-LOMA', 'PILEN-VALLE', 'TEQUEL-LOMA', 'TEQUEL-VALLE', 'PINO-LA-GRANJA'))
novo_pts <- novo_obs %>% filter(field_1 %in% c('18935', '18939', '18944', '18949', '18954', '18958', '17212'))

# ¿Dónde se realizaron estos puntos?
img = raster("~/Casanova/Universidad/Master/Tesis/Datos_Giancarlo/Trabajados/raster/lc_mask.tif")
raster::extract(img, novo_pts)
novo_pts$lc <- raster::extract(img, novo_pts)

#Puntos de observaciones----
pts_soilData2 = soilData2 %>% filter(top %in% c('0','3') | 
                                       field_1 %in% c('17105', '17223', '17683', '17969', '18688', '18706'))

soilData2$y %>% unique() == pts_soilData2$y %>% unique()

pts_soilData2$x %>% unique() %>% length(); pts_soilData2$x %>% length() #los datos no cuadran porque justamente estamos agregando nuevas observaciones subsuperficiales producto de que las superficiales no poseen todos los registros

pts_uchile2 = pts_soilData2 %>% filter(Fuente == "UChile")
pts_uchile1 = pts_soilData1 %>% filter(Fuente == "UChile")
soil_uchile2$x %>% unique() == pts_uchile2$x %>% unique()
pts_uchile2$x %>% unique() %>% length(); pts_uchile2$x %>% length() #Lo mismo, no cuadran producto de que estamos agregando observaciones subsuperficiales las cuales indudablemente tendran las mismas coordenadas

#Mediciones----
  #Observaciones
ala = soil_uchile2 %>% filter(L != -999.00)
da = soil_uchile2 %>% filter(Da != -999.00)
pdr_pdl = soil_uchile2 %>% filter(PDR != -999.00)
pau = soil_uchile2 %>% filter(PAU != -999.00)
cc_pmp = soil_uchile2 %>% filter(PMP != -999.00)
n = soil_uchile2 %>% filter(N != -999.00)
p = soil_uchile2 %>% filter(P != -999.00)

ph = soil_uchile2 %>% filter(pH != -999.00)

mo = soilData2 %>% filter(MO != -999.00)
mo1 = soilData1 %>% filter(MO != -999.00)

  #Obs especial (Playa Blanca no posee mediciones de Da y PDR-PDL en el horizonte superficial pero sí en los demás)
obs_pb <- soilData2 %>% filter(field_1 == 18706) %>% as.data.frame(obs_pb) #se debe transformar a data frame debido a que full_join() no aplica para objetos sf

  #Puntos
ala_pts2 = pts_uchile2 %>% filter(L != -999.00 & field_1 != 17223)
da_pts2 = pts_uchile2 %>% filter(Da != -999.00 & field_1 != 17223 &  field_1 != 17683) %>% full_join(obs_pb)
pdr_pdl_pts2 = pts_uchile2 %>% filter(PDR != -999.00 & field_1 != 17223) %>% full_join(obs_pb)
pau_pts2 = pts_uchile2 %>% filter(PAU != -999.00 & field_1 != 17223)
cc_pmp_pts2 = pts_uchile2 %>% filter(CC != -999.00 & field_1 != 17223 & field_1 != 17683)
n_pts2 = pts_uchile2 %>% filter(N != -999.00)
p_pts2 = pts_uchile2 %>% filter(P != -999.00)
ph_pts2 = pts_uchile2 %>% filter(pH != -999.00)

mo_pts2 = pts_soilData2 %>% filter(MO != -999.00)

  #Congruencia de coordenadas
da$y %>% unique() == da_pts2$y %>% unique() #ahora sí hay congruencia!

da_pts2$x %>% unique() %>% length(); da_pts2$x %>% length()

# Profundidad
prof_pts2 <- soil_uchile2 %>% filter(field_1 %in% c(17109, 17114, 17119, 17125, 17131, 
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
                                               18939, 18944, 18949, 18954, 18958))
                                    

# Profundidad efectiva
prof_efectiva <- soil_uchile2 %>% filter(field_1 %in% c(17135, 17140, 17159, 17194, 
                                                        17221, 17237, 17266, 17291, 
                                                        17307))

pts_interes <- soil_uchile2 %>% filter(field_1 %in% c(17172, 17260))

#Guardar shapes (sf)----
  #Observaciones
setwd("~/Casanova/Universidad/Master/Tesis/Datos_Giancarlo/Trabajados/vector/version 3/Observaciones")
write_sf(soilData2,'soilData.shp')
write_sf(soil_uchile2,'soil_uchile.shp')

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
setwd("~/Casanova/Universidad/Master/Tesis/Datos_Giancarlo/Trabajados/vector/version 3/Puntos")
write_sf(pts_soilData2,'pts_soilData.shp',overwrite = T) #no es necesario escribir el overwrite, por default esta en verdadero
write_sf(pts_uchile2,'pts_uchile.shp')

write_sf(ala_pts2,'ala_pts.shp')
write_sf(cc_pmp_pts2,'cc_pmp_pts.shp')
write_sf(da_pts2,'da_pts.shp')
write_sf(mo_pts2,'mo_pts.shp')
write_sf(n_pts2,'n_pts.shp')
write_sf(p_pts2,'p_pts.shp')
write_sf(pau_pts2,'pau_pts.shp')
write_sf(pdr_pdl_pts2,'pdr_pdl_pts.shp')
write_sf(ph_pts2,'ph_pts.shp')

write_sf(prof_pts2, 'prof_pts.shp')

# BÚSQUEDA

soilData2 %>% select(field_1,ID,Nombre,Comentario,top,bottom) %>% filter(bottom== 13) %>% View()

soilData2 %>% names()

soilData2 %>% head()

soilData2$Fuente2 %>% unique()

Bosque_Cte_1 <- soilData2 %>% filter(ID == 13389)

BNReferencia <- soilData2 %>% filter(ID == 13498)


lwando <- soilData2 %>% filter(Fuente2 == "Macari  y Lwando")

ggplot() + geom_sf(data=ae) + geom_sf(data=lwando)

ggplot() + geom_sf(data=ae) + geom_sf(data=BNReferencia, color = "blue") + geom_sf(data = Bosque_Cte_1, color = "red")


