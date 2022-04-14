#Paquetes----
library(sf)
library(tidyverse)

#Observaciones----
setwd("~/Casanova/Universidad/Master/Tesis/Datos_Giancarlo/Originales/Suelos")
list.files(pattern = '.shp')

soilData=read_sf("Datos_Suelos_Cauquenes.shp")

soil_uchile = soilData %>% filter(Fuente == "UChile")

#Leli Soto
soilData %>% select(ID, Datos, Fuente, Fuente2, top, bottom) %>% 
  filter(top == 40 & bottom == 60) %>% View()

macalwando = soilData %>% select(ID, Datos,Fuente, Fuente2, top, bottom, x, y) %>%
  filter(Fuente2 == "Macari  y Lwando")
  
ggplot() + geom_sf(data = ae) + 
  geom_sf(data = macalwando) +
  ggtitle('Macari y Lwando')

  #Para profundidad
soil_seguel = soil_uchile %>% filter(Datos %in% c('Seguel','Edu'))

#Puntos de observaciones----
pts_soilData = soilData %>% filter(top %in% c('0','3') | ID == '13498' & top == '10' |
                                     ID == '13238' & top == '6' | ID == '13396' & top == '20' |
                                     ID == '13262' & top == '13') %>% filter(ID != 13272) #Se elimina las observaciones de PRAD-LA-GRANJA producto de que posee las mismas coordenadas que PINO-LA-GRANJA
soilData$x %>% unique() == pts_soilData$x %>% unique() #se cumple la igualdad de puntos

pts_uchile = soil_uchile %>% filter(top %in% c('0','3') | ID == '13498' & top == '10' |
                                      ID == '13238' & top == '6' | ID == '13396' & top == '20' |
                                      ID == '13262' & top == '13')
soil_uchile$x %>% unique() == pts_uchile$x %>% unique()

pts_seguel = soil_seguel %>% filter(top %in% c('0','3') | ID == '13498' & top == '10' |
                                      ID == '13238' & top == '6' | ID == '13396' & top == '20' |
                                      ID == '13262' & top == '13') %>% filter(ID != 13272)
soil_seguel$x %>% unique() == pts_seguel$x %>% unique()

#Filtrado especial----
soil_uchile[soil_uchile$Fuente2 != "Pino Eli 22-julio-2019",] %>% dim()
soil_uchile[soil_uchile$Fuente2 != "Matorral Eli 22-julio-2019",] %>% dim()

soil_uchile[soil_uchile$Fuente2 != "Pino Eli 22-julio-2019" & soil_uchile$Fuente2 != "Matorral Eli 22-julio-2019",] %>% dim()

soil_uchile %>% dplyr::filter(Fuente2 != "Pino Eli 22-julio-2019" & Fuente2 != "Matorral Eli 22-julio-2019") %>% dim()
soil_uchile %>% filter(Fuente2 != "Pino Eli 22-julio-2019") %>% dim()
soil_uchile %>% filter(Fuente2 != "Matorral Eli 22-julio-2019") %>% dim()
soil_uchile %>% filter(Fuente2(contains('Eli'))) %>% dim()

dplyr::filter(soil_uchile, Fuente2 != "Matorral Eli 22-julio-2019") %>% dim()

soil_uchile %>% Filter(Fuente2 )

#Mediciones----
  #Observaciones
ala = soil_uchile %>% filter(L != -999.00)
da = soil_uchile %>% filter(Da != -999.00)
pdr_pdl = soil_uchile %>% filter(PDR != -999.00)
pau = soil_uchile %>% filter(PAU != -999.00)
cc_pmp = soil_uchile %>% filter(PMP != -999.00)
n = soil_uchile %>% filter(N != -999.00)
p = soil_uchile %>% filter(P != -999.00)
ph = soil_uchile %>% filter(pH != -999.00)

mo = soilData %>% filter(MO != -999.00)

soil_uchile[soil_uchile$pH != -999.00,] %>% dim() #Las variables cuantitativas están correctamente filtradas
soilData[soilData$MO != -999.00,] %>% dim()

  #Puntos
ala_pts = pts_uchile %>% filter(L != -999.00); ala_pts = ala_pts[-25,]
da_pts = pts_uchile %>% filter(Da != -999.00); da_pts = da_pts[-c(26,53),]
pdr_pdl_pts = pts_uchile %>% filter(PDR != -999.00); pdr_pdl_pts = pdr_pdl_pts[-26,]
pau_pts = pts_uchile %>% filter(PAU != -999.00); pau_pts = pau_pts[-26,]
cc_pmp_pts = pts_uchile %>% filter(CC != -999.00); cc_pmp_pts = cc_pmp_pts[-c(26,52),]
n_pts = pts_uchile %>% filter(N != -999.00)
p_pts = pts_uchile %>% filter(P != -999.00)
ph_pts = pts_uchile %>% filter(pH != -999.00)

mo_pts = pts_soilData %>% filter(MO != -999.00)

  #Congruencia de coordenadas
ala$y %>% unique() == ala_pts$y %>% unique() #ahora sí hay congruencia!

ala_pts$x %>% unique() %>% length(); ala_pts$x %>% length()

#Comprobación
ala_pts = ala %>% filter(top %in% c('0','3') | ID == '13498' & top == '10' | ID == '13238' & top == '6' | ID == '13396' & top == '20')
  #sí es congruente

  #Visualización mediciones----
ggplot() + geom_sf(data = n) + ggtitle('n')

ggplot() + geom_sf(data = n_pts) + ggtitle('n pts')

  #Guardar shapes (sf)----
  #Observaciones
setwd("~/Casanova/Universidad/Master/Tesis/Datos_Giancarlo/Trabajados/vector/version 3/Observaciones")
write_sf(soilData,'soilData.shp')
write_sf(soil_uchile,'soil_uchile.shp')
write_sf(soil_seguel,'soil_seguel.shp')
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
write_sf(pts_soilData,'pts_soilData.shp',overwrite = T) #no es necesario escribir el overwrite, por default esta en verdadero
write_sf(pts_uchile,'pts_uchile.shp')
write_sf(pts_seguel,'pts_seguel.shp')
write_sf(ala_pts,'ala_pts.shp')
write_sf(cc_pmp_pts,'cc_pmp_pts.shp')
write_sf(da_pts,'da_pts.shp')
write_sf(mo_pts,'mo_pts.shp')
write_sf(n_pts,'n_pts.shp')
write_sf(p_pts,'p_pts.shp')
write_sf(pau_pts,'pau_pts.shp')
write_sf(pdr_pdl_pts,'pdr_pdl_pts.shp')
write_sf(ph_pts,'ph_pts.shp')

  #Profundidad----
soil_seguel %>% select(Datos,ID, top, bottom) %>% View()
bottom = c(6, 11, 16, 22, 28, 33, 37, 43, 48, 53, 56, 61, 67, 71, 75, 81, 86, 91, 97, 100, 104, 109, 114, 118, 122, 126, 133, 138, 143, 148, 153, 158, 162, 168, 172, 177, 183, 187, 192, 198, 203, 207, 213, 217, 220, 224, 227, 231, 234)

prof_pts = soil_seguel[bottom,]
View(prof_pts)

prof_pts$x %>% unique() == pts_seguel$x %>% unique()
prof_pts$x %>% unique() %>% length(); pts_seguel$x %>% unique() %>% length()

  #Estadísticas Profundidad
summary(prof_pts$bottom)
sd(prof_pts$bottom)

  #Guardar capa
setwd("~/Casanova/Universidad/Master/Tesis/Datos_Giancarlo/Trabajados/vector/version 3/Puntos")
write_sf(prof_pts, 'prof_pts.shp')

da_pts %>% dim()
pdr_pdl_pts %>% dim()

  