#Packages----
library(sf)
library(maptools)
library(raster)
library(spatstat)
library(tidyverse)
#PPA----
#Área de estudio
setwd("~/Casanova/Universidad/Master/Tesis/Datos_Giancarlo/Trabajados/vector")
list.files(pattern='.shp')
ae = st_read("Cau_18s.shp");ae
ae = as.owin(ae);ae #observation window

#Puntos
setwd("~/Casanova/Universidad/Master/Tesis/Datos_Giancarlo/Trabajados/vector/version 3/Puntos")
list.files(pattern='.shp')
muestras = st_read("pts_uchile.shp")
muestras = as.ppp(muestras);muestras
marks(muestras) = NULL
Window(muestras) = ae

#Landcover
setwd("~/Casanova/Universidad/Master/Tesis/Datos_Giancarlo/Trabajados/raster")
list.files(pattern = '.tif')
img = raster("lc_mask.tif")
lc = as.im(img)

plot(muestras,main = NULL,cols = rgb(0,0,0,.1),pch=20)

#Re-escalar las muestras (de m a km)----
muestras.km = rescale(muestras,1000,'km')
ae.km = rescale(ae,1000,'km')
lc.km = rescale(lc,1000,'km')

#Por muestras edáficas----
setwd("~/Casanova/Universidad/Master/Tesis/Datos_Giancarlo/Trabajados/vector/version 3/Puntos")
list.files(pattern = '.shp')
ala = st_read("ala_pts.shp"); ala = as.ppp(ala); marks(ala) = NULL; Window(ala) = ae
pmp = st_read("cc_pmp_pts.shp"); pmp = as.ppp(pmp); marks(pmp) = NULL; Window(pmp) = ae
da = st_read("da_pts.shp"); da = as.ppp(da); marks(da) = NULL; Window(da) = ae
mo = st_read("mo_pts.shp"); mo = as.ppp(mo); marks(mo) = NULL; Window(mo) = ae
n = st_read("n_pts.shp"); n = as.ppp(n); marks(n) = NULL; Window(n) = ae
p = st_read("p_pts.shp"); p = as.ppp(p); marks(p) = NULL; Window(p) = ae
pau = st_read("pau_pts.shp"); pau = as.ppp(pau); marks(pau) = NULL; Window(pau) = ae
pdr = st_read("pdr_pdl_pts.shp"); pdr = as.ppp(pdr); marks(pdr) = NULL; Window(pdr) = ae
prof = st_read("prof_pts.shp"); prof = as.ppp(prof); marks(prof) = NULL; Window(prof) = ae

prof_efect = st_read("~/Casanova/Universidad/Master/Tesis/Trabajos/Profundidad efectiva/Profundidad-Efectiva/prof_efect.shp"); prof_efect = as.ppp(prof_efect); marks(prof_efect) = NULL; Window(prof_efect) = ae

plot(ala, main = NULL, cols = rgb(0,0,0,.1), pch = 20)
plot(pmp, main = NULL, cols = rgb(0,0,0,.1), pch = 20)
plot(mo, main = NULL, cols = rgb(0,0,0,.1), pch = 20)
plot(n, main = NULL, cols = rgb(0,0,0,.1), pch = 20)
plot(p, main = NULL, cols = rgb(0,0,0,.1), pch = 20)
plot(pau, main = NULL, cols = rgb(0,0,0,.1), pch = 20)
plot(pdr, main = NULL, cols = rgb(0,0,0,.1), pch = 20)
plot(prof, main = NULL, cols = rgb(0,0,0,.1), pch = 20)
plot(prof, main = NULL, cols = rgb(0,0,0,.1), pch = 20)

plot(prof_efect, main = NULL, cols = rgb(0,0,0,.1), pch = 20)

#Re-escalar muestras edáficas
ala.km = rescale(ala, 1000, 'km')
da.km = rescale(da, 1000, 'km')
pmp.km = rescale(pmp, 1000, 'km')
mo.km = rescale(mo, 1000, 'km')
n.km = rescale(n, 1000, 'km')
p.km = rescale(p, 1000, 'km')
pau.km = rescale(pau, 1000, 'km')
pdr.km = rescale(pdr, 1000, 'km')
prof.km = rescale(prof, 1000, 'km')

#Gráfico Kernel density raster----
par(mfrow=c(1,1))
K.muestras = density(muestras.km); plot(K.muestras, main = 'GENERAL UChile', las = 1); contour(K.muestras, add = T)

K.ala = density(ala.km, sigma = 7.23); plot(K.ala, main = 'aLA', las = 1); contour(K.ala, add = T) #sigma default = 7.23 km (ancho de banda)
K.pmp = density(pmp.km, sigma = 7.23); plot(K.pmp, main = 'CC_PMP', las = 1); contour(K.pmp, add = T)
K.pau = density(pau.km); plot(K.pau, main = 'PAU', las = 1); contour(K.pau, add = T)
K.pdr = density(pdr.km); plot(K.pdr, main = 'PDR_PDL', las = 1); contour(K.pdr, add = T)
K.da = density(da.km, sigma = 7.23); plot(K.da, main = 'Da', las = 1); contour(K.da, add = T)
K.mo = density(mo.km, sigma = 7.23); plot(K.mo, main = 'MO', las = 1); contour(K.mo, add = T)
K.n = density(n.km); plot(K.n, main = 'N', las = 1); contour(K.n, add = T)
K.p = density(p.km); plot(K.p, main = 'P', las = 1); contour(K.p, add = T)
K.prof = density(prof.km); plot(K.prof, main = 'Profundidad', las = 1);contour(K.prof, add = T)

#Quadrat density----
Q = quadratcount(muestras.km, nx = 5, ny = 5)
plot(muestras.km, main = NULL, cols = rgb(0, 0, 0, .1), pch = 20); plot(Q, add = T)

Q.d = intensity(Q)
plot(intensity(Q, image = T), main = NULL, las = 1); plot(
  muestras.km, pch = 20, cex = 0.6, col = rgb(0, 0, 0, .5), add = T
)

#Quadrat density en una superficie teselada----
plot(lc)
ggplot() + geom_tile(data = lc)
plot(muestras)

#Puntos Generales
lc.km_tess = tess(image = lc.km)
Q_general = quadratcount(muestras.km, tess = tess(image = lc.km))
Q.d_general = intensity(Q_general); Q.d_general
plot(intensity(Q_general, image = T))

muestras.km %>% quadratcount(tess = lc.km_tess) %>% intensity()

#Puntos de las muestras----
setwd("~/Casanova/Universidad/Master/Tesis/Trabajos/1 Pts muestreo y densidad/Codigos")
#Datos de cantidad
muestras.km %>% quadratcount(tess = lc.km_tess) %>% as.data.frame() %>% rename (General = Freq)

ala_df_c = ala.km %>% quadratcount(tess = lc.km_tess) %>% as.data.frame() %>% rename(ala = Freq)
pmp_df_c = pmp.km %>% quadratcount(tess = lc.km_tess) %>% as.data.frame() %>% rename(pmp = Freq)
da_df_c = da.km %>% quadratcount(tess = lc.km_tess) %>% as.data.frame() %>% rename(da = Freq)
da_df_c$da %>% sum()

mo_df_c = mo.km %>% quadratcount(tess = lc.km_tess) %>% as.data.frame() %>% rename(mo = Freq)
n_df_c = n.km %>% quadratcount(tess = lc.km_tess) %>% as.data.frame() %>% rename(n = Freq)
p_df_c = p.km %>% quadratcount(tess = lc.km_tess) %>% as.data.frame() %>% rename(p = Freq)
pau_df_c = pau.km %>% quadratcount(tess = lc.km_tess) %>% as.data.frame() %>% rename(pau = Freq)
pdr_df_c = pdr.km %>% quadratcount(tess = lc.km_tess) %>% as.data.frame() %>% rename(pdr = Freq)
prof_df_c = prof.km %>% quadratcount(tess = lc.km_tess) %>% as.data.frame() %>% rename(prof = Freq)

count_df = pmp_df_c %>% full_join(ala_df_c) %>% full_join(da_df_c) %>% full_join(pdr_df_c) %>% 
  full_join(pau_df_c) %>% full_join(mo_df_c) %>% full_join(n_df_c) %>% full_join(p_df_c) %>%
  full_join(prof_df_c)
count_df = count_df[-c(1,2,6,10,11,12),]
saveRDS(count_df,'count.rds')
count_df1 <- readRDS('count.rds')
count_df - count_df1

#Datos de densidad
muestras_df = muestras.km %>% quadratcount(tess = lc.km_tess) %>% intensity() %>% round(3) %>% as.data.frame() %>% rename(General = Freq)

ala_df = ala.km %>% quadratcount(tess = lc.km_tess) %>% intensity() %>% round(3) %>% as.data.frame() %>% rename(ala = Freq)
pmp_df = pmp.km %>% quadratcount(tess = lc.km_tess) %>% intensity() %>% round(3) %>% as.data.frame() %>% rename(pmp = Freq)
da_df = da.km %>% quadratcount(tess = lc.km_tess) %>% intensity() %>% round(3) %>% as.data.frame() %>% rename(da = Freq)
mo_df = mo.km %>% quadratcount(tess = lc.km_tess) %>% intensity() %>% round(3) %>% as.data.frame() %>% rename(mo = Freq)
n_df = n.km %>% quadratcount(tess = lc.km_tess) %>% intensity() %>% round(3) %>% as.data.frame() %>% rename(n = Freq)
p_df = p.km %>% quadratcount(tess = lc.km_tess) %>% intensity() %>% round(3) %>% as.data.frame() %>% rename(p = Freq)
pau_df = pau.km %>% quadratcount(tess = lc.km_tess) %>% intensity() %>% round(3) %>% as.data.frame() %>% rename(pau = Freq)
pdr_df = pdr.km %>% quadratcount(tess = lc.km_tess) %>% intensity() %>% round(3) %>% as.data.frame() %>% rename(pdr = Freq)
prof_df = prof.km %>% quadratcount(tess = lc.km_tess) %>% intensity() %>% round(3) %>% as.data.frame() %>% rename(prof = Freq)

density_df = pmp_df %>% full_join(ala_df) %>% full_join(da_df) %>% full_join(pdr_df) %>% full_join(pau_df) %>%
  full_join(mo_df) %>% full_join(n_df) %>% full_join(p_df) %>% full_join(prof_df)
density_df = density_df[-c(1,2,6,10,11,12),]
saveRDS(density_df,'density.rds')
density_df1 <- readRDS('density.rds')

density_df - density_df

density = density_df %>% gather(key = variables, value = densidad,-tile)

density %>% group_by(variables) %>% select(variables, densidad) %>% summarise(media = mean())


