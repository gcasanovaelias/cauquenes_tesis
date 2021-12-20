#Paquetes----
library(sf)
library(tidyverse)

#Datos----
setwd("~/Casanova/Universidad/Master/Tesis/Datos")
list.files(pattern = ".shp")
  # Cuenca Cauquenes
cau <- read_sf("CAU_desembocadura.shp") %>% st_transform(crs = 32718)

  # Muestreo biomasa
bio_bn <- read_sf("./Biomasa_FONDECYT_1171560/shp/Biomasa_BN_actualizada_2021.shp") %>% 
  mutate(Cobertura = "Bosque nativo") %>% 
  select(Cobertura, Parcela, AGB..ton.h)

bio_mt <- read_sf("./Biomasa_FONDECYT_1171560/shp/matorrales_corr.shp") %>% 
  mutate(Cobertura = "Matorral") %>% 
  select(Cobertura, Parcela, AGB..ton.h)

bio_pf <- read_sf("./Biomasa_FONDECYT_1171560/shp/plantacion_forestal_cor.shp") %>% 
  mutate(Cobertura = "Plantacion") %>% 
  select(Cobertura, Parcela, AGB__ton_h) %>% 
  rename(AGB..ton.h = AGB__ton_h)

bio_pr <- read_sf("./Biomasa_FONDECYT_1171560/shp/s2_praderas_reference_cor.shp") %>% 
  mutate(Cobertura = "Pradera") %>% 
  select(Cobertura, Nombre, bio_t_ha) %>% 
  rename(AGB..ton.h = bio_t_ha,
         Parcela = Nombre)

  # df
bio_bn_df <- bio_bn %>% as.data.frame() %>% as_tibble()
bio_mt_df <- bio_mt %>% as.data.frame() %>% as_tibble()
bio_pf_df <- bio_pf %>% as_tibble()
bio_pr_df <- bio_pr %>% as_tibble()

  # full_join()
bio_df <- bio_bn_df %>% full_join(bio_mt_df) %>% full_join(bio_pf_df) %>% full_join(bio_pr_df)

bio_df %>% group_by(Cobertura) %>% 
  summarize(Produc.media = mean(AGB..ton.h),
            Produc.sd = sd(AGB..ton.h),
            Produc.min = min(AGB..ton.h),
            Produc.max = max(AGB..ton.h),
            Produc.median = median(AGB..ton.h),
            N = n())

#Gráficos----
# Espacial
ggplot() + geom_sf(data = cau) +
  geom_sf(data = bio_df,
          aes(geometry = geometry,
              color = Cobertura,
              size = AGB..ton.h)) +
  facet_wrap(~Cobertura) +
  labs(title = "Productividad por cobertura vegetacional",
       subtitle = "Cuenca Cauquenes",
       caption = "Elaboración propia, 2021.",
       y = NULL,
       x = NULL) +
  theme(text = element_text(size = 16,
                            family = "TT Times New Roman"))

ggplot(bio_bn, aes(y = AGB..ton.h)) + 
  geom_boxplot()

# Boxplot
ggplot(bio_df, aes(x = Cobertura, y = AGB..ton.h, color = Cobertura)) + 
  geom_boxplot(outlier.alpha = 0) +
  labs(title = "Productividad por cobertura vegetacional",
       subtitle = "Gráfico de cajas",
       caption = "Elaboración propia, 2021.",
       y = NULL,
       x = NULL) +
  theme(text = element_text(size = 16,
                            family = "TT Times New Roman"))

# Variaciones de gráfico
P <- ggplot(bio_df, aes(x = Cobertura, y = AGB..ton.h))

P + geom_point(aes(color = Cobertura))

P + geom_boxplot(aes(fill = Cobertura))

P + geom_violin(aes(fill = Cobertura))

P + geom_jitter(aes(color = Cobertura))








