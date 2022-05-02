# Packages ----------------------------------------------------------------

library(tidyverse)
library(stars)
library(sf)
library(ggspatial)
library(scales)
library(patchwork)

# Datos -------------------------------------------------------------------

cau <- read_sf("~/Casanova/Universidad/Master/Tesis/Datos/CAU_desembocadura.shp") %>% st_transform(crs = 32718)

read_csv("~/Casanova/Universidad/Master/Tesis/Datos/BD_soilprof_20SEP.csv") %>% 
  st_as_sf(coords = c(9,10),
           crs = 32719) %>%
  st_transform(crs = 32718) %>% 
  mutate(field_1 = seq(length.out = 18958)) %>% 
  relocate(field_1, .before = ID) %>% 
  st_crop(cau) %>% 
  filter(Fuente == "UChile") %>% 
  st_intersection(cau) -> pts_uch

# Cargar el LandUse_cau.rds que se encuentra en la carpeta "rds" en "Tesis/Datos"

LandUse_cau %>% 
  st_as_stars() %>% 
  as_tibble() %>% 
  filter(!is.na(LandCover_rf_2018_v4.3)) %>% 
  rename(LandUse = LandCover_rf_2018_v4.3) %>% 
  group_by(LandUse) %>% 
  summarise(n = n())

LandUse_cau %>%
  st_as_stars() %>%
  as_tibble() %>%
  filter(!is.na(LandCover_rf_2018_v4.3)) %>%
  rename(LandUse = LandCover_rf_2018_v4.3) %>%
  # mutate(x = replace(x, 1, 2))
  # mutate(x = recode(x, `1` = 2L))
  mutate(
    LandUse. = case_when(
      LandUse %in% c(1, 2, 5, 6, 7, 8, 13, 14, 15) ~ "Otras",
      LandUse %in% c(3, 4) ~ "BN", #Bosque Nativo
      LandUse == 9 ~ "MT", #Matorrales
      LandUse == 10 ~ "PP", #Plantación pinos
      LandUse %in% c(11, 12) ~ "PR", # Pradera
      TRUE ~ as.character(LandUse)
    ),
    LandUse = case_when(
      LandUse %in% c(1, 2, 5, 6, 7, 8, 13, 14, 15) ~ 5,
      LandUse %in% c(3, 4) ~ 1,
      LandUse == 9 ~ 2,
      LandUse == 10 ~ 3,
      LandUse %in% c(11, 12) ~ 4
    )
  ) %>%
  st_as_stars() -> l3


# Gráfico principal -------------------------------------------------------

l3 %>% as_tibble() %>% group_by(LandUse) %>% filter(!is.na(LandUse)) %>% dplyr::select(LandUse, LandUse.) %>% summarise(LandUse. = unique(LandUse.)) %>% attach()

windowsFonts("Times New Roman" = windowsFont("Times New Roman"))
theme_set(theme_bw(base_size = 17, base_family = "Times New Roman"))

# ecosistemas
(gg1 <- ggplot() +
  geom_sf(data = cau) +
  geom_stars(data = l3, aes(x = x, y = y, fill = as.character(LandUse))) +
  labs(title = NULL,
       subtitle = NULL,
       x = NULL,
       y = NULL) +
  scale_fill_manual(name = "Land use",
                    values = c("#04AB01", "#DFAE02", "#DF4802", "#1175D1", "#DAF7A6"),
                    labels = LandUse.,
                    # s/ valores Na 
                    na.value = NA,
                    na.translate = F) +
  scale_x_continuous(labels = label_number(
    accuracy = 0.1,
    suffix = paste0("\u00b0", "W"),
    scale = -1
  )) +
  scale_y_continuous(labels = label_number(
    accuracy = 0.1,
    suffix = paste0("\u00b0", "S"),
    scale = -1
  )) +
  annotation_scale(
    style = "ticks",
    location = "br",
    text_cex = 1.2,
    text_pad = unit(0.15, "cm"),
    text_face = 1
  ) +
  annotation_north_arrow(
    style = north_arrow_fancy_orienteering(line_width = 1.2),
    pad_x = unit(0.4, "cm"),
    pad_y = unit(0.4, "cm"),
    location = "tl"
  ))


# Red hidrográfica & Puntos de muetreo ------------------------------------

(red_hidro <- st_read("~/Casanova/Universidad/Master/Tesis/Datos/Geomorfologia/red_hidrografica/red_hidrografica/red_hidrografica.shp") %>% 
   st_transform(crs = 32718) %>% 
   st_intersection(cau))

(gg_red_hidro <- ggplot() +
    # Cuenca
    geom_sf(data = cau, fill = "white") +
    # Red hidrográfica
    geom_sf(
      data = red_hidro, 
      aes(color = "Red hidrográfica"),
      show.legend = "line"
    ) +
    # Puntos de muestreo
    geom_sf(
      data = pts_uch, 
      size = 1.8, 
      aes(color = "Muestreo"), 
      show.legend = "point") +
    scale_color_manual(
      values = c("Muestreo" = "#F8766D",
                 "Red hidrográfica" = "#619CFF"),
      guide = guide_legend(
        override.aes = list(linetype = c("blank", "solid"),
                            shape = c(16, NA)),
        title = NULL
      )
    ) +
    scale_x_continuous(labels = label_number(
      accuracy = 0.1,
      suffix = paste0("\u00b0", "W"),
      scale = -1
    )) +
    scale_y_continuous(labels = label_number(
      accuracy = 0.1,
      suffix = paste0("\u00b0", "S"),
      scale = -1
    )) +
    annotation_scale(
      style = "ticks",
      location = "br",
      text_cex = 1.2,
      text_pad = unit(0.15, "cm"),
      text_face = 1
    ) +
    annotation_north_arrow(
      style = north_arrow_fancy_orienteering(line_width = 1.2),
      pad_x = unit(0.4, "cm"),
      pad_y = unit(0.4, "cm"),
      location = "tl"
    )
)


# Patchwork & Pts de muestreo ---------------------------------------------

gg1 / gg_red_hidro + 
  plot_layout(guides = "collect") +
  plot_annotation(
    caption = "CRS: EPSG 32718 - WGS 84 / UTM zone 18S"
  )

ggsave(filename = "gg_dupla.png",
       plot = last_plot(),
       device = "png",
       width = 8,
       height = 12,
       units = "in",
       dpi = 600,
       bg = NULL)

