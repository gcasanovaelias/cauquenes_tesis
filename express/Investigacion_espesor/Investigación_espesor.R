library(tidyverse)
library(stars)
library(sf)
library(ggspatial)
library(scales)
library(patchwork)

utils::methods(class = "stars")

mapa <- read_stars("~/Casanova/Universidad/Master/Tesis/Datos/Galleguillos/soil_sediment_deposit_18.tif") %>% 
  st_crop(cau)

plot(mapa)

(mapa2 <- mapa %>% 
  as_tibble() %>% 
  drop_na() %>% 
  arrange(desc(soil_sediment_deposit_18.tif)) %>% 
  filter(soil_sediment_deposit_18.tif != 255,
         !is.na(soil_sediment_deposit_18.tif)) %>%
  rename(Espesor = soil_sediment_deposit_18.tif) %>% 
  st_as_stars())

write_stars(mapa2, "mapa2.tif")

mapa_factor <- mapa2 %>% 
  as_tibble() %>% 
  drop_na() %>% 
  mutate(Espesor = factor(Espesor)) %>% 
  filter(Espesor %in% c(1,2,3,4)) %>% 
  st_as_stars()

ggplot() +
  geom_stars(data = mapa_factor) +
  coord_equal() + 
  scale_fill_discrete(na.value = "transparent") +
  theme_void()

ggplot() +
  geom_sf(data = cau) +
  geom_stars(data = mapa2) +
  labs(title = NULL,
       subtitle = NULL,
       x = NULL,
       y = NULL,
       caption = "CRS: EPSG 32718 - WGS 84 / UTM zone 18S") +
  scale_fill_viridis_c(na.value = "transparent") +
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
  ) +
  theme_light()

ggsave(filename = "express/espesor_cau.png",
       plot = last_plot(),
       device = "png",
       width = 8,
       height = 6,
       units = "in",
       dpi = 600,
       bg = NULL)
