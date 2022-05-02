# Packages ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(scales)
library(stringr)
library(patchwork)


# Base de datos en Molco -------------------------------------------------------------------

# ¿Cuales son los ID de los nuevos puntos?
data %>% 
  filter(str_detect(Nombre, "PB") | str_detect(Nombre, "Playa Blanca")) %>% 
  pull(ID) %>% 
  unique() #13667 13932

# Base de datos Molco
(
  data_molco <- data %>%
  select(
    ID, Nombre, Datos, Fuente, Fuente2, top, bottom, a, L, A, Da, CC, PMP, PDR, PDL, PAU
  ) %>%
  filter(ID %in% c(
    13242, 13260, 13261, 13262, 13243, 13244, 13245, 13667, 13932
  )) %>%
  arrange(ID) %>%
  group_by(ID) %>%
  mutate(prof_efec = max(bottom)) %>%
  mutate(
    prof_efec = case_when(
      Nombre == "BN-MOLCO-BAJO" ~ 100,
      Nombre == "BN-PLAYA-BLANCA" ~ 101,
      Nombre == "PINO-MOLCO-BAJO" ~ 140,
      TRUE ~ prof_efec
    )
  ) %>%
  relocate(prof_efec, .after = bottom) %>% 
  st_as_sf()
)

# Guardar shp
dir.create("express/data_molco")

write_sf(
  obj = data_molco,
  dsn = "express/data_molco/data_molco.shp"
)


# Microcuenca de Molco ----------------------------------------------------

(molco <- st_read("~/Casanova/Universidad/Master/Tesis/Datos/microcuenca.kml") %>% 
   st_transform(crs = 32718))

ggplot() + 
  geom_sf(data = cau) +
  geom_sf(data = molco, fill = "pink")


# Cartografias ------------------------------------------------------------

# Mapa cuenca completa con todas las muestras, estas últimas diferenciadas en Molco

theme_set(theme_bw())

(g_cau_2 <- data %>% 
  filter(Fuente == "UChile") %>% 
  mutate(Muestras = case_when(
    ID %in% c(13242, 13260, 13261, 13262, 13243, 13244, 13245, 13667, 13932) ~ "Molco",
    TRUE ~ "Otros"
  )) %>% 
  ggplot() +
  geom_sf(data = cau) +
  geom_sf(data = molco, fill = "light green") +
  geom_sf(aes(color = Muestras), shape = 8, size = 2) +
  # geom_rect(aes(xmin = 734228, xmax = 736985, ymin =6031000, ymax = 6035500), color = "dark red", alpha = 0, size = 0.8) +
  geom_rect(aes(xmin = 733828, xmax = 737385, ymin = 6030600, ymax = 6036300), color = "dark red", alpha = 0, size = 0.8) +
  labs(
    title = "Muestreo cuenca Cauquenes",
    subtitle = "Molco",
    caption = "CRS: EPSG 32718 - WGS 84 / UTM zone 18S"
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
  )))

(
  g_zoom_full <- data_molco %>%
    ggplot() +
    geom_sf(data = cau) +
    geom_sf(data = molco, fill = "light green") +
    geom_sf(aes(color = Nombre), size = 2.5) +
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
    lims(x = c(734228, 736985), y = c(6031000, 6035500)) +
    theme_void() +
    theme(
      panel.border = element_rect(colour = "dark red", fill = NA, size = 2)
    )
)

(
  g_zoom_zoom <- data_molco %>%
    st_as_sf() %>%
    ggplot() +
    geom_sf(data = cau) +
    geom_sf(data = molco, fill = "light green") +
    geom_sf(aes(color = Nombre), size = 2.5) +
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
    lims(x = c(734128, 735750), y = c(6032550, 6035450)) +
    theme_void() +
    theme(
      panel.border = element_rect(colour = "dark red", fill = NA, size = 2)
    )
)

(
  g_zoom_3 <- data %>%
    filter(
      !ID %in% c(13242, 13260, 13261, 13262, 13243, 13244, 13245, 13667, 13932)
    ) %>%
    ggplot() +
    geom_sf(data = cau) +
    geom_sf(data = molco, fill = "light green") +
    geom_sf(
      color = "#00BFC4",
      shape = 8,
      size = 2.5
    ) +
    geom_sf(data = data_molco, aes(color = Nombre), size = 2.5) +
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
    lims(x = c(733828, 737385), y = c(6030600, 6036300)) +
    theme_void() +
    theme(
      panel.border = element_rect(colour = "dark red", fill = NA, size = 2)
    )
)

# g_cau + inset_element(g_zoom, left = 0, bottom = 0.55, right = 0.4, top = 1, align_to = "plot")

g_cau_2 + g_zoom_3 + plot_layout(guides = "collect")

ggsave(filename = "./express/gg_molco.png",
       plot = last_plot(),
       device = "png",
       width = 8,
       height = 7,
       units = "in",
       dpi = 600,
       bg = NULL)











