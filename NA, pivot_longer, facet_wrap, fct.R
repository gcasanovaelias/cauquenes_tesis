
library(sf)
library(tidyverse)
library(scales)

(cau <- read_sf("~/Casanova/Universidad/Master/Tesis/Datos/CAU_desembocadura.shp") %>% 
  st_transform(crs = 32718))

(
  data <-
    read_csv(
      "~/Casanova/Universidad/Master/Tesis/Datos/BD_soilprof_23MAR.csv"
    ) %>%
    st_as_sf(coords = c(9, 10),
             crs = 4326) %>%
    st_transform(crs = 32718) %>%
    mutate(field_1 = seq(lenght.out = 19421)) %>%
    relocate(field_1, .before = ID) %>%
    st_crop(cau) %>%
    st_intersection(cau) %>% 
    mutate(a = arena,
           L = Limo,
           A = Arcilla,
           .keep = "unused",
           .after = "bottom")
)

glimpse(data)

(
  data1 <- data %>%
    select(field_1, ID, Nombre, top, bottom, A, L, a, Da, CC, PMP, PDR, PDL, PAU) %>%
    # A = na_if(A, -999.00)
    mutate(across(
      .cols = c(A, L, a, Da, CC, PMP, PDR, PDL, PAU), .fns = ~ na_if(.x,-999)
    )) %>%
    pivot_longer(
      cols = c(A, L, a, Da, CC, PMP, PDR, PDL, PAU),
      names_to = "Property",
      values_to = "Value"
    ) %>% 
    filter(!is.na(Value)) %>% 
    relocate(geometry, .after = Value) %>% 
    mutate(Property = fct_relevel(Property, "a", "L", "A", "Da", "CC", "PMP", "PDR", "PDL", "PAU"))
)

theme_set(theme_bw())

data1 %>% 
  # dplyr::filter(Property %in% c("A", "Da", "CC", "PAU", "profundidad")) %>% 
  dplyr::filter(Property %in% c("A", "Da", "CC", "PAU")) %>% 
  # mutate(Property = fct_recode(Property, "aLA" = "A", "CC-PMP" = "CC", "Prof_efec" = "profundidad"),
  mutate(Property = fct_recode(Property, "aLA" = "A", "CC-PMP" = "CC"),
         # Property = fct_relevel(Property, "aLA", "Da", "CC-PMP", "PAU", "Prof_efec")) %>% 
         Property = fct_relevel(Property, "aLA", "Da", "CC-PMP", "PAU")) %>% 
  ggplot() +
  geom_sf(data = cau) +
  geom_sf(aes(color = Property)) +
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
  facet_wrap(~ Property)

data1 %>% 
  group_by(Property) %>% 
  filter(top == min(top)) %>% 
  summarize(n = n())

ggsave(filename = "p_fisicas.png",
       plot = last_plot(),
       device = "png",
       width = 11,
       height = 8,
       units = "in",
       dpi = 600,
       bg = NULL)


# Profundidad efectiva
(
data2 <- data %>% 
  filter(Fuente == "UChile") %>% 
  group_by(ID) %>% 
  filter(bottom == max(bottom)) %>% 
  select(ID, Nombre, Datos, Fuente2, top, bottom) %>% 
  rename(profundidad = bottom)
)



data2 %>% 
  mutate(Datos = fct_recode(Datos, "Prof_efec" = "Seguel", "Prof_efec" = "Edu", "Soto" = "Galleguillos")) %>% 
  ggplot() +
  geom_sf(data = cau) +
  geom_sf(aes(color = Datos)) +
  scale_x_continuous(labels = label_number(
    accuracy = 0.1,
    suffix = paste0("\u00b0", "W"),
    scale = -1
  )) +
  scale_y_continuous(labels = label_number(
    accuracy = 0.1,
    suffix = paste0("\u00b0", "S"),
    scale = -1
  ))

(prof_efec <- data %>% 
  filter(Fuente == "UChile",
         Datos %in% c("Seguel", "Edu")) %>% 
  group_by(ID) %>% 
  filter(bottom == max(bottom)) %>% 
  select(ID, Nombre, Fuente2, top, bottom))

View(data2)

data2 %>% pull(Nombre) %>% unique()

# Tantos datos de Da

data %>% select(ID, Nombre, Fuente, Fuente2, top, bottom, Da, A, L, a) %>% filter(Da != -999.00) %>% group_by(ID) %>% filter(bottom == max(bottom)) %>% filter(ID %in% 717:766) %>% ggplot() + geom_sf(data = cau) + geom_sf()

data %>% filter(ID %in% 13952:13957)
