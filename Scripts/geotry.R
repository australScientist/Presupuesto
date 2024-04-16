library(leaflet)
library(tidyverse)
library(geoAr)

geoArgentina = get_geo("ARGENTINA",level = "provincia")

plot(geoArgentina)
leaflet(geoArgentina) %>% addTiles()
left_join(x = geoArgentina,
          y = show_arg_codes(),
          by = join_by(codprov_censo)
          ) %>% 
  ggplot() +
  geom_sf(
    aes(
      fill = name_iso
      )
    )

left_join(
  x = geoArgentina,
  y = show_arg_codes(),
  by = join_by(codprov_censo)
  ) %>% 
  select(geometry,
         name_iso) %>%
  rename("Provincia" = name_iso) %>% 
  left_join(data %>% 
              filter(credito == "credito_devengado",
                     ubicacion_geografica_desc != "Nacional") %>% 
              group_by(ubicacion_geografica_desc) %>% 
              summarise(monto = sum(monto),
                        montoReal = sum(montoReal)) %>% 
              mutate(Provincia = ubicacion_geografica_desc %>%  
                       str_remove(regex(",.+")) %>% 
                       str_remove("Provincia de ") %>% 
                       str_remove("Provincia del ")
                     ),
            by = join_by(Provincia == Provincia)) %>% 
  ggplot() +
  geom_sf(aes(fill = montoReal)) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = data %>% 
                        filter(credito == "credito_devengado",
                               ubicacion_geografica_desc != "Nacional") %>% 
                        group_by(ubicacion_geografica_desc) %>% 
                        summarise(monto = sum(monto),
                                  montoReal = sum(montoReal)
                        ) %>%
                        select(montoReal) %>% 
                        unlist() %>% 
      median
  ) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
  
