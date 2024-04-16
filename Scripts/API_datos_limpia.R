
# Packages ----------------------------------------------------------------

library(tidyverse)
library(jsonlite)


# custom functions --------------------------------------------------------

`%notin%` <- Negate(`%in%`)
doesNotStartWith = Negate(startsWith)


group_category = function(x){
  case_when(x %>% str_detect("Universidad|Uiversidad|Uivesidad"
                             ) ~ "Universidades",
            x %>% str_detect("Becas|Pasantías"
                             ) ~ "Formación",
            x %>% str_detect("Unión|Federación|Confederación|Asociación|Consejo"
                             ) ~ "Otras Organizaciones",
            .default = "Otro"
            )
}

# Load data ---------------------------------------------------------------

data2023 <- fromJSON("Data/2023.json")
data2024 <- fromJSON("Data/2024.json")


# Manipulate data ---------------------------------------------------------
# Acá busca seleccionar sólo las entradas correspondientes con estos ministerios
# No resulta en difecencia
data2023ed <- data2023 %>% filter(entidad_desc == "Ministerio de Educación")
data2024ed <- data2024 %>% filter(jurisdiccion_desc == "Ministerio de Capital Humano")

dataed <- rbind(data2023ed, data2024ed)
#Preguntar a Rodrigo por esto
dataed<-dataed %>%  
  #If impacto_presupuestario_fecha is 2023-03-30 or 2023-03-31, then the value of impacto_presupuestario_mes should change to 4
  mutate(
    impacto_presupuestario_mes = ifelse(impacto_presupuestario_fecha >= as.Date("2023-03-30") & impacto_presupuestario_fecha <=as.Date("2023-03-31"),
                                        4,
                                        impacto_presupuestario_mes)
    ) %>%
  mutate(
    impacto_presupuestario_mes = ifelse(impacto_presupuestario_fecha >= as.Date("2023-07-30") & impacto_presupuestario_fecha <=as.Date("2023-07-31"),
                                        8, 
                                        impacto_presupuestario_mes)
    ) %>%
  mutate(
    impacto_presupuestario_mes = ifelse(impacto_presupuestario_fecha >= as.Date("2024-03-01") & impacto_presupuestario_fecha <=as.Date("2024-03-06"), 
                                        2, 
                                        impacto_presupuestario_mes)
    ) 
#create new date column using impacto_presupuestario_mes and impacto_presupuestario_año
dataed$fecha <- as.Date(paste(dataed$impacto_presupuestario_anio, dataed$impacto_presupuestario_mes, "01", sep = "-"), format = "%Y-%m-%d")

prunedData = dataed[,dataed %>% apply(2,function(x){unique(x) %>% length()}) >1] # select columns with at least two different cases
prunedLonger = prunedData %>%  
  pivot_longer(cols = starts_with("credito"),names_to = "credito",values_to = "monto")


#IPC
#create data frame with a column "fecha" with monthly dates from 2023-01-01 to 2024-03-01, and values equal to : 6.0,6.6,7.7,8.4,7.8,6.0,6.3,12.4,12.7,8.3,12.8,25.5,20.6,13.2,10.8
#Automatizzar esto
ipc <- data.frame(
  fecha = seq(as.Date("2023-01-01"),
              as.Date("2024-03-01"),
              by = "month"), 
  ipc = c(6.0,6.6,7.7,8.4,7.8,6.0,6.3,12.4,12.7,8.3,12.8,25.5,20.6,13.2,12) #¿Cómo obtuvieron el de marzo si no está en INDEC? con convendria usar otro indice?
  )
#add cumulative IPC column, multiplying each value by the previous one
ipc$cumulative <- cumprod(1+ipc$ipc/100)/1.06
#Divide cumulative by the value corresponding to 2024-02-01
normalize_value <- ipc %>% filter(as.Date(fecha) == as.Date("2024-02-01")) %>% pull(cumulative)
ipc <- ipc %>% mutate(cumulative = round(cumulative / normalize_value, 2))
prunedLonger = prunedLonger%>%
  left_join(ipc, by = "fecha") %>%
  mutate(
  "montoReal" = monto/cumulative
)

ipc_14_15_16<-dataed %>% 
  mutate(fecha = as.Date(fecha)) %>%  # convert to date if not already
  filter(actividad_id %in% c(14,15,16)) %>% 
  group_by(fecha) %>% 
  summarise(credito_devengado = sum(credito_devengado)) %>%
  left_join(ipc, by = "fecha") %>%
  mutate(credito_devengado_real = credito_devengado/cumulative)

ipc_14<-dataed %>% 
  mutate(fecha = as.Date(fecha)) %>%  # convert to date if not already
  filter(actividad_id %in% c(14)) %>% 
  group_by(fecha) %>% 
  summarise(credito_devengado = sum(credito_devengado)) %>%
  left_join(ipc, by = "fecha") %>%
  mutate(credito_devengado_real = credito_devengado/cumulative)

ipc_16<-dataed %>% 
  mutate(fecha = as.Date(fecha)) %>%  # convert to date if not already
  filter(actividad_id %in% c(16)) %>% 
  group_by(fecha) %>% 
  summarise(credito_devengado = sum(credito_devengado)) %>%
  left_join(ipc, by = "fecha") %>%
  mutate(credito_devengado_real = credito_devengado/cumulative)


### Crear dataframe para shiny que contenaga lo siguente
#ubicación grografica,
# función: 3 categorías a los que se asigno el dinero CyT, Saludo y Educación y cultura
# subparcial: Nombre de instituciones (universidades)
# actividad : representa los objetivos con los que se asignó el dinero
# inciso, casificador económico, principal y parcial son desgloces de mayor detalle que dan cuenta en qué se gastó el dinero
# fuente: de dónde vino el dinero
filteredData = prunedLonger %>%
  left_join(ipc, by = "fecha") %>% 
  select(
    fecha,
    ubicacion_geografica_desc,
    funcion_desc,
    subparcial_desc,
    actividad_desc,
    inciso_desc,
    clasificador_economico_8_digitos_desc,
    principal_desc,
    parcial_desc,
    credito,
    monto,
    cumulative.y
  ) %>% 
  mutate(montoReal = monto/cumulative.y)

filteredData = filteredData %>%  group_by(
  fecha,
  ubicacion_geografica_desc,
  funcion_desc,
  subparcial_desc,
  actividad_desc,
  inciso_desc,
  clasificador_economico_8_digitos_desc,
  principal_desc,
  parcial_desc,
  credito
) %>% 
  summarise_all(sum) 
filteredData = filteredData %>% 
  mutate(
    "Categoria" = subparcial_desc %>% 
      group_category()
  )
filteredData%>% 
  write_tsv(file = "app/DataBase.tsv")



#unidad ejecutora no tiene sentido, sólo tiene los nombres de ministerio o secretaría
# Explorations ------------------------------------------------------------


feb23<- data2023ed %>%                  #filter(impacto_presupuestario_mes==2) %>% 
  filter(impacto_presupuestario_mes==2 & programa_id==26 & actividad_id %in% c(14,15,16)) %>% 
  group_by(actividad_desc) %>% 
  summarise(credito_vigente = sum(credito_vigente),
            credito_comprometido = sum(credito_comprometido),
            credito_devengado = sum(credito_devengado),
            credito_pagado = sum(credito_pagado),
            credito_devengado = sum(credito_devengado))

View(data2024ed %>% 
       filter(impacto_presupuestario_mes==2) %>%
       group_by(actividad_desc) %>% # S+olo hay un caso "Secretaría de Políticas Universitarias"
       summarise(
         
         
         credito_vigente = sum(credito_vigente),
         credito_comprometido = sum(credito_comprometido),
         credito_devengado = sum(credito_devengado),
         credito_pagado = sum(credito_pagado),
         credito_devengado = sum(credito_devengado)
       )
)
multidimExp = prunedData[33:37] %>% prcomp(scale. = T) %>% summary
prunedData %>% filter(fecha == "01.01.2023" %>% dmy(),
                      subparcial_desc == "Universidad Nacional de Córdoba") %>% View

# Plots -------------------------------------------------------------------
# Gráfico crédito TOTAL (suma) mensual devengado 22023-2024

dataed %>% 
  mutate(fecha = as.Date(fecha)) %>%  # convert to date if not already
  filter(actividad_id %in% c(14,15,16)) %>% # Se seleccionan las actividades: Hospitales, CyT y Asistencia finianciera 
  filter(fecha <= as.Date("2024-02-01")) %>% # Fechas anteriores a febrero
  group_by(fecha) %>% 
  summarise(credito_devengado = sum(credito_devengado)) %>% 
  ggplot(aes(x = fecha, y = credito_devengado)) +
  geom_bar(stat = "identity", fill = "blue", width = 20) +  # set width to 1 to fill the entire day
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",expand = c(0.01,0.01)) +  # set date breaks and labels
  scale_y_continuous(labels = scales::dollar_format(scale = 1),limits=c(0,15000)) +
  theme_light(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Mes", y = "Devengado en $ (millones)", title = "Crédito mensual devengado 2023-2024 actividad 14+15+16\n (funcionamiento+salud+CyT)")


prunedLonger %>% 
  # mutate(fecha = as.Date(fecha)) %>%  # convert to date if not already
  filter(actividad_id %in% c(14,15,16)) %>% # Se seleccionan las actividades: Hospitales, CyT y Asistencia finianciera 
  filter(fecha <= as.Date("2024-02-01")) %>% # Fechas anteriores a febrero
  group_by(fecha,credito,subparcial_desc) %>% 
  summarise(monto = sum(monto)) %>% 
  ggplot(aes(x = fecha, y = monto)) +
  geom_bar(stat = "identity", fill = subparcial_desc, width = 20) +  # set width to 1 to fill the entire day
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",expand = c(0.01,0.01)) +  # set date breaks and labels
  scale_y_continuous(labels = scales::dollar_format(scale = 1),limits=c(0,15000)) +
  theme_light(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Mes", y = "Devengado en $ (millones)", title = "Crédito mensual devengado 2023-2024 actividad 14+15+16\n (funcionamiento+salud+CyT)") +
  facet_wrap(~ credito)

prunedData[33:37] %>% plot
prunedLonger %>% ggplot(aes(monto)) + geom_density() + facet_wrap(~ credito) + scale_x_log10()

multidimExp %>% summary()




