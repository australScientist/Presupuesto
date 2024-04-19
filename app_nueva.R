# Load the necessary packages
library(shiny)
library(tidyverse)
library(plotly)
library(readr)

# Load the data and format column types. Column 1 is a date in dd-mm-yyyy format, columns 2-10 are factors, columns 11-13 are numeric and 14 is a factor
#column names are: fecha	ubicacion_geografica_desc	funcion_desc	subparcial_desc	actividad_desc	inciso_desc	clasificador_economico_8_digitos_desc	principal_desc	parcial_desc	credito	monto	cumulative.y	montoReal	Categoria
data <- read_tsv("app/DataBase.tsv", col_types = cols(fecha = col_date(), ubicacion_geografica_desc = col_factor(), funcion_desc = col_factor(), subparcial_desc = col_factor(), actividad_desc = col_factor(), inciso_desc = col_factor(), clasificador_economico_8_digitos_desc = col_factor(), principal_desc = col_factor(), parcial_desc = col_factor(), credito = col_factor(), monto = col_double(), cumulative.y = col_double(), montoReal = col_double(), Categoria = col_factor()))

#Filter only levels in subparcial_desc which contain the word "Universidad" somewhere in the level name
data <- data %>% filter(str_detect(subparcial_desc, "Universidad"))

# Add the "Todas" level to the "actividad_desc", "subparcial_desc", and "credito" columns
data1 <- data %>%
    group_by(across(c(-actividad_desc))) %>%
    mutate(actividad_desc = "Todas") %>%
    mutate(monto = sum(data$monto, na.rm = TRUE)) %>%
    ungroup()

data <- bind_rows(data, data1)

data2 <- data %>%
    group_by(across(c(-subparcial_desc))) %>%
    mutate(subparcial_desc = "Todas") %>%
    mutate(monto = sum(data$monto, na.rm = TRUE)) %>%
    ungroup()

#combine all
data <- bind_rows(data, data2)
#View(data %>% filter(fecha==as.Date("2023-06-01") & funcion_desc=="Ciencia, Tecnología e Innovación" & credito=="credito_devengado"))

# Define the UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("credito", "Seleccioná qué crédito analizar", choices = sort(unique(data$credito)), selected = "credito_devengado"),
      selectInput("subparcial_desc", "Elige la Universidad", choices = sort(unique(data$subparcial_desc)), selected = "Todas"),
      selectInput("actividad_desc", "Elige la Actividad", choices = sort(unique(data$actividad_desc)), selected = "Todas")
    ),
    mainPanel(
      plotlyOutput("barplot")
    )
  )
)
#TEST Create input data frame
#input<- data.frame(credito="credito_pagado", subparcial_desc="Todas", actividad_desc="Todas")

# Define the server
server <- function(input, output, session) {
  output$barplot <- renderPlotly({
    #if input$analisis == "Destinatario", group by fecha and subparcial_desc, else group by fecha and actividad_desc
    data_filtered <- data %>%
    filter(credito == input$credito,
         subparcial_desc == input$subparcial_desc,
         actividad_desc == input$actividad_desc) %>%
    group_by(fecha, subparcial_desc, actividad_desc, credito) %>%
    summarize(monto = sum(monto, na.rm = TRUE), .groups = "drop") %>%
    arrange(fecha)

    p <- data_filtered %>%
      ggplot(aes(x = fecha, y = monto)) +
      geom_col() +

      theme_light()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)