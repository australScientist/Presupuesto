# Load libraries ----------------------------------------------------------
library(shiny)
library(tidyverse)
library(bslib)
library(bsicons)
library(htmlwidgets)
library(treemapify)
library(plotly)
library(leaflet)
library(tidyverse)
library(geoAr)
library(d3treeR)

geoArgentina = get_geo("ARGENTINA",level = "provincia")

data = read_tsv(file = "DataBase.tsv")

# Define UI ----
ui <- fluidPage(
  
  # Nombre de la app
  titlePanel("Explorá Nuestro Presupuesto Universitario"),
  
  # Panel Lateral
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "exploracion",label = "Quiero ver",
                   choices = list(
                     "Cómo Varió el Presupuesto en el Tiempo" = 1,
                     "La Composición de nuestro Presupuesto" = 2
                   ),
                   selected = 1),
      uiOutput("modo"),
      uiOutput("elegir"),
      # uiOutput("colorear"),
      # uiOutput("resolucion"),
      # uiOutput("categoria"),
      uiOutput("Personalizar")

    ),
    
    # Main panel for displaying the plot
    mainPanel(
      # Plot output
      #plotlyOutput("budgetPlot"),
      uiOutput("plot"),
     # tableOutput("try"),
      card("Texto dinámico explicando brevemente la importancia del gráfico y el significado de algunas variables")
    )
  )
)


# serverLogic -------------------------------------------------------------


server <- function(input, output,session) {
  # COndicionar Universidad en Región Geográfica
  
  output$modo = renderUI({
    if (input$exploracion == 2){
      radioButtons(
        "filtroTreeMap", 
        "Seleccioná qué tipo de dato:",
        choices = list("Provincia" = 1,
                       "Universidades" = 2,
                       "Formación" = 3,
                       "Otras Organizaciones" = 4,
                       "Funcionamiento" = 5,
                       "Categoría de gasto" = 6,
                       "Finalidad" = 7
        )
      )
    } else {
      radioButtons(
        "filtro", 
        "Seleccioná como agrupar los datos:",
        choices = list(                       
                       "Universidades" = 2,
                       "Provincia" = 1,
                       "Formación" = 3,
                       "Otras Organizaciones" = 4,
                       "Funcionamiento" = 5
        ),
        selected = 2
      )
    }
  })
  
  output$elegir = renderUI({
    if (input$filtro == 1 & input$exploracion == 1) {
      selectInput("elegir", "Elegí tu institución:",
                  choices = data %>%  
                    select(ubicacion_geografica_desc) %>% 
                    unlist() %>%
                    unique,
                  selected = 1
      )
    } else if (input$filtro == 2 & input$exploracion == 1) {
      selectInput("elegir", "Elegí tu institución:",
                  choices = data %>%  
                    filter(
                      startsWith(subparcial_desc,"Uiversidad") |
                        startsWith(subparcial_desc,"Universidad") |
                        startsWith(subparcial_desc,"Univesidad")) %>% 
                    select(subparcial_desc) %>%  
                    unlist() %>%
                    unique,
                  selected = 1
      )
    } else if (input$filtro == 3 & input$exploracion == 1) {
      selectInput("elegir", "Tipo:",
                  choices = data %>%  
                    filter(subparcial_desc == "Becas"|
                             subparcial_desc == "Pasantías") %>% 
                    select(subparcial_desc) %>%  
                    unlist() %>%
                    unique,
                  selected = 1
      )
    } else if (input$filtro == 4 & input$exploracion == 1) {
      selectInput("elegir", "Elegí tu institución:",
                  choices = data %>%  
                    filter(
                      startsWith(subparcial_desc,"Unión") |
                        startsWith(subparcial_desc,"Federación") |
                        startsWith(subparcial_desc,"Confederación") |
                        startsWith(subparcial_desc,"Asociación") |
                        startsWith(subparcial_desc,"Consejo")) %>% 
                    select(subparcial_desc) %>%  
                    unlist() %>%
                    unique,
                  selected = 1
      )
    }else if (input$filtro == 5 & input$exploracion == 1) {
      selectInput("elegir", "Gastos:",
                  choices = data %>%  
                    filter(
                        !startsWith(subparcial_desc,"Uiversidad"),
                        !startsWith(subparcial_desc,"Universidad"),
                        !startsWith(subparcial_desc,"Univesidad"),
                        !startsWith(subparcial_desc,"Unión"),
                        !startsWith(subparcial_desc,"Federación"),
                        !startsWith(subparcial_desc,"Confederación"),
                        !startsWith(subparcial_desc,"Asociación"),
                        !startsWith(subparcial_desc,"Consejo"),
                        subparcial_desc != "Becas",
                          subparcial_desc != "Pasantías") %>% 
                    select(subparcial_desc) %>%  
                    unlist() %>%
                    unique,
                  selected = 1
      )
    }
    
    
    })
  
  # output$colorear = renderUI({
  #   if (input$exploracion == 2) {
  #     checkboxInput(inputId = "colorearPorCategoria",
  #                   label = "Colorear por Categoría",
  #                   value = F)
  #   }
  # })
  # 
  # output$resolucion = renderUI({
  #   if (input$colorearPorCategoria & input$exploracion == 2) {
  #     radioButtons(
  #         "elegirResolucion",
  #         "Nivel de detalle:",
  #         choices = list("Baja" = 1, "Alta" = 2),
  #         selected = 1
  #       )
  #   }
  # })
  # 
  # output$categoria = renderUI({
  #   if (input$elegirResolucion == 1 & input$exploracion == 2) {
  #     checkboxGroupInput(inputId = "elegirCategoria",
  #                        label = "Personalizá el gráfico:",
  #                        choices = data$funcion_desc %>% unique(),
  #                        selected = 1
  #     )
  #   } else if (input$elegirResolucion == 2 & input$exploracion == 2) {
  #     checkboxGroupInput(inputId = "elegirCategoria",
  #                        label = "Categoría de Gasto:",
  #                        choices = data$actividad_desc %>% unique(),
  #                        selected = 1
  #     )
  #   }
  # })
  
  output$Personalizar = renderUI({
    if (input$exploracion == 1) {
      checkboxGroupInput(inputId = "personalizarGrafico",
                         label = "Personalizá el gráfico:",
                         choices = c("Puntos",
                                     "Lineas verticales",
                                     "Tendencia",
                                     "Anotaciones"),
                         selected = 
                         )
    }
  })
  
  
  output$plot = renderUI({
    if (input$exploracion == 2){
      plotOutput("budgetPlot")
    } else if (input$exploracion == 1){
      plotlyOutput("budgetPlotly")
    }
  })
  output$budgetPlotly = renderPlotly({
    if (input$exploracion == 2) {
      
      if (input$filtroTreeMap == 1) {
        p = left_join(
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
        
      } else if (input$filtroTreeMap == 2) {
        readyToPlot = data %>%  
          filter(
            startsWith(subparcial_desc,"Uiversidad") |
              startsWith(subparcial_desc,"Universidad") |
              startsWith(subparcial_desc,"Univesidad"),
            credito == "credito_devengado") %>% 
          group_by(subparcial_desc) %>% 
          summarise(montoReal = sum(montoReal),
                    monto = sum(monto)) 
      } else if (input$filtroTreeMap == 3) {
        readyToPlot = data %>%  
          filter(
            subparcial_desc == "Becas"|
              subparcial_desc == "Pasantías",
            credito == "credito_devengado") %>% 
          group_by(subparcial_desc) %>% 
          summarise(montoReal = sum(montoReal),
                    monto = sum(monto)
          )
      } else if (input$filtroTreeMap == 4) {
        readyToPlot = data %>%  
          filter(
            startsWith(subparcial_desc,"Unión") |
              startsWith(subparcial_desc,"Federación") |
              startsWith(subparcial_desc,"Confederación") |
              startsWith(subparcial_desc,"Asociación") |
              startsWith(subparcial_desc,"Consejo"),
            credito == "credito_devengado") %>% 
          group_by(subparcial_desc) %>% 
          summarise(montoReal = sum(montoReal),
                    monto = sum(monto)
          )
      } else if (input$filtroTreeMap == 5) {
        readyToPlot = data %>%  
          filter(
            !startsWith(subparcial_desc,"Uiversidad"),
            !startsWith(subparcial_desc,"Universidad"),
            !startsWith(subparcial_desc,"Univesidad"),
            !startsWith(subparcial_desc,"Unión"),
            !startsWith(subparcial_desc,"Federación"),
            !startsWith(subparcial_desc,"Confederación"),
            !startsWith(subparcial_desc,"Asociación"),
            !startsWith(subparcial_desc,"Consejo"),
            subparcial_desc != "Becas",
            subparcial_desc != "Pasantías",
            credito == "credito_devengado") %>% 
          group_by(subparcial_desc) %>% 
          summarise(montoReal = sum(montoReal),
                    monto = sum(monto)
          )
      } else if (input$filtroTreeMap == 6) {
        readyToPlot = data %>% 
          filter(credito == "credito_devengado") %>% 
          group_by(funcion_desc) %>% 
          summarise(montoReal = sum(montoReal),
                    monto = sum(monto))
      } else if (input$filtroTreeMap == 7) {
        readyToPlot = data %>%
          filter(credito == "credito_devengado") %>% 
          group_by(actividad_desc) %>% 
          summarise(montoReal = sum(montoReal),
                    monto = sum(monto))
      }
      if (input$filtroTreeMap != 1) {
        colnames(readyToPlot)[1] = "grupo"
        
        p = readyToPlot %>%  ggplot(
          aes(
            area = monto,
            fill = grupo
          )
        ) +
          geom_treemap()+
          theme_classic()
      }
      
      
    } else if (input$exploracion == 1) {
      if (input$filtro == 1) {
        readyToPlot = data %>%
          filter(
            ubicacion_geografica_desc == input$elegir,
            # credito == "credito_presupuestado" |
            credito == "credito_devengado"
          ) %>%
          group_by(
            credito,
            fecha
          ) %>%
          summarise(
            monto = sum(monto),
            montoReal = sum(montoReal)
          )
      } else if (input$filtro == 2) {
        readyToPlot = data %>%
          filter(
            subparcial_desc == input$elegir,
            # credito == "credito_presupuestado" |
            credito == "credito_devengado"
          ) %>%
          group_by(
            credito,
            fecha
          ) %>%
          summarise(
            monto = sum(monto),
            montoReal = sum(montoReal)
          )
      } else if (input$filtro == 3) {
        readyToPlot = data %>%
          filter(
            subparcial_desc == input$elegir,
            # credito == "credito_presupuestado" |
            credito == "credito_devengado"
          ) %>%
          group_by(
            credito,
            fecha
          ) %>%
          summarise(
            monto = sum(monto),
            montoReal = sum(montoReal)
          )
      } else if (input$filtro == 4) {
        readyToPlot = data %>%
          filter(
            subparcial_desc == input$elegir,
            # credito == "credito_presupuestado" |
            credito == "credito_devengado"
          ) %>%
          group_by(
            credito,
            fecha
          ) %>%
          summarise(
            monto = sum(monto),
            montoReal = sum(montoReal)
          )
      } else if (input$filtro == 5) {
        readyToPlot = data %>%
          filter(
            subparcial_desc == input$elegir,
            # credito == "credito_presupuestado" |
            credito == "credito_devengado"
          ) %>%
          group_by(
            credito,
            fecha
          ) %>%
          summarise(
            monto = sum(monto),
            montoReal = sum(montoReal)
          )
      }
      
      p = readyToPlot  %>%
        ggplot(aes(fecha,monto)) +
        geom_segment(aes(x = fecha,
                         xend = fecha,
                         y = monto,
                         yend = 0),
                     colour = "blue4",
                     alpha = 0.5) +
        geom_point(
          size = 5,
          lineWidth = 1.3,
          colour = "blue4",
          fill = "#6CACE4",
          shape = 21,
          alpha = 1,
          stroke = 1
        ) +
        geom_line(stat = "smooth",colour = "#FFB81C") +
        
        # facet_wrap(~ credito) +
        theme_classic()
      p = p %>% ggplotly()
    }
    
    # Plot the ggplot object
    print(p)
  })

  output$budgetPlot = renderPlot({
    if (input$exploracion == 2) {
      
      if (input$filtroTreeMap == 1) {
        p = left_join(
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
        
      } else if (input$filtroTreeMap == 2) {
        readyToPlot = data %>%  
          filter(
            startsWith(subparcial_desc,"Uiversidad") |
              startsWith(subparcial_desc,"Universidad") |
              startsWith(subparcial_desc,"Univesidad"),
            credito == "credito_devengado") %>% 
          group_by(subparcial_desc) %>% 
          summarise(montoReal = sum(montoReal),
                    monto = sum(monto)) 
      } else if (input$filtroTreeMap == 3) {
        readyToPlot = data %>%  
          filter(
            subparcial_desc == "Becas"|
              subparcial_desc == "Pasantías",
            credito == "credito_devengado") %>% 
          group_by(subparcial_desc) %>% 
          summarise(montoReal = sum(montoReal),
                    monto = sum(monto)
          )
      } else if (input$filtroTreeMap == 4) {
        readyToPlot = data %>%  
          filter(
            startsWith(subparcial_desc,"Unión") |
              startsWith(subparcial_desc,"Federación") |
              startsWith(subparcial_desc,"Confederación") |
              startsWith(subparcial_desc,"Asociación") |
              startsWith(subparcial_desc,"Consejo"),
            credito == "credito_devengado") %>% 
          group_by(subparcial_desc) %>% 
          summarise(montoReal = sum(montoReal),
                    monto = sum(monto)
          )
      } else if (input$filtroTreeMap == 5) {
        readyToPlot = data %>%  
          filter(
            !startsWith(subparcial_desc,"Uiversidad"),
            !startsWith(subparcial_desc,"Universidad"),
            !startsWith(subparcial_desc,"Univesidad"),
            !startsWith(subparcial_desc,"Unión"),
            !startsWith(subparcial_desc,"Federación"),
            !startsWith(subparcial_desc,"Confederación"),
            !startsWith(subparcial_desc,"Asociación"),
            !startsWith(subparcial_desc,"Consejo"),
            subparcial_desc != "Becas",
            subparcial_desc != "Pasantías",
            credito == "credito_devengado") %>% 
          group_by(subparcial_desc) %>% 
          summarise(montoReal = sum(montoReal),
                    monto = sum(monto)
          )
      } else if (input$filtroTreeMap == 6) {
        readyToPlot = data %>% 
          filter(credito == "credito_devengado") %>% 
          group_by(funcion_desc) %>% 
          summarise(montoReal = sum(montoReal),
                    monto = sum(monto))
      } else if (input$filtroTreeMap == 7) {
        readyToPlot = data %>%
          filter(credito == "credito_devengado") %>% 
          group_by(actividad_desc) %>% 
          summarise(montoReal = sum(montoReal),
                    monto = sum(monto))
      }
      if (input$filtroTreeMap != 1) {
        colnames(readyToPlot)[1] = "grupo"
        
        p = readyToPlot %>%  ggplot(
          aes(
            area = monto,
            fill = grupo
          )
        ) +
          geom_treemap()+
          theme_classic()
      }
      
      
    } else if (input$exploracion == 1) {
      if (input$filtro == 1) {
        readyToPlot = data %>%
          filter(
            ubicacion_geografica_desc == input$elegir,
            # credito == "credito_presupuestado" |
            credito == "credito_devengado"
          ) %>%
          group_by(
            credito,
            fecha
          ) %>%
          summarise(
            monto = sum(monto),
            montoReal = sum(montoReal)
          )
      } else if (input$filtro == 2) {
        readyToPlot = data %>%
          filter(
            subparcial_desc == input$elegir,
            # credito == "credito_presupuestado" |
            credito == "credito_devengado"
          ) %>%
          group_by(
            credito,
            fecha
          ) %>%
          summarise(
            monto = sum(monto),
            montoReal = sum(montoReal)
          )
      } else if (input$filtro == 3) {
        readyToPlot = data %>%
          filter(
            subparcial_desc == input$elegir,
            # credito == "credito_presupuestado" |
            credito == "credito_devengado"
          ) %>%
          group_by(
            credito,
            fecha
          ) %>%
          summarise(
            monto = sum(monto),
            montoReal = sum(montoReal)
          )
      } else if (input$filtro == 4) {
        readyToPlot = data %>%
          filter(
            subparcial_desc == input$elegir,
            # credito == "credito_presupuestado" |
            credito == "credito_devengado"
          ) %>%
          group_by(
            credito,
            fecha
          ) %>%
          summarise(
            monto = sum(monto),
            montoReal = sum(montoReal)
          )
      } else if (input$filtro == 5) {
        readyToPlot = data %>%
          filter(
            subparcial_desc == input$elegir,
            # credito == "credito_presupuestado" |
            credito == "credito_devengado"
          ) %>%
          group_by(
            credito,
            fecha
          ) %>%
          summarise(
            monto = sum(monto),
            montoReal = sum(montoReal)
          )
      }
      
      p = readyToPlot  %>%
        ggplot(aes(fecha,montoReal)) +
        geom_segment(aes(x = fecha,
                         xend = fecha,
                         y = montoReal,
                         yend = 0),
                     colour = "blue4",
                     alpha = 0.5) +
        geom_point(
          size = 5,
          lineWidth = 1.3,
          colour = "blue4",
          fill = "#6CACE4",
          shape = 21,
          alpha = 1,
          stroke = 1
        ) +
        geom_line(stat = "smooth",colour = "#FFB81C") +

        # facet_wrap(~ credito) +
        theme_classic()

      # p = readyToPlot  %>%
      #   ggplot(aes(fecha,montoReal)) 
      # 
      # if(1 %in% input$personalizarGrafico) {
      #   p = p +  geom_point(
      #     size = 5,
      #     lineWidth = 1.3,
      #     colour = "blue4",
      #     fill = "#6CACE4",
      #     shape = 21,
      #     alpha = 1,
      #     stroke = 1
      #   )
      # } else if(2 %in% input$personalizarGrafico) {
      #   p = p +
      #     geom_segment(aes(x = fecha,
      #                      xend = fecha,
      #                      y = montoReal,
      #                      yend = 0),
      #                  colour = "blue4",
      #                  alpha = 0.5)
      # } else if(3 %in% input$personalizarGrafico) {
      #   p = p +
      #     geom_line(stat = "smooth",colour = "#FFB81C")
      # } else if(4 %in% input$personalizarGrafico) {
      #   
      # }
      #   
      #   # facet_wrap(~ credito) +
      #   theme_classic()
      # 
      
      p = p %>% ggplotly()
    }
    
    # Plot the ggplot object
    print(p)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)


