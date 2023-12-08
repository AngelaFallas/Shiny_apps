library(shiny)
library(ggplot2)
library(shinydashboard)
library(dplyr)
library(readxl)
library(ggplot2)
library(gapminder)
library(readr)
library(janitor)

datos_empleo_genero <- read_csv("datos/datos_empleo_genero.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Género y Desempleo en Latinoamérica", titleWidth = 700),
  dashboardSidebar(
    disable = FALSE,
    width = 300,
    sidebarMenu(
      menuItem("Bienvenida", tabName = "bienvenida", icon = icon("person")),
      menuItem("Tabla", tabName = "tabla", icon = icon("th")),
      menuItem("Gráfico de barras", tabName = "bars", icon = icon("chart-bar")),
      menuItem("Histograma", tabName = "histograma", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "bienvenida",
        h2("Bienvenida"),
        p("Hola! A continuación pueden encontrar datos relacionados con al desempleo en América Latína"),
        img(src = "Latinoamerica", width = 500, height = 500)
      ),
      tabItem(
        tabName = "tabla",
        h2("Tabla"),
        p("La siguiente tabla muestra datos generales del género y el desempleo en América Latina"),
        selectInput("columna_seleccionada", 
                    label = "Selecciona las columnas que deseas visualizar en la tabla:", 
                    choices = colnames(datos_empleo_genero),
                    multiple = TRUE),
        dataTableOutput("tabla_seleccionada")
      ),
      tabItem(
        tabName = "bars",
        h2("Gráfico de barras"),
        selectInput("variable_seleccionada_yg", 
                    label = "Selecciona una variable continua:", 
                    choices = c(colnames(datos_empleo_genero)),
                    selected = "Pais"),
        plotOutput("bars1"),
      ),
      tabItem(
        tabName = "histograma",
        h2("Histograma"),
        selectInput("variable_seleccionada_xh", 
                    label = "Selecciona una variable continua:", 
                    choices = c(colnames(datos_empleo_genero)),
                    selected = "Pais"),
        plotOutput("histograma1")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  output$tabla_seleccionada <- renderDataTable({
    datos_empleo_genero[, input$columna_seleccionada, drop = FALSE]
  })
  
  output$bars1 <- renderPlot({
    ggplot(datos_empleo_genero, aes(y = input$variable_seleccionada_yg, x = pais_region)) +
      geom_bar(stat = "identity", fill = "#0A4B60") +
      labs(title = "Gráfico de Barras", y = "Empleadoras mujeres", x = "País") +
      theme_classic()
    
  })
  
  output$histograma1 <- renderPlot({
    ggplot(datos_empleo_genero, aes_string(x = input$variable_seleccionada_xh)) + 
      geom_histogram(binwidth = 0.5, fill = "#0A4B60", color = "#f5f5f5", alpha = 0.7) +
      labs(title = paste("Histograma de", input$variable_seleccionada), 
           x = input$variable_seleccionada) +
      theme_classic()
  })
}

shinyApp(ui, server)
