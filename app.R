library(shiny)
library(ggplot2)
library(shinydashboard)
library(dplyr)
library(readr)
library(janitor)

datos_empleo_genero <- read_csv("datos/datos_empleo_genero.csv")

ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Género y Desempleo en Latinoamérica", titleWidth = 700),
  dashboardSidebar(
    disable = FALSE,
    width = 300,
    sidebarMenu(
      menuItem("Bienvenida", tabName = "bienvenida", icon = icon("person")),
      menuItem("Tabla", tabName = "tabla", icon = icon("th")),
      menuItem("Gráfico de empleabilidad en mujeres", tabName = "barra_1", icon = icon("chart-bar")),
      menuItem("Gráfico de empleabilidad en hombres", tabName = "barra_2", icon = icon("chart-bar")),
      menuItem("Histograma", tabName = "histograma", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "bienvenida",
        h2("Bienvenida"),
        p("Hola! A continuación pueden encontrar datos relacionados con al desempleo en América Latina"),
        img(src = "latinoamerica_2.jpg", width = "100%")
      ),
      tabItem(
        tabName = "tabla",
        h2("Tabla"),
        p("La siguiente tabla muestra datos generales del género y el desempleo en América Latina"),
        selectInput("columna_seleccionada", 
                    label = "Selecciona las columnas que deseas visualizar en la tabla:", 
                    choices = colnames(datos_empleo_genero),
                    multiple = TRUE),
        dataTableOutput("tabla_seleccionada"),
        actionButton(inputId = "mostrar_tabla", label = "Mostrar tabla"),
        textOutput(outputId = "datos_seleccionados")
      ),
      tabItem(
        tabName = "barra_1",
        h2("Gráfico: empleo en mujeres"),
        p("El siguiente gráfico de barras muestra información relacionada con el empleo de mujeres en América Latina"),
        selectInput("variable_seleccionada_xg", 
                    label = "Selecciona una variable continua:", 
                    choices = colnames(datos_empleo_genero),
                    selected = "Pais"),
        plotOutput("barra_1")
      ),
      tabItem(
        tabName = "barra_2",
        h2("Gráfico de empleabilidad en hombres"),
        p("El siguiente gráfico de barras muestra información relacionada con el empleo de hombres en América Latina"),
        selectInput("variable_seleccionada_xg_2", 
                    label = "Selecciona una variable continua:", 
                    choices = c(colnames(datos_empleo_genero)),
                    selected = "Pais"),
        plotOutput("barra_2")
      ),
      tabItem(
        tabName = "histograma",
        h2("Histograma: Conteo de datos sobre empleo en hombres y mujeres"),
        selectInput("variable_seleccionada_xh", 
                    label = "Selecciona una variable continua:", 
                    choices = c(colnames(datos_empleo_genero)),
                    selected = "Pais"),
        plotOutput("histograma1")
      )
    )
  )
)

server <- function(input, output, session) {
  
  datos_filtrados <- reactive({
    select_cols <- input$columna_seleccionada
    if (length(select_cols) == 0) {
      return(NULL)
    }
    datos_empleo_genero |> 
      select(all_of(select_cols))
  })
  
  output$tabla_seleccionada <- renderDataTable({
    req(input$mostrar_tabla)
    datos_filtrados()
  })
  
  output$barra_1 <- renderPlot({
    ggplot(datos_empleo_genero, aes(x = get(input$variable_seleccionada_xg), y = pais_region)) +
      geom_bar(stat = "identity", fill = "#0A4B60") +
      labs(title = "Gráfico: Empleo en Mujeres", x = input$variable_seleccionada_xg , y = "País") +
      theme_classic()
  })
  
  output$barra_2 <- renderPlot({
    ggplot(datos_empleo_genero, aes(x = get(input$variable_seleccionada_xg_2), y = pais_region)) +
      geom_bar(stat = "identity", fill = "#0A4B60") +
      labs(title = "Gráfico: Empleo en Hombres", x = "Empleadores hombres", y = "País") +
      theme_classic()
  })
  
  output$histograma1 <- renderPlot({
    ggplot(datos_empleo_genero, aes_string(x = input$variable_seleccionada_xh)) + 
      geom_histogram(binwidth = 0.5, fill = "#0A4B60", color = "#f5f5f5", alpha = 0.7) +
      labs(title = paste("Histograma de", input$variable_seleccionada_xh), 
           x = input$variable_seleccionada_xh) +
      theme_classic()
  })
}

shinyApp(ui, server)

