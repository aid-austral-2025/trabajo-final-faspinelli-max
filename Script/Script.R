#paquetes
#install.packages("shiny")

#librerias
library(shiny)
library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)
library(plotly)


# importar archivo de Solicitudes
ticket <- read_xlsx("datos/ticket.xlsx")

#str(ticket)

######################
#limpieza y analisis de variables del archivo
######################

#analizo las columnas para pasar a boolean cuando sea necesario

#glimpse(ticket)

ticket$activo <- as.logical(ticket$activo)
ticket$ticket_finalizado <- as.logical(ticket$ticket_finalizado)
ticket$ticket_mercaderiarecibida <- as.logical(ticket$ticket_mercaderiarecibida)

#paso a mayusculas columnas de textos que son ingresadas por los usuarios finales

ticket$descripcion <- toupper(ticket$descripcion)
ticket$asunto <- toupper(ticket$asunto)
ticket$tipofondocomentario <- toupper(ticket$tipofondocomentario)

#nos quedamos solo con las solicitudes activas
ticket <- ticket %>%
  filter(activo == TRUE)

#pasamos a fecha las columnas tipo fecha
ticket$fecha <- as.Date(ticket$fecha)
ticket$fechavenc <- as.Date(ticket$fechavenc)

#listamos las columnas para saber cuales no son necesarias o indican id
#cat(names(ticket), sep = "\n")

#ordenar las columnas del dataset para que sean mas facil de interpretar
#y eliminamos columnas que no tienen información importante

ticket <- ticket %>%
  select (fecha,
          fechavenc,
          sede_nombre_sectororigen,
          sector_origen,
          motivo,
          comprobante,
          asunto,
          descripcion,
          prioridad,
          importesolicitado,
          importeaprobado,
          razonsocial,
          tipofondo,
          tipofondocomentario,
          estado_activo_nombre,
          sede_nombre_sectoractual,
          sector_actual_nombre,
          ticket_mercaderiarecibida,
          ticket_finalizado)

#agregamos columnas de año, mes, y mes en letras

ticket <- ticket %>%
  mutate(
    anio = format(fecha, "%Y") %>% as.integer(),
    mes  = format(fecha, "%m") %>% as.integer(),
    mes_label = format(fecha, "%B") %>% tools::toTitleCase()
  )


# vamos a plantear el primer gráfico que muestre la cantidad de ticket solicitadas por sector

ui <- fluidPage(
  titlePanel("Tablero simple de tickets por sector"),
  
  sidebarLayout(
    sidebarPanel(
      # Filtro por año
      selectInput(
        "filtro_anio",
        "Año:",
        choices = sort(unique(ticket$anio)),
        selected = max(ticket$anio)
      ),
      
      # Filtro por mes (1..12)
      selectInput(
        "filtro_mes",
        "Mes:",
        choices = setNames(1:12, month.name),  # muestra Enero, Febrero, etc.
        selected = max(ticket$mes)
      )
    ),
    
    mainPanel(
      h3(textOutput("titulo_resumen")),
      plotOutput("grafico_tickets"),
      br(),
      tableOutput("tabla_resumen")
    )
  )
)

server <- function(input, output, session) {
  
  # Datos filtrados según año y mes elegidos
  ticket_filtrado <- reactive({
    req(input$filtro_anio, input$filtro_mes)
    
    ticket %>%
      filter(
        anio == input$filtro_anio,
        mes  == as.integer(input$filtro_mes)
      )
  })
  
  # Resumen: tickets por sector
  ticket_resumen <- reactive({
    ticket_filtrado() %>%
      count(sector_origen, name = "cantidad_tickets") %>%
      arrange(desc(cantidad_tickets))
  })
  
  # Título dinámico
  output$titulo_resumen <- renderText({
    paste0(
      "Tickets por sector - ",
      input$filtro_anio, " / ", input$filtro_mes,
      "  (Total: ", nrow(ticket_filtrado()), " tickets)"
    )
  })
  
  # Gráfico de barras
  output$grafico_tickets <- renderPlot({
    data <- ticket_resumen()
    
    ggplot(data, aes(x = reorder(sector_origen, cantidad_tickets),
                     y = cantidad_tickets)) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Sector de origen",
        y = "Cantidad de tickets"
      ) +
      theme_minimal()
  })
  
  # Tabla con el resumen
  output$tabla_resumen <- renderTable({
    ticket_resumen()
  })
}

shinyApp(ui, server)





