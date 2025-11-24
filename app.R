#paquetes
#install.packages("shiny")
# install.packages("DT")


#librerias
library(shiny)
library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)
library(scales)
library(DT)


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

#eliminamos los ticket fusionados (ya que existen en un nuevo ticket)
ticket <- ticket %>%
  filter(estado_activo_nombre != "fusionado")

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
meses_castellano <- c(
  "Enero", "Febrero", "Marzo", "Abril",
  "Mayo", "Junio", "Julio", "Agosto",
  "Septiembre", "Octubre", "Noviembre", "Diciembre"
)

ticket <- ticket %>%
  mutate(
    anio = format(fecha, "%Y") %>% as.integer(),
    mes  = format(fecha, "%m") %>% as.integer(),
    mes_label = meses_castellano[mes]
  )


# vamos a plantear el primer gráfico que muestre la cantidad de ticket solicitadas por sector

ui <- fluidPage(
  titlePanel("Tablero de tickets por sector"),
  
  fluidRow(
    column(
      width = 2,   # filtros más angostos
      wellPanel(   # opcional, para que se vea como un panel
        selectInput(
          "filtro_anio",
          "Año:",
          choices = sort(unique(ticket$anio)),
          selected = max(ticket$anio)
        ),
        
        selectInput(
          "filtro_mes",
          "Mes:",
          choices = setNames(1:12, meses_castellano),
          selected = max(ticket$mes)
        ),
        
        selectInput(
          "filtro_motivo",
          "Motivo:",
          choices = c("Todos", sort(unique(ticket$motivo))),
          selected = "Todos"
        )
      )
    ),
    
    column(
      width = 9,   # acá van gráficos y tablas, bien ancho
      h3(textOutput("titulo_resumen")),
      
      fluidRow(
        column(
          width = 6,
          h3("Tickets por sector"),
          plotOutput("grafico_tickets", height = "350px")
        ),
        column(
          width = 6,
          h3("Importes solicitados por sector"),
          plotOutput("grafico_importes", height = "350px"),
          br(),
          DT::dataTableOutput("tabla_importes")
        )
      ),
      br(),
      
      DT::dataTableOutput("tabla_resumen")
    )
  )
)
#---------------- SERVER ----------------#

server <- function(input, output, session) {
  
  # datos filtrados según año, mes y motivo
  ticket_filtrado <- reactive({
    req(input$filtro_anio, input$filtro_mes)
    
    data <- ticket %>%
      filter(
        anio == input$filtro_anio,
        mes  == as.integer(input$filtro_mes)
      )
    
    if (input$filtro_motivo != "Todos") {
      data <- data %>% filter(motivo == input$filtro_motivo)
    }
    
    data
  })
  
  # resumen: cantidad de tickets por sector
  ticket_resumen <- reactive({
    ticket_filtrado() %>%
      count(sector_origen, name = "cantidad_tickets") %>%
      arrange(desc(cantidad_tickets))
  })
  
  # resumen: importe total solicitado por sector
  ticket_importes <- reactive({
    ticket_filtrado() %>%
      group_by(sector_origen) %>%
      summarise(importe_total = sum(importesolicitado, na.rm = TRUE)) %>%
      mutate(
        importe_millones = importe_total / 1e6,
        importe_millones_label = scales::dollar_format(
          prefix = "$",
          suffix = "M",
          big.mark = ".",
          decimal.mark = ",",
          scale = 1e-6
        )(importe_total)
      ) %>%
      arrange(desc(importe_total))
  })
  
  # título dinámico
  output$titulo_resumen <- renderText({
    paste0(
      "Tickets - ",
      input$filtro_anio, " / ", input$filtro_mes,
      "  (Total tickets: ", nrow(ticket_filtrado()), ")"
    )
  })
  
  # gráfico: cantidad de tickets
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
  
  # gráfico: importe total solicitado por sector (en millones)
  output$grafico_importes <- renderPlot({
    data <- ticket_importes()
    
    # pasamos a millones para que se vea más prolijo
    data <- data %>%
      mutate(importe_millones = importe_total / 1e6)
    
    ggplot(data, aes(x = reorder(sector_origen, importe_millones),
                     y = importe_millones)) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Sector de origen",
        y = "Importe solicitado (millones)",
        title = NULL
      ) +
      theme_minimal()
  })
  
  # tabla con el resumen (cantidad por sector)
  output$tabla_resumen <- DT::renderDataTable({
    ticket_resumen()
  },
  options = list(
    pageLength = 10,       # filas por página
    lengthChange = TRUE,   # permitir cambiar cantidad de filas
    searching = TRUE,      # buscador
    ordering = TRUE,       # ordenar por columnas
    scrollX = TRUE         # scroll horizontal si hace falta
  ))
  
  # tabla con el resumen (importes por sector)
  output$tabla_importes <- DT::renderDataTable({
    
    data <- ticket_importes() %>%
      mutate(
        importe_total = round(importe_total, 0)  # sin decimales
      ) %>%
      select(
        sector_origen,
        importe_total,
        importe_millones_label
      )
    
    dt <- DT::datatable(
      data,
      colnames = c("Sector", "Importe total", "Importe (millones)"),
      options = list(
        pageLength   = 10,
        lengthChange = TRUE,
        ordering     = TRUE,
        searching    = FALSE,
        scrollX      = TRUE
      )
    )
    
    # Colorear según el importe total (barra de intensidad en la celda)
    dt %>%
      DT::formatStyle(
        "importe_total",
        background = DT::styleColorBar(
          range(ticket_importes()$importe_total, na.rm = TRUE),
          "#cce5ff"
        ),
        backgroundSize     = "100% 90%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      )
    
  })
  
  
}

shinyApp(ui, server)





