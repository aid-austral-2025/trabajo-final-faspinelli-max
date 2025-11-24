#paquetes
#install.packages("shiny")
# install.packages("DT")
#install.packages("shinydashboard")
#install.packages("bs4Dash")


#librerias
library(shiny)
library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)
library(scales)
library(DT)
library(shinydashboard)
library(bs4Dash)


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

#eliminamos los ticket fusionados(ya que existen en un nuevo ticket) y Cancelados (cargados por error)
ticket <- ticket %>%
  filter(!estado_activo_nombre %in% c("Fusionado", "Cancelado"))

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


ticket <- ticket %>%
  mutate(
    estado_resumen_ticket = case_when(
      estado_activo_nombre %in% c("Rechazado") ~ estado_activo_nombre,
      TRUE ~ "Aprobado"
    )
  )

# vamos a plantear el primer gráfico que muestre la cantidad de ticket solicitadas por sector
ui <- fluidPage(
  titlePanel("Tablero de Solicitudes por sector"),
  
  fluidRow(
    # Filtros (columna izquierda)
    column(
      width = 2,   # filtros más angostos
      wellPanel(
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
          "filtro_estado",
          "Estado:",
          choices = c("Todos", sort(unique(ticket$estado_resumen_ticket))),
          selected = "Aprobado"
        ),
        
        selectInput(
          "filtro_motivo",
          "Motivo:",
          choices = c("Todos", sort(unique(ticket$motivo))),
          selected = "Todos"
        )
      )
    ),
    
    # Columna derecha: tarjetas + gráficos + tablas
    column(
      width = 10,
      
      # Fila de tarjetas KPI
      fluidRow(
        column(3, uiOutput("card_periodo")),
        column(3, uiOutput("card_tickets")),
        column(3, uiOutput("card_importe_solicitado")),
        column(3, uiOutput("card_importe_aprobado"))
      ),
      
      br(),
      
      # Fila de gráficos y tablas
      fluidRow(
        column(
          width = 6,
          h3("Tickets por sector"),
          plotOutput("grafico_tickets", height = "350px"),
          br(),
          DT::dataTableOutput("tabla_resumen")
        ),
        column(
          width = 6,
          h3("Importes solicitados por sector"),
          plotOutput("grafico_importes", height = "350px"),
          br(),
          DT::dataTableOutput("tabla_importes")
        )
      )
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
    
    if (!is.null(input$filtro_estado) && input$filtro_estado != "Todos") {
      data <- data %>% filter(estado_resumen_ticket == input$filtro_estado)
    }
    
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
        # valor en millones (numérico)
        importe_millones = importe_total / 1e6,
        
        # etiqueta en millones ($12,5M)
        importe_millones_label = scales::dollar_format(
          prefix = "$",
          suffix = "M",
          big.mark = ".",
          decimal.mark = ",",
          scale = 1e-6
        )(importe_total),
        
        importe_total_label = scales::number(
          importe_total,
          big.mark = ".",      # miles
          decimal.mark = ",",  # decimales
          accuracy = 1         # sin decimales (si querés decimales pongo 0.1)
        )
      ) %>%
      arrange(desc(importe_total))
  })
  
 
  # Estilo común para todas las tarjetas
  card_style_dashboard <- "
    padding:20px;
    border-radius:10px;
    color:white;
    text-align:center;
    box-shadow:0 4px 10px rgba(0,0,0,0.15);
  "
  
  
  # 🟦 Período
  output$card_periodo <- renderUI({
    tags$div(
      style = paste0(card_style_dashboard, "background:#1976d2;"),
      tags$h3(tags$b(paste0(input$filtro_mes, "/", input$filtro_anio))),
      tags$div("Período", style="font-size:15px; opacity:0.9;"),
      tags$i(class="fa fa-calendar", style="font-size:26px; margin-top:10px;")
    )
  })
  
  
  # Total solicitudes
  output$card_tickets <- renderUI({
    df <- ticket_filtrado()
    total <- nrow(df)
    
    tags$div(
      style = paste0(card_style_dashboard, "background:#f9a825;"),
      tags$h3(tags$b(scales::number(total, big.mark = "."))),
      tags$div("Total solicitudes", style="font-size:15px; opacity:0.9;"),
      tags$i(class="fa fa-ticket-alt", style="font-size:26px; margin-top:10px;")
    )
  })
  
  
  # Importe solicitado
  output$card_importe_solicitado <- renderUI({
    df <- ticket_filtrado()
    total <- sum(df$importesolicitado, na.rm = TRUE)
    
    tags$div(
      style = paste0(card_style_dashboard, "background:#43a047;"),
      tags$h3(tags$b(
        scales::dollar(total, prefix="$", big.mark=".", decimal.mark=",", accuracy=1)
      )),
      tags$div("Importe solicitado", style="font-size:15px; opacity:0.9;"),
      tags$i(class="fa fa-money-bill-wave", style="font-size:26px; margin-top:10px;")
    )
  })
  
  # Importe aprobado
  output$card_importe_aprobado <- renderUI({
    df <- ticket_filtrado()
    total <- sum(df$importeaprobado, na.rm = TRUE)
    
    tags$div(
      style = paste0(card_style_dashboard, "background:#7e57c2;"),
      tags$h3(tags$b(
        scales::dollar(total, prefix="$", big.mark=".", decimal.mark=",", accuracy=1)
      )),
      tags$div("Importe aprobado", style="font-size:15px; opacity:0.9;"),
      tags$i(class="fa fa-check-circle", style="font-size:26px; margin-top:10px;")
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
        y = "Cantidad de solicitudes"
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





