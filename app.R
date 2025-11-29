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
          area,
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

# definimos os filtros
ui <- fluidPage(
  
  tags$head(
    tags$link(rel="stylesheet",
              href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css")
  ),
  
  tags$h1(
    "Tablero de Solicitudes por sector",
    style = "font-weight:700; margin-bottom:20px;"
  ),
  
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
          "filtro_area",
          "Area:",
          choices = c("Todos", sort(unique(ticket$area))),
          selected = "Club"
        ),
        selectInput(
          "filtro_estado",
          "Estado:",
          choices = c("Todos", sort(unique(ticket$estado_resumen_ticket))),
          selected = "Todos"
        )
      )
    ),
    
    # Columna derecha: tarjetas + gráficos + tablas
    column(
      width = 10,
      
      tabsetPanel(
        id = "tabs",
        
        # ----------------------------- #
        # TAB PRINCIPAL (Resumen)
        # ----------------------------- #
        tabPanel(
          "Resumen",
          
          fluidRow(
            style = "margin-top:20px;",
            column(3, uiOutput("card_periodo")),
            column(3, uiOutput("card_tickets")),
            column(3, uiOutput("card_importe_solicitado")),
            column(3, uiOutput("card_importe_aprobado"))
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 6,
              h3("Solicitudes por sector"),
              plotlyOutput("grafico_tickets", height = "350px"),   # 👈 CAMBIAMOS A plotly
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
        ),
        
        # ----------------------------- #
        # TAB DETALLE SECTOR
        # ----------------------------- #
        tabPanel(
          "Detalle sector",
          
          # Encabezado tipo tarjeta
          div(
            h3(textOutput("titulo_detalle_sector")),
            style = "
              background:#f5f5f5;
              padding:12px 18px;
              border-radius:8px;
              margin-top:10px;
              margin-bottom:18px;
              font-weight:700;
              color:#333;
              box-shadow:0 2px 6px rgba(0,0,0,0.08);
            "
                  ),
          
          plotOutput("grafico_evolucion_sector", height = "300px"),
          br(),
          DTOutput("tabla_evolucion_sector")),
          
        # ----------------------------- #
        # TAB ACERDA DEL TABLERO
        # ----------------------------- #
        
        tabPanel(
          "Acerca del Tablero",
          h2("Descripción del tablero"),
          p("
            
          "),
          p("
            
          "),
          p("
            .
          "
          )
        )
      )
    )
  )
)

sector_click <- reactiveVal(NULL)

##########################################
#---------------- SERVER ----------------#
##########################################

server <- function(input, output, session) {
  
  # datos filtrados según año, mes y area
  ticket_filtrado <- reactive({
    req(input$filtro_anio, input$filtro_mes)
    
    data <- ticket %>%
      filter(
        anio == input$filtro_anio,
        mes  == as.integer(input$filtro_mes)
      )
    if (input$filtro_area != "Todos") {
      data <- data %>% filter(area == input$filtro_area)
    }
    
    if (!is.null(input$filtro_estado) && input$filtro_estado != "Todos") {
      data <- data %>% filter(estado_resumen_ticket == input$filtro_estado)
    }
    
    data
  })
  
  # resumen: cantidad de tickets por sector
  ticket_resumen <- reactive({
    ticket_filtrado() %>%
      count(sector_origen, name = "cantidad_tickets") %>%
      arrange(desc(cantidad_tickets))
  })
  
  sector_default <- reactive({
    df <- ticket_resumen()
    if (nrow(df) == 0) return(NULL)
    
    df$sector_origen[1]   # primer sector del ranking
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
    padding:14px 10px;
    border-radius:8px;
    color:white;
    text-align:center;
    box-shadow:0 3px 8px rgba(0,0,0,0.14);
  "
  
  
  # 🟦 Período
  output$card_periodo <- renderUI({
    tags$div(
      style = paste0(card_style_dashboard, "background:#1976d2;"),
      tags$h4(tags$b(paste0(input$filtro_mes, "/", input$filtro_anio))),
      tags$div("Período", style="font-size:15px; opacity:0.9;"),
      tags$i(class="fa fa-calendar", style="font-size:40px; margin-top:10px;")
    )
  })
  
  
  # Total solicitudes
  output$card_tickets <- renderUI({
    df <- ticket_filtrado()
    total <- nrow(df)
    
    tags$div(
      style = paste0(card_style_dashboard, "background:#f9a825;"),
      tags$h4(tags$b(scales::number(total, big.mark = ".", decimal.mark=","))),
      tags$div("Total solicitudes", style="font-size:15px; opacity:0.9;"),
      tags$i(class="fa fa-ticket-alt", style="font-size:40px; margin-top:10px;")
    )
  })
  
  
  # Importe solicitado
  output$card_importe_solicitado <- renderUI({
    df <- ticket_filtrado()
    total <- sum(df$importesolicitado, na.rm = TRUE)
    
    tags$div(
      style = paste0(card_style_dashboard, "background:#43a047;"),
      tags$h4(tags$b(
        scales::dollar(total, prefix="$", big.mark=".", decimal.mark=",", accuracy=1)
      )),
      tags$div("Importe solicitado", style="font-size:15px; opacity:0.9;"),
      tags$i(class="fa fa-money-bill-wave", style="font-size:40px; margin-top:10px;")
    )
  })
  
  # Importe aprobado
  output$card_importe_aprobado <- renderUI({
    df <- ticket_filtrado()
    total <- sum(df$importeaprobado, na.rm = TRUE)
    
    tags$div(
      style = paste0(card_style_dashboard, "background:#7e57c2;"),
      tags$h4(tags$b(
        scales::dollar(total, prefix="$", big.mark=".", decimal.mark=",", accuracy=1)
      )),
      tags$div("Importe aprobado", style="font-size:15px; opacity:0.9;"),
      tags$i(class="fa fa-check-circle", style="font-size:40px; margin-top:10px;")
    )
  })
  
  
  # gráfico: cantidad de tickets
  output$grafico_tickets <- plotly::renderPlotly({
    df <- ticket_resumen() %>%
      arrange(desc(cantidad_tickets))   # 👈 ordenar desc
    
    plotly::plot_ly(
      df,
      x = ~cantidad_tickets,            # 👈 horizontal
      y = ~reorder(sector_origen, cantidad_tickets),
      type = "bar",
      orientation = "h",                # 👈 clave
      source = "sectores"
    ) %>%
      layout(
        xaxis = list(title = "Cantidad de solicitudes"),
        yaxis = list(title = "Sector"),
        margin = list(l = 150)            # deja espacio para nombres largos
      )
  })
  
  
  observeEvent(plotly::event_data("plotly_click", source = "sectores"), {
    evento <- plotly::event_data("plotly_click", source = "sectores")
    req(evento$x)
    
    sector_click(evento$y)  # guardamos el sector clickeado
    
    updateTabsetPanel(session, "tabs", selected = "Detalle sector")
  })
  
  observeEvent(input$tabla_resumen_rows_selected, {
    fila <- input$tabla_resumen_rows_selected
    req(length(fila) == 1)
    
    # obtenemos el sector según la fila seleccionada
    df_resumen <- ticket_resumen()
    sector_sel <- df_resumen$sector_origen[fila]
    
    sector_click(sector_sel)   # usamos el mismo reactiveVal que el gráfico
    
    updateTabsetPanel(session, "tabs", selected = "Detalle sector")
  })
  
  # Datos de evolución mensual para el sector clickeado
  ticket_evolucion_sector <- reactive({
    sec <- sector_click()
    if (is.null(sec)) {
      sec <- sector_default()    # 👈 usamos el primer sector
    }
    
    ticket %>%
      filter(
        sector_origen == sec,
        if (input$filtro_estado != "Todos") estado_resumen_ticket == input$filtro_estado else TRUE,
        if (input$filtro_area   != "Todos") area == input$filtro_area else TRUE
      ) %>%
      group_by(anio, mes) %>%
      summarise(
        cantidad           = n(),
        importe_solicitado = sum(importesolicitado, na.rm = TRUE),
        importe_aprobado   = sum(importeaprobado,   na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(anio, mes) %>%
      mutate(periodo = sprintf("%02d/%d", mes, anio))
  })
  
  
  # Gráfico de evolución en la pestaña "Detalle sector"
  output$grafico_evolucion_sector <- renderPlot({
    df <- ticket_evolucion_sector()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = periodo, y = importe_aprobado, group = 1)) +
      geom_line(color = "forestgreen", linewidth = 1.2) +
      geom_point(size = 3, color = "forestgreen") +
      labs(
        x = "Período",
        y = "Importe aprobado ($)",
        title = "Evolución mensual del importe aprobado"
      ) +
      scale_y_continuous(
        labels = function(x) scales::dollar(x, prefix = "$", big.mark = ".", decimal.mark = ",")
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title  = element_text(size = 14, face = "bold")
      )
  })
  
  # Tabla de evolución debajo del gráfico
  output$tabla_evolucion_sector <- DT::renderDT({
    df <- ticket_evolucion_sector()
    
    df2 <- df %>%
      mutate(
        porcentaje_aprobado = ifelse(
          importe_solicitado > 0,
          importe_aprobado / importe_solicitado * 100,
          NA
        )
      ) %>%
      transmute(
        Período                = periodo,
        `Cant. solicitudes` = cantidad,
        `Importe solicitado`   = scales::dollar(
          importe_solicitado,
          prefix = "$", big.mark = ".", decimal.mark = ",", accuracy = 1
        ),
        `Importe aprobado`     = scales::dollar(
          importe_aprobado,
          prefix = "$", big.mark = ".", decimal.mark = ",", accuracy = 1
        ),
        `% aprobado` = scales::percent(
          porcentaje_aprobado / 100,
          accuracy = 0.1,
          decimal.mark = ","
        )
      )
    
    datatable(
      df2,
      options = list(
        pageLength = 12,
        columnDefs = list(
          list(targets = c(2, 3, 4, 5), className = "dt-right")   
        )
      )
    )
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
    datatable(
      ticket_resumen(),
      colnames = c("Sector", "Cantidad de tickets"),
      selection = "single",
      options = list(
        pageLength   = 10,       # filas por página
        lengthChange = TRUE,     # permitir cambiar cantidad de filas
        searching    = TRUE,     # buscador
        ordering     = TRUE,     # ordenar por columnas
        scrollX      = TRUE      # scroll horizontal si hace falta
      )
    )
  })
  
  # tabla con el resumen (importes por sector)
  output$tabla_importes <- DT::renderDataTable({
    
    data <- ticket_importes() %>%
      select(
        sector_origen,
        importe_total,
        importe_millones_label
      )
    
    dt <- DT::datatable(
      data,
      colnames = c("Sector", "Importe total", "Imp. (millones)"),
      options = list(
        pageLength   = 10,
        lengthChange = TRUE,
        ordering     = TRUE,
        searching    = FALSE,
        scrollX      = TRUE,
        columnDefs = list(
          list(className = "dt-right", targets = c(1, 2, 3))  # columnas de importes a la derecha
        )
      )
    ) %>%
      # 👇 Formateamos importe_total con . y , y símbolo $
      DT::formatCurrency(
        "importe_total",
        currency = "$",
        mark = ".",        # separador de miles
        dec.mark = ",",    # separador decimales
        digits = 0         # sin decimales
      )
    
    # Colorear según el importe total (barra azul)
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
  
  
  
  output$titulo_detalle_sector <- renderText({
    sec <- sector_click()
    
    # si no hay nada seleccionado, usamos el primer sector del ranking
    if (is.null(sec)) {
      sec <- sector_default()
    }
    
    paste("Evolución del sector:", sec)
  })
}

shinyApp(ui, server)





