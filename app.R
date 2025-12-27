#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(plotly)
library(shinythemes)
library(rsconnect)
source("dashboard_femenil.R")  # Ensure this file returns all needed plot functions and data

player_info <- tibble::tibble(
  player = c("Itzel Velasco", "Sandra Paños", "Irene Guerrero",
             "Montse Saldívar", "Kimberly Rodríguez", "Scarlett Camberos", "Alexa Soto Ramírez", 
             "Kiana Palacios", "Sofía Ramos", "Nancy Antonio", "Karina Rodríguez", 
             "Bárbara Del Real Gómez", "Chidinma Okeke", 
             "Jana Gutiérrez", "Vanessa Paredes", "Karen Luna", "Sarah Luebbert", "Aylin Aviléz", 
             "Daniela Espinosa", "Alondra Cabanillas", "Isa Haas", "Xcaret Pineda", "Carol Acuña"
             # "Bruna Vilamala",
             ),
  image = c("itzel.jpg", "paños.jpg", "irene.jpg", 
                    "montse.jpg", "kimberly.jpg", "scarlett.jpg", "alexa_soto.jpg", "kiana.jpg", 
                    "ramos.jpg", "nancy.jpg", "karina.jpg", "barbara.jpg", 
                     "okeke.jpg", "jana.jpg", "vanessa.webp", 
                    "luna.jpg", "sarah.jpg", "aylin.jpg", "dani.jpg", "alondra.jpg", "haas.jpg", "xcaret.webp",
                    "carol.jpeg"
            # "bruna.jpg",
            ),
  age = c(20, 32, 28, 18, 26, 24, 18, 28, 20, 29, 26, 16, 24, 21, 16, 27, 27, 22, 26, 19, 24, 21, 18
          # 23
          ),
  height = c("1.72m", "1.69m", "1.68m", "1.69m", "1.69m", "1.73m", "1.60m", "1.67m",
             "1.64m", "1.71m", "1,72m", "1.72m", "1.75", "1.72m", "1.55m", "1.69m", 
             "1.72m", "1.54m", "1.72m", "1.56m", "1.77m", "1.75m", "1.53m"
             # "1.63m",
             )
)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Dashboard de Recuperación Física – Club América Femenil"),
  fluidRow(
    column(
      10,
      tabsetPanel(id = "top_tabs",
                  tabPanel("Recuperación", plotlyOutput("acwr_scatter")),
                  tabPanel("RPE",           plotlyOutput("acwr_rpe_scatter")),
                  tabPanel("Descanso",      plotlyOutput("acwr_rest_scatter")),
                  tabPanel("Dolor Muscular",         plotlyOutput("acwr_pain_scatter"))   
      )
    ),
    column(
      2,
      selectInput(
        inputId = "player_select",
        label = "Buscar jugadora",
        choices = sort(player_info$player),
        selected = NULL
      ),
      uiOutput("player_info_box")
    )
  ),
  fluidRow(
    column(12, h3(NULL, align = "center"), plotlyOutput("survey_plot", height = "600px"))
  ),
  fluidRow(
    column(
      12,
      tabsetPanel(
        tabPanel("HSR (>21 km/h)", plotlyOutput("hsr_plot", height = "500px")),
        tabPanel("Carga Aguda vs Crónica (A:C)", plotlyOutput("ac_plot", height = "500px"))
      )
    )
  ),
  fluidRow(
    column(
      12,
      h3("Interpretación de Métricas", align = "center"),
      tags$div(
        style = "margin: 10px 25px;",
        tags$details(
          tags$summary("Estado de la jugadora", style = "font-weight:bold; font-size:16px;"),
          "Si la jugadora está en verde, tiene un score de recuperación de 6 o mayor y su índice de carga esta en rango.",
          tags$br(),
          "Si la jugadora está en amarillo, uno de los valores, ya sea score de recuperación o índice de carga, es menor a 6 o está fuera de rango.",
          tags$br(),
          "Si la jugadora está en rojo, tiene un score de recuperación menor a 6 y su índice de carga esta fuera rango."
        ),
        tags$details(
          tags$summary("Interpretación Índice de Carga", style = "font-weight:bold; font-size:16px;"),
          "El índice de carga es el cociente de la carga aguda dividida por la carga crónica.",
          tags$br(),
          "La carga aguda es la suma de la distancia absoluta de High Speed Running (HSR_abs_dist) y la carga de la jugadora (player_load), ambos datos capturados por WIMU.",
          tags$br(),
          "La carga crónica es la media rodante de los valores de carga aguda, calculada tomando en cuenta los últimos siete días de carga aguda.",
          tags$br(),
          "Esta carga aguda se divide por la carga crónica para así tener el A:C ratio, en este caso interpretado como Índice de Carga."
        ),
        tags$details(
          tags$summary("Interpretación Estatus de Carga", style = "font-weight:bold; font-size:16px;"),
          "El valor del índice de carga, idealmente, debe de estar en un rango de 0.8 a 1.3.",
          tags$br(),
          "Si está dentro de este rango, se considera 'Punto Ideal'.",
          tags$br(),
          "Debajo de 0.8: 'Carga Baja'. Sobre 1.3: 'Sobrecargada'."
        ),
        tags$details(
          tags$summary("Interpretación Score de Recuperación", style = "font-weight:bold; font-size:16px;"),
          "Compuesto por fatiga, sueño, calidad de sueño y dolor muscular. Cada categoría tiene un puntaje entre 0.5 y 2.5, dependiendo de la respuesta de la jugadora en la encuesta de bienestar.",
          tags$br(),
          "A la mejor respuesta posible se le asigna un valor de 2.5, y la peor respuesta posible de 0.5 o 1. Si elige alguna opción entre la mejor y la peor opción, se le asigna un valor intermedio a su respuesta.",
          tags$br(),
          "Este proceso se lleva a cabo para cada una de las cuatro categorías. La suma total genera el score de recuperación sobre 10."
        ),
        tags$details(
          tags$summary("Interpretación Estatus de Recuperación", style = "font-weight:bold; font-size:16px;"),
          "El estatus de recuperación se calcula directamente del score de recuperación.",
          tags$br(),
          "Si una jugadora presenta un score de recuperación de 6 o mayor, su estatus será 'Recuperada'.",
          tags$br(),
          "Si es menor a 6, su estatus será 'Fatigada'."
        ),
        tags$details(
          tags$summary("Scores de Indicadores de Recuperación Diaria", style = "font-weight:bold; font-size:16px;"),
          "Son basados en las respuestas diarias de las jugadoras en la encuesta de bienestar."
        )
      )
    )
  )
)

server <- function(input, output, session) {
  selected <- reactiveVal(NULL)
  
  # Keep selected player in sync with dropdown
  observeEvent(input$player_select, {
    selected(input$player_select)
  })
  
  # --- Click sync: Recovery scatter
  observeEvent(event_data("plotly_click", source = "acwr_scatter"), {
    click_data <- event_data("plotly_click", source = "acwr_scatter")
    if (!is.null(click_data)) {
      selected(click_data$customdata)
      updateSelectInput(session, "player_select", selected = click_data$customdata)
    }
  })
  
  # --- Click sync: RPE scatter
  observeEvent(event_data("plotly_click", source = "acwr_rpe_scatter"), {
    click_data <- event_data("plotly_click", source = "acwr_rpe_scatter")
    if (!is.null(click_data)) {
      selected(click_data$customdata)
      updateSelectInput(session, "player_select", selected = click_data$customdata)
    }
  })
  
  # --- Click sync: REST scatter (NEW)
  observeEvent(event_data("plotly_click", source = "acwr_rest_scatter"), {
    click_data <- event_data("plotly_click", source = "acwr_rest_scatter")
    if (!is.null(click_data)) {
      selected(click_data$customdata)
      updateSelectInput(session, "player_select", selected = click_data$customdata)
    }
  })
  
  # --- Click sync: PAIN scatter (NEW)
  observeEvent(event_data("plotly_click", source = "acwr_pain_scatter"), {
    click_data <- event_data("plotly_click", source = "acwr_pain_scatter")
    if (!is.null(click_data)) {
      selected(click_data$customdata)
      updateSelectInput(session, "player_select", selected = click_data$customdata)
    }
  })
  
  # =========================
  #   TOP PLOTS (4 tabs)
  # =========================
  
  # 1) ACWR x RECOVERY
  output$acwr_scatter <- renderPlotly({
    req(selected())
    
    df <- scatter_df |>
      dplyr::mutate(
        selected_flag = player == selected(),
        hover_text = paste0(
          "Jugadora: ", player,
          "<br>Fecha: ", latest_date,
          "<br>Score de Recuperación: ", recovery_score,
          "<br>Índice de Carga: ", round(ac_ratio, 2),
          "<br>Estatus de Recuperación: ", recovery_status,
          "<br>Estatus de Carga: ", load_status,
          ifelse(pain_flag, paste0("<br>Zona Adolorida: ", zona_adolorida), "")
        )
      )
    
    # one ring per player with pain_flag == TRUE
    rings_df <- df |>
      dplyr::filter(pain_flag) |>
      dplyr::distinct(player, .keep_all = TRUE)
    
    p <- ggplot(df, aes(x = recovery_score, y = ac_ratio)) +
      geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
      # base points
      geom_point(aes(fill = color_status, text = hover_text, customdata = player),
                 size = 6, alpha = 0.3, color = "black", shape = 21) +
      # selected highlight
      geom_point(data = df |> dplyr::filter(selected_flag),
                 aes(fill = color_status, text = hover_text, customdata = player),
                 size = 8, shape = 21, stroke = 2, color = "black") +
      # RED RINGS for pain_flag
      geom_point(data = rings_df,
                 aes(x = recovery_score, y = ac_ratio),
                 inherit.aes = FALSE,
                 shape = 21, size = 9, stroke = 1, fill = NA, color = "#d62728") +
      scale_fill_manual(values = c(green = "#2ca02c", yellow = "#ffbf00", red = "#d62728")) +
      labs(x = "Score de Recuperación", y = "Índice de Carga (ACWR)",
           title = "ACWR & Recuperación: Resumen del equipo de hoy") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank())
    
    ggplotly(p, tooltip = "text", source = "acwr_scatter")
  })
  
  # 2) ACWR x RPE
  output$acwr_rpe_scatter <- renderPlotly({
    req(selected())
    df <- scatter_df_rpe |>
      dplyr::mutate(
        selected_flag = player == selected(),
        hover_text = paste0(
          "Jugadora: ", player,
          "<br>Fecha (RPE): ", latest_date,
          "<br>RPE: ", rpe_label,
          "<br>Índice de Carga: ", round(ac_ratio, 2),
          "<br>Estatus de Carga: ", load_status
        )
      )
    
    p2 <- ggplot(df, aes(x = rpe_val, y = ac_ratio)) +
      geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
      scale_x_continuous(breaks = 1:9) +
      geom_point(aes(fill = color_status, text = hover_text, customdata = player),
                 size = 6, alpha = 0.3, color = "black") +
      geom_point(data = df |> dplyr::filter(selected_flag),
                 aes(fill = color_status, text = hover_text, customdata = player),
                 size = 8, shape = 21, stroke = 2, color = "black") +
      scale_fill_manual(values = c("green"="#2ca02c","yellow"="#ffbf00","red"="#d62728")) +
      labs(x = "RPE de la sesión (1–10)", y = "Índice de Carga (ACWR)",
           title = "ACWR & RPE: Resumen del equipo de hoy") +
      theme_minimal(base_size = 14) +
      theme(legend.position="none",
            plot.title = element_text(hjust=0.5, face="bold", size=20),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank())
    
    ggplotly(p2, tooltip = "text", source = "acwr_rpe_scatter")
  })
  
  # 3) ACWR x REST (NEW)
  output$acwr_rest_scatter <- renderPlotly({
    req(selected())
    df <- rest_scatter_df |>
      dplyr::mutate(
        selected_flag = player == selected(),
        hover_text = paste0(
          "Jugadora: ", player,
          "<br>Fecha: ", latest_date,
          "<br>Score de Descanso: ", rest_score,
          "<br>Índice de Carga: ", round(ac_ratio, 2),
          "<br>Estatus de Descanso: ", rest_status,
          "<br>Estatus de Carga: ", load_status
        )
      )
    
    p3 <- ggplot(df, aes(x = rest_score, y = ac_ratio)) +
      geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
      geom_point(aes(fill = color_status_rest, text = hover_text, customdata = player),
                 size = 6, alpha = 0.3, color = "black") +
      geom_point(data = df |> dplyr::filter(selected_flag),
                 aes(fill = color_status_rest, text = hover_text, customdata = player),
                 size = 8, shape = 21, stroke = 2, color = "black") +
      scale_fill_manual(values = c("green"="#2ca02c","yellow"="#ffbf00","red"="#d62728")) +
      labs(x = "Score de Descanso", y = "Índice de Carga (ACWR)",
           title = "ACWR & Descanso: Resumen del equipo de hoy") +
      theme_minimal(base_size = 14) +
      theme(legend.position="none",
            plot.title = element_text(hjust=0.5, face="bold", size=20),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank())
    
    ggplotly(p3, tooltip = "text", source = "acwr_rest_scatter")
  })
  
  # 4) ACWR x PAIN 
  output$acwr_pain_scatter <- renderPlotly({
    req(selected())
    
    # scatter data must already include: pain_flag, zona_adolorida
    df <- pain_scatter_df |>
      dplyr::mutate(
        selected_flag = player == selected(),
        hover_text = paste0(
          "Jugadora: ", player,
          "<br>Fecha: ", latest_date,
          "<br>Score de Dolor Muscular: ", pain_score,
          "<br>Índice de Carga: ", round(ac_ratio, 2),
          "<br>Estatus de Dolor Muscular: ", pain_status,
          "<br>Estatus de Carga: ", load_status,
          ifelse(pain_flag, paste0("<br>Zona Adolorida: ", zona_adolorida), "")
        )
      )
    
    # one red ring per flagged player (avoid duplicates)
    rings_df <- df |>
      dplyr::filter(pain_flag) |>
      dplyr::distinct(player, .keep_all = TRUE)
    
    p4 <- ggplot(df, aes(x = pain_score, y = ac_ratio)) +
      geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
      # base points
      geom_point(
        aes(fill = color_status_pain, text = hover_text, customdata = player),
        shape = 21, size = 6, alpha = 0.3, color = "black", stroke = 0.7
      ) +
      # selected highlight
      geom_point(
        data = df |> dplyr::filter(selected_flag),
        aes(fill = color_status_pain, text = hover_text, customdata = player),
        shape = 21, size = 8, stroke = 2, color = "black"
      ) +
      # red rings for 3-day pain flag
      geom_point(
        data = rings_df,
        aes(x = pain_score, y = ac_ratio),
        inherit.aes = FALSE,
        shape = 21, size = 9, stroke = 1, fill = NA, color = "#d62728"
      ) +
      scale_fill_manual(values = c("green"="#2ca02c","yellow"="#ffbf00","red"="#d62728")) +
      labs(x = "Score de Dolor Muscular", y = "Índice de Carga (ACWR)",
           title = "ACWR & Dolor Muscular: Resumen del equipo de hoy") +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()
      )
    
    ggplotly(p4, tooltip = "text", source = "acwr_pain_scatter")
  })
  
  # =========================
  #   LOWER PANELS
  # =========================
  output$survey_plot <- renderPlotly({
    req(selected())
    ggplotly(plot_player_recuperacion(selected()), tooltip = "text")
  })
  
  output$hsr_plot <- renderPlotly({
    req(selected())
    ggplotly(plot_individual_hsr(selected()), tooltip = "text")
  })
  
  output$ac_plot <- renderPlotly({
    req(selected())
    ggplotly(plot_individual_ac(selected()), tooltip = "text")
  })
  
  # =========================
  #   PLAYER INFO
  # =========================
  output$player_info_box <- renderUI({
    req(selected())
    player_row <- player_info |> dplyr::filter(player == selected())
    if (nrow(player_row) == 0) return(NULL)
    tags$div(
      style = "text-align:center;",
      tags$img(src = file.path("player_images", player_row$image),
               width = "100%", style = "max-width:200px; border-radius:10px; margin-bottom:10px;"),
      tags$h4(player_row$player),
      tags$p(paste("Edad:", player_row$age)),
      tags$p(paste("Estatura:", player_row$height))
    )
  })
}

shinyApp(ui = ui, server = server)
