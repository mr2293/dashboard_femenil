library(tidyverse)
library(zoo)
library(reshape2)
library(gt)
library(ggrepel)
library(lubridate)
library(readxl)
library(dplyr)
library(zoo)

# Leer Cuestionario de Bienestar de Jugadoras ---------
survey_path <- "data/cuestionario_femenil.xlsx"

recuperacion_df <- read_excel(survey_path)

# Darle Puntuación a Recuperación de Jugadoras -------------

compute_recovery_score <- function(fatigue, sleep_quality, sleep_hours, soreness, pain_zone) {
  score <- 0
  
  # Fatigue scoring
  score <- score + case_when(
    fatigue == "Muy fresco" ~ 2.5,
    fatigue == "Fresco" ~ 2.2,
    fatigue == "Mejor que lo normal" ~ 1.8,
    fatigue == "Normal" ~ 1.5,
    fatigue == "Peor que lo normal" ~ 1.2,
    fatigue == "Cansado" ~ 0.8,
    fatigue == "Muy cansado" ~ 0.5,
    TRUE ~ 0
  )
  
  # Sleep quality
  score <- score + case_when(
    sleep_quality == "Muy buena noche" ~ 2.5,
    sleep_quality == "Buena noche" ~ 2.2,
    sleep_quality == "Mejor que normal" ~ 1.8,
    sleep_quality == "Normal" ~ 1.5,
    sleep_quality == "Peor que normal" ~ 1.2,
    sleep_quality == "Mala noche" ~ 0.8,
    sleep_quality == "Muy mala noche" ~ 0.5,
    sleep_quality == "Me desperté mucho" ~ 0.5,
    TRUE ~ 0
  )
  
  # Sleep duration
  score <- score + case_when(
    sleep_hours == "Más de 8" ~ 2.5,
    sleep_hours == "6 a 8" ~ 2,
    sleep_hours == "Menos de 6" ~ 1,
    TRUE ~ 0
  )
  
  # Pain
  score <- score + case_when(
    soreness == "Normal" ~ 2.5,
    soreness == "No me duele nada" ~ 2,
    soreness == "Adolorido de una zona" ~ 1.5,
    soreness == "Muy adolorido en general" ~ 1,
    
    TRUE ~ 0
  )
  
  return(round(score, 1))  # Max = 10
}

compute_rest_score <- function(fatigue, sleep_quality, sleep_hours) {
  score <- 0
  
  # Fatigue scoring
  score <- score + case_when(
    fatigue == "Muy fresco" ~ 5,
    fatigue == "Fresco" ~ 4.5,
    fatigue == "Mejor que lo normal" ~ 3.5,
    fatigue == "Normal" ~ 2.5,
    fatigue == "Peor que lo normal" ~ 2,
    fatigue == "Cansado" ~ 1.5,
    fatigue == "Muy cansado" ~ 1,
    TRUE ~ 0
  )
  
  # Sleep quality
  score <- score + case_when(
    sleep_quality == "Muy buena noche" ~ 2.5,
    sleep_quality == "Buena noche" ~ 2.2,
    sleep_quality == "Mejor que normal" ~ 1.8,
    sleep_quality == "Normal" ~ 1.5,
    sleep_quality == "Peor que normal" ~ 1.2,
    sleep_quality == "Mala noche" ~ 0.8,
    sleep_quality == "Muy mala noche" ~ 0.5,
    sleep_quality == "Me desperté mucho" ~ 0.5,
    TRUE ~ 0
  )
  
  # Sleep duration
  score <- score + case_when(
    sleep_hours == "Más de 8" ~ 2.5,
    sleep_hours == "6 a 8" ~ 2,
    sleep_hours == "Menos de 6" ~ 1,
    TRUE ~ 0
  )
  
  return(round(score, 1))  # Max = 10
}

compute_pain_score <- function(soreness) {
  score <- 0
  
  # Pain
  score <- score + case_when(
    soreness == "Normal" ~ 3,
    soreness == "No me duele nada" ~ 4,
    soreness == "Adolorido de una zona" ~ 7,
    soreness == "Muy adolorido en general" ~ 8,
    
    TRUE ~ 0
  )
  
  return(round(score, 1))  # Max = 10
}

recuperacion_df <- recuperacion_df|>
  mutate(recovery_score = compute_recovery_score(
    `Nivel de Cansancio hoy (Fatiga)`,
    `Qué tal descansaste ayer?`,
    `Cuántas horas dormiste ayer?`,
    `Estás adolorido de alguna parte?`,
    `Donde te encuentras adolorido? Indica cada zona de dolor`
  ))

# Gráficas Sueño, Fatiga, Dolor Muscular -----------

recuperacion_df <- recuperacion_df |>
  mutate(`Marca temporal` = as.Date(`Marca temporal`))

# Scoring definitions
niveles_fatiga <- c("Muy fresco" = 10, "Fresco" = 9, "Mejor que lo normal" = 8, 
                    "Normal" = 6, "Peor que lo normal" = 5, "Cansado" = 4, "Muy cansado" = 3)
niveles_sueño <- c("Muy buena noche" = 10, "Buena noche" = 9, "Mejor que normal" = 8,
                   "Normal" = 6, "Peor que normal" = 5, "Mala noche" = 4, 
                   "Muy mala noche" = 3, "Me desperté mucho" = 3)
niveles_dolor <- c("No me duele nada" = 10, "Normal" = 8,
                   "Adolorido de una zona" = 6, "Muy adolorido en general" = 4)

# Apply mappings
recuperacion_df <- recuperacion_df |> 
  mutate(
    fatiga_score = niveles_fatiga[`Nivel de Cansancio hoy (Fatiga)`],
    sueño_score = niveles_sueño[`Qué tal descansaste ayer?`],
    dolor_score = niveles_dolor[`Estás adolorido de alguna parte?`]
  )

# Function to plot by player
plot_player_recuperacion <- function(player_name) {
  
  player_df <- recuperacion_df |> 
    filter(Nombre == player_name)
  
  max_day <- max(player_df$`Marca temporal`, na.rm = TRUE)
  player_df <- player_df |> filter(`Marca temporal` >= max_day - 20)
  
  player_long <- player_df |> 
    select(`Marca temporal`, Nombre,
           `Nivel de Cansancio hoy (Fatiga)`, 
           `Qué tal descansaste ayer?`,
           `Estás adolorido de alguna parte?`,
           `Donde te encuentras adolorido? Indica cada zona de dolor`,
           fatiga_score, sueño_score, dolor_score) |> 
    rename(`Zona Adolorida` = `Donde te encuentras adolorido? Indica cada zona de dolor`) |>
    pivot_longer(
      cols = c(fatiga_score, sueño_score, dolor_score),
      names_to = "score_type", values_to = "score"
    ) |> 
    mutate(
      type = case_when(
        score_type == "fatiga_score" ~ "Fatiga",
        score_type == "sueño_score" ~ "Sueño",
        score_type == "dolor_score" ~ "Dolor Muscular"
      ),
      respuesta = case_when(
        score_type == "fatiga_score" ~ `Nivel de Cansancio hoy (Fatiga)`,
        score_type == "sueño_score" ~ `Qué tal descansaste ayer?`,
        score_type == "dolor_score" ~ `Estás adolorido de alguna parte?`
      ),
      color = case_when(
        type == "Dolor Muscular" & score >= 7 ~ "Alta",
        type == "Dolor Muscular" & score < 7 ~ "Baja",
        type != "Dolor Muscular" & score >= 6 ~ "Alta",
        type != "Dolor Muscular" & score < 6 ~ "Baja",
        TRUE ~ "Indefinido"
      )    
    )
  
  ggplot(player_long, aes(x = `Marca temporal`, y = score, group = type)) +
    geom_line(aes(color = type), linewidth = 0.6, alpha = 0.5) +
    geom_point(aes(
      fill = color,
      text = paste0(
        "Jugador: ", Nombre,
        "<br>Fecha: ", `Marca temporal`,
        "<br>Respuesta: ", respuesta,
        ifelse(type == "Dolor Muscular" & !is.na(`Zona Adolorida`),
               paste0("<br>Zona Adolorida: ", `Zona Adolorida`), ""),
        "<br>Score: ", score
      )
    ),
    shape = 21, size = 5, color = "white") +
    facet_wrap(~type, ncol = 1, scales = "free_y") +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
    scale_fill_manual(values = c("Alta" = "#1a9850", "Baja" = "#d7191c")) +
    scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
    labs(
      title = paste("Indicadores de Recuperación Diaria –", player_name),
      x = NULL, y = "Puntuación", subtitle = "Score Diario"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_line(color = "gray85"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.text = element_text(face = "bold", size = 14),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}

# Example usage:
# plot_player_recuperacion("Álvaro Fidalgo")

## Plot A:C 7:21 Individual -----------

micros_shiny_comb_fem <- read_csv("micros/micros_shiny_comb_fem.csv")

micros_shiny_comb_fem <- micros_shiny_comb_fem |>
  mutate(player = case_when(
    player == "Itzel Velasco" ~ "Itzel Velasco",
    player == "Sandra Paños" ~ "Sandra Paños",
    player == "Irene Guerrero" ~ "Irene Guerrero",
    player == "Montse Saldivar" ~ "Montse Saldívar",
    player == "Kimberly Rodriguez" ~ "Kimberly Rodríguez",
    player == "Julie Prueba" ~ "Julie Prueba",
    player == "Scarlett Camberos" ~ "Scarlett Camberos",
    player == "Alexa Soto Ramirez" ~ "Alexa Soto Ramírez",
    player == "Kiana Palacios" ~ "Kiana Palacios",
    player == "Sofia  Ramos" ~ "Sofía Ramos",
    player == "Nancy Antonio" ~ "Nancy Antonio",
    player == "Karina Rodriguez" ~ "Karina Rodríguez",
    player == "Barbara Del Real Gomez" ~ "Bárbara Del Real Gómez",
    player == "Camila Jocelyn Lara Contreras" ~ "Camila Lara",
    player == "Jacqueline Tapia Martínez" ~ "Jacqueline Tapia",
    player == "Jana  Gutierrez" ~ "Jana Gutiérrez",
    player == "Vanessa Lyliana Paredes Vences" ~ "Vanessa Paredes",
    player == "Chidimna Okeke" ~ "Chidinma Okeke",
    TRUE ~ player
  ))

selected_players <- c(
  "Itzel Velasco", "Sandra Paños", "Irene Guerrero", "Xcaret Pineda", 
  "Montse Saldívar", "Kimberly Rodríguez", "Julie Prueba", "Scarlett Camberos",
  "Alexa Soto Ramírez", "Kiana Palacios", "Sofía Ramos", "Nancy Antonio", "Karina Rodríguez", 
  "Ana Paula Pedrero", "Aranza Segura", "Bárbara Del Real Gómez", 
  "Camila Lara", "Chidinma Okeke", "Jacqueline Tapia",
  "Jana Gutiérrez", "Valentina Murrieta", "Vanessa Paredes", "Karen Luna", "Sarah Luebbert", 
  "Aylin Aviléz", "Daniela Espinosa", "Alondra Cabanillas", "Isa Haas"
  # "Bruna Vilamala",
)

micros_individual <- micros_shiny_comb_fem |>
  filter(player %in% selected_players)

# Parámetros de suavizado exponencial
alpha_acute <- 0.75
alpha_chronic <- 0.35

# Función para calcular media ponderada exponencial
ewma <- function(x, alpha) {
  n <- length(x)
  if (n == 0) return(NA)
  weights <- (1 - alpha)^((n - 1):0)
  weights <- weights / sum(weights)
  sum(x * weights)
}

# Función para crear aguda, crónica y ACWR
process_ewma <- function(data, col) {
  data |>
    mutate(
      !!paste0("acute_load_", col) := zoo::rollapply(
        .data[[col]],
        width = 7,
        FUN = \(x) ewma(x, alpha_acute),
        align = "right",
        fill = NA,
        partial = TRUE
      ),
      !!paste0("chronic_load_", col) := zoo::rollapply(
        .data[[paste0("acute_load_", col)]],
        width = 21,
        FUN = \(x) ewma(x, alpha_chronic),
        align = "right",
        fill = NA,
        partial = TRUE
      ),
      !!paste0("ACWR_", col) := .data[[paste0("acute_load_", col)]] / .data[[paste0("chronic_load_", col)]]
    )
}

micros_individual <- micros_individual |>
  group_by(player, date) |>
  summarize(load = sum(HSR_abs_dist, na.rm = TRUE), .groups = "drop") |>
  arrange(player, date)

micros_individual <- micros_individual |>
  group_by(player) |>
  arrange(date) |>
  group_modify(~ process_ewma(.x, "load")) |>
  ungroup()

micros_individual <- micros_individual |>
  rename(
    carga_aguda = acute_load_load,
    carga_cronica = chronic_load_load,
    ac_ratio = ACWR_load
  )

plot_individual_ac <- function(player) {
  
  # limite_inferior <- 0.8
  # limite_superior <- 1.5
  scaling_factor <- 85
  
  ac_breaks <- seq(0.5, 2.5, by = 0.5)
  
  micros_individual <- micros_individual |>
    mutate(date = as.Date(date))
  
  # Get last 21 calendar days for selected player
  max_day <- max(micros_individual$date, na.rm = TRUE)
  filtered_data <- micros_individual |>
    filter(player == !!player & date >= max_day - 20)
  
  ggplot(filtered_data, aes(x = date)) +
    # Acute Load
    geom_line(aes(y = carga_aguda, color = "Carga Aguda"), linewidth = 2) +
    geom_point(aes(y = carga_aguda, color = "Carga Aguda",
                   text = paste0("Jugador: ", player,
                                 "<br>Fecha: ", date,
                                 "<br>Carga Aguda: ", round(carga_aguda, 1))),
               size = 3.5) +
    
    # Chronic Load
    geom_line(aes(y = carga_cronica, color = "Carga Crónica"), linewidth = 2) +
    geom_point(aes(y = carga_cronica, color = "Carga Crónica",
                   text = paste0("Jugador: ", player,
                                 "<br>Fecha: ", date,
                                 "<br>Carga Crónica: ", round(carga_cronica, 1))),
               size = 3.5) +
    
    # A:C Ratio (rescaled)
    geom_line(aes(y = ac_ratio * scaling_factor, color = "Relación A:C"), linewidth = 2, linetype = "dashed") + 
    geom_point(aes(y = ac_ratio * scaling_factor, color = "Relación A:C",
                   text = paste0("Jugador: ", player,
                                 "<br>Fecha: ", date,
                                 "<br>Relación A:C: ", round(ac_ratio, 2))),
               size = 3.5) +
    
    # Y-axis with secondary A:C axis
    scale_y_continuous(
      limits = c(0, max(c(filtered_data$carga_aguda,
                          filtered_data$carga_cronica,
                          filtered_data$ac_ratio * scaling_factor), na.rm = TRUE) * 1.2),
      name = NULL,
      sec.axis = sec_axis(~./scaling_factor, name = "A:C Ratio", breaks = seq(0, 3, 0.5))
    ) +
    
    # More ticks on X axis
    scale_x_date(date_breaks = "1 day", date_labels = "%d-%b") +
    
    # Labels and theme
    labs(
      title = paste("Relación A:C 7:21 –", player),
      x = NULL,
      y = "Nivel de Carga",
      color = NULL
    ) +
    geom_text(
      data = tibble(
        x = max(filtered_data$date) + 0.2,
        y = ac_breaks * scaling_factor,
        label = as.character(ac_breaks)
      ),
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 0, size = 5.8, fontface = "bold", color = "black"
    ) +
    # annotate("text",
    #          x = max(filtered_data$date) + 0.35,
    #          y = max(ac_breaks) * scaling_factor * 1.15,
    #          label = "Relación A:C",
    #          angle = 90,
    #          fontface = "bold",
    #          size = 5,
    #          color = "black",
    #          hjust = 0.5) +
    # coord_cartesian(clip = "off") +
    scale_color_manual(values = c(
      "Carga Aguda" = "#0072B2",
      "Carga Crónica" = "#4D4D4D",
      "Relación A:C" = "goldenrod"
    )) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
      axis.text.y = element_text(size = 16), 
      axis.title.y = element_text(size = 16, margin = margin(r = 10)),
      plot.title = element_text(hjust = 0.5, size = 22, face = "bold",
                                margin = margin(t = 35, b = 10)), 
      plot.subtitle = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.margin = margin(t = 25, r = 20, b = 20, l = 20), 
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 13)
    )
  
}

# plot_individual_ac("Álvaro Fidalgo")

# Plot Individual HSR -------

micros_hsr <- micros_shiny_comb_fem |>
  filter(player %in% selected_players)

micros_hsr <- micros_hsr |>
  mutate(date = as.Date(date)) |>
  group_by(player, date) |>
  summarize(HSR_abs_dist = sum(HSR_abs_dist, na.rm = TRUE), .groups = "drop") |>
  arrange(player, date) |>
  group_by(player) |>
  group_modify(~ process_ewma(.x, "HSR_abs_dist")) |>
  ungroup() |>
  rename(
    acute_HSR = acute_load_HSR_abs_dist,
    chronic_HSR = chronic_load_HSR_abs_dist
  )

plot_individual_hsr <- function(player) {
  max_day <- max(micros_hsr$date, na.rm = TRUE)
  
  filtered_data <- micros_hsr |>
    filter(player == !!player & date >= max_day - 20)
  
  # Safeguard against empty data
  if (nrow(filtered_data) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste("No hay datos recientes para", player),
                 size = 6, color = "red") +
        theme_void()
    )
  }
  
  ggplot(filtered_data, aes(x = date)) +
    geom_line(aes(y = acute_HSR, color = "HSR Agudo"), linewidth = 2) +
    geom_point(aes(y = acute_HSR, color = "HSR Agudo",
                   text = paste0("Jugador: ", player,
                                 "<br>Fecha: ", date,
                                 "<br>HSR Agudo: ", round(acute_HSR, 1))),
               size = 3.5) +
    
    geom_line(aes(y = chronic_HSR, color = "HSR Crónico"), linewidth = 2) +
    geom_point(aes(y = chronic_HSR, color = "HSR Crónico",
                   text = paste0("Jugador: ", player,
                                 "<br>Fecha: ", date,
                                 "<br>HSR Crónico: ", round(chronic_HSR, 1))),
               size = 3.5) +
    
    scale_x_date(date_breaks = "1 day", date_labels = "%d-%b") +
    labs(
      title = paste("HSR (>21 km/h) Agudo vs. Crónico –", player),
      x = NULL, y = "HSR en metros", color = NULL,
      subtitle = "Valores de distancia de HSR en metros"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 16, margin = margin(r = 10)),
      plot.title = element_text(hjust = 0.5, size = 22, face = "bold", margin = margin(t = 35, b = 10)),
      plot.subtitle = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.margin = margin(t = 25, r = 20, b = 20, l = 20),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 13)
    ) +
    scale_color_manual(values = c(
      "HSR Agudo" = "#0072B2",
      "HSR Crónico" = "#4D4D4D"
    ))
}

# plot_individual_hsr("Álvaro Fidalgo")

# Scatter de ACWR & Recuperación -------

recuperacion_df <- recuperacion_df |>
  mutate(date = as.Date(`Marca temporal`))

micros_individual <- micros_individual |>
  mutate(date = as.Date(date))

# === 3-day pain logic (one latest row per jugadora) ===
latest_pain2 <- recuperacion_df |>
  arrange(Nombre, date) |>
  group_by(Nombre) |>
  mutate(
    zona_adolorida = `Donde te encuentras adolorido? Indica cada zona de dolor`,
    pain_day = !is.na(zona_adolorida) & zona_adolorida != "Nada",
    three_day_pain = pain_day &
      dplyr::lag(pain_day, 1, default = FALSE) &
      dplyr::lag(pain_day, 2, default = FALSE)
  ) |>
  dplyr::slice_max(order_by = date, n = 1, with_ties = FALSE) |>
  dplyr::transmute(
    player = Nombre,
    zona_adolorida,
    pain_flag = three_day_pain
  )

# Último ACWR por jugador(a)
latest_acwr <- micros_individual |>
  group_by(player) |>
  filter(date == max(date, na.rm = TRUE)) |>
  select(player, ac_ratio)

# Último recovery score por jugador(a)
latest_recovery <- recuperacion_df |>
  group_by(Nombre) |>
  filter(date == max(date, na.rm = TRUE)) |>
  select(player = Nombre, recovery_score)

latest_dates <- recuperacion_df |>
  group_by(Nombre) |>
  summarize(latest_date = max(date, na.rm = TRUE), .groups = "drop") |>
  rename(player = Nombre)

# Combinar datasets + traer pain_flag de 3 días
scatter_df <- latest_acwr |>
  inner_join(latest_recovery, by = "player") |>
  inner_join(latest_dates,   by = "player") |>
  inner_join(latest_pain2 |> dplyr::rename(three_day_pain = pain_flag), by = "player") |>
  mutate(
    recovery_status = if_else(recovery_score >= 6, "Recuperada", "Fatigada"),
    load_status = case_when(
      ac_ratio < 0.8 ~ "Carga Baja",
      ac_ratio > 1.3 ~ "Carga Alta",
      TRUE ~ "Carga Óptima"
    ),
    color_status = case_when(
      ac_ratio >= 0.8 & ac_ratio <= 1.3 & recovery_score >= 6 ~ "green",
      (ac_ratio >= 0.8 & ac_ratio <= 1.3 & recovery_score < 6) |
        (recovery_score >= 6 & (ac_ratio < 0.8 | ac_ratio > 1.3)) ~ "yellow",
      TRUE ~ "red"
    ),
    pain_flag = three_day_pain
  ) |>
  select(-three_day_pain) |>
  # si tienes pain_score, usa el mismo filtro que el otro plot:
  filter(!is.na(ac_ratio)) |>
  # filter(!is.na(pain_score)) |>
  distinct(player, .keep_all = TRUE)

# capa deduplicada para los anillos rojos (1 por jugadora)
rings_df <- scatter_df |>
  dplyr::filter(pain_flag) |>
  dplyr::distinct(player, .keep_all = TRUE)

acwr_scatter_plot <- ggplot(
  scatter_df,
  aes(
    x = recovery_score,
    y = ac_ratio,
    fill = color_status,
    text = paste0(
      "Jugadora: ", player,
      "<br>Fecha: ", latest_date,
      "<br>Score de Recuperación: ", recovery_score,
      "<br>Índice de Carga: ", round(ac_ratio, 2),
      "<br>Estatus de Recuperación: ", recovery_status,
      "<br>Estatus de Carga: ", load_status,
      ifelse(pain_flag, paste0("<br>Zona Adolorida: ", zona_adolorida), "")
    ),
    customdata = player
  )
) +
  geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
  geom_point(shape = 21, size = 6, alpha = 0.9, color = "black") +
  # anillo rojo (usando rings_df para evitar duplicados visuales)
  geom_point(
    data = rings_df,
    aes(x = recovery_score, y = ac_ratio),
    inherit.aes = FALSE,
    shape = 21, size = 9, stroke = 1.8, fill = NA, color = "#d62728"
  ) +
  scale_fill_manual(
    values = c("green" = "#2ca02c", "yellow" = "#ffbf00", "red" = "#d62728")
  ) +
  labs(
    x = "Score de Recuperación",
    y = "Índice de Carga (ACWR)",
    title = "ACWR & Recuperación: Resumen del equipo de hoy"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  )

# ggplotly(acwr_scatter_plot, tooltip = "text")


# Scatter de ACWR & RPE -------


# 1) Cargar RPE desde el XLSX
# ---------------------------

path_rpe <- "data/rpe_femenil.xlsx"

rpe_raw <- read_excel(path_rpe)

# Asegurar tipos y nombres clave
rpe_df <- rpe_raw |>
  # Ajusta aquí si tus encabezados tienen pequeñas variaciones
  rename(
    fecha_rpe = `Marca temporal`,
    player = `Nombre del Jugador`,
    rpe_val = `RPE de la sesión`
  ) |>
  mutate(
    date = as.Date(fecha_rpe),
    # Forzar RPE a numérico por si viene como texto ("5 duro", etc.)
    rpe = suppressWarnings(as.numeric(rpe_val))
  ) 
# |>
#   # Si hay respuestas con rpe no numérico, descártalas
#   filter(!is.na(rpe))

rpe_df <- rpe_df |>
  mutate(
    rpe_val = case_when(
      rpe_val == "1 muy muy fácil" ~ 1,
      rpe_val == "2 fácil" ~ 2,
      rpe_val == "3 moderado" ~ 3,
      rpe_val == "4 algo duro" ~ 4,
      rpe_val == "5 duro" ~ 5,
      rpe_val == "6 más que duro" ~ 6,
      rpe_val == "7 muy duro" ~ 7,
      rpe_val == "8 muy muy duro" ~ 8,
      rpe_val == "9 casi lo máximo" ~ 9,
    )
  )

# ---------------------------
# 2) Asegurar fechas en tus dfs existentes
# ---------------------------
recuperacion_df <- recuperacion_df |>
  mutate(date = as.Date(`Marca temporal`))

micros_individual <- micros_individual |>
  mutate(date = as.Date(date))

# ---------------------------
# 3) Último ACWR por jugadora
# ---------------------------
latest_acwr_2 <- micros_individual |>
  group_by(player) |>
  filter(date == max(date, na.rm = TRUE)) |>
  ungroup() |>
  select(player, ac_ratio, acwr_date = date)

# ---------------------------
# 4) Último RPE por jugadora
# ---------------------------
latest_rpe <- rpe_df |>
  group_by(player) |>
  filter(date == max(date, na.rm = TRUE)) |>
  ungroup() |>
  select(player, rpe_val, latest_date = date)

# ---------------------------
# 5) Combinar y clasificar estatus/color
# ---------------------------
scatter_df_rpe <- latest_acwr_2 |>
  inner_join(latest_rpe, by = "player") |>
  mutate(
    # Etiquetas cualitativas de RPE (opcional; para el eje X y/o tooltip)
    rpe_label = dplyr::case_when(
      rpe_val == 1 ~ "1 muy muy fácil",
      rpe_val == 2 ~ "2 fácil",
      rpe_val == 3 ~ "3 moderado",
      rpe_val == 4 ~ "4 algo duro",
      rpe_val == 5 ~ "5 duro",
      rpe_val == 6 ~ "6 más que duro",
      rpe_val == 7 ~ "7 muy duro",
      rpe_val == 8 ~ "8 muy muy duro",
      rpe_val == 9 ~ "9 casi lo máximo",
      TRUE ~ as.character(rpe_val)
    ),
    load_status = case_when(
      ac_ratio < 0.8 ~ "Carga Baja",
      ac_ratio > 1.3 ~ "Carga Alta",
      TRUE ~ "Carga Óptima"
    ),
    # LÓGICA DE COLORES EXACTA
    color_status = case_when(
      ac_ratio > 0.8 & ac_ratio < 1.3 & rpe_val <= 4 ~ "green",
      rpe_val >= 7 & (ac_ratio < 0.8 | ac_ratio > 1.3) ~ "red",
      TRUE ~ "yellow"
    )
  )

# ---------------------------
# 6) Gráfico (X = RPE_VAL, Y = ACWR)
# ---------------------------
acwr_rpe_scatter <- ggplot(
  scatter_df_rpe,
  aes(
    x = rpe_val,
    y = ac_ratio,
    fill = color_status,
    text = paste0(
      "Jugadora: ", player,
      "<br>Fecha (RPE): ", latest_date,
      "<br>RPE: ", rpe_label,
      "<br>Índice de Carga (ACWR): ", round(ac_ratio, 2),
      "<br>Estatus de Carga: ", load_status
    ),
    customdata = player
  )
) +
  geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
  geom_point(size = 6, alpha = 0.9, color = "black", shape = 21) +
  scale_x_continuous(
    breaks = 1:9,
    labels = c(
      "1", "2", "3", "4", "5", "6", "7", "8", "9"
    )
  ) +
  scale_fill_manual(
    values = c(
      "green" = "#2ca02c",
      "yellow" = "#ffbf00",
      "red" = "#d62728"
    )
  ) +
  labs(
    x = "RPE de la sesión (1–10)",
    y = "Índice de Carga (ACWR)",
    title = "ACWR & RPE: Resumen del equipo (última respuesta por jugadora)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  )

# ACWR x Rest Score Plot

recuperacion_df <- recuperacion_df |>
  mutate(
    date = as.Date(`Marca temporal`),
    rest_score = compute_rest_score(
      `Nivel de Cansancio hoy (Fatiga)`,
      `Qué tal descansaste ayer?`,
      `Cuántas horas dormiste ayer?`
    ),
    pain_score = compute_pain_score(
      `Estás adolorido de alguna parte?`
    )
  )

micros_individual <- micros_individual |>
  mutate(date = as.Date(date))

# --- Latest ACWR per player (unchanged) ---
latest_acwr <- micros_individual |>
  group_by(player) |>
  filter(date == max(date, na.rm = TRUE)) |>
  select(player, ac_ratio)

# --- Latest dates per player from wellness survey (unchanged) ---
latest_dates <- recuperacion_df |>
  group_by(Nombre) |>
  summarize(latest_date = max(date, na.rm = TRUE), .groups = "drop") |>
  rename(player = Nombre)

# =========================
#   1) ACWR x REST SCORE
# =========================
# Latest rest score per player
latest_rest <- recuperacion_df |>
  group_by(Nombre) |>
  filter(date == max(date, na.rm = TRUE)) |>
  select(player = Nombre, rest_score)

rest_scatter_df <- latest_acwr |>
  inner_join(latest_rest,  by = "player") |>
  inner_join(latest_dates, by = "player") |>
  mutate(
    rest_status = if_else(rest_score >= 6, "Descansada", "No descansada"),
    load_status = case_when(
      ac_ratio < 0.8 ~ "Carga Baja",
      ac_ratio > 1.3 ~ "Carga Alta",
      TRUE ~ "Carga Óptima"
    ),
    color_status_rest = case_when(
      ac_ratio >= 0.8 & ac_ratio <= 1.3 & rest_score >= 6 ~ "green",
      (ac_ratio >= 0.8 & ac_ratio <= 1.3 & rest_score < 6) |
        (rest_score >= 6 & (ac_ratio < 0.8 | ac_ratio > 1.3)) ~ "yellow",
      TRUE ~ "red"
    )
  ) |>
  filter(!is.na(rest_score), !is.na(ac_ratio))

acwr_rest_scatter_plot <- ggplot(
  rest_scatter_df,
  aes(
    x = rest_score,
    y = ac_ratio,
    fill = color_status_rest,
    text = paste0(
      "Jugadora: ", player,
      "<br>Fecha: ", latest_date,
      "<br>Score de Descanso: ", rest_score,
      "<br>Índice de Carga: ", round(ac_ratio, 2),
      "<br>Estatus de Descanso: ", rest_status,
      "<br>Estatus de Carga: ", load_status
    ),
    customdata = player
  )
) +
  geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
  geom_point(size = 6, alpha = 0.9, color = "black") +
  scale_fill_manual(
    values = c("green" = "#2ca02c", "yellow" = "#ffbf00", "red" = "#d62728")
  ) +
  labs(
    x = "Score de Descanso",
    y = "Índice de Carga (ACWR)",
    title = "ACWR & Descanso: Resumen del equipo de hoy"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  )

# =========================
#   2) ACWR x PAIN SCORE
# =========================

# 3-day pain logic -> one latest row per player
latest_pain <- recuperacion_df |>
  arrange(Nombre, date) |>
  group_by(Nombre) |>
  mutate(
    zona_adolorida = `Donde te encuentras adolorido? Indica cada zona de dolor`,
    pain_day = !is.na(zona_adolorida) & zona_adolorida != "Nada",
    three_day_pain = pain_day &
      dplyr::lag(pain_day, 1, default = FALSE) &
      dplyr::lag(pain_day, 2, default = FALSE)
  ) |>
  slice_max(order_by = date, n = 1, with_ties = FALSE) |>
  transmute(
    player = Nombre,
    zona_adolorida,
    pain_flag = three_day_pain,
    pain_score = pain_score
  )

pain_scatter_df <- latest_acwr |>
  inner_join(latest_pain |> dplyr::rename(three_day_pain = pain_flag), by = "player") |>
  inner_join(latest_dates, by = "player") |>
  mutate(
    pain_status = if_else(pain_score < 6, "Sin dolor", "Con dolor"),
    load_status = case_when(
      ac_ratio < 0.8 ~ "Carga Baja",
      ac_ratio > 1.3 ~ "Carga Alta",
      TRUE ~ "Carga Óptima"
    ),
    color_status_pain = case_when(
      ac_ratio >= 0.8 & ac_ratio <= 1.3 & pain_score < 6 ~ "green",
      (ac_ratio >= 0.8 & ac_ratio <= 1.3 & pain_score >= 6) |
        (pain_score < 6 & (ac_ratio < 0.8 | ac_ratio > 1.3)) ~ "yellow",
      TRUE ~ "red"
    ),
    pain_flag = three_day_pain
  ) |>
  select(-three_day_pain) |>
  filter(!is.na(pain_score), !is.na(ac_ratio)) |>
  dplyr::distinct(player, .keep_all = TRUE)

# one red ring per flagged player (bullet-proof against dupes)
rings_df2 <- pain_scatter_df |>
  dplyr::filter(pain_flag) |>
  dplyr::distinct(player, .keep_all = TRUE)

acwr_pain_scatter_plot <- ggplot(
  pain_scatter_df,
  aes(
    x = pain_score,
    y = ac_ratio,
    fill = color_status_pain,
    text = paste0(
      "Jugadora: ", player,
      "<br>Fecha: ", latest_date,
      "<br>Score de Dolor Muscular: ", pain_score,
      "<br>Índice de Carga: ", round(ac_ratio, 2),
      "<br>Estatus de Dolor Muscular: ", pain_status,
      "<br>Estatus de Carga: ", load_status,
      ifelse(pain_flag, paste0("<br>Zona Adolorida: ", zona_adolorida), "")
    ),
    customdata = player
  )
) +
  geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
  geom_point(shape = 21, size = 6, alpha = 0.9, color = "black") +
  # red rings for 3-day pain
  geom_point(
    data = rings_df2,
    aes(x = pain_score, y = ac_ratio),
    inherit.aes = FALSE,
    shape = 21, size = 9, stroke = 1.8, fill = NA, color = "#d62728"
  ) +
  scale_fill_manual(values = c("green" = "#2ca02c", "yellow" = "#ffbf00", "red" = "#d62728")) +
  labs(
    x = "Score de Dolor Muscular",
    y = "Índice de Carga (ACWR)",
    title = "ACWR & Dolor Muscular: Resumen del equipo de hoy"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  )