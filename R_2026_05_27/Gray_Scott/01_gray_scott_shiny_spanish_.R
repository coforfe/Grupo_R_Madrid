# =============================================================================
# Gray-Scott Reaction-Diffusion System - Shiny App
# =============================================================================
# 
# DESCRIPCIÓN:
#   Simulación interactiva del modelo de Gray-Scott, un sistema de dos ecuaciones
#   diferenciales parciales (PDEs) que genera patrones de Turing: manchas, 
#   laberintos, espirales, etc.
#
# ECUACIONES:
#   du/dt = Du * laplacian(u) - u*v^2 + f*(1 - u)
#   dv/dt = Dv * laplacian(v) + u*v^2 - (f + k)*v
#
#   Donde:
#     u, v   = concentraciones de las dos sustancias químicas
#     Du, Dv = coeficientes de difusión
#     f      = feed rate  (tasa de alimentación de u)
#     k      = kill rate  (tasa de eliminación de v)
#
# REQUISITOS:
#   install.packages(c("shiny", "ggplot2", "reshape2", "viridis"))
#
# AUTOR: Generado con Claude (Anthropic) para exploración educativa
# =============================================================================

library(shiny)
library(ggplot2)
library(reshape2)
library(viridis)

# =============================================================================
# FUNCIÓN PRINCIPAL: Un paso de simulación Gray-Scott
# =============================================================================
# Aplica las PDEs usando diferencias finitas con condiciones de contorno
# periódicas (el grid se "envuelve" como un toro).

gray_scott_step <- function(u, v, Du, Dv, f, k, dt = 1.0) {
  
  n <- nrow(u)
  m <- ncol(u)
  
  # --- Laplaciano discreto (vecinos con condiciones periódicas) ---
  # Para cada celda: suma de 4 vecinos - 4 * celda_central
  laplacian <- function(mat) {
    # Índices con wrap-around (condiciones periódicas)
    up    <- rbind(mat[n, ], mat[-n, ])
    down  <- rbind(mat[-1, ], mat[1, ])
    left  <- cbind(mat[, m], mat[, -m])
    right <- cbind(mat[, -1], mat[, 1])
    
    up + down + left + right - 4 * mat
  }
  
  Lu <- laplacian(u)
  Lv <- laplacian(v)
  
  # --- Término de reacción autocatalítica ---
  uvv <- u * v * v   # u + 2v → 3v (autocatálisis)
  
  # --- Actualización de concentraciones (método de Euler explícito) ---
  u_new <- u + dt * (Du * Lu - uvv + f * (1 - u))
  v_new <- v + dt * (Dv * Lv + uvv - (f + k) * v)
  
  # Clip para mantener valores físicos [0, 1] sin perder la forma matricial
  u_new[] <- pmax(0, pmin(1, u_new))
  v_new[] <- pmax(0, pmin(1, v_new))
  
  list(u = u_new, v = v_new)
}

# =============================================================================
# FUNCIÓN: Inicializar el grid
# =============================================================================
# Empieza con u ≈ 1 (sustancia A llena el espacio) y v ≈ 0 (sustancia B ausente),
# excepto en una pequeña región central con perturbación aleatoria.

init_grid <- function(n = 100, seed = 42) {
  set.seed(seed)
  
  u <- matrix(1, nrow = n, ncol = n)
  v <- matrix(0, nrow = n, ncol = n)
  
  # Semilla central: zona de activación
  r <- round(n * 0.1)  # tamaño de la zona inicial (~10% del grid)
  cx <- round(n / 2)
  cy <- round(n / 2)
  
  rows <- max(1, cx - r):min(n, cx + r)
  cols <- max(1, cy - r):min(n, cy + r)
  
  u[rows, cols] <- 0.5 + runif(length(rows) * length(cols), -0.1, 0.1)
  v[rows, cols] <- 0.25 + runif(length(rows) * length(cols), -0.1, 0.1)
  
  list(u = u, v = v)
}

# =============================================================================
# PALETA PERSONALIZADA: Replica los colores de la imagen original
# Púrpura oscuro → marrón → naranja → amarillo brillante
# =============================================================================
grayscott_colors <- c(
  "#1a0030",   # púrpura muy oscuro (fondo profundo)
  "#3d0060",   # púrpura medio
  "#6b0080",   # púrpura-violeta
  "#8b1a6b",   # púrpura-rojizo
  "#a03020",   # marrón-rojizo
  "#c05010",   # naranja oscuro
  "#e07010",   # naranja
  "#f0a020",   # naranja-ámbar
  "#f5c040",   # ámbar
  "#fde060",   # amarillo-ámbar
  "#fff5a0"    # amarillo muy claro (centros brillantes)
)

grayscott_palette <- colorRampPalette(grayscott_colors)

# =============================================================================
# PRESETS: Parámetros conocidos que generan patrones distintos
# =============================================================================
presets <- list(
  "🌀 Laberintos"     = list(f = 0.022, k = 0.051, Du = 0.16, Dv = 0.08),
  "🔵 Manchas"        = list(f = 0.035, k = 0.065, Du = 0.16, Dv = 0.08),
  "⚙️  Engranajes"    = list(f = 0.025, k = 0.060, Du = 0.16, Dv = 0.08),
  "🌊 Ondas"          = list(f = 0.014, k = 0.054, Du = 0.16, Dv = 0.08),
  "💥 Caos"           = list(f = 0.026, k = 0.051, Du = 0.16, Dv = 0.08),
  "🐾 Piel de leopardo" = list(f = 0.037, k = 0.060, Du = 0.16, Dv = 0.08)
)

# =============================================================================
# UI
# =============================================================================
ui <- fluidPage(
  
  titlePanel("🧬 Gray-Scott Reaction-Diffusion"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # --- Presets ---
      h4("Presets de patrones"),
      selectInput("preset", "Selecciona un patrón:",
                  choices = names(presets)),
      
      hr(),
      
      # --- Parámetros de reacción ---
      h4("Parámetros de reacción"),
      
      sliderInput("f", 
                  label = "f — Feed rate (alimentación de u)",
                  min = 0.01, max = 0.08, value = 0.022, step = 0.001),
      
      sliderInput("k",
                  label = "k — Kill rate (eliminación de v)",
                  min = 0.04, max = 0.07, value = 0.051, step = 0.001),
      
      hr(),
      
      # --- Parámetros de difusión ---
      h4("Coeficientes de difusión"),
      
      sliderInput("Du",
                  label = "Du — Difusión de u (activador)",
                  min = 0.05, max = 0.25, value = 0.16, step = 0.01),
      
      sliderInput("Dv",
                  label = "Dv — Difusión de v (inhibidor)",
                  min = 0.02, max = 0.15, value = 0.08, step = 0.01),
      
      hr(),
      
      # --- Simulación ---
      h4("Control de simulación"),
      
      sliderInput("steps",
                  label = "Pasos por actualización",
                  min = 10, max = 200, value = 50, step = 10),
      
      sliderInput("grid_size",
                  label = "Tamaño del grid (n×n)",
                  min = 50, max = 200, value = 100, step = 10),
      
      selectInput("palette", "Paleta de colores:",
                  choices = c("🎨 Gray-Scott Original" = "grayscott",
                              "magma", "inferno", "plasma", "viridis"),
                  selected = "grayscott"),
      
      hr(),
      
      actionButton("reset",    "🔄 Reset",   class = "btn-warning", width = "31%"),
      actionButton("step_btn", "⏭ Paso",     class = "btn-info",    width = "31%"),
      actionButton("run_btn",  "▶ Simular",  class = "btn-success", width = "31%"),
      
      br(), br(),
      
      # --- Información de estado ---
      verbatimTextOutput("status")
    ),
    
    mainPanel(
      width = 9,
      
      # Visualización principal
      plotOutput("pattern_plot", height = "550px"),
      
      br(),
      
      # Panel explicativo
      fluidRow(
        column(6,
          wellPanel(
            h4("📐 Las ecuaciones"),
            withMathJax(
              helpText("$$\\frac{\\partial u}{\\partial t} = D_u \\nabla^2 u - uv^2 + f(1-u)$$"),
              helpText("$$\\frac{\\partial v}{\\partial t} = D_v \\nabla^2 v + uv^2 - (f+k)v$$")
            )
          )
        ),
        column(6,
          wellPanel(
            h4("🔬 ¿Qué significan los parámetros?"),
            tags$ul(
              tags$li(tags$b("f (feed):"), " controla cuánta sustancia u entra al sistema. Valores bajos → patrones estables; altos → patrones que 'mueren'."),
              tags$li(tags$b("k (kill):"), " controla cuánto v se elimina. La combinación f+k determina el tipo de patrón."),
              tags$li(tags$b("Du/Dv:"), " ratio típico Du/Dv ≈ 2 para generar patrones. Si son iguales, no hay patrón.")
            )
          )
        )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {
  
  # --- Estado reactivo del sistema ---
  state <- reactiveValues(
    u         = NULL,
    v         = NULL,
    iteration = 0,
    running   = FALSE
  )
  
  reset_simulation <- function(n) {
    grid <- init_grid(n = n)
    state$u         <- grid$u
    state$v         <- grid$v
    state$iteration <- 0
  }
  
  run_simulation_chunk <- function() {
    req(
      !is.null(state$u),
      !is.null(state$v),
      !is.null(input$steps),
      !is.null(input$Du),
      !is.null(input$Dv),
      !is.null(input$f),
      !is.null(input$k)
    )
    
    result <- list(u = state$u, v = state$v)
    for (i in seq_len(input$steps)) {
      result <- gray_scott_step(
        u  = result$u,
        v  = result$v,
        Du = input$Du,
        Dv = input$Dv,
        f  = input$f,
        k  = input$k,
        dt = 1.0
      )
    }
    
    state$u         <- result$u
    state$v         <- result$v
    state$iteration <- state$iteration + input$steps
  }
  
  # --- Inicializar al arrancar y al cambiar el tamaño del grid ---
  observeEvent(input$grid_size, {
    req(input$grid_size)
    state$running <- FALSE
    reset_simulation(n = input$grid_size)
  }, ignoreInit = FALSE)
  
  # --- Aplicar preset cuando se selecciona ---
  observeEvent(input$preset, {
    p <- presets[[input$preset]]
    updateSliderInput(session, "f",  value = p$f)
    updateSliderInput(session, "k",  value = p$k)
    updateSliderInput(session, "Du", value = p$Du)
    updateSliderInput(session, "Dv", value = p$Dv)
  })
  
  # --- Reset ---
  observeEvent(input$reset, {
    state$running   <- FALSE
    req(input$grid_size)
    reset_simulation(n = input$grid_size)
  })
  
  # --- Un bloque de simulación manual ---
  observeEvent(input$step_btn, {
    state$running <- FALSE
    run_simulation_chunk()
  })
  
  # --- Toggle simulación continua ---
  observeEvent(input$run_btn, {
    req(
      !is.null(state$u),
      !is.null(state$v),
      !is.null(input$steps),
      !is.null(input$Du),
      !is.null(input$Dv),
      !is.null(input$f),
      !is.null(input$k)
    )
    state$running <- !state$running
  })
  
  observe({
    label <- if (isTRUE(state$running)) "⏸ Pausar" else "▶ Simular"
    updateActionButton(session, "run_btn", label = label)
  })
  
  observe({
    req(input$steps)
    updateActionButton(
      session,
      "step_btn",
      label = paste0("⏭ Paso (+", input$steps, ")")
    )
  })
  
  # --- Loop de simulación con timer estable ---
  simulation_tick <- reactiveTimer(50, session)
  
  observe({
    simulation_tick()
    
    if (!isTRUE(state$running)) {
      return()
    }
    
    isolate({
      run_simulation_chunk()
    })
  })
  
  # --- Plot principal ---
  output$pattern_plot <- renderPlot({
    req(state$v)
    
    # Convertir matriz v a dataframe largo para ggplot2
    df <- melt(state$v)
    colnames(df) <- c("x", "y", "concentration")
    
    # Seleccionar paleta
    fill_scale <- if (input$palette == "grayscott") {
      scale_fill_gradientn(
        colours = grayscott_palette(256),
        name    = "[v]",
        limits  = c(0, 1)
      )
    } else {
      scale_fill_viridis_c(option = input$palette, name = "[v]", limits = c(0, 1))
    }
    
    ggplot(df, aes(x = y, y = x, fill = concentration)) +
      geom_raster(interpolate = TRUE) +
      fill_scale +
      coord_equal() +
      theme_void() +
      theme(
        legend.position  = "right",
        plot.title       = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle    = element_text(size = 10, hjust = 0.5, color = "gray50")
      ) +
      labs(
        title    = paste0("Gray-Scott: ", input$preset),
        subtitle = paste0("Iteración: ", state$iteration,
                          "  |  f = ", input$f,
                          "  |  k = ", input$k)
      )
  })
  
  # --- Status ---
  output$status <- renderText({
    if (is.null(state$u) || is.null(state$v)) {
      return("Inicializando simulación...")
    }
    
    status <- if (isTRUE(state$running)) "▶ Simulando..." else "⏸ Pausado"
    paste0(status, "\nIteraciones: ", state$iteration,
           "\nGrid: ", nrow(state$u), "×", ncol(state$u))
  })
}

# =============================================================================
# LANZAR
# =============================================================================
shinyApp(ui = ui, server = server)
