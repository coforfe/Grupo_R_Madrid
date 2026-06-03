# =============================================================================
# Gray-Scott Reaction-Diffusion System - Gallery Shiny App
# =============================================================================

library(shiny)
library(ggplot2)
library(reshape2)
library(viridis)

# =============================================================================
# NUMERICAL CORE
# =============================================================================

gray_scott_step <- function(u, v, Du, Dv, f, k, dt = 1.0) {
  n <- nrow(u)
  m <- ncol(u)

  laplacian <- function(mat) {
    up <- rbind(mat[n, ], mat[-n, ])
    down <- rbind(mat[-1, ], mat[1, ])
    left <- cbind(mat[, m], mat[, -m])
    right <- cbind(mat[, -1], mat[, 1])

    up + down + left + right - 4 * mat
  }

  Lu <- laplacian(u)
  Lv <- laplacian(v)
  uvv <- u * v * v

  u_new <- u + dt * (Du * Lu - uvv + f * (1 - u))
  v_new <- v + dt * (Dv * Lv + uvv - (f + k) * v)

  u_new[] <- pmax(0, pmin(1, u_new))
  v_new[] <- pmax(0, pmin(1, v_new))

  list(u = u_new, v = v_new)
}

init_grid <- function(n = 80, seed = 42) {
  set.seed(seed)

  u <- matrix(1, nrow = n, ncol = n)
  v <- matrix(0, nrow = n, ncol = n)

  r <- round(n * 0.1)
  cx <- round(n / 2)
  cy <- round(n / 2)

  rows <- max(1, cx - r):min(n, cx + r)
  cols <- max(1, cy - r):min(n, cy + r)

  u[rows, cols] <- 0.5 + runif(length(rows) * length(cols), -0.1, 0.1)
  v[rows, cols] <- 0.25 + runif(length(rows) * length(cols), -0.1, 0.1)

  list(u = u, v = v)
}

grayscott_colors <- c(
  "#1a0030",
  "#3d0060",
  "#6b0080",
  "#8b1a6b",
  "#a03020",
  "#c05010",
  "#e07010",
  "#f0a020",
  "#f5c040",
  "#fde060",
  "#fff5a0"
)

grayscott_palette <- colorRampPalette(grayscott_colors)

presets <- list(
  laberintos = list(title = "Laberintos", f = 0.022, k = 0.051, Du = 0.16, Dv = 0.08),
  manchas = list(title = "Manchas", f = 0.035, k = 0.065, Du = 0.16, Dv = 0.08),
  engranajes = list(title = "Engranajes", f = 0.025, k = 0.060, Du = 0.16, Dv = 0.08),
  ondas = list(title = "Ondas", f = 0.014, k = 0.054, Du = 0.16, Dv = 0.08),
  caos = list(title = "Caos", f = 0.026, k = 0.051, Du = 0.16, Dv = 0.08),
  piel_leopardo = list(title = "Piel de leopardo", f = 0.037, k = 0.060, Du = 0.16, Dv = 0.08)
)

preset_ids <- names(presets)

build_initial_states <- function(n) {
  simulations <- vector("list", length(preset_ids))
  names(simulations) <- preset_ids

  for (idx in seq_along(preset_ids)) {
    id <- preset_ids[[idx]]
    grid <- init_grid(n = n, seed = 100 + idx)
    simulations[[id]] <- list(
      u = grid$u,
      v = grid$v,
      iteration = 0
    )
  }

  simulations
}

format_params <- function(preset) {
  sprintf("f = %.3f   |   k = %.3f   |   Du = %.2f   |   Dv = %.2f",
          preset$f, preset$k, preset$Du, preset$Dv)
}

build_plot <- function(v_matrix, title, iteration, palette_name) {
  df <- melt(v_matrix)
  colnames(df) <- c("x", "y", "concentration")

  fill_scale <- if (palette_name == "grayscott") {
    scale_fill_gradientn(
      colours = grayscott_palette(256),
      limits = c(0, 1),
      guide = "none"
    )
  } else {
    scale_fill_viridis_c(
      option = palette_name,
      limits = c(0, 1),
      guide = "none"
    )
  }

  ggplot(df, aes(x = y, y = x, fill = concentration)) +
    geom_raster(interpolate = TRUE) +
    fill_scale +
    coord_equal() +
    theme_void() +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray35"),
      plot.margin = margin(4, 4, 4, 4)
    ) +
    labs(
      title = title,
      subtitle = paste0("Iteracion: ", iteration)
    )
}

preset_card <- function(id, preset) {
  div(
    class = "preset-card",
    div(class = "preset-header", preset$title),
    div(class = "preset-params", format_params(preset)),
    plotOutput(outputId = paste0("plot_", id), height = "240px")
  )
}

# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: #f4f1ea;
      }
      .app-title {
        margin-bottom: 16px;
      }
      .gallery-controls {
        background: linear-gradient(135deg, #f7edd8 0%, #f1dcc2 100%);
        border: 1px solid #d7c2a5;
        border-radius: 14px;
        padding: 14px 18px 4px 18px;
        margin-bottom: 18px;
      }
      .preset-card {
        background: #fffdf9;
        border: 1px solid #dcc8af;
        border-radius: 14px;
        padding: 12px;
        margin-bottom: 16px;
        box-shadow: 0 8px 18px rgba(83, 58, 24, 0.08);
      }
      .preset-header {
        font-size: 18px;
        font-weight: 700;
        color: #5e3b16;
        margin-bottom: 6px;
      }
      .preset-params {
        font-family: monospace;
        font-size: 12px;
        color: #6e5b48;
        margin-bottom: 8px;
      }
      .explain-card {
        background: #fffdf9;
        border: 1px solid #dcc8af;
        border-radius: 14px;
        padding: 18px;
        margin-bottom: 16px;
      }
      .tab-pane {
        padding-top: 16px;
      }
    "))
  ),
  div(class = "app-title", titlePanel("Gray-Scott: galeria paralela de patrones")),
  tabsetPanel(
    tabPanel(
      "Explicacion",
      fluidRow(
        column(
          6,
          div(
            class = "explain-card",
            h3("El sistema Gray-Scott"),
            p("La app simula un sistema de reaccion-difusion con dos sustancias quimicas, u y v."),
            p("Dependiendo de la combinacion de parametros, el sistema puede producir manchas, laberintos, ondas y estructuras complejas."),
            withMathJax(
              helpText("$$\\frac{\\partial u}{\\partial t} = D_u \\nabla^2 u - uv^2 + f(1-u)$$"),
              helpText("$$\\frac{\\partial v}{\\partial t} = D_v \\nabla^2 v + uv^2 - (f+k)v$$")
            )
          )
        ),
        column(
          6,
          div(
            class = "explain-card",
            h3("Que controla cada parametro"),
            tags$ul(
              tags$li(tags$b("f:"), " tasa de alimentacion de u."),
              tags$li(tags$b("k:"), " tasa de eliminacion de v."),
              tags$li(tags$b("Du y Dv:"), " controlan la difusion espacial."),
              tags$li(tags$b("Grid:"), " resolucion espacial de cada simulacion."),
              tags$li(tags$b("Pasos por tick:"), " cuantos pasos numericos se hacen antes de repintar cada panel.")
            ),
            p("En la pestana de galeria se muestran seis presets de forma simultanea con sus parametros visibles encima de cada grafico.")
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            class = "explain-card",
            h3("Notas de rendimiento"),
            p("Esta version no usa paralelizacion real por procesos. Ejecuta seis simulaciones dentro del mismo ciclo reactivo de Shiny, lo que simplifica la app y suele ser suficiente para grids moderados."),
            p("Si en tu equipo se quedara corta, el siguiente paso natural seria mover el nucleo numerico a Rcpp/C++ o reducir grid y pasos por tick.")
          )
        )
      )
    ),
    tabPanel(
      "Galeria",
      div(
        class = "gallery-controls",
        fluidRow(
          column(
            3,
            sliderInput("grid_size", "Tamano del grid", min = 50, max = 120, value = 80, step = 10)
          ),
          column(
            3,
            sliderInput("steps", "Pasos por tick", min = 5, max = 60, value = 20, step = 5)
          ),
          column(
            3,
            selectInput(
              "palette",
              "Paleta",
              choices = c("Gray-Scott Original" = "grayscott", "magma", "inferno", "plasma", "viridis"),
              selected = "grayscott"
            )
          ),
          column(
            3,
            br(),
            actionButton("reset_all", "Resetear galeria", class = "btn-warning", width = "48%"),
            actionButton("run_btn", "▶ Simular todas", class = "btn-success", width = "48%"),
            br(),
            br(),
            textOutput("gallery_status")
          )
        )
      ),
      fluidRow(
        column(4, preset_card("laberintos", presets$laberintos)),
        column(4, preset_card("manchas", presets$manchas)),
        column(4, preset_card("engranajes", presets$engranajes))
      ),
      fluidRow(
        column(4, preset_card("ondas", presets$ondas)),
        column(4, preset_card("caos", presets$caos)),
        column(4, preset_card("piel_leopardo", presets$piel_leopardo))
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  state <- reactiveValues(
    simulations = NULL,
    running = FALSE
  )

  reset_gallery <- function(n) {
    state$simulations <- build_initial_states(n = n)
  }

  run_all_simulations_chunk <- function() {
    req(state$simulations, input$steps)

    updated <- state$simulations

    for (id in preset_ids) {
      preset <- presets[[id]]
      sim <- updated[[id]]
      next_iteration <- sim$iteration + input$steps

      for (i in seq_len(input$steps)) {
        sim <- gray_scott_step(
          u = sim$u,
          v = sim$v,
          Du = preset$Du,
          Dv = preset$Dv,
          f = preset$f,
          k = preset$k,
          dt = 1.0
        )
      }
      sim$iteration <- next_iteration

      updated[[id]] <- sim
    }

    state$simulations <- updated
  }

  observeEvent(input$grid_size, {
    req(input$grid_size)
    reset_gallery(n = input$grid_size)
  }, ignoreInit = FALSE)

  observeEvent(input$reset_all, {
    req(input$grid_size)
    reset_gallery(n = input$grid_size)
  })

  observeEvent(input$run_btn, {
    state$running <- !isTRUE(state$running)
  })

  observe({
    label <- if (isTRUE(state$running)) "⏸ Pausar todas" else "▶ Simular todas"
    updateActionButton(session, "run_btn", label = label)
  })

  output$gallery_status <- renderText({
    req(state$simulations)
    first_iteration <- state$simulations[[preset_ids[[1]]]]$iteration
    status <- if (isTRUE(state$running)) "Estado: simulando" else "Estado: pausado"
    paste0(status, " | Iteracion comun: ", first_iteration)
  })

  observe({
    req(isTRUE(state$running))
    invalidateLater(120, session)
    
    if (is.null(isolate(state$simulations))) {
      return()
    }

    isolate({
      run_all_simulations_chunk()
    })
  })

  for (id in preset_ids) {
    local({
      local_id <- id
      local_preset <- presets[[local_id]]

      output[[paste0("plot_", local_id)]] <- renderPlot({
        req(state$simulations)
        sim <- state$simulations[[local_id]]
        build_plot(
          v_matrix = sim$v,
          title = local_preset$title,
          iteration = sim$iteration,
          palette_name = input$palette
        )
      }, res = 96)
    })
  }
}

shinyApp(ui = ui, server = server)
