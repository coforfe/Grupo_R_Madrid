# =============================================================================
# Gray-Scott Reaction-Diffusion System - Bilingual Gallery Shiny App
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
  labyrinths = list(f = 0.022, k = 0.051, Du = 0.16, Dv = 0.08),
  spots = list(f = 0.035, k = 0.065, Du = 0.16, Dv = 0.08),
  gears = list(f = 0.025, k = 0.060, Du = 0.16, Dv = 0.08),
  waves = list(f = 0.014, k = 0.054, Du = 0.16, Dv = 0.08),
  chaos = list(f = 0.026, k = 0.051, Du = 0.16, Dv = 0.08),
  leopard_skin = list(f = 0.037, k = 0.060, Du = 0.16, Dv = 0.08)
)

preset_ids <- names(presets)

translations <- list(
  es = list(
    app_title = "Gray-Scott: galeria paralela de patrones",
    language_label = "Idioma / Language",
    tabs = list(explanation = "Explicacion", gallery = "Galeria"),
    explanation = list(
      title_1 = "El sistema Gray-Scott",
      p_1 = "La app simula un sistema de reaccion-difusion con dos sustancias quimicas, u y v.",
      p_2 = "Dependiendo de la combinacion de parametros, el sistema puede producir manchas, laberintos, ondas y estructuras complejas.",
      title_2 = "Que controla cada parametro",
      bullet_f = "tasa de alimentacion de u.",
      bullet_k = "tasa de eliminacion de v.",
      bullet_diff = "controlan la difusion espacial.",
      bullet_grid = "resolucion espacial de cada simulacion.",
      bullet_steps = "cuantos pasos numericos se hacen antes de repintar cada panel.",
      p_3 = "En la pestana de galeria se muestran seis presets de forma simultanea con sus parametros visibles encima de cada grafico.",
      title_3 = "Notas de rendimiento",
      p_4 = "Esta version no usa paralelizacion real por procesos. Ejecuta seis simulaciones dentro del mismo ciclo reactivo de Shiny, lo que simplifica la app y suele ser suficiente para grids moderados.",
      p_5 = "Si en tu equipo se quedara corta, el siguiente paso natural seria mover el nucleo numerico a Rcpp/C++ o reducir grid y pasos por tick."
    ),
    controls = list(
      grid_size = "Tamano del grid",
      steps = "Pasos por tick",
      palette = "Paleta",
      reset = "Resetear galeria",
      run = "▶ Simular todas",
      pause = "⏸ Pausar todas",
      status_running = "Estado: simulando",
      status_paused = "Estado: pausado",
      common_iteration = "Iteracion comun",
      plot_iteration = "Iteracion",
      preset_original = "Gray-Scott Original"
    ),
    preset_titles = list(
      labyrinths = "Laberintos",
      spots = "Manchas",
      gears = "Engranajes",
      waves = "Ondas",
      chaos = "Caos",
      leopard_skin = "Piel de leopardo"
    )
  ),
  en = list(
    app_title = "Gray-Scott: parallel pattern gallery",
    language_label = "Language / Idioma",
    tabs = list(explanation = "Explanation", gallery = "Gallery"),
    explanation = list(
      title_1 = "The Gray-Scott system",
      p_1 = "This app simulates a reaction-diffusion system with two chemical species, u and v.",
      p_2 = "Depending on the parameter combination, the system can produce spots, labyrinths, waves, and complex structures.",
      title_2 = "What each parameter controls",
      bullet_f = "feed rate of u.",
      bullet_k = "removal rate of v.",
      bullet_diff = "control spatial diffusion.",
      bullet_grid = "spatial resolution of each simulation.",
      bullet_steps = "how many numerical steps are computed before each panel is redrawn.",
      p_3 = "The gallery tab shows six presets simultaneously, with their parameters displayed above each plot.",
      title_3 = "Performance notes",
      p_4 = "This version does not use true process-level parallelization. It runs six simulations inside the same Shiny reactive loop, which keeps the app simpler and is usually enough for moderate grid sizes.",
      p_5 = "If performance becomes limiting on your machine, the next natural step would be to move the numerical core to Rcpp/C++ or reduce the grid size and steps per tick."
    ),
    controls = list(
      grid_size = "Grid size",
      steps = "Steps per tick",
      palette = "Palette",
      reset = "Reset gallery",
      run = "▶ Run all",
      pause = "⏸ Pause all",
      status_running = "Status: running",
      status_paused = "Status: paused",
      common_iteration = "Common iteration",
      plot_iteration = "Iteration",
      preset_original = "Gray-Scott Original"
    ),
    preset_titles = list(
      labyrinths = "Labyrinths",
      spots = "Spots",
      gears = "Gears",
      waves = "Waves",
      chaos = "Chaos",
      leopard_skin = "Leopard skin"
    )
  )
)

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

build_plot <- function(v_matrix, title, iteration, palette_name, iteration_label) {
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
      subtitle = paste0(iteration_label, ": ", iteration)
    )
}

preset_card <- function(id, title, params_text) {
  div(
    class = "preset-card",
    div(class = "preset-header", title),
    div(class = "preset-params", params_text),
    plotOutput(outputId = paste0("plot_", id), height = "240px")
  )
}

language_choices <- c("Español" = "es", "English" = "en")

current_or <- function(value, default) {
  if (is.null(value)) default else value
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
      .top-bar {
        display: flex;
        justify-content: space-between;
        align-items: flex-end;
        gap: 18px;
        margin-bottom: 16px;
      }
      .app-title {
        margin: 0;
      }
      .lang-box {
        min-width: 240px;
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
  div(
    class = "top-bar",
    h2(class = "app-title", textOutput("page_title", inline = TRUE)),
    div(
      class = "lang-box",
      selectInput("lang", "Idioma / Language", choices = language_choices, selected = "es")
    )
  ),
  uiOutput("app_content")
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  state <- reactiveValues(
    simulations = NULL,
    running = FALSE
  )

  lang_text <- reactive({
    translations[[current_or(input$lang, "es")]]
  })

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

  output$page_title <- renderText({
    lang_text()$app_title
  })

  output$app_content <- renderUI({
    txt <- lang_text()

    tabsetPanel(
      id = "main_tabs",
      selected = current_or(isolate(input$main_tabs), "explanation"),
      tabPanel(
        txt$tabs$explanation,
        value = "explanation",
        fluidRow(
          column(
            6,
            div(
              class = "explain-card",
              h3(txt$explanation$title_1),
              p(txt$explanation$p_1),
              p(txt$explanation$p_2),
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
              h3(txt$explanation$title_2),
              tags$ul(
                tags$li(tags$b("f:"), paste0(" ", txt$explanation$bullet_f)),
                tags$li(tags$b("k:"), paste0(" ", txt$explanation$bullet_k)),
                tags$li(tags$b("Du y Dv:"), paste0(" ", txt$explanation$bullet_diff)),
                tags$li(tags$b("Grid:"), paste0(" ", txt$explanation$bullet_grid)),
                tags$li(tags$b("Steps:"), paste0(" ", txt$explanation$bullet_steps))
              ),
              p(txt$explanation$p_3)
            )
          )
        ),
        fluidRow(
          column(
            12,
            div(
              class = "explain-card",
              h3(txt$explanation$title_3),
              p(txt$explanation$p_4),
              p(txt$explanation$p_5)
            )
          )
        )
      ),
      tabPanel(
        txt$tabs$gallery,
        value = "gallery",
        div(
          class = "gallery-controls",
          fluidRow(
            column(
              3,
              sliderInput(
                "grid_size",
                txt$controls$grid_size,
                min = 50,
                max = 120,
                value = current_or(isolate(input$grid_size), 80),
                step = 10
              )
            ),
            column(
              3,
              sliderInput(
                "steps",
                txt$controls$steps,
                min = 5,
                max = 60,
                value = current_or(isolate(input$steps), 20),
                step = 5
              )
            ),
            column(
              3,
              selectInput(
                "palette",
                txt$controls$palette,
                choices = c(setNames("grayscott", txt$controls$preset_original), "magma", "inferno", "plasma", "viridis"),
                selected = current_or(isolate(input$palette), "grayscott")
              )
            ),
            column(
              3,
              br(),
              actionButton("reset_all", txt$controls$reset, class = "btn-warning", width = "48%"),
              actionButton(
                "run_btn",
                if (isTRUE(state$running)) txt$controls$pause else txt$controls$run,
                class = "btn-success",
                width = "48%"
              ),
              br(),
              br(),
              textOutput("gallery_status")
            )
          )
        ),
        fluidRow(
          column(4, preset_card("labyrinths", txt$preset_titles$labyrinths, format_params(presets$labyrinths))),
          column(4, preset_card("spots", txt$preset_titles$spots, format_params(presets$spots))),
          column(4, preset_card("gears", txt$preset_titles$gears, format_params(presets$gears)))
        ),
        fluidRow(
          column(4, preset_card("waves", txt$preset_titles$waves, format_params(presets$waves))),
          column(4, preset_card("chaos", txt$preset_titles$chaos, format_params(presets$chaos))),
          column(4, preset_card("leopard_skin", txt$preset_titles$leopard_skin, format_params(presets$leopard_skin)))
        )
      )
    )
  })

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

  output$gallery_status <- renderText({
    req(state$simulations)
    txt <- lang_text()
    first_iteration <- state$simulations[[preset_ids[[1]]]]$iteration
    status <- if (isTRUE(state$running)) txt$controls$status_running else txt$controls$status_paused
    paste0(status, " | ", txt$controls$common_iteration, ": ", first_iteration)
  })

  observe({
    txt <- lang_text()
    updateActionButton(
      session,
      "run_btn",
      label = if (isTRUE(state$running)) txt$controls$pause else txt$controls$run
    )
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

      output[[paste0("plot_", local_id)]] <- renderPlot({
        req(state$simulations)
        txt <- lang_text()
        sim <- state$simulations[[local_id]]
        build_plot(
          v_matrix = sim$v,
          title = txt$preset_titles[[local_id]],
          iteration = sim$iteration,
          palette_name = input$palette,
          iteration_label = txt$controls$plot_iteration
        )
      }, res = 96)
    })
  }
}

shinyApp(ui = ui, server = server)
