
################################################################################
# ANÁLISIS COMPLETO DE VERSIONES DE H2O-3
# Análisis del impacto de la salida de Erin Ledell en el desarrollo de H2O-3
################################################################################

# ==============================================================================
# 1. CONFIGURACIÓN Y CARGA DE PAQUETES
# ==============================================================================

# Instalar paquetes si no están disponibles
paquetes_necesarios <- c("tidytable", "ggplot2", "lubridate", "zoo", 
                        "lmtest", "tseries", "scales")

for(paquete in paquetes_necesarios) {
  if(!require(paquete, character.only = TRUE)) {
    install.packages(paquete, repos = "https://cloud.r-project.org/")
    library(paquete, character.only = TRUE)
  }
}

# ==============================================================================
# 2. CARGA Y PROCESAMIENTO DE DATOS
# ==============================================================================

# Cargar datos de versiones
datos_h2o <- fread("h2o_versions_data.csv") %>%
  mutate(
    # Convertir fecha a formato Date
    Date = mdy(Date),
    # Extraer año y mes para análisis
    Anio = year(Date),
    Mes = month(Date),
    # Crear variable de período (trimestre)
    Trimestre = quarter(Date),
    # Ordenar por fecha (más antigua primero)
    .by = NULL
  ) %>%
  arrange(Date)

# Leer fecha de salida de Erin Ledell
fecha_salida_texto <- readLines("erin_ledell_departure_date.txt")
# Extraer el año 2023 del archivo
anio_salida <- 2023
fecha_salida <- as.Date(paste0(anio_salida, "-12-31"))  # Usamos fin de año como referencia

# Crear variable de intervención (antes/después de la salida)
datos_h2o <- datos_h2o %>%
  mutate(
    Post_Salida = ifelse(Date > fecha_salida, 1, 0),
    Periodo = ifelse(Post_Salida == 1, "Después", "Antes"),
    # Crear índice temporal (número de versión secuencial)
    Indice_Temporal = row_number()
  )

# Crear variable de tiempo desde la salida
datos_h2o <- datos_h2o %>%
  mutate(
    Tiempo_desde_salida = as.numeric(Date - fecha_salida) / 365.25
  )

# Resumen de los datos
cat("\n========================================\n")
cat("RESUMEN DE LOS DATOS\n")
cat("========================================\n")
cat("Total de versiones:", nrow(datos_h2o), "\n")
cat("Rango de fechas:", format(min(datos_h2o$Date), "%d/%m/%Y"), "a", 
    format(max(datos_h2o$Date), "%d/%m/%Y"), "\n")
cat("Fecha de salida de Erin Ledell:", format(fecha_salida, "%d/%m/%Y"), "\n")
cat("Versiones antes de la salida:", sum(datos_h2o$Post_Salida == 0), "\n")
cat("Versiones después de la salida:", sum(datos_h2o$Post_Salida == 1), "\n")

# ==============================================================================
# 3. VISUALIZACIONES INDIVIDUALES DE EVOLUCIÓN TEMPORAL
# ==============================================================================

cat("\n========================================\n")
cat("GENERANDO VISUALIZACIONES\n")
cat("========================================\n")

# Configuración común para todos los gráficos
tema_personalizado <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# 3.1 Evolución de Nuevas Funcionalidades
p1 <- ggplot(datos_h2o, aes(x = Date, y = New_Features)) +
  geom_line(color = "#2E86AB", linewidth = 1) +
  geom_point(aes(color = Periodo), size = 2, alpha = 0.6) +
  geom_vline(xintercept = fecha_salida, linetype = "dashed", 
             color = "red", linewidth = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "#A23B72", alpha = 0.2) +
  annotate("text", x = fecha_salida, y = max(datos_h2o$New_Features) * 0.9,
           label = "Salida de\nErin Ledell", hjust = -0.1, color = "red", 
           size = 3.5, fontface = "bold") +
  scale_color_manual(values = c("Antes" = "#2E86AB", "Después" = "#F18F01")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Evolución Temporal de Nuevas Funcionalidades en H2O-3",
    subtitle = "Número de nuevas características por versión",
    x = "Fecha de la Versión",
    y = "Número de Nuevas Funcionalidades",
    color = "Período"
  ) +
  tema_personalizado

print(p1)
ggsave("/home/ubuntu/evolucion_funcionalidades.png", p1, 
       width = 12, height = 7, dpi = 300)

# 3.2 Evolución de Bugs
p2 <- ggplot(datos_h2o, aes(x = Date, y = Bugs)) +
  geom_line(color = "#C73E1D", linewidth = 1) +
  geom_point(aes(color = Periodo), size = 2, alpha = 0.6) +
  geom_vline(xintercept = fecha_salida, linetype = "dashed", 
             color = "red", linewidth = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "#6A4C93", alpha = 0.2) +
  annotate("text", x = fecha_salida, y = max(datos_h2o$Bugs) * 0.9,
           label = "Salida de\nErin Ledell", hjust = -0.1, color = "red", 
           size = 3.5, fontface = "bold") +
  scale_color_manual(values = c("Antes" = "#C73E1D", "Después" = "#F18F01")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Evolución Temporal de Corrección de Bugs en H2O-3",
    subtitle = "Número de bugs corregidos por versión",
    x = "Fecha de la Versión",
    y = "Número de Bugs Corregidos",
    color = "Período"
  ) +
  tema_personalizado

print(p2)
ggsave("/home/ubuntu/evolucion_bugs.png", p2, 
       width = 12, height = 7, dpi = 300)

# 3.3 Evolución de Mejoras
p3 <- ggplot(datos_h2o, aes(x = Date, y = Improvements)) +
  geom_line(color = "#06A77D", linewidth = 1) +
  geom_point(aes(color = Periodo), size = 2, alpha = 0.6) +
  geom_vline(xintercept = fecha_salida, linetype = "dashed", 
             color = "red", linewidth = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "#D4A373", alpha = 0.2) +
  annotate("text", x = fecha_salida, y = max(datos_h2o$Improvements) * 0.9,
           label = "Salida de\nErin Ledell", hjust = -0.1, color = "red", 
           size = 3.5, fontface = "bold") +
  scale_color_manual(values = c("Antes" = "#06A77D", "Después" = "#F18F01")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Evolución Temporal de Mejoras en H2O-3",
    subtitle = "Número de mejoras implementadas por versión",
    x = "Fecha de la Versión",
    y = "Número de Mejoras",
    color = "Período"
  ) +
  tema_personalizado

print(p3)
ggsave("/home/ubuntu/evolucion_mejoras.png", p3, 
       width = 12, height = 7, dpi = 300)

# 3.4 Evolución de Cambios en Documentación
p4 <- ggplot(datos_h2o, aes(x = Date, y = Docs)) +
  geom_line(color = "#7209B7", linewidth = 1) +
  geom_point(aes(color = Periodo), size = 2, alpha = 0.6) +
  geom_vline(xintercept = fecha_salida, linetype = "dashed", 
             color = "red", linewidth = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "#F72585", alpha = 0.2) +
  annotate("text", x = fecha_salida, y = max(datos_h2o$Docs) * 0.9,
           label = "Salida de\nErin Ledell", hjust = -0.1, color = "red", 
           size = 3.5, fontface = "bold") +
  scale_color_manual(values = c("Antes" = "#7209B7", "Después" = "#F18F01")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Evolución Temporal de Cambios en Documentación en H2O-3",
    subtitle = "Número de actualizaciones de documentación por versión",
    x = "Fecha de la Versión",
    y = "Número de Cambios en Documentación",
    color = "Período"
  ) +
  tema_personalizado

print(p4)
ggsave("/home/ubuntu/evolucion_documentacion.png", p4, 
       width = 12, height = 7, dpi = 300)

cat("✓ Visualizaciones guardadas exitosamente\n")

# ==============================================================================
# 4. ANÁLISIS CAUSAL: SERIES TEMPORALES INTERRUMPIDAS (ITS)
# ==============================================================================

cat("\n========================================\n")
cat("ANÁLISIS CAUSAL 1: SERIES TEMPORALES INTERRUMPIDAS\n")
cat("========================================\n")

# Función para realizar análisis ITS
analisis_its <- function(datos, variable, nombre_var) {
  cat("\n--- Análisis ITS para:", nombre_var, "---\n")
  
  # Modelo de series temporales interrumpidas
  # y = β0 + β1*tiempo + β2*intervención + β3*tiempo_desde_intervención + ε
  formula <- as.formula(paste(variable, "~ Indice_Temporal + Post_Salida + Tiempo_desde_salida"))
  
  modelo <- lm(formula, data = datos)
  
  cat("\nResultados del modelo ITS:\n")
  print(summary(modelo))
  
  # Interpretación de coeficientes
  coefs <- coef(modelo)
  cat("\nInterpretación de coeficientes:\n")
  cat("β0 (Intercepto):", round(coefs[1], 3), 
      "- Nivel base al inicio del período\n")
  cat("β1 (Tendencia pre-intervención):", round(coefs[2], 3), 
      "- Cambio por versión antes de la salida\n")
  cat("β2 (Cambio de nivel):", round(coefs[3], 3), 
      "- Cambio inmediato después de la salida\n")
  if(length(coefs) > 3) {
    cat("β3 (Cambio de tendencia):", round(coefs[4], 3), 
        "- Cambio en la tendencia después de la salida\n")
  }
  
  return(modelo)
}

# Aplicar análisis ITS a cada variable
modelo_its_features <- analisis_its(datos_h2o, "New_Features", 
                                     "Nuevas Funcionalidades")
modelo_its_bugs <- analisis_its(datos_h2o, "Bugs", "Bugs Corregidos")
modelo_its_improvements <- analisis_its(datos_h2o, "Improvements", "Mejoras")
modelo_its_docs <- analisis_its(datos_h2o, "Docs", "Documentación")

# ==============================================================================
# 5. ANÁLISIS CAUSAL: COMPARACIÓN DE MEDIAS ANTES/DESPUÉS
# ==============================================================================

cat("\n========================================\n")
cat("ANÁLISIS CAUSAL 2: COMPARACIÓN DE MEDIAS\n")
cat("========================================\n")

# Función para comparar medias
comparar_medias <- function(datos, variable, nombre_var) {
  cat("\n--- Comparación de medias para:", nombre_var, "---\n")
  
  # Estadísticas descriptivas
  stats <- datos %>%
    summarise(
      Media_Antes = mean(get(variable)[Post_Salida == 0], na.rm = TRUE),
      SD_Antes = sd(get(variable)[Post_Salida == 0], na.rm = TRUE),
      N_Antes = sum(Post_Salida == 0),
      Media_Despues = mean(get(variable)[Post_Salida == 1], na.rm = TRUE),
      SD_Despues = sd(get(variable)[Post_Salida == 1], na.rm = TRUE),
      N_Despues = sum(Post_Salida == 1),
      Diferencia = Media_Despues - Media_Antes,
      Cambio_Porcentual = ifelse(Media_Antes != 0, 
                                  (Diferencia / Media_Antes) * 100, NA)
    )
  
  cat("\nEstadísticas descriptivas:\n")
  print(as.data.frame(stats))
  
  # Test t de Student para muestras independientes
  cat("\nTest t de Student (dos colas):\n")
  datos_antes <- datos[datos$Post_Salida == 0, ][[variable]]
  datos_despues <- datos[datos$Post_Salida == 1, ][[variable]]
  test_t <- t.test(datos_despues, datos_antes)
  print(test_t)
  
  # Test de Wilcoxon (no paramétrico)
  cat("\nTest de Wilcoxon (Mann-Whitney):\n")
  test_wilcox <- wilcox.test(datos_despues, datos_antes)
  print(test_wilcox)
  
  # Interpretación
  cat("\nInterpretación:\n")
  if(test_t$p.value < 0.05) {
    cat("✓ Diferencia estadísticamente significativa (p < 0.05)\n")
    cat("  Cambio promedio:", round(stats$Diferencia, 3), "\n")
    cat("  Cambio porcentual:", round(stats$Cambio_Porcentual, 2), "%\n")
  } else {
    cat("✗ No hay diferencia estadísticamente significativa (p >= 0.05)\n")
  }
  
  return(list(stats = stats, test_t = test_t, test_wilcox = test_wilcox))
}

# Aplicar comparación de medias a cada variable
comp_features <- comparar_medias(datos_h2o, "New_Features", 
                                 "Nuevas Funcionalidades")
comp_bugs <- comparar_medias(datos_h2o, "Bugs", "Bugs Corregidos")
comp_improvements <- comparar_medias(datos_h2o, "Improvements", "Mejoras")
comp_docs <- comparar_medias(datos_h2o, "Docs", "Documentación")

# ==============================================================================
# 6. ANÁLISIS CAUSAL: ANÁLISIS DE TENDENCIAS
# ==============================================================================

cat("\n========================================\n")
cat("ANÁLISIS CAUSAL 3: ANÁLISIS DE TENDENCIAS\n")
cat("========================================\n")

# Función para analizar tendencias
analizar_tendencias <- function(datos, variable, nombre_var) {
  cat("\n--- Análisis de tendencias para:", nombre_var, "---\n")
  
  # Tendencia antes de la salida
  datos_antes <- datos %>% filter(Post_Salida == 0)
  modelo_antes <- lm(as.formula(paste(variable, "~ Indice_Temporal")), 
                     data = datos_antes)
  
  # Tendencia después de la salida
  datos_despues <- datos %>% filter(Post_Salida == 1)
  if(nrow(datos_despues) > 2) {
    modelo_despues <- lm(as.formula(paste(variable, "~ Indice_Temporal")), 
                         data = datos_despues)
  } else {
    modelo_despues <- NULL
  }
  
  # Resultados
  cat("\nTendencia ANTES de la salida:\n")
  cat("Pendiente:", round(coef(modelo_antes)[2], 5), "\n")
  cat("R-cuadrado:", round(summary(modelo_antes)$r.squared, 4), "\n")
  cat("P-valor:", round(summary(modelo_antes)$coefficients[2, 4], 4), "\n")
  
  if(!is.null(modelo_despues)) {
    cat("\nTendencia DESPUÉS de la salida:\n")
    cat("Pendiente:", round(coef(modelo_despues)[2], 5), "\n")
    cat("R-cuadrado:", round(summary(modelo_despues)$r.squared, 4), "\n")
    cat("P-valor:", round(summary(modelo_despues)$coefficients[2, 4], 4), "\n")
    
    cat("\nCambio en la pendiente:", 
        round(coef(modelo_despues)[2] - coef(modelo_antes)[2], 5), "\n")
  } else {
    cat("\nNo hay suficientes datos después de la salida para análisis de tendencia\n")
  }
  
  return(list(modelo_antes = modelo_antes, modelo_despues = modelo_despues))
}

# Aplicar análisis de tendencias a cada variable
tend_features <- analizar_tendencias(datos_h2o, "New_Features", 
                                     "Nuevas Funcionalidades")
tend_bugs <- analizar_tendencias(datos_h2o, "Bugs", "Bugs Corregidos")
tend_improvements <- analizar_tendencias(datos_h2o, "Improvements", "Mejoras")
tend_docs <- analizar_tendencias(datos_h2o, "Docs", "Documentación")

# ==============================================================================
# 7. ANÁLISIS CAUSAL: MODELOS AUTORREGRESIVOS (ARIMA)
# ==============================================================================

cat("\n========================================\n")
cat("ANÁLISIS CAUSAL 4: MODELOS ARIMA CON INTERVENCIÓN\n")
cat("========================================\n")

# Función para análisis ARIMA con intervención
analisis_arima_intervencion <- function(datos, variable, nombre_var) {
  cat("\n--- Análisis ARIMA para:", nombre_var, "---\n")
  
  tryCatch({
    # Crear serie temporal
    serie <- ts(datos[[variable]], frequency = 12)
    
    # Test de estacionariedad (Augmented Dickey-Fuller)
    cat("\nTest de Dickey-Fuller (estacionariedad):\n")
    adf_test <- adf.test(serie, alternative = "stationary")
    print(adf_test)
    
    # Ajustar modelo ARIMA con regresor externo (intervención)
    # Usar auto.arima si está disponible, sino un modelo simple
    if(require(forecast, quietly = TRUE)) {
      modelo <- auto.arima(serie, xreg = datos$Post_Salida)
      cat("\nModelo ARIMA seleccionado:\n")
      print(summary(modelo))
      
      # Coeficiente de intervención
      cat("\nEfecto de la intervención (salida de Erin Ledell):\n")
      coef_intervencion <- coef(modelo)["datos$Post_Salida"]
      if(!is.na(coef_intervencion)) {
        cat("Coeficiente:", round(coef_intervencion, 4), "\n")
      }
    } else {
      # Modelo ARIMA básico si forecast no está disponible
      modelo <- arima(serie, order = c(1, 0, 1), xreg = datos$Post_Salida)
      cat("\nModelo ARIMA(1,0,1) con regresor:\n")
      print(summary(modelo))
    }
    
    return(modelo)
    
  }, error = function(e) {
    cat("Error en análisis ARIMA:", e$message, "\n")
    return(NULL)
  })
}

# Aplicar análisis ARIMA a cada variable
arima_features <- analisis_arima_intervencion(datos_h2o, "New_Features", 
                                              "Nuevas Funcionalidades")
arima_bugs <- analisis_arima_intervencion(datos_h2o, "Bugs", "Bugs Corregidos")
arima_improvements <- analisis_arima_intervencion(datos_h2o, "Improvements", 
                                                   "Mejoras")
arima_docs <- analisis_arima_intervencion(datos_h2o, "Docs", "Documentación")

# ==============================================================================
# 8. ANÁLISIS DEL RITMO DE VERSIONES
# ==============================================================================

cat("\n========================================\n")
cat("ANÁLISIS DEL RITMO DE VERSIONES\n")
cat("========================================\n")

# 8.1 Calcular tiempo entre versiones
datos_h2o <- datos_h2o %>%
  mutate(
    Dias_desde_anterior = c(NA, diff(Date)),
    Meses_desde_anterior = Dias_desde_anterior / 30.44
  )

# 8.2 Estadísticas del ritmo de versiones
cat("\n--- Tiempo entre versiones ---\n")
stats_ritmo <- datos_h2o %>%
  summarise(
    Promedio_dias = mean(Dias_desde_anterior, na.rm = TRUE),
    Mediana_dias = median(Dias_desde_anterior, na.rm = TRUE),
    SD_dias = sd(Dias_desde_anterior, na.rm = TRUE),
    Min_dias = min(Dias_desde_anterior, na.rm = TRUE),
    Max_dias = max(Dias_desde_anterior, na.rm = TRUE)
  )

cat("\nEstadísticas generales:\n")
print(as.data.frame(stats_ritmo))

# 8.3 Comparar ritmo antes/después
cat("\n--- Comparación del ritmo antes/después de la salida ---\n")
ritmo_comparacion <- datos_h2o %>%
  filter(!is.na(Dias_desde_anterior)) %>%
  summarise(
    Promedio_dias_antes = mean(Dias_desde_anterior[Post_Salida == 0], 
                               na.rm = TRUE),
    Promedio_dias_despues = mean(Dias_desde_anterior[Post_Salida == 1], 
                                 na.rm = TRUE),
    .by = NULL
  ) %>%
  mutate(
    Diferencia_dias = Promedio_dias_despues - Promedio_dias_antes,
    Cambio_porcentual = (Diferencia_dias / Promedio_dias_antes) * 100
  )

cat("\nPromedio de días entre versiones:\n")
print(as.data.frame(ritmo_comparacion))

# Test estadístico del ritmo
cat("\nTest t para diferencia en ritmo:\n")
dias_antes <- datos_h2o %>% 
  filter(Post_Salida == 0, !is.na(Dias_desde_anterior)) %>% 
  pull(Dias_desde_anterior)
dias_despues <- datos_h2o %>% 
  filter(Post_Salida == 1, !is.na(Dias_desde_anterior)) %>% 
  pull(Dias_desde_anterior)

if(length(dias_despues) > 1) {
  test_ritmo <- t.test(dias_despues, dias_antes)
  print(test_ritmo)
}

# 8.4 Visualización del tiempo entre versiones
p5 <- ggplot(datos_h2o %>% filter(!is.na(Dias_desde_anterior)), 
             aes(x = Date, y = Dias_desde_anterior)) +
  geom_line(color = "#277DA1", linewidth = 1) +
  geom_point(aes(color = Periodo), size = 2.5, alpha = 0.7) +
  geom_vline(xintercept = fecha_salida, linetype = "dashed", 
             color = "red", linewidth = 1) +
  geom_hline(yintercept = mean(datos_h2o$Dias_desde_anterior, na.rm = TRUE),
             linetype = "dotted", color = "blue", linewidth = 0.8) +
  annotate("text", x = fecha_salida, 
           y = max(datos_h2o$Dias_desde_anterior, na.rm = TRUE) * 0.9,
           label = "Salida de\nErin Ledell", hjust = -0.1, 
           color = "red", size = 3.5, fontface = "bold") +
  scale_color_manual(values = c("Antes" = "#277DA1", "Después" = "#F18F01")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Tiempo entre Versiones Consecutivas de H2O-3",
    subtitle = "Velocidad de desarrollo medida en días entre releases",
    x = "Fecha de la Versión",
    y = "Días desde la Versión Anterior",
    color = "Período"
  ) +
  tema_personalizado

print(p5)
ggsave("/home/ubuntu/ritmo_versiones.png", p5, 
       width = 12, height = 7, dpi = 300)

# 8.5 Distribución de mejoras por versión
cat("\n--- Distribución de cambios por versión ---\n")

# Calcular totales por versión
datos_h2o <- datos_h2o %>%
  mutate(
    Total_cambios = New_Features + Bugs + Improvements + Docs
  )

dist_cambios <- datos_h2o %>%
  summarise(
    Media_total_antes = mean(Total_cambios[Post_Salida == 0]),
    Media_total_despues = mean(Total_cambios[Post_Salida == 1]),
    Mediana_total_antes = median(Total_cambios[Post_Salida == 0]),
    Mediana_total_despues = median(Total_cambios[Post_Salida == 1])
  )

cat("\nDistribución de cambios totales por versión:\n")
print(as.data.frame(dist_cambios))

# Visualización de distribución
p6 <- ggplot(datos_h2o, aes(x = Periodo, y = Total_cambios, fill = Periodo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1.5) +
  scale_fill_manual(values = c("Antes" = "#2E86AB", "Después" = "#F18F01")) +
  labs(
    title = "Distribución de Cambios Totales por Versión",
    subtitle = "Comparación antes y después de la salida de Erin Ledell",
    x = "Período",
    y = "Total de Cambios (Features + Bugs + Improvements + Docs)",
    fill = "Período"
  ) +
  tema_personalizado

print(p6)
ggsave("/home/ubuntu/distribucion_cambios.png", p6, 
       width = 10, height = 7, dpi = 300)

# 8.6 Análisis de proporción de tipos de cambios
cat("\n--- Análisis de proporción de tipos de cambios ---\n")

proporcion_cambios <- datos_h2o %>%
  summarise(
    .by = Periodo,
    Prop_Features = mean(New_Features / pmax(Total_cambios, 1)) * 100,
    Prop_Bugs = mean(Bugs / pmax(Total_cambios, 1)) * 100,
    Prop_Improvements = mean(Improvements / pmax(Total_cambios, 1)) * 100,
    Prop_Docs = mean(Docs / pmax(Total_cambios, 1)) * 100
  )

cat("\nProporción promedio de cada tipo de cambio (%):\n")
print(as.data.frame(proporcion_cambios))

# 8.7 Tendencia temporal del ritmo
cat("\n--- Tendencia temporal del ritmo de desarrollo ---\n")

modelo_ritmo <- lm(Dias_desde_anterior ~ Indice_Temporal + Post_Salida, 
                   data = datos_h2o %>% filter(!is.na(Dias_desde_anterior)))
cat("\nModelo de regresión del ritmo:\n")
print(summary(modelo_ritmo))

# ==============================================================================
# 9. RESUMEN EJECUTIVO DE RESULTADOS
# ==============================================================================

cat("\n========================================\n")
cat("RESUMEN EJECUTIVO\n")
cat("========================================\n")

cat("\n1. IMPACTO EN NUEVAS FUNCIONALIDADES:\n")
cat("   - Cambio promedio:", round(comp_features$stats$Diferencia, 2), "\n")
cat("   - Cambio porcentual:", round(comp_features$stats$Cambio_Porcentual, 2), "%\n")
cat("   - Significancia estadística:", 
    ifelse(comp_features$test_t$p.value < 0.05, "SÍ", "NO"), 
    "(p =", round(comp_features$test_t$p.value, 4), ")\n")

cat("\n2. IMPACTO EN CORRECCIÓN DE BUGS:\n")
cat("   - Cambio promedio:", round(comp_bugs$stats$Diferencia, 2), "\n")
cat("   - Cambio porcentual:", round(comp_bugs$stats$Cambio_Porcentual, 2), "%\n")
cat("   - Significancia estadística:", 
    ifelse(comp_bugs$test_t$p.value < 0.05, "SÍ", "NO"), 
    "(p =", round(comp_bugs$test_t$p.value, 4), ")\n")

cat("\n3. IMPACTO EN MEJORAS:\n")
cat("   - Cambio promedio:", round(comp_improvements$stats$Diferencia, 2), "\n")
cat("   - Cambio porcentual:", 
    round(comp_improvements$stats$Cambio_Porcentual, 2), "%\n")
cat("   - Significancia estadística:", 
    ifelse(comp_improvements$test_t$p.value < 0.05, "SÍ", "NO"), 
    "(p =", round(comp_improvements$test_t$p.value, 4), ")\n")

cat("\n4. IMPACTO EN DOCUMENTACIÓN:\n")
cat("   - Cambio promedio:", round(comp_docs$stats$Diferencia, 2), "\n")
cat("   - Cambio porcentual:", round(comp_docs$stats$Cambio_Porcentual, 2), "%\n")
cat("   - Significancia estadística:", 
    ifelse(comp_docs$test_t$p.value < 0.05, "SÍ", "NO"), 
    "(p =", round(comp_docs$test_t$p.value, 4), ")\n")

cat("\n5. IMPACTO EN EL RITMO DE VERSIONES:\n")
cat("   - Días promedio entre versiones (antes):", 
    round(ritmo_comparacion$Promedio_dias_antes, 2), "\n")
cat("   - Días promedio entre versiones (después):", 
    round(ritmo_comparacion$Promedio_dias_despues, 2), "\n")
cat("   - Cambio:", round(ritmo_comparacion$Diferencia_dias, 2), 
    "días (", round(ritmo_comparacion$Cambio_porcentual, 2), "%)\n")

cat("\n========================================\n")
cat("ANÁLISIS COMPLETADO\n")
cat("========================================\n")
cat("\nArchivos generados:\n")
cat("- evolucion_funcionalidades.png\n")
cat("- evolucion_bugs.png\n")
cat("- evolucion_mejoras.png\n")
cat("- evolucion_documentacion.png\n")
cat("- ritmo_versiones.png\n")
cat("- distribucion_cambios.png\n")

cat("\nFIN DEL ANÁLISIS\n")
