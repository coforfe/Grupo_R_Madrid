# Ejemplo de análisis de versiones H2O-3 en R
# Este script muestra cómo cargar y analizar los datos extraídos

# Cargar librerías necesarias
library(tidyverse)
library(lubridate)

# Cargar el dataset
h2o_data <- read_csv("/home/ubuntu/h2o_versions_data.csv")

# Vista previa de los datos
head(h2o_data)

# Resumen estadístico
summary(h2o_data)

# Convertir fecha a formato Date
h2o_data <- h2o_data %>%
  mutate(Date = mdy(Date))

# Agregar columna de total de cambios
h2o_data <- h2o_data %>%
  mutate(Total_Changes = New_Features + Bugs + Improvements + Docs)

# ===== ANÁLISIS BÁSICOS =====

# 1. Top 10 versiones con más cambios
cat("\n=== Top 10 versiones con más cambios ===\n")
h2o_data %>%
  arrange(desc(Total_Changes)) %>%
  select(Version, Date, Total_Changes, New_Features, Bugs, Improvements, Docs) %>%
  head(10) %>%
  print()

# 2. Distribución de tipos de cambios
cat("\n=== Distribución total de tipos de cambios ===\n")
h2o_data %>%
  summarise(
    Total_New_Features = sum(New_Features),
    Total_Bugs = sum(Bugs),
    Total_Improvements = sum(Improvements),
    Total_Docs = sum(Docs)
  ) %>%
  print()

# 3. Promedio de cambios por versión
cat("\n=== Promedio de cambios por versión ===\n")
h2o_data %>%
  summarise(
    Avg_New_Features = mean(New_Features),
    Avg_Bugs = mean(Bugs),
    Avg_Improvements = mean(Improvements),
    Avg_Docs = mean(Docs),
    Avg_Total = mean(Total_Changes)
  ) %>%
  print()

# 4. Análisis temporal (por año)
cat("\n=== Cambios por año ===\n")
h2o_data %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(
    Num_Versions = n(),
    Total_Features = sum(New_Features),
    Total_Bugs = sum(Bugs),
    Total_Improvements = sum(Improvements),
    Total_Docs = sum(Docs),
    Total_All = sum(Total_Changes)
  ) %>%
  arrange(desc(Year)) %>%
  print()

# ===== VISUALIZACIONES =====

# 1. Evolución temporal de cambios
ggplot(h2o_data, aes(x = Date, y = Total_Changes)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  theme_minimal() +
  labs(
    title = "Evolución de Cambios en H2O-3",
    subtitle = "Total de cambios por versión a lo largo del tiempo",
    x = "Fecha",
    y = "Total de Cambios"
  )

# 2. Distribución de tipos de cambios (stacked area)
h2o_data_long <- h2o_data %>%
  select(Date, New_Features, Bugs, Improvements, Docs) %>%
  pivot_longer(cols = -Date, names_to = "Type", values_to = "Count")

ggplot(h2o_data_long, aes(x = Date, y = Count, fill = Type)) +
  geom_area(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribución de Tipos de Cambios en H2O-3",
    subtitle = "Por tipo de cambio a lo largo del tiempo",
    x = "Fecha",
    y = "Número de Cambios",
    fill = "Tipo de Cambio"
  ) +
  scale_fill_brewer(palette = "Set2")

# 3. Comparación de categorías (boxplot)
ggplot(h2o_data_long, aes(x = Type, y = Count, fill = Type)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Distribución de Cambios por Categoría",
    x = "Tipo de Cambio",
    y = "Número de Cambios"
  ) +
  scale_fill_brewer(palette = "Pastel1")

cat("\n✅ Análisis completado!\n")
