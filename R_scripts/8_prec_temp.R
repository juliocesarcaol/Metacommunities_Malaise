# Instalar paquetes (si no lo has hecho)
# install.packages(c("terra", "ggplot2", "dplyr", "readr", "RColorBrewer", "viridis"))

library(terra)
library(ggplot2)
library(dplyr)
library(readr)
library(RColorBrewer)
library(viridis)

# Carpeta con los archivos TIFF
path <- "worldclim_2000_prec/"

# Cargar todos los TIFF de precipitación
prec_files <- list.files(path, pattern = "\\.tif$", full.names = TRUE)
prec_stack <- rast(prec_files)

# Leer coordenadas
coords <- read_delim("coordinates.csv", delim = ";")

# Extraer año y mes de los nombres de archivo
file_names <- basename(prec_files)
dates <- gsub(".*_(\\d{4})-(\\d{2})\\.tif", "\\1-\\2", file_names)
años <- as.numeric(substr(dates, 1, 4))
meses <- as.numeric(substr(dates, 6, 7))

# 1️⃣ Calcular el rango global de precipitaciones
prec_all <- terra::extract(prec_stack, coords[, c("longitude", "latitude")])
prec_vals <- as.numeric(unlist(prec_all[,-1]))  # aplanar a vector numérico
global_min <- min(prec_vals, na.rm = TRUE)
global_max <- max(prec_vals, na.rm = TRUE)

# 2️⃣ Crear carpeta para guardar los plots
if(!dir.exists("plots_precipitacion")) dir.create("plots_precipitacion")

# 3️⃣ Loop sobre cada coordenada
for(i in 1:nrow(coords)) {
  
  coord <- coords[i, c("longitude", "latitude")]
  Locality_name <- coords$Locality[i]
  
  # Extraer valores de precipitación
  prec_values <- terra::extract(prec_stack, coord)[,-1]
  
  # Crear data frame
  df <- data.frame(
    Año = años,
    Mes = meses,
    Precipitacion = as.numeric(prec_values)
  ) %>%
    arrange(Año, Mes)
  
  # Generar plot con escala Y fija
  p <- ggplot(df, aes(x = Mes, y = Precipitacion, color = as.factor(Año))) +
    geom_line(linewidth = 1.2, alpha = 0.5) +
    geom_point(size = 2, alpha = 0.7) +
    scale_color_viridis_d(name = "Año", option = "plasma") +
    scale_x_continuous(breaks = 1:12) +
    scale_y_continuous(limits = c(global_min, global_max)) +  # escala fija
    labs(
      title = paste("Precipitación mensual -", Locality_name),
      subtitle = paste("Lat:", coord$latitude, "Lon:", coord$longitude),
      x = "Mes",
      y = "Precipitación (mm)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold")
    )
  
  print(p)
  
  # Guardar como PDF (vectorial y de alta calidad)
  ggsave(filename = paste0("plots_precipitacion/precipitacion_", Locality_name, ".pdf"),
         plot = p, width = 8, height = 5, device = cairo_pdf)
}



# Instalar paquetes si no los tienes
#install.packages(c("terra", "ggplot2", "dplyr", "readr", "RColorBrewer"))

library(terra)
library(ggplot2)
library(dplyr)
library(readr)
library(RColorBrewer)
library(tidyr)  

# Carpetas con Tmax y Tmin
path_max <- "worldclim_2000_tmax/"
path_min <- "worldclim_2000_tmin/"

# Cargar archivos TIFF
tmax_files <- list.files(path_max, pattern = "\\.tif$", full.names = TRUE)
tmin_files <- list.files(path_min, pattern = "\\.tif$", full.names = TRUE)

tmax_stack <- rast(tmax_files)
tmin_stack <- rast(tmin_files)

# Leer coordenadas
coords <- read_delim("coordinates.csv", delim = ";")

# Extraer año y mes de los nombres de archivo (asumiendo formato igual que antes)
file_names <- basename(tmax_files)
dates <- gsub(".*_(\\d{4})-(\\d{2})\\.tif", "\\1-\\2", file_names)
años <- as.numeric(substr(dates, 1, 4))
meses <- as.numeric(substr(dates, 6, 7))

# Crear carpeta para guardar los plots
if(!dir.exists("plots_temperatura")) dir.create("plots_temperatura")

# Loop sobre cada localidad
for(i in 1:nrow(coords)) {
  
  # Extraer coordenada
  coord <- coords[i, c("longitude", "latitude")]
  Locality_name <- coords$Locality[i]
  
  # Extraer valores Tmax y Tmin
  tmax_values <- terra::extract(tmax_stack, coord)[,-1]
  tmin_values <- terra::extract(tmin_stack, coord)[,-1]
  
  # Crear data frame
  df <- data.frame(
    Año = años,
    Mes = meses,
    Tmax = as.numeric(tmax_values),
    Tmin = as.numeric(tmin_values)
  ) %>% arrange(Año, Mes)
  
  # Transformar a formato long para ggplot
  df_long <- df %>%
    pivot_longer(cols = c("Tmax", "Tmin"), names_to = "Tipo", values_to = "Temperatura")
  
  # Generar plot
p <- ggplot(df_long, aes(x = Mes, y = Temperatura, color = as.factor(Año), linetype = Tipo)) +
    geom_line(linewidth = 0.8, alpha = 0.5) +
    geom_point(size = 1) +
    scale_color_viridis_d(name = "Año", option = "plasma") +  # paleta continua discreta
    scale_linetype_manual(values = c("Tmax" = "solid", "Tmin" = "dashed"), name = "Tipo") +
    scale_x_continuous(breaks = 1:12) +
    labs(
      title = paste("Temperaturas mensuales -", Locality_name),
      subtitle = paste("Lat:", coord$latitude, "Lon:", coord$longitude),
      x = "Mes",
      y = "Temperatura (°C)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold")
    )
  
  # Mostrar plot en R
  print(p)
  
  # Guardar plot como PNG
  ggsave(filename = paste0("plots_temperatura/temperatura_", Locality_name, ".png"),
         plot = p, width = 8, height = 5)
}


# Extraer todos los valores de temperatura para todas las coordenadas
tmax_all <- terra::extract(tmax_stack, coords[, c("longitude", "latitude")])
tmin_all <- terra::extract(tmin_stack, coords[, c("longitude", "latitude")])

# Eliminar la primera columna (ID)
tmax_vals <- as.numeric(unlist(tmax_all[,-1]))
tmin_vals <- as.numeric(unlist(tmin_all[,-1]))

# Calcular límites globales
global_min <- min(c(tmax_vals, tmin_vals), na.rm = TRUE)
global_max <- max(c(tmax_vals, tmin_vals), na.rm = TRUE)


# 2️⃣ Loop por localidad
for(i in 1:nrow(coords)) {
  
  coord <- coords[i, c("longitude", "latitude")]
  Locality_name <- coords$Locality[i]
  
  tmax_values <- terra::extract(tmax_stack, coord)[,-1]
  tmin_values <- terra::extract(tmin_stack, coord)[,-1]
  
  df <- data.frame(
    Año = años,
    Mes = meses,
    Tmax = as.numeric(tmax_values),
    Tmin = as.numeric(tmin_values)
  ) %>% arrange(Año, Mes)
  
  df_long <- df %>%
    pivot_longer(cols = c("Tmax", "Tmin"), names_to = "Tipo", values_to = "Temperatura")
  
  # 3️⃣ Plot con escala Y fija
  p <- ggplot(df_long, aes(x = Mes, y = Temperatura, color = as.factor(Año), linetype = Tipo)) +
    geom_line(linewidth = 1.2, alpha = 0.5) +
    geom_point(size = 2, alpha = 0.7) +
    scale_color_viridis_d(name = "Año", option = "plasma") +
    scale_linetype_manual(values = c("Tmax" = "solid", "Tmin" = "dashed"), name = "Tipo") +
    scale_x_continuous(breaks = 1:12) +
    scale_y_continuous(limits = c(global_min, global_max)) +  # eje Y uniforme
    labs(
      title = paste("Temperaturas mensuales -", Locality_name),
      subtitle = paste("Lat:", coord$latitude, "Lon:", coord$longitude),
      x = "Mes",
      y = "Temperatura (°C)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold")
    )
  
  print(p)
  
  ggsave(filename = paste0("plots_temperatura/temperatura_", Locality_name, ".pdf"),
         plot = p, width = 20, height = 10)
}

