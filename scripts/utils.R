#############################################
# Funciones útiles - Histogramas Vicuñas
# Last version: 09-17-2025
# By: Fiorella Eduardo-Palomino / fioedupa@gmail.com
#############################################

# Cargar librerías necesarias
library(readr)
library(ggplot2)
library(ggbreak)
library(patchwork)
library(stringr)
library(scales)

# Función para calcular la moda
moda <- function(x) {
  x <- na.omit(x)                 # quitar NA
  ux <- unique(x)                 # valores únicos
  ux[which.max(tabulate(match(x, ux)))]  # valor más frecuente
}

# Función para limpiar datos
limpiar_datos <- function(path) {
  datos <- read.table(path, sep = ",", header = TRUE)
  datos[] <- lapply(datos, function(x) as.numeric(gsub(",", "", x)))
  datos <- na.omit(datos)
  return(datos)
}