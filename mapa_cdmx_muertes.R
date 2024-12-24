# Cargar las bibliotecas necesarias para análisis y visualización de datos
library(tidyverse)  # Conjunto de paquetes para manipulación y visualización de datos
library(readxl)     # Para leer archivos Excel
library(writexl)    # Para escribir archivos Excel
library(sf)         # Para manipulación de datos espaciales

# Establecer el directorio de trabajo
setwd("C:/Users/Arie Brito/OneDrive/Escritorio/el caracol")

# Leer los datos de mortalidad en un archivo Excel y convertirlos a tibble
muertes_cdmx <- read_excel("C:/Users/Arie Brito/OneDrive/Escritorio/el caracol/df_cdmx.xlsx",
                           col_types = c("text", "text", "text", "numeric")) %>%
  as_tibble()

# Leer el shapefile de los municipios (datos espaciales)
shape_municipios <- st_read(dsn="C:/Users/Arie Brito/OneDrive/Documentos/geoest/marco_geoestadistico_2022/conjunto_de_datos/00mun.shp", 
                            options = "ENCODING=UTF-8")

# Filtrar únicamente los municipios de la Ciudad de México (CVE_ENT == "09")
shape_cdmx <- subset(shape_municipios, CVE_ENT == "09")

# Asegurarse de que el identificador geográfico (CVEGEO) tenga un formato adecuado
shape_cdmx <- shape_cdmx %>% 
  mutate(CVEGEO = str_pad(CVEGEO, 2, pad = "0"))

# Combinar los datos de mortalidad con los datos geográficos usando el identificador CVEGEO
shape_un_estado <- left_join(shape_cdmx, muertes_cdmx, by = c("CVEGEO" = "CVEGEO"))

# Crear un mapa utilizando ggplot, coloreado por el porcentaje de muertes (POR)
mapa_total <- ggplot(data = shape_un_estado) +
  geom_sf(aes(fill = POR), color = "white") +  # Geometría espacial con colores representando POR
  scale_fill_gradient(low = "#ffccd5", high = "#e5383b") +  # Gradiente de color
  theme_minimal() +  # Tema limpio
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text = element_blank(),  # Eliminar etiquetas de los ejes
    axis.ticks = element_blank(),  # Eliminar marcas de los ejes
    panel.grid = element_blank(),  # Eliminar la cuadrícula
    panel.background = element_blank(),  # Fondo vacío
    plot.background = element_blank()  # Fondo del gráfico vacío
  ) +
  labs(title = "", x = "", y = "", fill = " %") +  # Etiquetas del gráfico
  coord_sf()  # Configuración de coordenadas espaciales

# Guardar el mapa en un archivo PNG con alta resolución
ggsave(filename = "total_cdmx_dist.png", plot = mapa_total, device = "png", units = "in", dpi = 600)

# Mostrar el mapa
mapa_total

#### MUJERES ####

# Leer los datos de mortalidad de mujeres desde un archivo Excel y convertirlos a tibble
mujeres_cdmx <- read_excel("C:/Users/Arie Brito/OneDrive/Escritorio/el caracol/df_sex2_cdmx.xlsx",
                           col_types = c("text", "numeric", "numeric")) %>%
  as_tibble()

# Leer el shapefile de los municipios (datos espaciales)
shape_municipios <- st_read(dsn="C:/Users/Arie Brito/OneDrive/Documentos/geoest/marco_geoestadistico_2022/conjunto_de_datos/00mun.shp", 
                            options = "ENCODING=UTF-8")

# Filtrar únicamente los municipios de la Ciudad de México (CVE_ENT == "09")
shape_cdmx <- subset(shape_municipios, CVE_ENT == "09")

# Asegurarse de que el identificador geográfico (CVEGEO) tenga un formato adecuado
shape_cdmx <- shape_cdmx %>% 
  mutate(CVEGEO = str_pad(CVEGEO, 2, pad = "0"))

# Combinar los datos de mortalidad de mujeres con los datos geográficos usando el identificador CVEGEO
shape_un_estado <- left_join(shape_cdmx, mujeres_cdmx, by = c("CVEGEO" = "CVEGEO"))

# Crear un mapa utilizando ggplot, coloreado por el porcentaje de muertes de mujeres (POR)
mapa_mujeres <- ggplot(data = shape_un_estado) +
  geom_sf(aes(fill = POR), color = "white") +  # Geometría espacial con colores representando POR
  scale_fill_gradient(low = "#dec9e9", high = "#6247aa") +  # Gradiente de color
  theme_minimal() +  # Tema limpio
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text = element_blank(),  # Eliminar etiquetas de los ejes
    axis.ticks = element_blank(),  # Eliminar marcas de los ejes
    panel.grid = element_blank(),  # Eliminar la cuadrícula
    panel.background = element_blank(),  # Fondo vacío
    plot.background = element_blank()  # Fondo del gráfico vacío
  ) +
  labs(title = "", x = "", y = "", fill = " %") +  # Etiquetas del gráfico
  coord_sf()  # Configuración de coordenadas espaciales

# Guardar el mapa en un archivo PNG con alta resolución
ggsave(filename = "mujeres_cdmx_dist.png", plot = mapa_mujeres, device = "png", units = "in", dpi = 600)

# Mostrar el mapa
mapa_mujeres
