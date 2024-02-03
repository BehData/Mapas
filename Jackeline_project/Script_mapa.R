install.packages("plotly")
install.packages("mapsPERU")
## Información del paquete https://github.com/musajajorge/mapsPERU 
install.packages("leaflet")
install.packages("leaflet.extras")
install.packages("rworldxtra")
install.packages("raster")
install.packages("mapview")
install.packages("sf")
install.packages("ggplot2")

library(plotly)
library(mapsPERU)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(sp)
library(rworldxtra)
library(raster)
library(sf)
library(tidyverse)

# Utilizamos el datasets "map_DEP" incluida en el paquete "mapsPERU", que incluyen los siguientes datos:
 
# COD_DEPARTAMENTO: Código del departamento
# DEPARTAMENTO: Nombre del departamento
# coords_x: Longitud del centroide del departamento
# coords_y: Latitud del centroide del departamento
# geometría: MULTIPOLYGON Objeto geométrico

df <- map_DEP

ggplot(df, aes(geometry=geometry)) +
  geom_sf(aes(fill=DEPARTAMENTO)) +
  geom_text(data = df, aes(coords_x, coords_y, group=NULL, label=DEPARTAMENTO), size=2.5)+
  labs(x="", y="")

# Edición de la data: cambiamos la columna de DEPARTAMENTO a mayúscula y sin tilde
df_1 <- mutate_if(df, is.character, toupper)

df_1$DEPARTAMENTO <- chartr('ÁÉÍÓÚ', "AEIOU", df_1$DEPARTAMENTO)

# Importamos la base de datos
ingresantes <- readxl::read_excel("Ingresantes2022.xlsx")

# Unimos la base de datos que tiene los centroides con la columna de datos de los ingresantes 2021 
mapa <- df_1 %>% 
  left_join(ingresantes)

# Creamos la base del mapa
leaflet() %>% addTiles()

# Agregamos etiquetas al mapa
leaflet() %>% addTiles() %>% 
  addCircles(data = mapa, lat = ~coords_y, 
             lng = ~coords_x)

# Encapsulamos los nombres
regiones <- mapa$DEPARTAMENTO %>% unique()

# Definimos colores para las macroregiones

Colores <- c('#4daf4a', "#F38458", 
             "#E6D04F", "#E6D04F", 
             "#EB7999", "#F38458", 
             "#5195A9", "#E6D04F", 
             "#EB7999", "#EB7999",
             "#EB7999", "#EB7999", 
             "#F38458", "#F38458",
             "#5195A9", '#4daf4a',
             '#4daf4a', "#E6D04F",
             "#EB7999", "#F38458", 
             "#E6D04F", '#4daf4a', 
             "#E6D04F", "#F38458",
             '#4daf4a')


# Se vincula la paleta de colores con los nombres de las regiones
pal <- colorFactor(Colores, domain = regiones)

# Se crean las etiquetas con la cantidad de los ingresantes según los departamentos
p <- leaflet() %>% addTiles() %>% addCircleMarkers(data = mapa, lat = ~coords_y, radius = 7,
                                        lng = ~coords_x, color = ~pal(DEPARTAMENTO),
                                        fillOpacity = 1.8, label = ~INGRESANTES, 
                                        group = "Codigo")

# Generamos las leyendas y la opción de ver el código o la leyenda 

p <- p %>% addLegend(data = mapa, "bottomright", pal = pal, 
                     values = ~DEPARTAMENTO, title = "Regiones", 
                     opacity = 0.8, group = "Leyenda")
p

p <- p %>% addLayersControl(overlayGroups = c("Codigo", "Leyenda"), 
                            options = layersControlOptions(collapsed = F))
p
