install.packages("sf")
install.packages("mapview")
install.packages("ggrepel")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("plotly")
install.packages("htmlwidgets")
library(sf)
library(mapview)
library(ggrepel)
library(tidyverse)
library(ggplot2)
library(plotly)
library(htmlwidgets)

peru <- st_read("BAS_LIM_DEPARTAMENTO.shp")
mapview::mapview(peru,legend = TRUE, labels=F)

peru <- peru %>% mutate(centroid = map(geometry, st_centroid), 
                    coords = map(centroid, st_coordinates), 
                    coords_x = map_dbl(coords,1), 
                    coords_y = map_dbl(coords, 2))

peru <- peru %>% rename(DEPARTAMENTO = NOMBDEP)

covid <- read_delim("positivos_covid.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(encoding = "ASCII"), 
                    trim_ws = TRUE)

covid_freq <- covid %>%
  group_by(DEPARTAMENTO) %>%
  summarise(Frequency=n())

covid_freq <- covid_freq %>% rename(CASOS = Frequency)

mapa <- peru %>% 
  left_join(covid_freq)

plot <- ggplot(mapa, aes(geometry=geometry)) +
  geom_sf(aes(fill=CASOS)) +
  geom_text(data = mapa, aes(coords_x, coords_y, group=NULL, label=CASOS), size=2.5) +
  labs(x="", y="")

ggplotly(plot, width = 700, height = 700, tooltip = c("DEPARTAMENTO"))
saveWidget(plot, file = "Mapa_covid.html")
