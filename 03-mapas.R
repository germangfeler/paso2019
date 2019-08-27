##----------------------------------------------------------------------
## Analisis PASO - Mapas por provincia
## Analisis: German Gonzalez
## Fecha creacion: 13/08/2019
##----------------------------------------------------------------------

## Cargamos los paquetes 
library("raster")
library("leaflet")
library("tidyverse")

## Seteamos el directorio de trabajo
setwd("/home/paso2019/120819-054029/")

##------ Datos necesarios para mapas -------

## Cargamos las tablas creadas en el script 02
load("tablas_paso_pct.Rdata")

## Convertimos los datos al formato 'ancho' para graficar
xprov_wide <- xprov %>%
   select(-VOTOS) %>%
   spread(NOMBRE_AGRUPACION, PCT) %>%
   mutate_at("PROV", as.character)

## Obtengo el mapa
argentina <- getData("GADM", country="Argentina", level=1)

## Incluyo los votos en la tabla del mapa
argentina@data <- left_join(argentina@data, xprov_wide, by=c("NAME_1" = "PROV"))

##----------- Creamos mapas --------------

## Mapa Juntos por el Cambio
contenido_popup <- paste0("<strong>Provincia: </strong>", 
                      argentina$NAME_1, 
                      "<br><strong>Votos: </strong>", 
                      argentina$`JUNTOS POR EL CAMBIO`,
                      " <strong>%</strong>")
pal <- colorQuantile(palette = "Oranges", domain = NULL, n = 5)
                      
jxc <- leaflet(data = argentina) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(`JUNTOS POR EL CAMBIO`), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = contenido_popup)
jxc

## Mapa Frente de Todos
contenido_popup <- paste0("<strong>Provincia: </strong>", 
                      argentina$NAME_1, 
                      "<br><strong>Votos: </strong>", 
                      argentina$`FRENTE DE TODOS`,
                      " <strong>%</strong>")
pal <- colorQuantile(palette = "Blues", domain = NULL, n = 5)
                      
fdt <- leaflet(data = argentina) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(`FRENTE DE TODOS`), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = contenido_popup)
fdt

## Mapa NOS
contenido_popup <- paste0("<strong>Provincia: </strong>", 
                      argentina$NAME_1, 
                      "<br><strong>Votos: </strong>", 
                      argentina$`FRENTE NOS`,
                      " <strong>%</strong>")
pal <- colorQuantile(palette = "Purples", domain = NULL, n = 5)
                      
nos <- leaflet(data = argentina) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(`FRENTE NOS`), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = contenido_popup)
nos
              
## Mapa FIT
contenido_popup <- paste0("<strong>Provincia: </strong>", 
                      argentina$NAME_1, 
                      "<br><strong>Votos: </strong>", 
                      argentina$`FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD`,
                      " <strong>%</strong>")
pal <- colorQuantile(palette = "Reds", domain = NULL, n = 5)
                      
fit <- leaflet(data = argentina) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(`FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD`), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = contenido_popup)
fit              
