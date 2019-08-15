##----------------------------------------------------------------------
## Analisis PASO
## Resultados descargados desde https://www.resultados2019.gob.ar/
## Analisis: German Gonzalez
## Fecha creacion: 13/08/2019
##----------------------------------------------------------------------

## Cargamos los paquetes 
library("raster")
library("leaflet")
library("tidyverse")

## Seteamos el directorio de trabajo
setwd("/home/datos/paso2019/120819-054029/")

##----------- Cargamos los datos ----------

## Mesas por lista
mesas_lista <- read_delim("mesas_totales_lista.dsv", delim="|")
mesas_lista
glimpse(mesas_lista)

## Descripcion postulaciones 
desc_post <- read_delim("descripcion_postulaciones.dsv", delim="|")
desc_post
glimpse(desc_post)

## Codigos provincias
desc_prov <- data.frame(CODIGO_DISTRITO = c("02", "04", "21", "01", "13", 
"23", "08", "17", "14", "05", "22", "06", "18", "16", "15", "10", 
"09", "07", "19", "11", "12", "03", "20", "24"), 
PROV = c("Buenos Aires", "Córdoba", "Santa Fe", "Ciudad de Buenos Aires","Mendoza", 
"Tucumán", "Entre Ríos", "Salta",  "Misiones", "Corrientes", "Santiago del Estero", 
"Chaco", "San Juan", "Río Negro", "Neuquén", "Jujuy", "Formosa","Chubut", 
 "San Luis", "La Pampa", "La Rioja", "Catamarca", "Santa Cruz", 
"Tierra del Fuego"))

##----------- Unimos dato con descripcion ----------

## Agregamos descripcion de categoria
categoria <- desc_post %>%
   select(CODIGO_CATEGORIA, NOMBRE_CATEGORIA) %>%
   distinct()

mesas <- left_join(mesas_lista, categoria, by="CODIGO_CATEGORIA") %>%
                   select(CODIGO_DISTRITO, CODIGO_AGRUPACION, NOMBRE_CATEGORIA, VOTOS_LISTA)

## Agregamos descripcion de agrup politica
agrup <- desc_post %>%
   select(CODIGO_AGRUPACION, NOMBRE_AGRUPACION) %>%
   distinct()
                   
mesas <- left_join(mesas, agrup, by="CODIGO_AGRUPACION") %>%
                   select(CODIGO_DISTRITO,NOMBRE_CATEGORIA, NOMBRE_AGRUPACION, VOTOS_LISTA)

## Agregamos nombre de provincia
mesas <- left_join(mesas, desc_prov, by="CODIGO_DISTRITO") %>%
                   select(PROV,NOMBRE_CATEGORIA, NOMBRE_AGRUPACION, VOTOS_LISTA)
                   
mesas <- mesas %>% filter(NOMBRE_CATEGORIA == "Presidente y Vicepresidente de la República")

glimpse(mesas)


##----------- Algunos calculos simples ----------

## Sumamos las mesas a nivel de provincia
xprov <- mesas %>% select(PROV, NOMBRE_AGRUPACION, VOTOS_LISTA) %>% 
          group_by(PROV, NOMBRE_AGRUPACION) %>% 
          summarize(VOTOS=sum(VOTOS_LISTA)) %>%
          arrange(PROV, desc(VOTOS)) 
          
xprov

## Calculamos porcentajes
xprov %<>% 
    group_by(PROV) %>%
    mutate(PCT=round(VOTOS / sum(VOTOS) * 100,2)) %>%
    ungroup()    
  
#write_excel_csv(xprov, "xprov.csv")

xprov_wide <- xprov %>%
   select(-VOTOS) %>%
   spread(NOMBRE_AGRUPACION, PCT)

 ##----------- Creamos mapa --------------


## Obtengo el mapa
argentina <- getData("GADM", country="Argentina", level=1)

## Incluyo los votos en la tabla del mapa
argentina@data <- left_join(argentina@data, xprov_wide, by=c("NAME_1" = "PROV"))


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
pal <- colorQuantile(palette = "Purples", domain = NULL, n = 5)
                      
fit <- leaflet(data = argentina) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(`FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD`), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = contenido_popup)
fit              
