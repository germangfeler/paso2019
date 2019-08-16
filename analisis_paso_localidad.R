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

## Descripcion ergiones 
desc_reg <- read_delim("descripcion_regiones.dsv", delim="|")
desc_reg
glimpse(desc_reg)

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
                   select(CODIGO_DISTRITO, CODIGO_SECCION, CODIGO_AGRUPACION, NOMBRE_CATEGORIA, VOTOS_LISTA)

## Agregamos descripcion de agrup politica
agrup <- desc_post %>%
   select(CODIGO_AGRUPACION, NOMBRE_AGRUPACION) %>%
   distinct()
                   
mesas <- left_join(mesas, agrup, by="CODIGO_AGRUPACION") %>%
                   select(CODIGO_DISTRITO, CODIGO_SECCION, NOMBRE_CATEGORIA, NOMBRE_AGRUPACION, VOTOS_LISTA)

## Agregamos nombre de provincia
mesas <- left_join(mesas, desc_prov, by="CODIGO_DISTRITO") %>%
                   select(PROV, CODIGO_SECCION, NOMBRE_CATEGORIA, NOMBRE_AGRUPACION, VOTOS_LISTA)

                   
## Agregamos localidad
reg <- desc_reg %>%
   select(CODIGO_REGION, NOMBRE_REGION) %>%
   rename(CODIGO_SECCION = CODIGO_REGION) %>%
   distinct()
                   
mesas <- left_join(mesas, reg, by="CODIGO_SECCION") %>%
                   select(PROV, NOMBRE_REGION, NOMBRE_CATEGORIA, NOMBRE_AGRUPACION, VOTOS_LISTA)
                   
mesas <- mesas %>% filter(NOMBRE_CATEGORIA == "Presidente y Vicepresidente de la República")

glimpse(mesas)


##----------- Algunos calculos simples ----------

## Sumamos las mesas a nivel de region
xreg <- mesas %>% select(PROV, NOMBRE_REGION, NOMBRE_AGRUPACION, VOTOS_LISTA) %>% 
          group_by(PROV, NOMBRE_REGION, NOMBRE_AGRUPACION) %>% 
          summarize(VOTOS=sum(VOTOS_LISTA)) %>%
          arrange(PROV, NOMBRE_REGION, desc(VOTOS)) 
          
xreg

## Calculamos porcentajes
xreg %<>% 
    group_by(PROV, NOMBRE_REGION) %>%
    mutate(PCT=round(VOTOS / sum(VOTOS) * 100,2)) %>%
    ungroup()    
  
#write_excel_csv(xreg, "xprov.csv")

## Se puede filtrar por localidad de esta manera
filter(xreg, NOMBRE_REGION == "PARANÁ")




