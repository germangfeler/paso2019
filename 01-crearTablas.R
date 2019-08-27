##----------------------------------------------------------------------
## Analisis PASO - creacion de tablas de datos a partir de los 
## resultados descargados desde https://www.resultados2019.gob.ar/
## Analisis: German Gonzalez
## Fecha creacion: 13/08/2019
##----------------------------------------------------------------------

## Cargamos los paquetes 
library("tidyverse")

## Seteamos el directorio de trabajo
setwd("/home/paso2019/120819-054029/")

##----------- Cargamos los datos ----------

## Mesas por lista
mesas_lista <- read_delim("mesas_totales_lista.dsv", delim="|")
mesas_lista
glimpse(mesas_lista)

## Descripcion postulaciones 
desc_post <- read_delim("descripcion_postulaciones.dsv", delim="|")
desc_post
glimpse(desc_post)

## Voto por categoria (blanco, impugnado, etc) 
vcat <- read_delim("mesas_totales.dsv", delim="|")
vcat
glimpse(vcat)

## Descripcion regiones 
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

## Agregamos descripcion de agrup politica
agrup <- desc_post %>%
   select(CODIGO_AGRUPACION, NOMBRE_AGRUPACION) %>%
   distinct()
                   
mesas <- left_join(mesas_lista, agrup, by="CODIGO_AGRUPACION") %>%
          select(-CODIGO_AGRUPACION, -CODIGO_LISTA)

## Agregamos el voto en blanco
vblanco <- vcat %>% 
   filter(CONTADOR == "VB") %>%
   rename("VOTOS_LISTA" = VALOR) %>%
   mutate(NOMBRE_AGRUPACION = "EN BLANCO") %>%
   select(-CONTADOR)

mesas <- bind_rows(mesas, vblanco) %>% arrange(CODIGO_MESA)

## Agregamos nombre de provincia
mesas <- left_join(mesas, desc_prov, by="CODIGO_DISTRITO") %>%
                   select(-CODIGO_DISTRITO)
glimpse(mesas)

## Agregamos seccion
sec <- desc_reg %>%
   select(CODIGO_REGION, NOMBRE_REGION) %>%
   rename(CODIGO_SECCION = CODIGO_REGION, SECCION=NOMBRE_REGION) %>%
   distinct()
                   
mesas <- left_join(mesas, sec, by="CODIGO_SECCION") %>%
                   select(-CODIGO_SECCION)

## Agregamos circuito
cir <- desc_reg %>%
   select(CODIGO_REGION, NOMBRE_REGION) %>%
   rename(CODIGO_CIRCUITO = CODIGO_REGION, CIRCUITO=NOMBRE_REGION) %>%
   distinct()
                   
mesas <- left_join(mesas, cir, by="CODIGO_CIRCUITO") %>%
                   select(-CODIGO_CIRCUITO)
                   
## Agregamos descripcion de categoria (presidente, diputados, etc)
categoria <- desc_post %>%
   select(CODIGO_CATEGORIA, NOMBRE_CATEGORIA) %>%
   distinct()

mesas <- left_join(mesas, categoria, by="CODIGO_CATEGORIA") %>%
                   select(PROV, SECCION, CIRCUITO, NOMBRE_CATEGORIA, NOMBRE_AGRUPACION, VOTOS_LISTA)

                   
## Nos quedamos solo con los resultados presidenciales                   
mesas <- mesas %>% filter(NOMBRE_CATEGORIA == "Presidente y Vicepresidente de la República")

glimpse(mesas)

##------ Guardamos en archivo ------

save(mesas, file="tabla_paso.Rdata")

