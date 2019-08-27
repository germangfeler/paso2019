##----------------------------------------------------------------------
## Analisis PASO - Calculo de totales y porcentajes
## Analisis: German Gonzalez
## Fecha creacion: 13/08/2019
##----------------------------------------------------------------------

## Cargamos los paquetes 
library("tidyverse")

## Seteamos el directorio de trabajo
setwd("/home/paso2019/120819-054029/")

## Cargamos el Rdata creado en el script 01
load("tabla_paso.Rdata")

##----------- Por provincia ----------

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
xprov

#write_excel_csv(xprov, "xprov.csv")


##------ Por seccion ------
   
## Sumamos las mesas a nivel de seccion
xsec <- mesas %>% select(PROV, SECCION, NOMBRE_AGRUPACION, VOTOS_LISTA) %>% 
          group_by(PROV, SECCION, NOMBRE_AGRUPACION) %>% 
          summarize(VOTOS=sum(VOTOS_LISTA)) %>%
          arrange(PROV, SECCION, desc(VOTOS)) 
          
xsec

## Calculamos porcentajes
xsec %<>% 
    group_by(PROV, SECCION) %>%
    mutate(PCT=round(VOTOS / sum(VOTOS) * 100,2)) %>%
    ungroup()    
  
#write_excel_csv(xsec, "xsec.csv")

## Se puede filtrar por localidad de esta manera
filter(xsec, SECCION == "ROSARIO")


##------ Por circuito ------
   
## Sumamos las mesas a nivel de seccion
xcir <- mesas %>% select(PROV, SECCION, CIRCUITO, NOMBRE_AGRUPACION, VOTOS_LISTA) %>% 
          group_by(PROV, SECCION, CIRCUITO, NOMBRE_AGRUPACION) %>% 
          summarize(VOTOS=sum(VOTOS_LISTA)) %>%
          arrange(PROV, SECCION, CIRCUITO, desc(VOTOS)) 
          
xcir

## Calculamos porcentajes
xcir %<>% 
    group_by(PROV, SECCION, CIRCUITO) %>%
    mutate(PCT=round(VOTOS / sum(VOTOS) * 100,2)) %>%
    ungroup()    
  
#write_excel_csv(xcir, "xsec.csv")

## Se puede filtrar por localidad de esta manera
filter(xcir, SECCION == "ROSARIO")

##------ Guardamos en archivo ------

save(xprov, xsec, xcir, file="tablas_paso_pct.Rdata")


