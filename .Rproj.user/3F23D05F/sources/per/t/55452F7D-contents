##Ajustes Iniciales
rm(list=ls())
options(scipen=999)


##Cargar paquetes
pacman::p_load(tidyverse, #Conjunto de paquetes, sobre todo dplyr y ggplot2
               car, #Para recodificar
               haven,
               summarytools, #Para descriptivos
               sjmisc,
               psych     # para Alfa de Chronbach
)

##Cargar Base
load(url("https://github.com/cursos-metodos-facso/investigacion-cuantitativa/raw/main/files/data/casen2022.RData")) #Cargar base de datos

##Descriptivos
view(dfSummary(casen2022, headings=FALSE, graph.col = FALSE))

indicadores2014 <- casen2022 %>% select(asistencia, 
                                        rezago, 
                                        escolaridad, 
                                        malnutricion, 
                                        sist_salud, 
                                        atencion, 
                                        ocupacion, 
                                        seg_social, 
                                        jubilacion, 
                                        hacinamiento, 
                                        estado_vivienda=vivienda, 
                                        serv_basicos)  %>% 
  na.omit() %>% # Eliminar Na's
  mutate_all(~(as.numeric(.))) # Convertimos todas las variables a numéricas

indicadores2014 = indicadores2014 %>% 
  rowwise() %>%
  mutate(educ = mean(c(asistencia, rezago, escolaridad)),
         salud = mean(c(malnutricion, sist_salud, atencion)),
         trabajo= mean(c(ocupacion, seg_social, jubilacion)),
         vivienda= mean(c(hacinamiento, estado_vivienda, serv_basicos))) %>% 
  ungroup()

indicadores2014 = indicadores2014 %>% 
  rowwise() %>%
  mutate(pobreza = mean(c(educ, salud, trabajo, vivienda))) %>% 
  ungroup()

indicadores2014 %>% select(pobreza) %>% head(10) # Primeros 10 casos

summary(indicadores2014$pobreza) # Resumen

indicadores2014 <- indicadores2014 %>% mutate(pobreza = case_when(pobreza>=0.25~"si",
                                                                  pobreza<0.25~"no")
)
prop.table(table(indicadores2014$pobreza))*100

indicadores2016 <- casen2022 %>% select(asistencia, 
                                        rezago, 
                                        escolaridad, 
                                        malnutricion, 
                                        sist_salud, 
                                        atencion, 
                                        ocupacion, 
                                        seg_social, 
                                        jubilacion, 
                                        habitabilidad, 
                                        serv_basicos,
                                        entorno,
                                        ap_part_social,
                                        trato,
                                        seguridad,
                                        area,
                                        region) %>% 
  na.omit() %>% # Eliminar Na's
  mutate_all(~(as.numeric(.))) # Convertimos todas las variables a numéricas

indicadores2016 = indicadores2016 %>% 
  rowwise() %>%
  mutate(educ = mean(c(asistencia, rezago, escolaridad)),
         salud = mean(c(malnutricion, sist_salud, atencion)),
         trabajo= mean(c(ocupacion, seg_social, jubilacion)),
         vivienda= mean(c(habitabilidad, serv_basicos, entorno)),
         redes_cohesion= mean(c(ap_part_social, trato, seguridad))) %>% 
  ungroup()

indicadores2016 = indicadores2016 %>% 
  rowwise() %>%
  mutate(pobreza_pond = (educ*22.5) + (salud*22.5) + (trabajo*22.5) + (vivienda*22.5) + (redes_cohesion*10)) %>%  
  ungroup()

indicadores2016 %>% select(pobreza_pond) %>% head(10) # Primeros 10 casos

indicadores2016 <- indicadores2016 %>% mutate(pobreza = case_when(pobreza_pond>=22.5~"si",
                                                                  pobreza_pond<22.5~"no")
)

prop.table(table(indicadores2016$pobreza))*100

prop.table(table(indicadores2016$region, indicadores2016$pobreza), margin = 1)

prop.table(table(indicadores2016$area, indicadores2016$pobreza), margin = 1)

