##Ajustes Iniciales
rm(list=ls())
options(scipen=999)

#Cargar paquetes
install.packages("pacman")
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)

#Cargar base
load(url("https://github.com/Kevin-carrasco/metod1-MCS/raw/main/files/data/external_data/latinobarometro2020.RData"))


dim(latinobarometro2020) # dimension de la base
View(latinobarometro2020)


#Selección de variables
find_var(data = latinobarometro2020,"Confianza")
proc_data <- latinobarometro2020 %>% select(p13st_e, # Confianza en el Gobierno
                                            p13st_d, # Confianza en el congreso
                                            p13st_f, # Confianza en el Poder Judicial
                                            p13st_g, # Confianza en los partidos políticos
                                            reeduc_1,# nivel educacional
                                            sexo,# sexo
                                            edad,# edad
                                            idenpa) # pais 

names(proc_data)

sjlabelled::get_label(proc_data)

proc_data <- proc_data %>% dplyr::filter(idenpa==152)

#Procesamiento de variables
frq(proc_data$p13st_e)

#4.1 confianza en el gob
proc_data$p13st_e <- recode(proc_data$p13st_e, "c(-2,-1)=NA")
proc_data$p13st_d <- recode(proc_data$p13st_d, "c(-2,-1)=NA")
proc_data$p13st_f <- recode(proc_data$p13st_f, "c(-2,-1)=NA")
proc_data$p13st_g <- recode(proc_data$p13st_g, "c(-2,-1)=NA")

proc_data <- proc_data %>% set_na(., na = c(-2, -1))

proc_data$p13st_e <- recode(proc_data$p13st_e, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_d <- recode(proc_data$p13st_d, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_f <- recode(proc_data$p13st_f, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_g <- recode(proc_data$p13st_g, "1=3; 2=2; 3=1; 4=0")

proc_data <- proc_data %>% rename("conf_gob"=p13st_e, # Confianza en el gobierno
                                  "conf_cong"=p13st_d, # Confianza en el congreso
                                  "conf_jud"=p13st_f, # Confianza en el Poder Judicial
                                  "conf_partpol"=p13st_g) # Confianza en los partidos políticos 

proc_data$conf_gob <- set_label(x = proc_data$conf_gob,label = "Confianza: Gobierno")
get_label(proc_data$conf_gob)

proc_data$conf_cong  <- set_label(x = proc_data$conf_cong, label = "Confianza: Congreso")
get_label(proc_data$conf_cong)

proc_data$conf_jud  <- set_label(x = proc_data$conf_jud, label = "Confianza: Poder judicial")
get_label(proc_data$conf_jud)

proc_data$conf_partpol  <- set_label(x = proc_data$conf_partpol, label = "Confianza: Partidos politicos")
get_label(proc_data$conf_partpol)

proc_data$conf_inst <- (proc_data$conf_gob+proc_data$conf_cong+proc_data$conf_jud+proc_data$conf_partpol)
summary(proc_data$conf_inst)

get_label(proc_data$conf_inst)

proc_data$conf_inst  <- set_label(x = proc_data$conf_inst, label = "Confianza en instituciones")

frq(proc_data$conf_gob)
frq(proc_data$conf_cong)
frq(proc_data$conf_inst)

proc_data$conf_gob <- set_labels(proc_data$conf_gob,
                                 labels=c( "Ninguna"=0,
                                           "Poca"=1,
                                           "Algo"=2,
                                           "Mucha"=3))

proc_data$conf_cong <- set_labels(proc_data$conf_cong,
                                  labels=c( "Ninguna"=0,
                                            "Poca"=1,
                                            "Algo"=2,
                                            "Mucha"=3))

proc_data$conf_jud <- set_labels(proc_data$conf_jud,
                                 labels=c( "Ninguna"=0,
                                           "Poca"=1,
                                           "Algo"=2,
                                           "Mucha"=3))

proc_data$conf_partpol <- set_labels(proc_data$conf_partpol,
                                     labels=c( "Ninguna"=0,
                                               "Poca"=1,
                                               "Algo"=2,
                                               "Mucha"=3))

frq(proc_data$conf_gob)
frq(proc_data$conf_cong)

#4.2 educación
frq(proc_data$reeduc_1)

# recodificacion usando funcion 'recode' de la libreria car
proc_data$reeduc_1 <- car::recode(proc_data$reeduc_1, "c(1,2,3)=1; c(4,5)=2; c(6,7)=3")

frq(proc_data$reeduc_1)

proc_data$reeduc_1 <- set_labels(proc_data$reeduc_1,
                                 labels=c( "Educacion basica"=1,
                                           "Educacion media"=2,
                                           "Educacion superior"=3))
proc_data <- rename(proc_data,"educacion"=reeduc_1)

get_label(proc_data$educacion)
proc_data$educacion <- set_label(x = proc_data$educacion,label = "Educación")

#4.3 sexo
frq(proc_data$sexo)

proc_data$sexo <- car::recode(proc_data$sexo, "1=0;2=1")
proc_data$sexo <- set_labels(proc_data$sexo,
                             labels=c( "Hombre"=0,
                                       "Mujer"=1))
get_label(proc_data$sexo)
proc_data$sexo <- set_label(x = proc_data$sexo,label = "Sexo")
frq(proc_data$sexo)

#4.4 edad
frq(proc_data$edad)
get_label(proc_data$edad)
proc_data$edad <- set_label(x = proc_data$edad,label = "Edad")

#Generación de base procesada
proc_data <-as.data.frame(proc_data)
stargazer(proc_data, type="text")

save(proc_data,file = "C:/Users/Alumno/Desktop/trabajo1/Input/latinobarometro_proc.RData")

#descriptivos
proc_data %>% dplyr::group_by(sexo) %>% summarise(mean(conf_inst, na.rm=TRUE))
proc_data %>% dplyr::group_by(educacion) %>% summarise(mean(conf_inst, na.rm=TRUE))

install.packages("sjPlot")
library(sjPlot)

sjt.xtab(proc_data$educacion, proc_data$conf_inst, encoding = "UTF-8")
