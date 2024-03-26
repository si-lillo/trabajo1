rm(list=ls())
options(scipen=999)

install.packages("pacman")
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)

load(url("https://github.com/Kevin-carrasco/metod1-MCS/raw/main/files/data/external_data/latinobarometro2020.RData"))

