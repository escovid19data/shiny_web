library(shiny)
library(shinythemes)

library(shinyWidgets)

library(knitr)
library(markdown)
library(plotly)
library(DT)
library(tidyverse)
library(dplyr)
library(shinyjs)
library(zoo)

# rmdfiles <- c("RMarkdownFile.rmd")
# sapply(rmdfiles, knit, quiet = T)

data_provinces <- read.csv("https://raw.githubusercontent.com/montera34/escovid19data/master/data/output/covid19-provincias-spain_consolidated.csv")


cases_data <- data_provinces %>% 
  select(1:8, cases_accumulated, cases_accumulated_PCR, poblacion) %>% 
  mutate(cases_accumulated = if_else(is.na(cases_accumulated),cases_accumulated_PCR, cases_accumulated)) %>% 
  group_by(province) %>% 
  arrange(date) %>% 
  fill(cases_accumulated) %>% 
  mutate(`Casos Nuevos` = cases_accumulated-lag(cases_accumulated,1)) %>% 
  mutate(`IA 14` = (cases_accumulated-lag(cases_accumulated,14))/poblacion*100000) %>% 
  mutate(`IA 14` = ifelse(`IA 14`<= -0.001,NA,`IA 14`)) %>% 
  mutate(`IA 7` = (cases_accumulated-lag(cases_accumulated,7))/poblacion*100000) %>% 
  mutate(`IA 7` = ifelse(`IA 7`<= -0.001,NA,`IA 7`)) %>% 
  mutate(`Razón de Tasas` = round(`IA 14`/lag(`IA 14`,7), digits = 2)) %>% 
  mutate(`Media móvil semanal` = round(lag(rollmean(`Casos Nuevos`, 7, na.pad = T, align = "right"), 0), digits = 0)) %>% 
  rename(`Casos Totales` = cases_accumulated)



deceased_data <- data_provinces %>% 
  select(1:4, cases_accumulated, deceased, poblacion) %>% 
  group_by(province) %>% 
  arrange(date) %>% 
  fill(cases_accumulated) %>% 
  mutate(`Fallecidos diarios` = cases_accumulated-lag(cases_accumulated,1)) %>% 
  mutate(`Fallecidos por 100.000 h` = (deceased/poblacion*100000)) %>% 
  mutate(`Tasa de mortalidad %` = deceased/poblacion*100) %>% 
  mutate(`Media móvil semanal` = round(lag(rollmean(`Fallecidos diarios`, 7, na.pad = T, align = "right"), 0), digits = 0)) %>% 
  rename(`Total fallecidos` = deceased)
  

