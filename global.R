library(shiny)
library(shinythemes)
library(shinydashboard)
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
  rename(`Casos Totales` = cases_accumulated) %>% 
  filter(date >= "2020-02-20")



deceased_data <- data_provinces %>% 
  select(1:4, cases_accumulated, deceased, poblacion) %>% 
  group_by(province) %>% 
  arrange(date) %>% 
  fill(cases_accumulated) %>% 
  mutate(`Fallecidos diarios` = deceased-lag(deceased,1)) %>% 
  mutate(`Fallecidos por 100.000 h` = (deceased/poblacion*100000)) %>% 
  mutate(`Tasa de mortalidad %` = deceased/poblacion*100) %>% 
  mutate(`Media móvil semanal` = round(lag(rollmean(`Fallecidos diarios`, 7, na.pad = T, align = "right"), 0), digits = 0)) %>% 
  rename(`Total fallecidos` = deceased) %>% 
  filter(date >= "2020-02-20")

hospi_data <- data_provinces %>% 
  select(1:4, Hospitalizados = num_hosp,
         `Hospitalizados acumulados` = num_hosp_cum, 
         `UCI` = num_uci, `UCI Acumulados` = num_uci_cum, poblacion) %>% 
  group_by(province) %>% 
  arrange(date) %>% 
  mutate(`Hospitalizados media móvil semanal` = round(lag(rollmean(Hospitalizados, 7, na.pad = T, align = "right"), 0), digits = 0)) %>% 
  mutate(`UCI media móvil semanal` = round(lag(rollmean(UCI, 7, na.pad = T, align = "right"), 0), digits = 0)) %>% 
  filter(date >= "2020-02-20")

ccaa_pob <- read.csv("https://raw.githubusercontent.com/montera34/escovid19data/master/data/output/covid19-ccaa-spain_consolidated.csv") %>% 
  select(ccaa, poblacion) %>% 
  unique() %>% 
  mutate(ccaa = gsub("Asturias, Principado de", "Asturias", ccaa)) %>% 
  mutate(ccaa = gsub("Balears, Illes", "Baleares", ccaa)) %>% 
  mutate(ccaa = gsub("Castilla y León", "Castilla y Leon", ccaa)) %>% 
  mutate(ccaa = gsub("Castilla - La Mancha", "Castilla La Mancha", ccaa)) %>% 
  mutate(ccaa = gsub("Comunitat Valenciana", "C. Valenciana", ccaa)) %>% 
  mutate(ccaa = gsub("Madrid, Comunidad de", "Madrid", ccaa)) %>% 
  mutate(ccaa = gsub("Rioja, La", "La Rioja", ccaa)) %>% 
  mutate(ccaa = gsub("País Vasco", "País Vasco", ccaa)) %>% 
  mutate(ccaa = gsub("Murcia, Región de", "Murcia", ccaa)) %>% 
  mutate(ccaa = gsub("Navarra, Comunidad Foral de", "Navarra", ccaa))
  
  

vac_data <- read_csv("https://raw.githubusercontent.com/montera34/escovid19data/master/data/original/vacunas/estado_vacunacion_.csv") %>% 
  mutate(date = as.Date(date_pub,format='%d/%m/%y'),
         `Total pauta completada` = replace_na(`Total pauta completada`,0)) %>% 

  mutate(`Total 1 dosis` = `Dosis administradas`-`Total pauta completada`) %>% 
  left_join(ccaa_pob) %>% 
  filter(ccaa != "Totales") %>% 
  group_by(ccaa) %>% 
  arrange(date) %>% 
  mutate(`Administradas diarias` = `Dosis administradas`-lag(`Dosis administradas`,1)) %>% 
  mutate(`Administradas media móvil semanal` = round(lag(rollmean(`Administradas diarias`, 7, na.pad = T, align = "right"), 0), digits = 0)) %>% 
  ungroup() %>% 
  select(date,ccaa, `Total 1 dosis`, `Total pauta completada`,`Administradas diarias`,
         `Administradas media móvil semanal`,poblacion) %>% 
  filter(ccaa != "Fuerzas Armadas") %>% 
  mutate(`% 1 dosis` = round(`Total 1 dosis`/poblacion*100, digits = 1)) %>% 
  mutate(`% pauta completada` = round(`Total pauta completada`/poblacion*100, digits = 1))

vac_etarios_1 <- read_csv("https://raw.githubusercontent.com/montera34/escovid19data/master/data/original/vacunas/estado_vacunacion_etarios_1dosis_.csv") %>% 
  mutate(date = as.Date(date_pub,format='%d/%m/%y')) %>% 
  select(date, ccaa, `80+` = `%`,  `70-79` = `%.1`, `60-69` = `%.2`, 
         `50-59` = `%.3`, `25-49` = `%.4`,`18-24` = `%.5`,
         `16-17` = `%.6`) %>%
  filter(ccaa != "Fuerzas Armadas") %>% 
  mutate_at(vars(3:9), list(as.numeric)) %>% 
  mutate_at(vars(3:9), ~.*100) %>% 
  #mutate_at(vars(3:9), list(round)) %>% 
  mutate(dosis = "primera dosis")

vac_etarios_2 <- read_csv("https://raw.githubusercontent.com/montera34/escovid19data/master/data/original/vacunas/estado_vacunacion_etarios_pauta_completa_.csv") %>% 
  mutate(date = as.Date(date_pub,format='%d/%m/%y')) %>% 
  select(date, ccaa, `80+` = `%`,  `70-79` = `%.1`, `60-69` = `%.2`, 
         `50-59` = `%.3`, `25-49` = `%.4`,`18-24` = `%.5`,
         `16-17` = `%.6`) %>% 
  filter(ccaa != "Fuerzas Armadas") %>% 
  mutate_at(vars(3:9), list(as.numeric)) %>% 
  mutate_at(vars(3:9), ~.*100) %>% 
  #mutate_at(vars(3:9), list(round)) %>% 
  mutate(dosis = "pauta completa")

vac_etarios <- vac_etarios_2 %>% 
  rbind(vac_etarios_1)


# jsCodeEx <- "shinyjs.activateTab = function(name){
#                   setTimeout(function(){
#                   $('a[href$=' + '\"#shiny-tab-' + name + '\"' + ']').addClass('active')
#                   
#                   }, 200);
#                   }"


# 
# vaccines_data_1_dosis <- read_csv("https://raw.githubusercontent.com/montera34/escovid19data/master/data/original/vacunas/estado_vacunacion_etarios_1dosis_.csv") %>% 
#   select(date_pub,ccaa,contains("%")) %>% 
#   gather(grupo_edad, porcen_vac1, 3:10) %>% 
#   mutate(date = as.Date(date_pub,
#                         format = "%d/%m/%y")) %>% 
#   filter(date == max(date))
# 
# 
# %>% 
#   gather(grupo_Edad, primera_Dosis, c(contains("dosis"))) %>% 
#   gather(grupo_edadPob, poblacion, contains("Población")) %>% 
#   unique() %>%  
#   mutate(grupo_edadPob = gsub("Población INE","",grupo_edadPob)) %>% 
#   mutate(grupo_edadPob = gsub("Personas con al menos 1 dosis","",grupo_edadPob)) %>% 
#   mutate(date = as.Date(date_pub,
#           format = "%d/%m/%y")) %>% 
#   filter(date == max(date)) %>% 
#   select(date,ccaa,grupo_edadPob,primera_Dosis,poblacion) %>% 
#   mutate(poblacion = as.numeric(poblacion)) %>% 
#   mutate(porcentaje = poblacion/primera_Dosis*100)
#   
#   
# 
# vaccines_data_2_dosis <- read_csv("https://raw.githubusercontent.com/montera34/escovid19data/master/data/original/vacunas/estado_vacunacion_etarios_pauta_completa_.csv") %>% 
#   select(-contains("%")) %>% 
#   gather(grupo_Edad, primera_Dosis, contains("pauta")) %>% 
#   gather(grupo_edadPob, poblacion, contains("Población")) %>% 
#   unique()
# 
# 
