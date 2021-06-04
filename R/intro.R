introUI <- function(id) {
  ns <- NS(id)
  div(style = "margin-left:15px; margin-right:15px;",
      shinydashboard::box(width = 12, 
      h1("escovid19data", style = "text-align:center;"),
      p("Este es un proyecto colaborativo para recopilar 
        datos sobre COVID-19 en España por provincias. Más info y datos", 
        a(href = "https://github.com/montera34/escovid19data", target = "blank",
          "aquí"), style = "padding:15px;text-align:center;")
      ),
      box(width = 6, title = h2("Casos nuevos e IA 14 a nivel nacional",style = "text-align:center;"),style = "text-align:right;",
          actionButton(ns("casos_go"), "Ir a análisis completo por provincias"),
          # radioGroupButtons(ns("select1"), choices = c("IA 14","Casos Nuevos", "Casos Totales"),
          #                   selected = "IA 14"),
          plotlyOutput(ns("graph"),width = "100%")),
      box(width = 6, title = h2("% de población vacunada",style = "text-align:center;"),style = "text-align:right;",
          actionButton(ns("vac_go"), "Ir a análisis completo por CCAA"),
          # radioGroupButtons(ns("select1"), choices = c("IA 14","Casos Nuevos", "Casos Totales"),
          #                   selected = "IA 14"),
          plotlyOutput(ns("graph_vac"),width = "100%")),
      box(width = 6, title = h2("Fallecidos diarios",style = "text-align:center;"),style = "text-align:right;",
          actionButton(ns("falle_go"), "Ir a análisis completo por provincias"),
          # radioGroupButtons(ns("select1"), choices = c("IA 14","Casos Nuevos", "Casos Totales"),
          #                   selected = "IA 14"),
          plotlyOutput(ns("graph_falle"),width = "100%")),
      box(width = 6, title = h2("Hospitalizados e ingresados en UCI",style = "text-align:center;"),style = "text-align:right;",
          actionButton(ns("hospi_go"), "Ir a análisis completo por provincias"),
          # radioGroupButtons(ns("select1"), choices = c("IA 14","Casos Nuevos", "Casos Totales"),
          #                   selected = "IA 14"),
          plotlyOutput(ns("graph_hospi"),width = "100%"))
      
      
  )
}

introServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$graph <- renderPlotly({
        
        
        cases_data_chart <- cases_data %>% 
          group_by(date) %>% 
          summarise(`Casos Nuevos` = sum(`Casos Nuevos`),
                    `Casos Totales` = sum(`Casos Totales`),
                    poblacion = sum(poblacion)) %>% 
          ungroup() %>% 
          arrange(date) %>% 
          mutate(`IA 14` = round((`Casos Totales`-lag(`Casos Totales`,14))/poblacion*100000, digits = 0)) %>% 
          filter(poblacion > 46000000)

        plot_ly(cases_data_chart, x = ~date, y = ~`Casos Nuevos`, type = "bar", name = "Casos Nuevos",
                hovertemplate = paste("%{x}: <br> Casos Nuevos: %{y}<extra></extra>")) %>%
          add_trace(x = ~date, y = ~`IA 14`, type = "scatter", mode = "lines", yaxis = "y2", name = "IA 14",
                    hovertemplate = paste("%{x}: <br> IA 14: %{y}<extra></extra>")) %>%
          layout(yaxis2 = list(title = "",overlaying = "y", side = "right",rangemode = "tozero"),
                 xaxis= list(title = "",autotick = F, dtick = 30),
                 yaxis = list( title = ""),
                 legend = list(orientation = "h",   # show entries horizontally
                               xanchor = "center",  # use center of legend as anchor
                               x = 0.5,
                               y = -0.2)) %>% 
          config(displayModeBar = F)
      })
      
      output$graph_vac <- renderPlotly({
        
        cases_data_chart <- vac_data %>% 
          group_by(date) %>% 
          summarise(`Total 1 dosis` = sum(`Total 1 dosis`),
                    `Total pauta completada` = sum(`Total pauta completada`),
                    poblacion = sum(poblacion)) %>% 
          ungroup() %>% 
          mutate(`% 1 dosis` = round(`Total 1 dosis`/poblacion*100, digits = 1)) %>% 
          mutate(`% pauta completada` = round(`Total pauta completada`/poblacion*100, digits = 1)) %>% 
          arrange(date) %>% 
          gather(key, value,`% 1 dosis`, `% pauta completada`)
      
        plot_ly(data = cases_data_chart, x = ~date, y = ~value, type = 'scatter',
                mode = 'lines', fill = ~key, color= ~key, colors = "Paired",
                text = ~key,
                hovertemplate = paste("%{x}: <br> %{text}: %{y}<extra></extra>")) %>%
          layout(xaxis= list(title = ""),
                 yaxis = list( title = ""),
                 legend = list(orientation = "h",   # show entries horizontally
                               xanchor = "center",  # use center of legend as anchor
                               x = 0.5,
                               y = -0.2)) %>% 
          config(displayModeBar = F)
        
        
      })
      
      output$graph_falle <- renderPlotly({
        
        
        cases_data_chart <- deceased_data %>% 
          mutate(`Fallecidos diarios` = replace_na(`Fallecidos diarios`,0)) %>% 
          group_by(date) %>% 
          summarise(`Fallecidos diarios` = sum(`Fallecidos diarios`),
                    poblacion = sum(poblacion)) %>% 
          ungroup() %>% 
          arrange(date) %>% 
          filter(date != "2020-05-03")
        
        
        plot_ly(cases_data_chart, x = ~date, y = ~`Fallecidos diarios`, type = "bar", name = "Fallecidos diarios",
                hovertemplate = paste("%{x}: <br> Fallecidos diarios: %{y}<extra></extra>")) %>%
          layout(xaxis= list(title = "",autotick = F, dtick = 30),
                 yaxis = list( title = ""),
                 legend = list(orientation = "h",   # show entries horizontally
                               xanchor = "center",  # use center of legend as anchor
                               x = 0.5,
                               y = -0.2)) %>% 
          config(displayModeBar = F)
      })
      
      output$graph_hospi <- renderPlotly({
        
        
        cases_data_chart <- hospi_data %>% 
          group_by(date) %>% 
          summarise(`Hospitalizados` = sum(`Hospitalizados`),
                    `UCI` = sum(`UCI`),
                    poblacion = sum(poblacion)) %>% 
          ungroup() %>% 
          arrange(date)
        
        
        plot_ly(cases_data_chart, x = ~date, y = ~`Hospitalizados`, type = "bar", name = "Hospitalizados",
                hovertemplate = paste("%{x}: <br> Hospitalizados: %{y}<extra></extra>")) %>%
          add_trace(x = ~date, y = ~`UCI`, type = "scatter", mode = "lines", yaxis = "y2", name = "UCI",
                    hovertemplate = paste("%{x}: <br> UCI: %{y}<extra></extra>")) %>%
          layout(yaxis2 = list(title = "",overlaying = "y", side = "right",rangemode = "tozero"),
                 xaxis= list(title = "",autotick = F, dtick = 30),
                 yaxis = list( title = ""),
                 legend = list(orientation = "h",   # show entries horizontally
                               xanchor = "center",  # use center of legend as anchor
                               x = 0.5,
                               y = -0.2)) %>% 
          config(displayModeBar = F)
      })
      
    }
  )
}