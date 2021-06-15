introUI <- function(id) {
  ns <- NS(id)
  div(style = "margin-left:15px; margin-right:15px;",
      shinydashboard::box(width = 12, 
      h1("escovid19data", style = "text-align:center;"),
      p("Este es un proyecto colaborativo para recopilar 
        datos sobre COVID-19 en España por provincias. Más información y datos", 
        a(href = "https://github.com/montera34/escovid19data", target = "blank",
          "aquí."), style = "padding:15px;text-align:center;")
      ),
      box(width = 6, 
          title = h2("Casos nuevos e IA 14 a nivel nacional",style = "text-align:center;"),style = "text-align:right;",
          uiOutput(ns("ia_date"),style = "text-align:center;"),
          uiOutput(ns("ia"),
                          style = "display: inline;
                          float: left;
                          text-align: center;
                          margin-left: 35px;
                          margin-bottom: 0px;"
                   ),
          actionButton(ns("casos_go"), "Ir a análisis completo por provincias"),
          plotlyOutput(ns("graph"),width = "100%")),
         
      box(width = 6, title = h2("% de población vacunada",style = "text-align:center;"),style = "text-align:right;",
          uiOutput(ns("vacu_date"),style = "text-align:center;"),
          uiOutput(ns("vacu"),
                   style = "display: inline;
                          float: left;
                          text-align: center;
                          margin-left: 35px;
                          margin-bottom: 0px;"
          ),
          actionButton(ns("vac_go"), "Ir a análisis completo por CCAA"),
          plotlyOutput(ns("graph_vac"),width = "100%")),
      box(width = 6, title = h2("Fallecidos diarios",style = "text-align:center;"),style = "text-align:right;",
          uiOutput(ns("falle_date"),style = "text-align:center;"),
          uiOutput(ns("falle"),
                   style = "display: inline;
                          float: left;
                          text-align: center;
                          margin-left: 35px;
                          margin-bottom: 0px;"
          ),
          actionButton(ns("falle_go"), "Ir a análisis completo por provincias"),
          plotlyOutput(ns("graph_falle"),width = "100%")),
      box(width = 6, title = h2("Hospitalizados e ingresados en UCI",style = "text-align:center;"),style = "text-align:right;",
          uiOutput(ns("hospi_date"),style = "text-align:center;"),
          uiOutput(ns("hospi"),
                   style = "display: inline;
                          float: left;
                          text-align: center;
                          margin-left: 35px;
                          margin-bottom: 0px;"
          ),
          actionButton(ns("hospi_go"), "Ir a análisis completo por provincias"),


          plotlyOutput(ns("graph_hospi"),width = "100%"))
      
      
  )
}

introServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      cases_data_chart <- cases_data %>% 
        group_by(date) %>% 
        summarise(`Casos Nuevos` = sum(`Casos Nuevos`),
                  `Casos Totales` = sum(`Casos Totales`),
                  poblacion = sum(poblacion)) %>% 
        ungroup() %>% 
        arrange(date) %>% 
        mutate(`IA 14` = round((`Casos Totales`-lag(`Casos Totales`,14))/poblacion*100000, digits = 0)) %>% 
        filter(poblacion > 46000000)
      
      
      output$graph <- renderPlotly({
        
        

        plot_ly(cases_data_chart, x = ~date, y = ~`Casos Nuevos`, type = "bar", name = "Casos Nuevos",
                hovertemplate = paste("%{x}: <br> Casos Nuevos: %{y}<extra></extra>")) %>%
          add_trace(x = ~date, y = ~`IA 14`, type = "scatter", mode = "lines", yaxis = "y2", name = "IA 14",
                    hovertemplate = paste("%{x}: <br> IA 14: %{y}<extra></extra>")) %>%
          layout(yaxis2 = list(title = "",overlaying = "y", side = "right",rangemode = "tozero"),
                 xaxis= list(title = "",autotick = T, dtick = 30),
                 yaxis = list( title = ""),
                 legend = list(orientation = "h",   # show entries horizontally
                               xanchor = "center",  # use center of legend as anchor
                               x = 0.5,
                               y = -0.2),
                 margin = list(r = 50)) %>% 
          config(displayModeBar = F)
      })
      
      output$ia_date <- renderUI({
        div(
        p(paste(unique(max(cases_data$date))), 
          style = "text-align:center; ")
        )
        
      })
      
      output$vacu_date <- renderUI({
        div(
          p(paste(unique(max(cases_data$date))), 
            style = "text-align:center; ")
        )
        
      })
      
      
      output$ia <- renderUI({
        div(style = "text-align:left;",
        p(paste("IA 14", cases_data_chart %>% filter(date == max(date)) %>% 
                                                       select(`IA 14`) %>% 
                                                         unique() %>% 
                                                         pull(), sep = ": "), 
                    style = "padding-botom: 0px; margin-bottom: 0px; color: #FF7F0E;
          font-weight:bold;"
        ),
        p(paste("Casos nuevos", cases_data_chart %>% filter(date == max(date)) %>% 
                  select(`Casos Nuevos`) %>% 
                  unique() %>% 
                  pull(), sep = ": "), 
          style = "padding-botom: 0px; margin-bottom: 0px; color: #1f77b4;
          font-weight:bold;")
        )
        
      })
      

      vacu_data_chart <- vac_data %>% 
        group_by(date) %>% 
        summarise(`Total 1 dosis` = sum(`Total 1 dosis`),
                  `Total pauta completada` = sum(`Total pauta completada`),
                  poblacion = sum(poblacion)) %>% 
        ungroup() %>% 
        mutate(`% 1 dosis` = round(`Total 1 dosis`/poblacion*100, digits = 1)) %>% 
        mutate(`% pauta completada` = round(`Total pauta completada`/poblacion*100, digits = 1)) %>% 
        arrange(date) %>% 
        gather(key, value,`% 1 dosis`, `% pauta completada`)
      
      
      output$graph_vac <- renderPlotly({
        

      
        plot_ly(data = vacu_data_chart, x = ~date, y = ~value, type = 'scatter',
                mode = 'lines', fill = ~key, color= ~key, colors = "Paired",
                text = ~key,
                hovertemplate = paste("%{x}: <br> %{text}: %{y}<extra></extra>")) %>%
          layout(xaxis= list(title = ""),
                 yaxis = list( title = ""),
                 legend = list(orientation = "h",   # show entries horizontally
                               xanchor = "center",  # use center of legend as anchor
                               x = 0.5,
                               y = -0.2),
                 margin = list(r = 50)) %>% 
          config(displayModeBar = F)
        
        
      })
      
      
      output$vacu <- renderUI({
        div(style = "text-align:left;",
            p(paste("% 1 dosis", vacu_data_chart %>% filter(date == max(date)) %>% 
                      filter(key == "% 1 dosis") %>% 
                      select(`value`) %>% 
                      unique() %>% 
                      pull(), sep = ": "), 
              style = "padding-botom: 0px; margin-bottom: 0px; color: #a7cee3;
          font-weight:bold;"
            ),
            p(paste("% pauta completada", vacu_data_chart %>% filter(date == max(date)) %>% 
                      filter(key == "% pauta completada") %>% 
                      select(value) %>% 
                      unique() %>% 
                      pull(), sep = ": "), 
              style = "padding-botom: 0px; margin-bottom: 0px; color: #2078b4;
          font-weight:bold;")
        )
        
      })
      
      falle_data_chart <- deceased_data %>% 
        mutate(`Fallecidos diarios` = replace_na(`Fallecidos diarios`,0)) %>% 
        group_by(date) %>% 
        summarise(`Fallecidos diarios` = sum(`Fallecidos diarios`),
                  poblacion = sum(poblacion)) %>% 
        ungroup() %>% 
        arrange(date) %>% 
        mutate(`Media móvil semanal` = round(lag(rollmean(`Fallecidos diarios`, 7, na.pad = T, align = "right"), 0), digits = 0)) %>% 
        filter(date != "2020-05-03")
      
      
      output$graph_falle <- renderPlotly({
        
        
        plot_ly(falle_data_chart, x = ~date, y = ~`Fallecidos diarios`, type = "bar", name = "Fallecidos diarios",
                hovertemplate = paste("%{x}: <br> Fallecidos diarios: %{y}<extra></extra>")) %>%
          add_trace(x = ~date, y = ~`Media móvil semanal`, type = "scatter", mode = "lines", yaxis = "y", name = "Media móvil semanal",
                    hovertemplate = paste("%{x}: <br> Media móvil semanal: %{y}<extra></extra>")) %>%
          layout(#yaxis2 = list(title = "",overlaying = "y", side = "right",rangemode = "tozero"),
                 xaxis= list(title = "",autotick = T, dtick = 30),
                 yaxis = list( title = ""),
                 legend = list(orientation = "h",   # show entries horizontally
                               xanchor = "center",  # use center of legend as anchor
                               x = 0.5,
                               y = -0.2),
                 margin = list(r = 50)) %>% 
          config(displayModeBar = F)
      })
      
      
      
      output$falle_date <- renderUI({
        div(
          p(paste(unique(max(falle_data_chart$date))), 
            style = "text-align:center; ")
        )
        
      })
      
      
      output$falle <- renderUI({
        div(style = "text-align:left;",
            p(paste("Fallecidos media"), 
              style = "padding-botom: 0px; margin-bottom: 0px; color: #FF7F0E;
          font-weight:bold;"
            ),
            p(paste("móvil semanal", falle_data_chart %>% filter(date == max(date)) %>% 
                      select(`Media móvil semanal`) %>% 
                      unique() %>% 
                      pull(), sep = ": "), 
              style = "padding-botom: 0px; margin-bottom: 0px; color: #FF7F0E;
          font-weight:bold;"
            )
            
        )
        
      })
      
      hospi_data_chart <- hospi_data %>% 
        group_by(date) %>% 
        summarise(`Hospitalizados` = sum(`Hospitalizados`),
                  `UCI` = sum(`UCI`),
                  poblacion = sum(poblacion)) %>% 
        ungroup() %>% 
        arrange(date)
      
      output$graph_hospi <- renderPlotly({
        
        

        
        
        plot_ly(hospi_data_chart, x = ~date, y = ~`Hospitalizados`, type = "bar", name = "Hospitalizados",
                hovertemplate = paste("%{x}: <br> Hospitalizados: %{y}<extra></extra>")) %>%
          add_trace(x = ~date, y = ~`UCI`, type = "bar", yaxis = "y", name = "UCI",
                    hovertemplate = paste("%{x}: <br> UCI: %{y}<extra></extra>")) %>%
          layout(barmode = 'overlay',#yaxis2 = list(title = "",overlaying = "y", side = "right",rangemode = "tozero"),
                 xaxis= list(title = "",autotick = T, dtick = 30),
                 yaxis = list( title = ""),
                 legend = list(orientation = "h",   # show entries horizontally
                               xanchor = "center",  # use center of legend as anchor
                               x = 0.5,
                               y = -0.2),
                 margin = list(r = 50)) %>% 
          config(displayModeBar = F)
      })
      
      output$hospi_date <- renderUI({
        div(
          p(paste(hospi_data_chart  %>% filter(date != max(date)) %>% 
                    #filter(date != max(date)) %>% 
                    filter(date == max(date)) %>% 
                    select(`date`) %>% 
                    unique() %>% 
                    pull()), 
            style = "text-align:center; ")
        )
        
      })
      
      
      output$hospi <- renderUI({
        div(style = "text-align:left;",
            p(paste("Hospitalizados", hospi_data_chart %>% filter(date != max(date)) %>% 
                      #filter(date != max(date)) %>% 
                      filter(date == max(date)) %>% 
                      select(`Hospitalizados`) %>% 
                      unique() %>% 
                      pull(), sep = ": "), 
              style = "padding-botom: 0px; margin-bottom: 0px; color: #1f77b4;
          font-weight:bold;"
            ),
            p(paste("UCI", hospi_data_chart %>% filter(date != max(date)) %>% 
                      filter(date != max(date)) %>% 
                      filter(date == max(date)) %>% 
                      select(`UCI`) %>% 
                      unique() %>% 
                      pull(), sep = ": "), 
              style = "padding-botom: 0px; margin-bottom: 0px; color: #FF7F0E;
          font-weight:bold;")
        )
        
      })
      
    }
  )
}
