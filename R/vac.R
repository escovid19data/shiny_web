vacUI <- function(id) {
  ns <- NS(id)
  div(style = "margin-left:30px; margin-right:30px;",
      fluidRow(
        column(12,
               uiOutput(ns("title"))
               
        )
      ),
      fluidRow(
        column(12,
               column(3,
                      selectInput(ns("select1"), "Variable",
                                  choices = c("Total 1 dosis", "Total pauta completada",
                                              "% 1 dosis", "% pauta completada", "Administradas diarias",
                                              "Administradas media móvil semanal"))
               ),
               column(3,
                      selectInput(ns("select_province"),"CCAA", choices= c("Todas" = "",unique(vac_data$ccaa)),
                                  multiple = TRUE, 
                      )
                      
               ),
               column(3,
                      sliderTextInput(ns("select_fecha"), "Periodo",
                                      choices = seq(as.Date("2020-02-20"), as.Date(Sys.Date()), "days"),
                                      selected = c(as.Date("2020-02-20"), as.Date(Sys.Date()))
                      )
               ),
               column(3,style = "padding-top: 20px; text-align: center;",
                      awesomeCheckbox(ns("logar100"),
                                      label = "Escala logarítmica",
                                      value = FALSE,
                                      status = "danger")),
        )
      ),
      fluidRow(
        column(12,
               plotlyOutput(ns("graph"),width = "100%")
        )
      ),
      fluidRow(
        column(12,
               h1("% Vacunados por grupo de edad", 
                  style = "text-align:center; padding-botom: 25px;"),
               uiOutput(ns("plots"))
        )
      )
  )
}

vacServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$title <- renderUI({
        div(tags$h1(paste(input$select1, "del", input$select_fecha[1], "al",input$select_fecha[2], sep = " "), 
                    style = "text-align:center; padding-botom: 25px;"))
        
      })
      
      logar <- reactive({
        if(input$logar100 == TRUE){"log"}
        
        else{
          ""
        }
      })
      
      output$graph <- renderPlotly({
        
        
        cases_data_chart <- vac_data %>% 
          filter(date >= input$select_fecha[1] & date <= input$select_fecha[2])
        
        if(is.null(input$select_province)) {
          df <- cases_data_chart
        } else {
          df <- cases_data_chart %>% filter(ccaa %in% input$select_province)
        }
        
        plot_ly(data = df, x = ~date, y = ~get(input$select1), type = 'scatter',
                mode = 'lines', fill = ~ccaa, color= ~ccaa, colors = "Paired",
                text = ~ccaa,
                hovertemplate = paste("%{x}: <br> %{text}: %{y}<extra></extra>")) %>% 
          layout(yaxis = list(type = logar(), title = "", side = "right"),
                 xaxis= list(title = ""),
                 legend = list(x = -20, y = 0.5)) #%>% 
        #config(displayModeBar = F)
      })
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      reactivedataset <- reactive({
         vac_etarios %>% 
          filter(date == max(date)) %>% 
          select(-date) %>% 
          gather(edad, value, 2:8) %>% 
          mutate(value = round(value, digits = 1)) %>% 
          spread(edad, value) %>% 
          select(ccaa, dosis, `80+`,  `70-79`, `60-69`, 
                 `50-59`, `25-49`,`18-24`)
        
        

      })
      
      reactivedatanames <- reactive({
        req(reactivedataset())
        reactivedataset() %>%
          select(-ccaa, -dosis)
      })
      
      reactivedatanames2 <- reactive({
        
        paste(names(reactivedatanames()))
      })
      
      observe( 
        for(.y in names(reactivedatanames())) { 
          local({ 
            y <-  .y
            output[[y]] <- renderPlotly({ 
              
              
              aa <- reactivedataset() %>% 
                select(dosis, ccaa, sym(y)) %>% 
                spread(dosis, sym(y)) %>% 
                arrange(desc(`pauta completa`)) %>% 
                mutate(ccaa = factor(ccaa, levels = unique(ccaa)[order(`pauta completa`, decreasing = F)]))

              edad <- y
              
              plot_ly(aa, color = I("gray80")) %>%
                add_segments(x = ~`primera dosis`, xend = ~`pauta completa`, y = ~ccaa, yend = ~ccaa, showlegend = FALSE) %>% 
                add_markers(x = ~`primera dosis`, y = ~ccaa, name = "Una dosis", color = I("#0083cf"),
                            hovertemplate = paste("%{y}: <br> Una dosis: %{x}<extra></extra>")) %>%
                add_markers(x = ~`pauta completa`, y = ~ccaa, name = "Pauta completa", color = I("#00b1d9"),
                            hovertemplate = paste("%{y}: <br> Pauta completa: %{x}<extra></extra>")) %>%
                  layout(title = edad,
                    yaxis = list( title = "", side = "left"),
                         xaxis= list(title = "",ticksuffix = "%"),
                    legend = list(orientation = "h",   # show entries horizontally
                                  xanchor = "center",  # use center of legend as anchor
                                  x = 0.5))  %>% 
              config(displayModeBar = F)
              
            })
            
          }
          )}
      )
      
      output$plots <- renderUI(
        # lapply(
        #   lapply(names(paste(reactivedatanames)), renderText)),
        lapply(
          lapply(paste("vac", names(reactivedatanames()), sep = "-"), plotlyOutput, width = 400),
          div,
          style = htmltools::css(display = "inline-block", margin = "0px")
        )
      )
      
      

      
    }
  )
}