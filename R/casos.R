casosUI <- function(id) {
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
                              choices = c("Casos Totales", "Casos Nuevos", "Media móvil semanal",
                                          "IA 14", "IA 7", "Razón de Tasas"))
           ),
           column(3,
                  selectInput(ns("select_province"),"Provincias", choices= c("Todas" = "",unique(cases_data$province)),
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
           plotlyOutput(ns("graph2"),width = "96%")
    )
  )
)
  
}

casosServer <- function(id) {
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
        
        
        cases_data_chart <- cases_data %>% 
          filter(date >= input$select_fecha[1] & date <= input$select_fecha[2])
        
        if(is.null(input$select_province)) {
          df <- cases_data_chart
        } else {
          df <- cases_data_chart %>% filter(province %in% input$select_province)
        }
        
        plot_ly(data = df, x = ~date, y = ~get(input$select1), type = 'scatter',
                mode = 'lines', fill = ~province, color= ~province, colors = "Paired",
                text = ~province,
                hovertemplate = paste("%{x}: <br> %{text}: %{y}<extra></extra>")) %>% 
          layout(yaxis = list(type = logar(), title = "", side = "right"),
                 xaxis= list(title = ""),
                 legend = list(x = -20, y = 0.5)) #%>% 
        #config(displayModeBar = F)
      })
      
      
      output$graph2 <- renderPlotly({
        
        varis <- rlang::sym(input$select1)
        
        data_for_table <- cases_data %>%
          as.data.frame() %>% 
          select(date, province, !!varis) %>%
          unique() %>%
          ungroup() %>% 
          filter(date >= input$select_fecha[1] & date <= input$select_fecha[2]) %>% 
          rename(z = !!varis) %>% 
          mutate(z = as.numeric(z)) %>% 
          arrange(province)
        
        # group_by(province) %>%
        # spread(date, !!varis) %>%
        # column_to_rownames(var="province")
        plot_ly(data = data_for_table,
                x = ~date, y = ~province,
                text = ~province, showscale=FALSE,
                z = ~z, type = "heatmap", colors = colorRamp(c("#ffff66", "#ff0000")),
                height = 700, hovertemplate = paste("%{x}: <br> %{y}: %{z}<extra></extra>")
        )  %>% 
          config(displayModeBar = F) %>% 
          layout(showlegend = F,
                 xaxis= list(title = ""),
                 yaxis= list(title = ""))
        #plot_ly(cases_data, x = date, y = province, colors = "Greys", type = "heatmap") 
        
        #ggplotly(p, tooltip="text")
      })
      
    }
  )
}
  


  
  