function(input, output, session) {

  
  
  output$tweet <-   renderUI({
    tagList(
      tags$blockquote(class = "twitter-tweet",
                      tags$a(href = "https://twitter.com/escovid19data/status/1382631030004207622")),
      tags$script('twttr.widgets.load(document.getElementById("tweet"));')
    )
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
             legend = list(x = -20, y = 0.5)) %>% 
      config(displayModeBar = F)
  })
  
  
  
  # output$table <- renderDT({
  #   
  #   varis <- sym(input$select1)
  #   
  # 
  #   
  #   data_for_table <- cases_data %>% 
  #     select(date, province, !!varis) %>% 
  #     unique() %>% 
  #     filter(date != "2020-02-24" | province !="Araba/Álava") %>% 
  #     group_by(province) %>% 
  #     spread(date, !!varis) %>% 
  #     column_to_rownames(var="province")
  #     
  #   
  #   
  #   
  #   brks <- quantile(data_for_table, probs = seq(.05, .95, .05), na.rm = TRUE)
  #   clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
  #     {paste0("rgb(255,", ., ",", ., ")")}
  #   
  #   datatable(data_for_table) %>% 
  #     formatStyle(names(df), backgroundColor = styleInterval(brks, clrs))
  #   
  #   
  # })
  
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
        layout(showlegend = F)
      #plot_ly(cases_data, x = date, y = province, colors = "Greys", type = "heatmap") 
  
  #ggplotly(p, tooltip="text")
  })
  
  
}


# plot_ly(data_provinces, x = ~date, y = ~new_cases, type = 'scatter',
#         mode = 'lines', fill = ~province, color= ~province)
