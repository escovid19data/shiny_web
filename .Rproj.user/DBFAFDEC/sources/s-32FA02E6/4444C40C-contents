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
      df <- cases_data_chart %>% filter(province == input$select_province)
    }
    
    plot_ly(data = df, x = ~date, y = ~get(input$select1), type = 'scatter',
            mode = 'lines', fill = ~province, color= ~province, colors = "Paired",
            text = ~province,
            hovertemplate = paste("%{x}: <br> %{text}: %{y}<extra></extra>")) %>% 
      layout(yaxis = list(type = logar(), title = "")) %>% 
      config(displayModeBar = F)
  })
  
  
  
  output$table <- renderDT({
    
    varis <- sym(input$select1)
    
    data_for_table <- cases_data %>% 
      select(date, province, !!varis) %>% 
      unique() %>% 
      filter(date != "2020-02-24" | province !="Araba/Álava") %>% 
      group_by(province) %>% 
      spread(date, !!varis)
    
    datatable(data_for_table)
    
    
  })
  
  
}


# plot_ly(data_provinces, x = ~date, y = ~new_cases, type = 'scatter',
#         mode = 'lines', fill = ~province, color= ~province)
