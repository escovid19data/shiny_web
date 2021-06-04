function(input, output, session) {

  vals <- reactiveValues(selected_tab_new = NULL,
                         selected_tab_old = NULL)
  
  observeEvent(input$navbarID, {
    # source(paste0("R/", input$navbarID, ".R"), local = FALSE)
    vals$selected_tab_new <- input$navbarID
    
    vals$selected_tab_old <- input$navbarID
  })
  
  observe({
    if (!is.null(vals$selected_tab_new)) {
      output$module <- renderUI({
        rlang::as_function(paste0(vals$selected_tab_new, "UI"))(vals$selected_tab_new)
      })
      rlang::as_function(paste0(vals$selected_tab_new, "Server"))(vals$selected_tab_new)
    }
  })
  
#Casos----  
  
  #activeTab <- reactiveValues(tree = "intro")
  

  
  onclick("intro-casos_go",


          c(
            output$module <- renderUI({
              rlang::as_function(paste0("casos", "UI"))("casos")
            }),

            rlang::as_function(paste0("casos", "Server"))("casos")

          )
  )
  
  onclick("intro-falle_go",
          
          
          c(
            output$module <- renderUI({
              rlang::as_function(paste0("falle", "UI"))("falle")
            }),
            
            rlang::as_function(paste0("falle", "Server"))("falle")
            
          )
  )
  
  onclick("intro-vac_go",
          
          
          c(
            output$module <- renderUI({
              rlang::as_function(paste0("vac", "UI"))("vac")
            }),
            
            rlang::as_function(paste0("vac", "Server"))("vac")
            
          )
  )
  
  onclick("intro-hospi_go",
          
          
          c(
            output$module <- renderUI({
              rlang::as_function(paste0("hospi", "UI"))("hospi")
            }),
            
            rlang::as_function(paste0("hospi", "Server"))("hospi")
            
          )
  )

  
#Fallecidos----
  
  
  
#Novedades----
  
  # output$tweet <-   renderUI({
  #   tagList(
  #     tags$blockquote(class = "twitter-tweet",
  #                     tags$a(href = "https://twitter.com/escovid19data/status/1382631030004207622")),
  #     tags$script('twttr.widgets.load(document.getElementById("tweet"));')
  #   )
  # })
  
  
  
  
}


# plot_ly(data_provinces, x = ~date, y = ~new_cases, type = 'scatter',
#         mode = 'lines', fill = ~province, color= ~province)
