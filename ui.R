fluidRow(
  column(
    12,
    class = "col-md-12",
    style = "padding-right: 15px;",
    
    navbarPage(
      id = "navbarID",
      title = "esCOVID-19data", 
      #header=singleton(tags$head(includeScript("google-analytics.js"))),#href = "//cultureofinsight.com", target = "blank"),)),
      inverse = F,
      collapsible = TRUE, 
      theme = shinytheme("yeti"),
      
      #style="margin-bottom: 0px;",
      
      # tags$link(
      #   rel = "stylesheet", type = "text/css", href = "styles.css"
      # )
      #),
      
      #1.0.Resumen----
      tabPanel(
        navid = "intro",
        id = "intro",
        value = "intro",
        title = "Inicio",
        tags$head(
          
          tags$link(
            rel = "stylesheet", type = "text/css", href = "styles.css"
          )
        )
        

        
      ),
      
      tabPanel(
        navid = "casos",
        id = "casos",
        value = "casos",
        title = "Casos",
        # tags$head(
        #   useShinyjs(),
        #   tags$link(
        #     rel = "stylesheet", type = "text/css", href = "styles.css"
        #   )
        # )
        
      ),
      
      tabPanel(
        navid = "falle",
        id = "falle",
        value = "falle",
        title = "Fallecidos",
        useShinyjs()
        
        
      ),
      
      tabPanel(
        navid = "hospi",
        id = "hospi",
        value = "hospi",
        title = "Hospitailizados",
        useShinyjs()
        
        
      ),
      
      
      tabPanel(navid = "uso_datos",
               id = "uso_datos",
               value = "uso_datos",
               title = "Uso de los datos",
                 useShinyjs()
               ),
      
      # tabPanel(
      # 
      # 
      #   resumenUI("resumen")
      # 
      # ),
      
      # tabPanel(
      #   navid = "provs",
      #   id = "provs",
      #   value = "provs",
      #   title = "Provincias",
      #   tags$head(
      #     
      #     tags$link(
      #       rel = "stylesheet", type = "text/css", href = "styles.css"
      #     )
      #   ),
      #   
      #   provsUI("provs")
      #   
      # ),
      
      tabPanel(
        navid = "vac",
        id = "vac",
        value = "vac",
        title = "Vacunas",
        # tags$head(
        #   
        #   tags$link(
        #     rel = "stylesheet", type = "text/css", href = "styles.css"
        #   )
        # ),
        
        fluidRow()
        
      ),
      tabPanel(
        navid = "datos",
        id = "datos",
        value = "datos",
        title = "Acceso a datos",
        # tags$head(
        #   
        #   tags$link(
        #     rel = "stylesheet", type = "text/css", href = "styles.css"
        #   )
        # ),
        
        
      )
    ),
    uiOutput("module")
  )
)


