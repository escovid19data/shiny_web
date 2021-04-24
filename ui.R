navbarPage(
  id = "escovid19",
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
    title = "Proyecto",
    tags$head(
      
      # tags$link(
      #   rel = "stylesheet", type = "text/css", href = "styles.css"
      # )
    ),
    
    fluidRow(
      column(12,
      includeMarkdown("https://raw.githubusercontent.com/montera34/escovid19data/master/README.md")
    ))
    
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
    # ),
    
    fluidRow(
      column(12,
             column(3,
                    selectInput("select1", "Variable",
                                choices = c("Casos Totales", "Casos Nuevos", "Media móvil semanal",
                                            "IA 14", "IA 7", "Razón de Tasas"))
                    ),
             column(3,
                    selectInput("select_province","Provincias", choices= c("Todas" = "",unique(cases_data$province)),
                                   multiple = TRUE, 
                                  )
                    
             ),
             column(3,
                    sliderInput("select_fecha",
                                "Periodo:",
                                min = as.Date("2020-01-01","%Y-%m-%d"),
                                max = as.Date(Sys.Date(),"%Y-%m-%d"),
                                value=c(as.Date("2020-01-01","%Y-%m-%d"),as.Date(Sys.Date())),
                                timeFormat="%Y-%m-%d")
                    # dateRangeInput("select_fecha", "Periodo", start = "2020-01-01",
                    #                end = Sys.Date())
                   
             ),
             column(3,
                    awesomeCheckbox("logar100",
                                    label = "Escala logarítmica",
                                    value = FALSE,
                                    status = "danger")),
             )
    ),
    fluidRow(
      column(12,
             plotlyOutput("graph")
             )
    ),
    fluidRow(
      column(12,
             plotlyOutput("graph2")
      )
    )
    
  ),
  
  tabPanel(
    navid = "falle",
    id = "falle",
    value = "falle",
    title = "Fallecidos",
    useShinyjs(),
    tags$head(
      tags$script(async = NA, src = "https://platform.twitter.com/widgets.js")
    ),
    
    fluidRow(
      column(12,
             uiOutput("tweet")
             )
    )
    
  ),
  
  # tabPanel(
  #   navid = "resumen",
  #   id = "resumen",
  #   value = "resumen",
  #   title = "Resumen",
  #   tags$head(
  #     useShinyjs(),
  #     tags$link(
  #       rel = "stylesheet", type = "text/css", href = "styles.css"
  #     )
  #   ),
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
    navid = "vacunas",
    id = "vacunas",
    value = "vacunas",
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
    navid = "infocon",
    id = "infocon",
    value = "infocon",
    title = "Información y contacto",
    # tags$head(
    #   
    #   tags$link(
    #     rel = "stylesheet", type = "text/css", href = "styles.css"
    #   )
    # ),
    
    fluidRow()
    
  )
)