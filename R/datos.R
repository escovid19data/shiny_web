datosUI <- function(id) {
  ns <- NS(id)
  div(style = "margin-left:0px; margin-right:0px;",
      fluidRow(
        column(12,
               h1("Acceso a datos",style = "margin-left:30px; margin-right:30px;"),
               p("En esta sección puedes descargar los diferentes archivos utilizados y generados en escovid19data,
                 seleccionando incluso la version de los mismos por fecha.",style = "margin-left:30px; margin-right:30px;"),
               p("Para ello, utiliza el filtro 'Data File' para seleccionar el archivo (por ejemplo 'data/output/covid19-provincias-spain_consolidated.csv')
                  y el filtro 'Commit' para seleccionar la version del mismo (por ejemplo 'actualiza datos provincias 2021-05-31')",style = "margin-left:30px; margin-right:30px;"),
               htmlOutput(ns("frame"))
               
        )
      )
  )
}

datosServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$frame <- renderUI({
        tags$iframe(src="https://flatgithub.com/montera34/escovid19data", height=400, width= "100%")
      })
    }
  )
}