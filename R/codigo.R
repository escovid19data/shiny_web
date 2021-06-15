codigoUI <- function(id) {
  ns <- NS(id)
  div(style = "margin-left:0px; margin-right:0px;",
      fluidRow(
        column(12,
               p("Accede al codigo de este dashboard", 
                 a(href = "https://github.com/escovid19data/shiny_web/", target = "blank",
                   "aquí."), style = "padding:15px;text-align:center;")
               
        )
      )
  )
}

codigoServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}