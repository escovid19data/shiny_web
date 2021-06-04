uso_datosUI <- function(id) {
  ns <- NS(id)
  div(style = "margin-left:30px; margin-right:30px;",
      includeMarkdown("www/uso_de_datos.Rmd")
  )
}

uso_datosServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      
      
    }
  )
}