app_ui <- function(){
  shiny::addResourcePath("spatialCatalogueViewer", system.file("R", package="spatialCatalogueViewer"))
  
  ui <- shinyUI(  
    fluidPage(
      theme = shinythemes::shinytheme(getShinyOption("theme")),
      HTML(getShinyOption("text.title")),
      uiOutput("tab.contents")
    ) 
  )
} 



