library(shiny)

shinyUI(
  fluidPage(
    titlePanel(
      "ProteomicsTools",
      title = tags$strong("Proteomics Data Normalization and Batch Effect Correction")),
    
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          id="tabset",
          tabPanel(
            "Single Run",
            value = "single"
          ),
          
          tabPanel(
            "Multiple Runs",
            value = "multi"
          ),
          
          tabPanel(
            "About",
            value = "about"
          )
        )
      ),
      
      mainPanel()
    )
  )
)