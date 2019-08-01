library(shiny)

source("parameter.R")

shinyUI(
  fluidPage(
    titlePanel(
      "ProteomicsTools",
      title = tags$strong("Proteomics Data Normalization and Batch Effect Correction (Beta)")),
    
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          id="tabset",
          tabPanel(
            "Single Run",
            value = "single",
            
            tags$div(
              title = "Proteomics data. Each row is proteomics data for a protein. Each column is a sample.",
              fileInput(
                "inputprotein.single",
                "Choose Protein File",
                multiple = FALSE,
                accept =  c("text", "csv file", ".txt", ".csv")
              )
            ),
            
            tags$div(
              title = "Sample information. Each row is a sample.",
              fileInput(
                "inputsample.single",
                "Choose Sample Information File",
                multiple = FALSE,
                accept =  c("text", "csv file", ".txt", ".csv")
              )
            ),
            
            tags$div(
              title = "Please select a normalization method.",
              selectInput("norm.single", label = h4("Normalization Method"),
                          choices = norm.method,
                          selected = 1, multiple = TRUE)
            ),
            
            tags$div(
              title = "What kind of delimiter is used in input file",
              radioButtons(
                "delimiter.single",
                h4("Delimiters"),
                choices = list(
                  "Comma" = ",",
                  "Semicolon" = ";",
                  "Tab" = "\t",
                  "Space" = " "
                ),
                selected = ","
              )
            ),
            
            
            p(a("Example of proteomics data", href="protein.single.csv", download="protein.single.csv")),
            p(a("Example of sample information data", href="sample.single.csv", download="sample.single.csv")),
            actionLink("link_to_tabpanel_about_single", "Details about input file format"),
            
            uiOutput('ui.action.single')
          ),
          
          
          tabPanel(
            "Multiple Runs", 
            p("Under development"),
            value = "multi"
          ),
          
          tabPanel(
            "About",
            p("An online tool for Proteomics data normalization and batch effect correction"),
            value = "about"
          )
        ),
        
        hr(),
        p(
          "Report issues to the",
          a("developers.",
            href = "mailto:gang.peng@yale.edu")
        )
      ),
      
      mainPanel(
        plotOutput("plot.box", width = 600),
        plotOutput("plot.cv", width = 600),
        plotOutput("plot.cluster", width = 600),
        fluidRow(column(7, align="right",
                        uiOutput('ui.download')))
      )
    )
  )
)