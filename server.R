library(shiny)
library(reshape2)
library(ggplot2)

source("parameter.R")
source("functions.R")

shinyServer(function(input, output, session) {
  rlt.single <- reactiveValues(
    box = NULL,
    cv = NULL,
    cluster = NULL
  )
  
  rlt.multi <- reactiveValues(
    box = NULL,
    cv = NULL,
    cluster = NULL
  )
  
  raw.data.clean.single <- NULL
  raw.data.clean.multi <- NULL
  sample.info.multi <- NULL
  
  
  input.protein.single <- reactive({
    infile <- input$inputprotein.single
    if(is.null(infile)){
      return(NULL)
    }
    read.table(
      infile$datapath,
      header = TRUE,
      sep = input$delimiter.single,
      stringsAsFactors = FALSE
    )
  })
  
  input.sample.single <- reactive({
    infile <- input$inputsample.single
    if(is.null(infile)){
      return(NULL)
    }
    read.table(
      infile$datapath,
      header = TRUE,
      sep = input$delimiter.single,
      stringsAsFactors = FALSE
    )
  })
  
  
  
  input.protein.multi <- reactive({
    infile <- input$inputprotein.multi
    if(is.null(infile)){
      return(NULL)
    }
    read.table(
      infile$datapath,
      header = TRUE,
      sep = input$delimiter.multi,
      stringsAsFactors = FALSE
    )
  })
  
  input.sample.multi <- reactive({
    infile <- input$inputsample.multi
    if(is.null(infile)){
      return(NULL)
    }
    read.table(
      infile$datapath,
      header = TRUE,
      sep = input$delimiter.multi,
      stringsAsFactors = FALSE
    )
  })
  
  
  
  output$ui.action.single <- renderUI({
    if(is.null(input.protein.single()) || is.null(input.sample.single())){
      return()
    }
    
    if(length(input$norm.single) == 0){
      return()
    }
    
    actionButton("action.single", "Run Normalization")
  })
  
  
  output$ui.action.multi <- renderUI({
    if(is.null(input.protein.multi()) || is.null(input.sample.multi())){
      return()
    }
    
    if(length(input$norm.multi) == 0){
      return()
    }
    
    actionButton("action.multi", "Run Normalization and Batch Correction")
  })
  
  
  
  output$ui.download <- renderUI({
    if(input$tabset=="single"){
      if (is.null(input$action.single)) return()
      if (input$action.single==0) return()
    } else if(input$tabset == "multiple"){
      if(is.null(input$action.multi)) return()
      if(input$action.multi == 0) return()
    } else if(input$tabset == "about"){
      return()
    }

    actionButton('download', "Download Results")
  })
  
  
  #modal
  #modal single
  normMethodModalSingle <- function(failed = FALSE){
    modalDialog(
      selectInput("norm.single.output", label = h4("Normalization Method"),
                  choices = norm.method,
                  selected = 1, multiple = FALSE),
      if(failed)
        div(tags$b("Please select a normalization method", style = "color: red;")),
      
      footer = tagList(
        fluidRow(
          column(2, offset = 3, modalButton("Cancel")),
          column(2, uiOutput('ui.download.single'))
        )
        #modalButton("Cancel"),
        #uiOutput('ui.download.single')
        #actionButton("ok.single", "OK")
      )
    )
  }
  
  output$ui.download.single <- renderUI({
    if(is.null(input$norm.single.output)){
      return()
    } else {
      downloadButton("download.single", "Download")
    }
  })
  
  output$download.single <- downloadHandler(
    filename = function(){
      idx.norm.single.output <- as.integer(input$norm.single.output)
      paste0(names(norm.method)[idx.norm.single.output], ".norm.csv")
    },
    
    content = function(file) {
      idx.norm.single.output <- as.integer(input$norm.single.output)
      rlt <- norm.functions[[idx.norm.single.output]](raw.data.clean.single)
      write.csv(rlt, file, quote = FALSE)
    }
  )
  
  normMethodModalMulti <- function(failed = FALSE){
    modalDialog(
      selectInput("norm.multi.output", label = h4("Normalization Method"),
                  choices = norm.method,
                  selected = 1, multiple = FALSE),
      
      selectInput("batch.multi.output", label = h4("Batch Effect Correction"),
                  choices = batch.method,
                  selected = 1, multiple = FALSE),
      
      if(failed)
        div(tags$b("Please select a normalization and a batch effect correction method", style = "color: red;")),
      
      footer = tagList(
        fluidRow(
          column(2, offset = 3, modalButton("Cancel")),
          column(2, uiOutput('ui.download.multi'))
        )
      )
    )
  }
  
  output$ui.download.multi <- renderUI({
    if(is.null(input$norm.multi.output) || is.null(input$batch.multi.output)){
      return()
    } else {
      downloadButton("download.multi", "Download")
    }
  })
  
  output$download.multi <- downloadHandler(
    filename = function(){
      idx.norm.multi.output <- as.integer(input$norm.multi.output)
      idx.batch.multi.output <- as.integer(input$batch.multi.output)
      paste0(names(batch.method)[idx.batch.multi.output], "_",
             names(norm.method)[idx.norm.multi.output], ".norm.csv")
    },
    
    #TODO: check pool
    content = function(file) {
      idx.norm.multi.output <- as.integer(input$norm.multi.output)
      idx.batch.multi.output <- as.integer(input$batch.multi.output)
      rlt <- batch.correct(
        raw.data.clean.multi, sample.info.multi$batch, 
        as.integer(idx.norm.multi.output), as.integer(idx.batch.multi.output), 
        pool=sample.info.multi$pool)
      write.csv(rlt, file, quote = FALSE)
    }
  )
  
  observeEvent(input$link_to_tabpanel_about_single, {
    newvalue <- "about"
    updateTabsetPanel(session, "tabset", newvalue)
  })
  
  observeEvent(input$link_to_tabpanel_about_multi, {
    newvalue <- "about"
    updateTabsetPanel(session, "tabset", newvalue)
  })
  
  
  ## normalization single run
  observeEvent(
    input$action.single, 
    handlerExpr = {
      #idx.norm.single <- 1:4
      idx.norm.single <- input$norm.single
      
      #raw.data <- read.csv("www/data.single.csv", stringsAsFactors = FALSE)
      #sample.info <- read.csv("www/sample.single.csv", stringsAsFactors = FALSE)
      raw.data <- input.protein.single()
      raw.data.clean.single <<- data.clean(raw.data)
      sample.info <- input.sample.single()
      
      norm.data <- list()
      for(i in 1:length(idx.norm.single)){
        idx <- as.integer(idx.norm.single[i])
        norm.data[[i]] <- norm.functions[[idx]](raw.data.clean.single)
      }
      
      output$plot.box <- renderPlot({
        rlt.single$box <- box.compare.single(raw.data.clean.single, norm.data, idx.norm.single)
        rlt.single$box
      })
      
      output$plot.cv <- renderPlot({
        rlt.single$cv <- cv.compare.single(raw.data.clean.single, norm.data, idx.norm.single, sample.info)
        rlt.single$cv
      })
      
      output$plot.cluster <- renderPlot({
        rlt.single$cluster <- cluster.compare.single(raw.data.clean.single, norm.data, idx.norm.single)
        rlt.single$cluster
      })
    }
  )
  
  
  ## normalization multiple runs
  observeEvent(
    input$action.multi, 
    handlerExpr = {
      #idx.norm.multi <- 1:3
      idx.norm.multi <- input$norm.multi
      #idx.batch.multi <- 1:3
      idx.batch.multi <- input$batch.multi
      
      
      #raw.data <- read.csv("www/protein.multi.csv", stringsAsFactors = FALSE)
      #sample.info.multi <- read.csv("www/sample.multi.csv", stringsAsFactors = FALSE)
      raw.data <- input.protein.multi()
      raw.data.clean.multi <<- data.clean(raw.data)
      sample.info.multi <<- input.sample.multi()
      #sample.info <- input.sample.multi()
      
      #TODO: chekc error if there is no pool information
      norm.data <- list()
      name.method <- NULL
      idx.norm.data <- 1
      if(length(idx.batch.multi)==0){
        for(i in 1:length(idx.norm.multi)){
          norm.data[[idx.norm.data]] <- batch.correct(
            raw.data.clean.multi, sample.info.multi$batch, 
            as.integer(idx.norm.multi[i]), NULL, pool=sample.info.multi$pool)
          name.method <- c(name.method, names(norm.method)[as.integer(idx.norm.multi[i])])
          idx.norm.data <- idx.norm.data + 1
        }
      } else {
        for(i in 1:length(idx.batch.multi)){
          for(j in 1:length(idx.norm.multi)){
            norm.data[[idx.norm.data]] <- batch.correct(
              raw.data.clean.multi, sample.info.multi$batch, 
              as.integer(idx.norm.multi[j]), as.integer(idx.batch.multi[i]), pool=sample.info.multi$pool)
            name.method <- c(
              name.method, 
              paste(names(batch.method)[as.integer(idx.batch.multi[i])],
                    names(norm.method)[as.integer(idx.norm.multi[j])], sep = "_"))
            idx.norm.data <- idx.norm.data + 1
          }
        }
      }
      
      output$plot.box <- renderPlot({
        rlt.multi$box <- box.compare.multi(
          raw.data.clean.multi, norm.data, 
          name.method, sample.info.multi
        )
        rlt.multi$box
      })
      
      output$plot.cv <- renderPlot({
        rlt.multi$cv <- cv.compare.multi(
          raw.data.clean.multi, norm.data, 
          name.method, sample.info.multi
        )
        rlt.multi$cv
      })
      
      output$plot.cluster <- renderPlot({
        rlt.multi$cluster <- cluster.compare.multi(
          raw.data.clean.multi, norm.data, 
          name.method
        )
        rlt.multi$cluster
      })
    }
  )
  
  
  observeEvent(
    input$download, 
    handlerExpr = {
      if(input$tabset=="single"){
        showModal(normMethodModalSingle())
      } else if(input$tabset=="multi"){
        showModal(normMethodModalMulti())
      } else if(input$tabset=="about"){
        
      }
      
    })
  
  
  observeEvent(
    input$tabset,
    handlerExpr = {
      if(input$tabset == "single"){
        output$plot.box <- NULL
        output$plot.cv <- NULL
        output$plot.cluster <- NULL
        if(!is.null(rlt.single$box)){
          output$plot.box <- renderPlot(rlt.single$box)
        }
        
        if(!is.null(rlt.single$cv)){
          output$plot.cv <- renderPlot(rlt.single$cv)
        }
        
        if(!is.null(rlt.single$cluster)){
          output$plot.cluster <- renderPlot(rlt.single$cluster)
        }
      } else if(input$tabset == "multi"){
        output$plot.box <- NULL
        output$plot.cv <- NULL
        output$plot.cluster <- NULL
        
        if(!is.null(rlt.multi$box)){
          output$plot.box <- renderPlot(rlt.multi$box)
        }
        
        if(!is.null(rlt.multi$cv)){
          output$plot.cv <- renderPlot(rlt.multi$cv)
        }
        
        if(!is.null(rlt.multi$cluster)){
          output$plot.cluster <- renderPlot(rlt.multi$cluster)
        }
      } else if(input$tabset == "about"){
        
      }
    }
  )
  
})