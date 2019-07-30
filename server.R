library(shiny)

shinyServer(function(input, output, session) {
  
  
  observeEvent(input$link_to_tabpanel_about_single, {
    newvalue <- "about"
    updateTabsetPanel(session, "tabset", newvalue)
  })
  
})