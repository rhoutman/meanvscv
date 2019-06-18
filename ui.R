library(shiny)
library(shinyWidgets)
library(plotly)

shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  
  # Application title
  titlePanel("Mean vs. CV plot"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
uiOutput("colorlist"),
actionButton("runBtn", "save plot", disabled=TRUE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("meanvscvplot"),
      shinyjs::hidden(p(id = "runStatus", "Processing..."))
  
      
    )
  )
))