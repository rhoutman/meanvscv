
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyWidgets)

shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  
  # Application title
  titlePanel("Mean vs.CV"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
uiOutput("colorlist")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("meanvscvplot"),
      shinyjs::hidden(p(id = "runStatus", "Processing...")),
      actionButton("runBtn", "Run", disabled=TRUE)
      
    )
  )
))