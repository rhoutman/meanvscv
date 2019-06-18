options("tercen.serviceUri"="http://tercen:5400/api/v1/")
options("tercen.username"="admin")
options("tercen.password"="admin")

library(shiny)
library(shinyjs)
library(tercen)
library(tidyverse)
library(plotly)

shinyServer(function(input, output, session) {

ctxcore <- reactive(getValues(session)) 
  
output$meanvscvplot <- renderPlotly({

ctxcore <- ctxcore() %>%
      filter(color %in% input$selectcolor &
             cv %>%  is.finite())

    theme_set(theme_bw())
    p1 <- ggplot(ctxcore, aes(mean, cv)) +
      geom_point(aes(color = color), size = 2, shape = 1) +
      theme(
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5)
      ) +
      ylim(0, 100) +
      xlab("mean") +
      ylab("CV (%)")

colnum <- ctxcore %>% pull(color) %>% unique() %>% length()

    if (colnum == 0) {
      p1 <- p1 +
        theme(legend.position = "none")
      p1 <- p1 +
        scale_color_manual(guide = guide_legend(title = ""), values = c("orangered2"))
    }

    if (colnum < 5) {
      p1 <- p1 +
        scale_color_manual(guide = guide_legend(title = ""), values = c("orangered2", "blue3", "chartreuse3", "darkgoldenrod1"))
    }

    if (colnum > 4 & colnum < 10) {
      p1 <- p1 +
        scale_colour_brewer(guide = guide_legend(title = ""), palette = "Set1")
    }

    if (colnum > 9) {
      p1 <- p1 +
        guides(color = guide_legend(title = "", title.hjust = 0, ncol = ceiling(colnum / 20)))
    }

    p1 <- p1 +
      theme(legend.position = c(0.75, 0.75))

    p1 %>%
      ggplotly(height=500, width=600 )


  })

  output$colorlist <- renderUI({
    thechoices <-  ctxcore() %>% pull(color) %>% unique()
    pickerInput("selectcolor","Select data", as.list(thechoices), options = list(`actions-box` = TRUE),selected= as.list(thechoices), multiple = T)
  })


  
})

getCtx = function(session){
  # retreive url query parameters provided by tercen
  query = parseQueryString(session$clientData$url_search)

  token = query[["token"]]
  taskId = query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx = tercenCtx(taskId=taskId, authToken=token)
  
  return(ctx)
}

getValues = function(session){

  ctx = getCtx(session)

  # extract the data
  ctxcore <- ctx$select(
      c(".ci",".ri", ".y", ctx$colors) %>% 
             unlist()
      )
  
  colfac <- ctx$colors %>% length()
  if (colfac > 0) {
    acolor <- ctx$colors %>% as.character()
    ctxcore <- ctxcore %>%
      unite("color", acolor, remove = T)
  } else {
    ctxcore$color <- "all"
  }
  
  
  
  # filter for needed data
  ctxcore <- ctxcore %>%
    group_by(.ci, .ri, color) %>%
    summarize(
      mean = mean(.y),
      cv = 100 * sd(.y) / mean
    )

  return(ctxcore)
}
