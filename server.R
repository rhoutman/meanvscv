
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

options("tercen.username" = "admin")
options("tercen.password" = "admin")


# options("tercen.workflowId" = "d30382066b71e6e7995cee981c001603")
# options("tercen.stepId" = "54-7")

library(shiny)
library(shinyjs)
library(tercen)
library(tidyverse())
library(plotly)

# devtools::install_github("tercen/rtercen")

shinyServer(function(input, output, session) {
  
  dataInput = reactive({getValues(session)})
  mode = reactive({getMode(session)})
  
  observeEvent(input$runBtn, {
    
    shinyjs::disable("runBtn")
    shinyjs::show("runStatus")
    
    (ctx = getCtx(session))  %>% 
      select(.y, .ci, .ri) %>% 
      group_by(.ci, .ri) %>%
      summarise(meanvscv = 1) %>%
      ctx$addNamespace() %>%
      ctx$save()
    
    
  })
  

  ctxcore <- reactive(getValues(session)) 
  
  
  output$meanvscvplot <- renderPlotly({

    ctxcore <- ctxcore() %>% 
      filter(color %in% input$selectcolor)

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
    # legend <- extractlegend(p1)
    p1 <- p1 +
      theme(legend.position = c(0.75, 0.75))
    
    p1 %>% 
      ggplotly(height=500, width=600 ) 
    # %>%
    #   layout(legend = list(x = 0.5, y = 0.5))

  })
  
  output$colorlist <- renderUI({
    thechoices <-  ctxcore() %>% pull(color) %>% unique()
    pickerInput("selectcolor","Select data", as.list(thechoices), options = list(`actions-box` = TRUE),selected= as.list(thechoices), multiple = T)
  })
  
# output$plot <- renderImage({
#     
#     tile <- hmtile()
#     # alegend <- basetile()$alegend
#     # A temp file to save the output. It will be deleted after renderImage
#     # sends it, because deleteFile=TRUE.
#     outfile <- tempfile(fileext='.pdf')
#     # outfile <-"temp.png"
#     
#     #  Generatefigure
#     # png(outfile, width=input$figwidth, height=input$figheight, units = "cm", res=300)
#     pdf(outfile, width=600, height=500)
#     
#     # print(tile)
#     plotheatmap(tile ,dd.row = clusterdf()$dd.row, dd.col= clusterdf()$dd.col,alegend=  basetile()$alegend,  xvp=input$legendx, yvp= input$legendy,legendsize=input$legendsize, plotfolder = NULL,  filename="heatmap",filetype=".pdf", width=input$figwidth, height=input$figheight)
#     dev.off()
#     
#     
#     file.copy(from=outfile, to=file.path("hmtemp", "heatmap.pdf"),overwrite = T)
#     
#     #  Return a list
#     list(src = outfile,
#          alt = "This is alternate text")
#   }, deleteFile = F)
  
  
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
  ctxcore <- ctx %>%
    select()
  
 
  
  colfac <- ctx$colors %>% length()
  if (colfac > 0) {
    acolor <- ctx$colors %>% as.character()
    ctxcore <- ctxcore %>%
      unite("color", acolor, remove = F)
  } else {
    ctxcore$color <- "all"
  }
  
  
  
  # filter for needed data
  ctxcore <- ctxcore %>%
    select(
      .ri,
      .ci,
      color,
      .y
    ) %>%
    group_by(.ci, .ri, color) %>%
    summarize(
      mean = mean(.y),
      cv = 100 * sd(.y) / mean
    )
  
  ctxcore$cv <- with(ctxcore, replace(cv, cv %in% c("NaN", "NA", "Na", "inf"), 10000))
  ctxcore$cv[is.na(ctxcore$cv)] <- 10000
  

  return(ctxcore)
}
