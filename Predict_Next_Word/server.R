# This is the server logic for a Shiny web application.
# Author: Enrique Reveron
# Date: 2016-06-27
# Content: This Shiny Apps predict the next word for english language using
#          Knersey-ney Algoritm and Backoff

library(shiny)
library(googleVis)
require(shiny)

setwd("D:/Coursera/Capstone Project/Coursera-SwiftKey/final/en_US/FINAL2/Final 27-06")
source("Pred Next Word vFinal.R")
source("Pred Next Word Regex vFinal.R")
source("Final.R")

load_DT_prob_table(1,100)
load_DT_prob_table(2,100)
load_DT_prob_table(3,100)
load_DT_prob_table(4,100)

shinyServer(function(input, output) {
  

  
  # For reactive purposes
  myVar <- reactive({
    input$var
  })
  
  # For reactive purposes
  myYear1 <- reactive({
    input$range[1]
  })
  
  # For reactive purposes
  myYear2 <- reactive({
    input$range[2]
  })
  
  # For reactive purposes
  myInputString <- reactive({
    input$string
  })
  
  # This function will create the Tittle
  output$var <- renderText({
    
    if (input$var == 1) { myvariable <- "Fixed Phone Lines"
    } else { myvariable <- "Mobile Phone Subscriptions" } 
    
    if (input$range[1] == input$range[2]) {
      title_map <- paste(myYear1()," Total Worldwide ITU ",
                         myvariable," (thousands)")
    } else {
      title_map <- paste(myYear1(),"-",myYear2()," Increment Worldwide ITU ",
                         myvariable, " (thousands)")
    }
    title_map
  })
  
  
  # This function will create the Tittle
  output$prediction <- renderText({
    myds <<- main_predict_word(input$string)
    unlist(myds[1:5,word,])
    #input$string
    
  })
  

  
  
  # This function will create the table 
  output$table <- renderGvis({

    if (length(input$string) < 0) { NULL}
    
    gvisTable(as.data.frame(myds), 
              options=list(page='enable',
                           height='automatic',
                           width='automatic'))
  
    
  })
  
  
})