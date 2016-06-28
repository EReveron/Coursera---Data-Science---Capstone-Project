# This is the user-interface definition of a Shiny web application.
# Author: Enrique Reveron
# Date: 2016-06-27
# Content: This Shiny Apps predict the next word for english language using
#          Knersey-ney Algoritm and Backoff
#          
#

shinyUI(
  fluidPage(
    
    titlePanel("Predict Next Word with Knersey-ney Algorithm and Backoff"),
    
    sidebarLayout(
      sidebarPanel(
        helpText(paste("This app create demographic maps with information from ITU-T 2000-2014.",
                       "This apps use the GoogleVis library to make the plots.")),
        
        helpText(paste("Please select the variable to display (Fixed, Mobile) and the",
                       "years Range of interest.")),
        helpText(paste("If you want to plot the total amount of subscriptions in a specific",
                       "year, please select the same year from beginning to end.")),
        helpText(paste("When is selected a year range, the Apps will show the diff (Year2 - Year1).")),
        
        selectInput("pred_method", 
                    label = "Select the Prediction method to display:",
                    choices = c("Based on Complete Words" = 1, 
                                "Based on Incomplete Words" = 2)),
        
        sliderInput("prob",
                    label = "Minimun Probability of the Words to Get:",
                    min = 0, max = 1,
                    value = 0.5)
        
      ),
      
      mainPanel(
        h3(textOutput("var")),  
        textInput("text_string", 
                  label = "", 
                  value = "",
                  placeholder = "Type your text here ..."),
        
        hr(),
        fluidRow(column(1, textOutput("value")),
                 column(2, textOutput("value2"))),
        #htmlOutput("table"),
        
        h3(textOutput("prediction1")),
        h3(textOutput("prediction2")),
        h3(textOutput("prediction3")),
        h3(textOutput("prediction4")),
        h3(textOutput("prediction5")),
        plotOutput("word_cloud")
        
      )
    )
  )
)
    
  
