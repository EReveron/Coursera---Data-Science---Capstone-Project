# This is the user-interface definition of a Shiny web application.
# Author: Enrique Reveron
# Date: 2016-06-27
# Content: This Shiny Apps predict the next word for english language using
#          Knersey-ney Algoritm and Backoff
#          
#

shinyUI(
  fluidPage(
    navbarPage("Predictor!",
               tabPanel("Main",
                        #titlePanel("Predict Next Word with Knersey-ney Algorithm and Backoff"),
                        sidebarLayout(
                          sidebarPanel(
                            #helpText(paste("This app create demographic maps with information from ITU-T 2000-2014.",
                            #               "This apps use the GoogleVis library to make the plots.")),
                            
                            #helpText(paste("Please select the variable to display (Fixed, Mobile) and the",
                            #               "years Range of interest.")),
                            #helpText(paste("If you want to plot the total amount of subscriptions in a specific",
                            #               "year, please select the same year from beginning to end.")),
                            #helpText(paste("When is selected a year range, the Apps will show the diff (Year2 - Year1).")),
                            div(class = "option-group",
                                radioButtons("prob_table", "Ngram Probability Table",
                                             choices = c("With Freq >= 5", "With Freq > 1"), inline = TRUE)
                            ),
                            div(class = "option-group",
                                radioButtons("pred_method", "Prediction Method",
                                             choices = c("Complete Words", "Incomplete Words"), inline = TRUE)
                            ),
                            
                            div(class = "option-group",
                                sliderInput("minprob",
                                            label = "Probability Range:",
                                            min = 0, max = 1,
                                            value = 0)
                            ),
                            div(class = "option-group",
                                sliderInput("maxwords",
                                            label = "Maximun Number of Words:",
                                            min = 0, max = 20,
                                            value = 5)
                            ),
                            h4("Results Table:"),
                            div(class = "option-result",
                                div(class = "option-header", "Results"),
                                div(class = "option-header", 
                                    dataTableOutput("table"))
                            )
                          ), 
                          mainPanel(
                            textInput("text_string", 
                                      label = "", 
                                      value = "",
                                      width = '100%',
                                      placeholder = "Type your text here ..."),
                            plotOutput("word_cloud")
                          )
                        )
               ),
               #setwd(wd.R),
               
               navbarMenu("Reports",
                          tabPanel('Middle Report',
                                   htmlOutput("report1")),
                          tabPanel('Final Report',
                                   htmlOutput("report2")),
                          tabPanel('Output Report',
                                   htmlOutput("report3"))
               ),
               #setwd(wd.RData),
               navbarMenu("More",
                          tabPanel('Unigrams Probability Table',
                                   titlePanel("Unigrams Probability Table"),
                                   dataTableOutput("unigrams_table")),
                          tabPanel('Bigrams Probability Table',
                                   titlePanel("Bigrams Probability Table"),
                                   dataTableOutput("bigrams_table")),
                          tabPanel('Trigrams Probability Table',
                                   titlePanel("Trigrams Probability Table"),
                                   dataTableOutput("trigrams_table")),
                          tabPanel('Quadgrams Probability Table',
                                   titlePanel("Quadgrams Probability Table"),
                                   dataTableOutput("quadgrams_table")),
                          tabPanel('Prob Data Table Frequency Analisys',
                                   titlePanel("Prob Data Table Frequency Analisys"),
                                   dataTableOutput("freq_table"),
                                   plotOutput("freq_plot"))
               )
    )
  )
)
               
                            
                          
                 
                 
                       
      