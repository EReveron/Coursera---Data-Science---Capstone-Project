# This is the user-interface definition of a Shiny web application.
# Author: Enrique Reveron
# Date: 2016-07-17
# Content: This Shiny Apps predict the next word for english language using
#          Knersey-ney Algoritm and Backoff
#          
#

shinyUI(
  fluidPage(
    navbarPage("Predictor!",
               tabPanel("Main",
                        sidebarLayout(
                          sidebarPanel(
                            div(class = "option-group",
                                radioButtons("prob_table", "Ngram Probability Table",
                                             choices = c("With Freq >= 5", "With Freq > 1"), inline = TRUE)
                            ),
                            div(class = "option-group",
                                radioButtons("pred_method", "Prediction Method",
                                             choices = c("Incomplete Words","Complete Words"), inline = TRUE)
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
                            h4("Next Word Prediction:"),
                            htmlOutput("text_pred"),
                            hr(),
                            plotOutput("word_cloud")
                          )
                        )
               ),
               
               navbarMenu("Reports",
                          tabPanel('Milestone Report',
                                   htmlOutput("report1")),
                          tabPanel('Final Report',
                                   htmlOutput("report2")),
                          tabPanel('Final Test Report',
                                   htmlOutput("report3"))
               ),
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
               
                            
                          
                 
                 
                       
      