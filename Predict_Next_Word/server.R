# This is the server logic for a Shiny web application.
# Author: Enrique Reveron
# Date: 2016-06-27
# Content: This Shiny Apps predict the next word for english language using
#          Knersey-ney Algoritm and Backoff


shinyServer(function(input, output,session) {

 
  load_Prob_Table <- reactive({
    input$prob_table
    
    isolate({
      withProgress({
        setProgress(message = "Loading Data Tables ...")
        
        if (input$prob_table == "With Freq >= 5") {
          load_DT_prob_final_table(1,"50_5")
          load_DT_prob_final_table(2,"50_5")
          load_DT_prob_final_table(3,"50_5")
          load_DT_prob_final_table(4,"50_5")
        }
        else {
          load_DT_prob_final_table(1,"50_1")
          load_DT_prob_final_table(2,"50_1")
          load_DT_prob_final_table(3,"50_1")
          load_DT_prob_final_table(4,"50_1")
        }
      })
    })
  })
        
    
  
  get_predicted_words <- reactive({
    input$text_string
    input$pred_method
    input$minprob
    input$maxwords
    #input$prob_table
    
    
    isolate({
      withProgress({
        setProgress(message = "Predicting words...")
        
        if (input$pred_method == "Incomplete Words") {
          with_regex <- TRUE
        } else {
          with_regex <- FALSE
        }
        as.data.frame(main_predict_word(input$text_string,input$minprob,input$maxwords,with_regex))
      })
    })
  })
  
  output$table <- renderDataTable({
    
    load_Prob_Table()
    
    myds <- get_predicted_words()
    
    
    if (nrow(myds) > 0) {
      myds
    }
  }, options = list(lengthChange = FALSE, orderClasses = FALSE, scroller = FALSE))
  
  output$unigrams_table <- renderDataTable({
    load_Prob_Table()
    DT.uni.prob.final
  }, options = list(lengthChange = TRUE,orderClasses = TRUE, scroller = TRUE))
  
  output$bigrams_table <- renderDataTable({
    load_Prob_Table()
    DT.bi.prob.final
  }, options = list(lengthChange = TRUE,orderClasses = TRUE, scroller = TRUE))
  
  output$trigrams_table <- renderDataTable({
    load_Prob_Table()
    DT.tri.prob.final
  }, options = list(lengthChange = TRUE,orderClasses = TRUE, scroller = TRUE))
  
  output$quadgrams_table <- renderDataTable({
    load_Prob_Table()
    DT.quad.prob.final
  }, options = list(lengthChange = TRUE,orderClasses = TRUE, scroller = TRUE))
  
  # This function will create the wordcloud 
  
  output$word_cloud <- renderPlot({
    #input$text_string
    load_Prob_Table()
    myds <- get_predicted_words()
    
    if (nrow(myds) > 0)
    {
      wordcloud(myds$word,myds$prob,
                max.words = input$maxwords, random.order = FALSE, random.color = FALSE,
                rot.per=0,scale=c(8,5), fixed.asp = TRUE,
                colors = brewer.pal(6, "Dark2"))
    } 
    
  })
})
 
