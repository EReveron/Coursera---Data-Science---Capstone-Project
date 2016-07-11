# This is the server logic for a Shiny web application.
# Author: Enrique Reveron
# Date: 2016-06-27
# Content: This Shiny Apps predict the next word for english language using
#          Knersey-ney Algoritm and Backoff


shinyServer(function(input, output,session) {

 
  
  get_predicted_words <- reactive({
    input$text_string
    input$pred_method
    input$minprob
    input$maxwords
    
    isolate({
      withProgress({
        setProgress(message = "Predicting words...")
        
        if (input$pred_method == "Incomplete Words") {
          with_regex <- TRUE
        } else {
          with_regex <- FALSE
        }
        
        x <- main_predict_word(input$text_string,input$minprob,input$maxwords,with_regex)
        print("X vale")
        print(x)
        as.data.frame(x)
      })
    })
  })
  
  output$table <- renderDataTable({
    
    #input$text_string
    
    myds <- get_predicted_words()
    
    print(paste("NROW in Rendertable",nrow(myds)))
  
    #if (nrow(myds) > 0) {
      
    #    datatable(myds,options = list(lengthChange = FALSE, scroller = FALSE))
    #}
    #else {
    #  myds
    #}
    
    myds
    
    
    
  }, options = list(lengthChange = FALSE, scroller = FALSE))
  
  output$unigrams_table <- renderDataTable({
    datatable(DT.uni.prob.final,options = list(lengthChange = FALSE, scroller = TRUE))
  })
  
  output$bigrams_table <- renderDataTable({
    DT.bi.prob.final
  }, options = list(lengthChange = FALSE, scroller = FALSE))
  
  output$trigrams_table <- renderDataTable({
    DT.tri.prob.final
  }, options = list(orderClasses = TRUE))
  
  # This function will create the wordcloud 
  
  output$word_cloud <- renderPlot({
    input$text_string
    myds <- get_predicted_words()
    
    print(paste("Nrow in wordcloud",nrow(myds)))
    
    
    
    if (nrow(myds) > 0)
    {
      wordcloud(myds$word,myds$prob,
                max.words = input$maxwords, random.order = FALSE, random.color = FALSE,
                rot.per=0,scale=c(8,5), fixed.asp = TRUE,
                colors = brewer.pal(6, "Dark2"))
    }
    
  })
})
 
