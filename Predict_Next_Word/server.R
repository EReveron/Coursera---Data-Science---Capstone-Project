# This is the server logic for a Shiny web application.
# Author: Enrique Reveron
# Date: 2016-06-27
# Content: This Shiny Apps predict the next word for english language using
#          Knersey-ney Algoritm and Backoff


shinyServer(function(input, output,session) {

  # For reactive purposes
  #myVar <- reactive({
  #  input$var
  #})
  
  # For reactive purposes
  #myYear1 <- reactive({
  #  input$range[1]
  #})
  
  # For reactive purposes
  #myYear2 <- reactive({
  #  input$range[2]
  #})
  
  # For reactive purposes
  #myInputString <- reactive({
  #  input$text_string
  #})
  
  get_predicted_words <- reactive({
    input$text_string
    


    isolate({
      withProgress({
        setProgress(message = "Predicting words...")
        as.data.frame(main_predict_word(input$text_string))
      })
    })
    
  })
  
  # This function will create the Tittle
  output$var <- renderText({
    myds <- get_predicted_words()
    print("Corre ouput var")
    myds$word[1]
  })
  
  
  
  
  # This function will create the Tittle
  output$prediction1 <- renderText({
    input$text_string
    myds <- get_predicted_words()
    print("Corre ouput prediction1")
    if (nrow(myds) > 0) {
      myds$word[1]
    }
  })
  
  # This function will create the Tittle
  output$prediction2 <- renderText({
    input$text_string
    myds <- get_predicted_words()
    print("Corre ouput prediction2")
    if (nrow(myds) > 1) {
      myds$word[2]
    }
  })
  
  # This function will create the Tittle
  output$prediction3 <- renderText({
    input$text_string
    myds <- get_predicted_words()
    print("Corre ouput prediction3")
    if (nrow(myds) > 2) {
      myds$word[3]
    }
  })
  
  
  # This function will create the Tittle
  output$prediction4 <- renderText({
    input$text_string
    myds <- get_predicted_words()
    print("Corre ouput prediction4")
    if (nrow(myds) > 3) {
      myds$word[4]
    }
  })
  
  # This function will create the Tittle
  output$prediction5 <- renderText({
    input$text_string
    myds <- get_predicted_words()
    print("Corre ouput prediction3")
    if (nrow(myds) > 4) {
      myds$word[5]
    }
  })
  
  
  
  # This function will create the table 
  #output$table <- renderGvis({
  #  input$text_string
  #  myds <- get_predicted_words()
    
    
    

    #if (length(input$text_string) < 0) { NULL}
  #  if (nrow(myds) > 0)
  #  {
  #    gvisTable(myds)
  #  }
    
    #gvisTable(as.data.frame(myds),
              
            #  options=list(page='enable',
            #               height='automatic',
            #               width='automatic'))
    

 # })
  
  # Make the wordcloud drawing predictable during a session
  #wordcloud_rep <- repeatable(wordcloud)

  # This function will create the wordcloud 
  
  output$word_cloud <- renderPlot({
    input$text_string
    myds <- get_predicted_words()
    
    
  
   # prediction()
  #output$wordcloud <- renderGvis({
    
    #if (length(input$text_string) > 1) {
    
    if (nrow(myds) > 0)
    {
      wordcloud(myds$word,myds$prob,
                max.words = 5, random.order = FALSE, random.color = FALSE,
                rot.per=0,scale=c(3,1), fixed.asp = TRUE,
                colors = brewer.pal(6, "Dark2"))
    }
    #}
    #wordcloud_rep(myds$word, 
    #              round(myds$prob * 10000), 
    #              scale=c(4,0.5), 
    #              colors=brewer.pal(8, "Dark2"))
  })
  
})
 
