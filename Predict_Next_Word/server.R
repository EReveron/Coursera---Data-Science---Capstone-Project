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
          training_set <<- "80.5"
        } else {
          training_set <<- "80.2"
        }
        rm(DT.uni.prob.final,envir = .GlobalEnv)
        rm(DT.bi.prob.final,envir = .GlobalEnv)
        rm(DT.tri.prob.final,envir = .GlobalEnv)
        rm(DT.quad.prob.final,envir = .GlobalEnv)
        load_DT_prob_final_table(1,training_set)
        load_DT_prob_final_table(2,training_set)
        load_DT_prob_final_table(3,training_set)
        load_DT_prob_final_table(4,training_set)
      })
    })
  })
        
    
  
  get_predicted_words <- reactive({
    input$text_string
    input$pred_method
    input$minprob
    input$maxwords
    input$prob_table
    
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
  
  output$freq_table <- renderDataTable({
    load_Prob_Table()
    DT_prob_freq(training_set)
    
  }, options = list(lengthChange = TRUE,orderClasses = TRUE, scroller = TRUE))
  
  output$freq_plot <- renderPlot({
    isolate({
      withProgress({
        setProgress(message = "Generating Data ...")

        load_Prob_Table()
        
        g1 <- ggplot(data=DT.uni.prob.final, aes(freq1)) +
          geom_histogram(breaks=seq(1, 50, by = 1),
                         col="red", 
                         fill="green") + 
          labs(title="Histogram for Unigrams Frequency") +
          labs(x="Frequency",y="Number of Unigrams") 
        
        
        g2 <- ggplot(data=DT.bi.prob.final, aes(freq2)) +
          geom_histogram(breaks=seq(1, 50, by = 1),
                         col="red", 
                         fill="blue") +
          labs(title="Histogram for Bigrams Frequency") +
          labs(x="Frequency",y="Number of Bigrams") 
        
        g3 <- ggplot(data=DT.tri.prob.final, aes(freq3)) +
          geom_histogram(breaks=seq(1, 50, by = 1),
                         col="red", 
                         fill="white") + 
          labs(title="Histogram for Trigrams Frequency") +
          labs(x="Frequency",y="Number of Trigrams") 
        
        g4 <- ggplot(data=DT.quad.prob.final, aes(freq4)) +
          geom_histogram(breaks=seq(1, 50, by = 1),
                         col="yellow", 
                         fill="red") +
          labs(title="Histogram for Quadgrams Frequency") +
          labs(x="Frequency",y="Number of Quadgrams") 
        
        grid.arrange(g1, g2, g3,g4, ncol = 2, nrow = 2)
      })
    })
  })
            
  
  # This function will create the wordcloud 
  
  output$word_cloud <- renderPlot({
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
  

  getPage<-function(x) {
    return(includeHTML(x))
  }
  
  output$report1 <-renderUI({getPage("Middle_Report.html")})
  
  output$report2 <-renderUI({getPage("Final_Report_v2.html")})
  
  output$report3 <-renderUI({getPage("Output_Report_v2.html")})
  
  
})
 
