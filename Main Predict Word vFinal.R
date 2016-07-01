
library(stringi)
library(data.table)

#setwd("D:/Coursera/Capstone Project/Coursera-SwiftKey/final/en_US/FINAL2/Final 27-06")
#setwd("D:/001 -- Coursera/Capstone Project/Coursera-SwiftKey/final/en_US")

# For reproducibility
set.seed(12345)

source("Pred Next Word vFinal.R")
source("Pred Next Word Regex vFinal.R")


last_n_words <- function(s, num_words = 3) {
  
  # Lower, trim and remove additional blank spaces
  
  s <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", tolower(s))
  s <- gsub("[[:space:]]+", " ", s)
  
  words_list <- unlist(stri_split_fixed(s," "))
  
  l <- length(words_list)
  
  if (l < num_words) {
    # Not enough words in the string, return all the words
    words_list
  } else {
    # Return only the last n words
    words_list[(l-num_words+1):l]
  }
}


main_predict_word <- function(s, p=0,n=5, consider_regex_words = TRUE, lines = -1) {
  
  if (s != "") {
    s <- tolower(s)
    print(paste("String:",s,"...",sep=""))
    
    is_final_word <- stri_endswith_fixed(s," ")
    
    list_words <- last_n_words(s)
    
    if (consider_regex_words) {
      # Consider Regex Words in the Prediction
     
      if (is_final_word) {
        print("---> Considering Regex: YES, Predicting Next Word with Complete Words ...")
        result <- predict_nextword(list_words,p,n,lines)
      } else {
        print("---> Considering Regex: YES, Predicting Next Word with Regex Words...")
        result <- predict_nextword_regex(list_words,p,n,lines)
        
      }
    }
    else {
      # Consider Only Complete Words in the Prediction 
      
      if (is_final_word) {
        print("---> Considering Regex: NO, Predicting Next Word with Complete Words ...")
        result <- predict_nextword(list_words,p,n,lines)
      } else {
        print("---> Considering Regex: NO, Is a Regex Words ... Wait until a Complete Word Appear ...")
        result <- as.data.frame(character())
      }
    }
  } else {
    result <- as.data.frame(character())
  } 
  result
}

