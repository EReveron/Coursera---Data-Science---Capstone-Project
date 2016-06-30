#####################################################################
# Predict next word regex vFinal using the backoff implementation with incomplete
#   and the knersey-ney probabilities already calculated previously:
#
#             DT_uni_prob.Rdata: data table that include all the unigrams 
#                       frequency and the knersey-ney prob
#             DT_bi_prob.Rdata: data table that include all the bigrams 
#                       frequency and the knersey-ney prob
#             DT_tri_prob.Rdata: data table that include all the trigrams 
#                       frequency and the knersey-ney prob
#             DT_quad_prob.Rdata: data table that include all the quadgrams 
#                       frequency and the knersey-ney prob

library(data.table)

#setwd("D:/Coursera/Capstone Project/Coursera-SwiftKey/final/en_US")
#setwd("D:/001 -- Coursera/Capstone Project/Coursera-SwiftKey/final/en_US")

# For reproducibility
set.seed(12345)

elapsed_time <- function(tic1,tic2) {
  format((tic2-tic1)[3][[1]], digits = 2)
}


topn_predict_regex <- function(x,p=0,n=5,f=1) {
  # f is the factor
  
  
  print(paste("-----> topn_predict_regex(",
              " word:=(",paste(x, collapse=","),")",
              ", prob:=",p,
              ", nun_words:=",n,
              ", factor:=",f,
              ").......",sep=""))
  
  
  #print(paste("-----> topn_predict(",x,",",p,",",n,",",f,").......",sep=""))
  t1 <- proc.time()
  topn <- NULL
  
  l <- length(x)
  
  if (l == 1) {
    #uni-gram level, let's check the unigram table for the regex and apply the factor for backoff

    regex_word <- paste("^",x[1],".",sep="")
    
    topn <- DT.uni.prob[grepl(regex_word,t1),,]
    topn <- topn[((prob*f) >= p),,]
    topn <- topn[head(order(-prob),n)] 
    topn[,word:=t1,] 
    topn <- topn[,list(word,prob),]

    num_words <- nrow(topn)
    
    print(paste("...Found:",num_words," words ..."))
    print(topn)
  } else if (l == 2) {
    #bi-gram level, let's check the unigram table for the regex and apply the factor for backoff
    
    regex_word <- paste("^",x[2],".",sep="")
    
    topn <- DT.bi.prob[t1 == x[1] & ((prob*f) >= p),,]
    topn <- topn[grepl(regex_word,t2),,]
    
    topn <- topn[head(order(-prob),n)] 
    topn[,word:=t2,] 
    topn <- topn[,list(word,prob),]
    
    num_words <- nrow(topn)
    
    print(paste("...Found:",num_words," words ..."))
    print(topn)
    

    if (num_words < n) {
      print(paste("... Backoff to Unigram Level with factor:",0.4*f))
      topn <- rbind(topn,topn_predict_regex(c(x[2]),p,n,0.4*f))
    }
  }  else if (l == 3) {
    #tri-gram level, let's check the trigram table for the regex and apply the factor for backoff
    
    regex_word <- paste("^",x[3],".",sep="")
    
    topn <- DT.tri.prob[t1 == x[1] & t2 == x[2] & ((prob*f) >= p),,]
    topn <- topn[grepl(regex_word,t3),,]
    
    topn <- topn[head(order(-prob),n)] 
    topn[,word:=t3,] 
    topn <- topn[,list(word,prob),]
    
    num_words <- nrow(topn)
    
    print(paste("...Found:",num_words," words ..."))
    print(topn)
    
    
    if (num_words < n) {
      print(paste("... Backoff to Bigram Level with factor:",0.4*f))
      topn <- rbind(topn,topn_predict_regex(c(x[2],x[3]),p,n,0.4*f))
    }
  } else if (l == 4) {
    #quad-gram level, let's check the quadgram table for the regex and apply the factor for backoff
    
    regex_word <- paste("^",x[4],sep="")
    
    topn <- DT.quad.prob[t1 == x[1] & t2 == x[2] & t3 == x[3] & ((prob*f) >= p),,]
    topn <- topn[grepl(regex_word,t4),,]
    
    topn <- topn[head(order(-prob),n)] 
    topn[,word:=t4,] 
    topn <- topn[,list(word,prob),]
    
    num_words <- nrow(topn)
    
    print(paste("...Found:",num_words," words ..."))
    print(topn)
    
    
    if (num_words < n) {
      print(paste("... Backoff to Trigram Level with factor:",0.4*f))
      topn <- rbind(topn,topn_predict_regex(c(x[2],x[3],x[4]),p,n,0.4*f))
    }
  }
  t2 <- proc.time()
  
  print(paste("-----> topn_predict_regex: Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
  
  topn
}


predict_nextword_regex <- function(x,p=0,n=5,lines=-1) {
  
  l <- length(x)
  
  load_DT_prob_table(1,lines)
  load_DT_prob_table(2,lines)
  load_DT_prob_table(3,lines)
  load_DT_prob_table(4,lines)
  
  print(paste("-----> predict_nextword_regex(",
              " word:=(",paste(x, collapse=","),")",
              ", lines:=",lines,
              ", prob:=",p,
              ", n:=",n,
              ").......",sep=""))
  t1 <- proc.time()
  
  result <- topn_predict_regex(x,p,n,1)

  t2 <- proc.time()
  print(paste("-----> predict_nextword_regex: Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
  
  # Remove duplicated values, some words could appers duplicated as a part of
  # backoff strategy
  setkey(result,word)
  result <- unique(result)
  result[head(order(-prob),n)]

}  


