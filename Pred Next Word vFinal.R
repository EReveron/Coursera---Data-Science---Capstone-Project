#####################################################################
# Predict Next Word vFinal using the backoff implementation 
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

create_filename <- function(x,lines=-1) {
  
  if (lines < 0) {
    paste(x,"_all.RData",sep= "")
  }
  else {
    paste(x,"_",lines,".RData",sep= "")
  }
}



load_DT_prob_table <- function (n,lines=-1) {
  
  if (n == 1) {
    var.name <- "DT.uni.prob"
    file.name <- create_filename("DT_uni_prob_final",lines)
  } else if (n == 2) {
    var.name <- "DT.bi.prob"
    file.name <- create_filename("DT_bi_prob_final",lines)
  } else if (n == 3) {
    var.name <- "DT.tri.prob"
    file.name <- create_filename("DT_tri_prob_final",lines)
  } else if (n == 4) {
    var.name <- "DT.quad.prob"
    file.name <- create_filename("DT_quad_prob_final",lines)
  }
  
  #Validate if the DT exists in the enviroment
  if (!exists(var.name)) {
    #Validate if the file exists an load the value
    if (file.exists(file.name)) {
      print(paste("-----> load_DT_prob_table(",n,").......",sep=""))
      t1 <- proc.time()
      
      print(paste("Loading DT probability file: ",file.name, sep =""))
      load(file.name,.GlobalEnv) 
      
      print(paste("Initialization of DT Prob Table from:",var.name, sep =""))
      
      if (n == 1) {
        DT.uni.prob <<- as.data.table(DT.uni.prob, key = "t1")
        #DT.uni.prob[,prob:=pkn1,]
        
        
      } else if (n == 2) {
        DT.bi.prob <<- as.data.table(DT.bi.prob, key = "t1,t2")
        #DT.bi.prob[,prob:=pkn1,]
      } else if (n == 3) {
        DT.tri.prob <<- as.data.table(DT.tri.prob, key = "t1,t2,t3")
        #DT.tri.prob[,prob:=pkn1,]
      } else if (n == 4) {
        DT.quad.prob <<- as.data.table(DT.quad.prob, key = "t1,t2,t3,t4")
        #DT.quad.prob[,prob:=pkn1,]
      }
      t2 <- proc.time()
      
      print(paste("-----> load_DT_prob_table: Running Time .......",
                  elapsed_time(t1,t2)," seconds ...",sep=""))
    }
    else {
      # Error file doesn't exists
      print(paste("Error file doesnt exist:",file.name, sep=""))
    }
  }
}


topn_predict <- function(x,p=0,n=5,f=1) {
  # f is the factor
  
  
  print(paste("-----> topn_predict(",
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
    #uni-gram level, let's check the bigram table and apply the factor for backoff
    #topn <- DT.bi[t1 == x[1] & ((prob*f) >= p),
    #              list(t2, prob2 = prob*f),] [head(order(-prob2),n)]

    #topn <- DT.bi.prob[t1 == x[1] & ((prob*f) >= p),][,word:=t2,][,prob:= prob*f,][,list(word,prob),][head(order(-prob),n)]
    #
    
    topn <- DT.bi.prob[t1 == x[1] & ((prob*f) >= p),,]
    topn[,c("word","prob") := list(t2,prob*f),]
    topn <- topn[,list(word,prob),][head(order(-prob),n)]

        
    #topn[,word:=t2,] 
    #topn[,prob:= prob*f,]
    #topn <- topn[,list(word,prob),]
    #topn <- topn[head(order(-prob),n)]
    #print(topn)
    
    num_words <- nrow(topn)
    
    print(paste("...Found:",num_words," words ..."))
    print(topn)
    
    if (num_words < n) {
      
      print("... Default Checking Probability in Unigram Data Table")
      
      #a <- DT.uni.prob[head(order(-prob),n -num_words)] 
      
      a <- DT.uni.prob[prob >= p,,]
      
      a <- a[head(order(-prob),n)] 
      a[,word:=t1,] 
      a <- a[,list(word,prob),]
  
      print(paste("...Found:",nrow(a)," words ..."))
      print(a)
      
      
      topn <- rbind(topn,a)
      
    }
  }
  else if (l == 2) {
    #bi-gram level, let's check the trigram table and apply the factor for backoff
    #topn <- DT.tri[t1 == x[1] & t2 == x[2] & (prob*f) >= p,
    #               list(t3,prob2 = prob*f),] [head(order(-prob2),n)]
    
    #topn <- DT.tri.prob[t1 == x[1] & t2 == x[2] & (prob*f) >= p,] [,list(word:=t3,prob:= prob*f),] [head(order(prob),n)]

    
    topn <- DT.tri.prob[t1 == x[1] & t2 == x[2] & ((prob*f) >= p),,]
    topn[,c("word","prob") := list(t3,prob*f),]
    topn <- topn[,list(word,prob),][head(order(-prob),n)]
    
        
    num_words <- nrow(topn)
    
    print(paste("...Found:",num_words," words ..."))
    print(topn)
    
    if (num_words < n) {
      print(paste("... Backoff to Unigram Level with factor:",0.4*f))
      #topn <- rbind(topn,topn_predict(c(x[2]),p,n-num_words,0.4*f))
      topn <- rbind(topn,topn_predict(c(x[2]),p,n,0.4*f))
    } 
  }
  else if (l == 3) {
    #tri-gram level, let's check the quad.gram table
    #topn <- DT.quad[t1 == x[1] & t2 == x[2] & t3 == x[3] & (prob*f) >= p,
    #                list(t4,prob2 = prob*f),] [head(order(-prob2),n)]
    
    #topn <- DT.quad.prob[t1 == x[1] & t2 == x[2] & t3 == x[3] & (prob*f) >= p,
    #                list(word:=t4,prob:= prob*f),] [head(order(prob2),n)]
    
    
    topn <- DT.quad.prob[t1 == x[1] & t2 == x[2] & t3 ==x[3] & ((prob*f) >= p),,]
    topn[,c("word","prob") := list(t4,prob*f),]
    topn <- topn[,list(word,prob),][head(order(-prob),n)]
    
    num_words <- nrow(topn)
    
    print(paste("...Found:",num_words," words..."))
    print(topn)
    
    if (num_words < n) {
      print(paste("... Backoff to Bigram Level with factor:",0.4*f))
      #topn <- rbind(topn,topn_predict(c(x[2],x[3]),p,n-num_words,0.4*f))
      topn <- rbind(topn,topn_predict(c(x[2],x[3]),p,n,0.4*f))
      
    }
  }
  #colnames(topn) <- c("word","prob")
  
  t2 <- proc.time()
  
  print(paste("-----> topn_predict: Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
  
  topn
}


predict_nextword <- function(x,p=0,n=5,lines=-1) {
  
  l <- length(x)
  
  load_DT_prob_table(1,lines)
  load_DT_prob_table(2,lines)
  load_DT_prob_table(3,lines)
  load_DT_prob_table(4,lines)
  
  print(paste("-----> predict_nextword(",
              " word:=(",paste(x, collapse=","),")",
              ", lines:=",lines,
              ", prob:=",p,
              ", n:=",n,
              ").......",sep=""))
  t1 <- proc.time()
  
  result <- topn_predict(x,p,n,1)

  t2 <- proc.time()
  print(paste("-----> predict_nextword: Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
  
  # Remove duplicated values, some words could appers duplicated as a part of
  # backoff strategy
  setkey(result,word)
  result <- unique(result)
  result[head(order(-prob),n)]

}  


