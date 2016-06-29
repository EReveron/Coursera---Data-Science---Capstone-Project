#####################################################################
# Knersey-ney VFinal
# Calculate Knersey-ney of the differents ngrams (unigram, bigram, trigram and quadgrams)
#          to be used later with Kneser-ney algortihm to calculate the
#          probability of each ngram for predict the next word.
#          This script was run on my PC and takes long time to finish,
#          at the end this script saves the data into four files:
#
#             uni_dt.Rdata: data frame that include all the unigrams and the
#                          frequency for each one.
#             bi_dt.Rdata: data frame that include all the bigrams and the
#                          frequency for each one.
#             tri_dt.Rdata: data frame that include all the trigrams and the
#                          frequency for each one.
#             quad_dt.Rdata: data frame that include all the quadgrams and the
#                          frequency for each one.
# In this version we did some optimizations related with the recursion.
# We start calculating the unigrams probability. This calculation could be used
# on bigrams calculations. To do that we also store lambda(x). 

library(data.table)

#setwd("D:/Coursera/Capstone Project/Coursera-SwiftKey/final/en_US")
setwd("D:/001 -- Coursera/Capstone Project/Coursera-SwiftKey/final/en_US")

# For reproducibility
set.seed(12345)

elapsed_time <- function(tic1,tic2) {
  format((tic2-tic1)[3][[1]], digits = 2)
}

create_filename <- function(x,lines) {
  
  if (lines < 0) {
    paste(x,"_all.RData",sep= "")
  }
  else {
    paste(x,"_",lines,".RData",sep= "")
  }
}

load_DT_table <- function (n,lines=-1) {
  
  if (n == 1) {
    var.name <- "DT.uni"
    file.name <- create_filename("DT_uni",lines)
  } else if (n == 2) {
    var.name <- "DT.bi"
    file.name <- create_filename("DT_bi",lines)
  } else if (n == 3) {
    var.name <- "DT.tri"
    file.name <- create_filename("DT_tri",lines)
  } else if (n == 4) {
    var.name <- "DT.quad"
    file.name <- create_filename("DT_quad",lines)
  }
  
  #Validate if the DT exists in the enviroment
  if (!exists(var.name)) {
    #Validate if the file exists an load the value
    if (file.exists(file.name)) {
      print(paste("-----> load_DT_table(",n,").......",sep=""))
      t1 <- proc.time()
      
      print(paste("Loading DT file: ",file.name, sep =""))
      load(file.name,.GlobalEnv) 
      
      print(paste("Creating DT Table from:",var.name, sep =""))
      
      if (n == 1) {
        DT.uni <<- as.data.table(DT.uni, key = "t1")
      } else if (n == 2) {
        DT.bi <<- as.data.table(DT.bi, key = "t1,t2")
      } else if (n == 3) {
        DT.tri <<- as.data.table(DT.tri, key = "t1,t2,t3")
      } else if (n == 4) {
        DT.quad <<- as.data.table(DT.quad, key = "t1,t2,t3,t4")
      }
      t2 <- proc.time()
      print(paste("-----> load_DT_table: Running Time .......",
                  elapsed_time(t1,t2)," seconds ...",sep=""))
      
    }
    else {
      # Error file doesn't exists
      print(paste("Error file doesnt exist:",file.name, sep=""))
    }
  }
}


init_DT_table <- function (n,lines=-1,p1=5) { 

  # Values needed for Knersey-prob
  p1 <<- p1
  p2 <<- p1 + 1
  
  if (n == 1) {
    file.name <- create_filename("DT_uni_prob_temp",lines)
  } else if (n == 2) {
    var.name <- "DT.bi.prob"
    file.name <- create_filename("DT_bi_prob_temp",lines)
  } else if (n == 3) {
    file.name <- create_filename("DT_tri_prob_temp",lines)
  } else if (n == 4) {
    file.name <- create_filename("DT_quad_prob_temp",lines)
  }
  
  #Validate if exists a DT probability temporal table
  if (file.exists(file.name)) {
    print(paste("Loading DT with probability temporal file: ",file.name, sep =""))
    load(file.name,.GlobalEnv) 
  } else {
    
    # Init DT table
    print(paste("-----> init_DT_table(",n,",",p1,").......",sep=""))
    t1 <- proc.time()
    
    if (n==1) {
      DT.uni <<- DT.uni[freq >= p1,,]
      DT.uni[,n11:=-1,]
      DT.uni[,n12:=-1,]
      DT.uni[,l1:=-1,]
      DT.uni[,l2:=-1,]
      DT.uni[,a1:=-1,]
      DT.uni[,a2:=-1,]
      DT.uni[,pkn1:=-1,]
      DT.uni[,pkn2:=-1,]
      
    } else if (n==2) {
      DT.bi <<- DT.bi[freq >= p1,,]
      DT.bi[,n11:=-1,]
      DT.bi[,n12:=-1,]
      DT.bi[,l1:=-1,]
      DT.bi[,l2:=-1,]
      DT.bi[,pkn1:=-1,]
      DT.bi[,pkn2:=-1,]
      DT.bi[,a1:=-1,]
      DT.bi[,a2:=-1,]
      
    } else if (n==3) {
      DT.tri <<- DT.tri[freq >= p1,,]
      DT.tri[,n11:=-1,]
      DT.tri[,n12:=-1,]
      DT.tri[,l1:=-1,]
      DT.tri[,l2:=-1,]
      DT.tri[,pkn1:=-1,]
      DT.tri[,pkn2:=-1,]
      DT.tri[,a1:=-1,]
      DT.tri[,a2:=-1,]
      
    } else if (n==4) {
      DT.quad <<- DT.quad[freq >= p1,,]
      DT.quad[,n11:=-1,]
      DT.quad[,n12:=-1,]
      DT.quad[,l1:=-1,]
      DT.quad[,l2:=-1,]
      DT.quad[,pkn1:=-1,]
      DT.quad[,pkn2:=-1,]
      DT.quad[,a1:=-1,]
      DT.quad[,a2:=-1,]
      
    }
    t2 <- proc.time()
    
    
    print(paste("-----> init_DT_table: Running Time .......",
                elapsed_time(t1,t2)," seconds ...",sep=""))
    
  }
  
  if (n == 1) {
    n.uni <<- nrow(DT.uni)

  } else if (n ==2) {
    n.bi <<- nrow(DT.bi)
    ######### Discount values (D1, D2, D3) for lambda function
    #
    #   n1: number of ngrams that occurs exactly one time
    #   n2: number of ngrams that occurs exactly two times
    #    D1 = n1 / (n1 + 2 * n2)
    
    n1.bi <<- nrow(DT.bi[freq == p1,,])
    n2.bi <<- nrow(DT.bi[freq == p2,,])
    D2 <<- n1.bi / (n1.bi + 2 * n2.bi) 
  } else if (n==3) {
    n.tri <<- nrow(DT.tri)
    ######### Discount values (D1, D2, D3) for lambda function
    #
    #   n1: number of ngrams that occurs exactly one time
    #   n2: number of ngrams that occurs exactly two times
    #    D1 = n1 / (n1 + 2 * n2)
    
    n1.tri <<- nrow(DT.tri[freq == p1,,])
    n2.tri <<- nrow(DT.tri[freq == p2,,])
    D3 <<- n1.tri / (n1.tri + 2 * n2.tri) 
  } else if (n==4) {
    n.quad <<- nrow(DT.quad)
    ######### Discount values (D1, D2, D3) for lambda function
    #
    #   n1: number of ngrams that occurs exactly one time
    #   n2: number of ngrams that occurs exactly two times
    #    D1 = n1 / (n1 + 2 * n2)
    #    
    
    n1.quad <<- nrow(DT.quad[freq == p1,,])
    n2.quad <<- nrow(DT.quad[freq == p2,,])
    D4 <<- n1.quad / (n1.quad + 2 * n2.quad) 
  }
  
}
  
  

init_DT_tables <- function (lines=-1,p1=5) { 
  
  print(paste("-----> init_DT_tables(",p1,").......",sep=""))
  t1 <- proc.time()

  ## Load and init the DT table  
  load_DT_table(1,lines)
  init_DT_table(1,lines,p1)
  
  load_DT_table(2,lines)
  init_DT_table(2,lines,p1)
  
  load_DT_table(3,lines)
  init_DT_table(3,lines,p1)
  
  load_DT_table(4,lines)
  init_DT_table(4,lines,p1)
  
  t2 <- proc.time()
  
  print(paste("-----> init_DT_tables: Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
}

N1plus_pre <- function(x) {
  # Count the number of unique words that precede the n-gram x 
  # with at least 1 appareance.
  #
  # Parameters:
  #   x:  n-gram to be considered, with the following format
  #       c("t1") # unigram
  #       c("t1",t2") # bigram
  #   l:  integer that identified the type of x
  #       1 #unigram
  #       2 #bigram
  #   dt: data table that have the frequency for each ngram
  #     for bigrams:
  #     c("t1","t2","freq","prob") # bigram
  #     c("t1","t2","t3","freq","prob") # trigram
  
  l <- length(x)
  r <- -1
  if (l == 1) {
    ## Count the predecessor words in bigram table
    #Check if the value doesn't exists and calculate it
    r <- DT.uni[t1 == x[1],n11,]

    if (r < 0) {
      #nrow(bi.dt[which(bi.dt$t2 == x[1] & bi.dt$freq >= 1),])
      r <- nrow(DT.bi[t2 == x[1] & freq >= 1,,])
      DT.uni[t1 == x[1],n11:=r,]
    }
    
  } else if (l == 2) {
    ## Count the predecessor words in trigram table
    
    #nrow(tri.dt[which(tri.dt$t2 == x[1] &
    #                    tri.dt$t3 == x[2] &
    #                    tri.dt$freq >=1 ),])
    r <- DT.bi[t1 == x[1] & t2 == x[2],n11,]
    if (r < 0) {
      r <- nrow(DT.tri[t2 == x[1] & t3 == x[2] & freq >= 1,,])
      DT.bi[t1 == x[1] & t2 == x[2],n11:=r,]
    }      
  } else if (l == 3) {
    ## Count the predecessor words in quad table
    
    #nrow(quad.dt[which(quad.dt$t2 == x[1] &
    #                     quad.dt$t3 == x[2] &
    #                     quad.dt$t4 == x[3] &
    #                     quad.dt$freq >=1 ),])
    r <- DT.tri[t1 == x[1] & t2 == x[2] & t3 == x[3],n11,]
    
    if (r < 0) {
      r <- nrow(DT.quad[t2 == x[1] & t3 == x[2] & t4 == x[3] & freq >= 1,,])
      DT.tri[t1 == x[1] & t2 == x[2] & t3 == x[3],n11:=r,]
    }
  }
  r
}



N1plus_suc <- function(x) {
  # Count the number of unique words that succeed the n-gram x 
  # with at least 1 appareance.
  #
  # Parameters:
  #   x:  n-gram to be considered, with the following format
  #       c("t1") # unigram
  #       c("t1",t2") # bigram
  #   l:  integer that identified the type of x
  #       1 #unigram
  #       2 #bigram
  #   dt: data table that have the frequency for each ngram
  #     for bigrams:
  #     c("t1","t2","freq","prob") # bigram
  #     c("t1","t2","t3","freq","prob") # trigram
  l <- length(x)
  r <- -1
  if (l == 1){
    ## Count the succeed words in bigram table
    
    #nrow(bi.dt[which(bi.dt$t1 == x[1] & bi.dt$freq >= 1),])
    
    r <- DT.uni[t1 == x[1],n12,]
    
    if (r<0)
    {
      r <- nrow(DT.bi[t1 == x[1] & freq >= 1,,])
      DT.uni[t1 == x[1],n12:=r,]
    }
    
  } else if (l == 2) {
    
    ## Count the succeed words in trigram table
    
    #nrow(tri.dt[which(tri.dt$t1 == x[1] & 
    #                    tri.dt$t2 == x[2] &
    #                    tri.dt$freq >=1 ),])
    
    r <- DT.bi[t1 == x[1] & t2 == x[2],n12,]
    
    if (r<0)
    {
      r <- nrow(DT.tri[t1 == x[1] & t2 == x[2] & freq >= 1,,])
      DT.bi[t1 == x[1] & t2 == x[2],n12:=r,]
    }
    
  } else if (l == 3) {
    
    ## Count the succeed words in quadgram table
    
    #nrow(quad.dt[which(quad.dt$t1 == x[1] & 
    #                     quad.dt$t2 == x[2] &
    #                     quad.dt$t3 == x[3] &
    #                     quad.dt$freq >=1 ),])
    
    r <-  DT.tri[t1 == x[1] & t2 == x[2] & t3 == x[3], n12,]
    
    if (r < 0) {
      r <- nrow(DT.quad[t1 == x[1] & t2 == x[2] & t3 == x[3] & freq >= 1,,])
      DT.tri[t1 == x[1] & t2 == x[2] & t3 == x[3], n12:=r,]
    }
    
  }
  r
}

ngram_count <- function(x) {
  # Count the number of times that the n-gram x appears on dt
  # Parameters:
  #   x:  n-gram to be considered, with the following format
  #       c("t1") # unigram
  #       c("t1",t2") # bigram
  #   l:  integer that identified the type of x
  #       1 #unigram
  #       2 #bigram
  #   dt: data table that have the frequency for each ngram
  #     for bigrams:
  #     c("t1","t2","freq","prob") # bigram
  #     c("t1","t2","t3","freq","prob") # trigram
  
  l <- length(x)
  if (l == 1) {
    ## Provide the frequency of the ngram on unigram table
    
    #(uni.dt[which(uni.dt$t1 == x[1]),])$freq
    
    DT.uni[t1 == x[1],freq,]
    
  } else if (l == 2) {
    ## Provide the frequency of the ngram on bigram table
    
    #(bi.dt[which(bi.dt$t1 == x[1] & 
    #               bi.dt$t2 == x[2]),])$freq
    DT.bi[t1 == x[1] & t2 == x[2],freq,]
    
  } else if (l == 3) {
    ## Provide the frequency of the ngram on trigram table
    
    #(tri.dt[which(tri.dt$t1 == x[1] & 
    #                tri.dt$t2 == x[2] &
    #                tri.dt$t3 == x[3] ),])$freq
    DT.tri[t1 == x[1] & t2 == x[2] & t3 == x[3],freq,]
    
  } else if (l == 4) {
    ## Provide the frequency of the ngram on quadgram table
    
    #(quad.dt[which(quad.dt$t1 == x[1] & 
    #                 quad.dt$t2 == x[2] &
    #                 quad.dt$t3 == x[3] &
    #                 quad.dt$t4 == x[4]),])$freq
    
    DT.quad[t1 == x[1] & t2 == x[2] & t3 == x[3] & t4 == x[4],freq,]
    
  }
  
}


count_kn <- function(x,high_order) {
  # Count the number of times that the n-gram x appears on dt
  # Parameters:
  #   x:  n-gram to be considered, with the following format
  #       c("t1") # unigram
  #       c("t1",t2") # bigram
  #   l:  integer that identified the type of x
  #       1 #unigram
  #       2 #bigram
  #   dt: data table that have the frequency for each ngram
  #     for bigrams:
  #     c("t1","t2","freq","prob") # bigram
  #     c("t1","t2","t3","freq","prob") # trigram
  
  if (high_order) {
    # For high order calculations, use ngram_count(x)
    ngram_count(x)
  } else {
    # For low order calculation, use 
    N1plus_pre(x)
  } 
}



####################################

alpha <- function(x,high_order) {
  # Lambda function for Kneser_ney
  # Parameters:
  #   lambda(x) = (D / N1+_prec(x)) * N1+_suc(x)
  #   x:  n-gram to be considered, with the following format
  #       c("t1") # unigram
  #       c("t1",t2") # bigram
  #   dt: data table that have the frequency for each ngram
  #     c("t1","t2","freq","prob") # bigram
  #     c("t1","t2","t3","freq","prob") # trigram
  l <- length(x)
  r <- -1
  if (l == 2) {
    # For bigrams use D2 for calculation
    
    if (high_order) {
      r <- DT.bi[t1 == x[1] & t2 == x[2],a1,]
    } else {
      r <- DT.bi[t1 == x[1] & t2 == x[2],a2,]
    }
    
    
    if (r<0)
    {
      #r <- (D2 / count_kn(x,high_order)) * N1plus_suc(x)
      r <- max( count_kn(x,high_order) - D2, 0) / count_kn(x[1],high_order)
      
      if (high_order) {
        DT.bi[t1 == x[1] & t2 == x[2],a1:=r,]
      } else {
        DT.bi[t1 == x[1] & t2 == x[2],a2:=r,]
      }
    }
    
    
  } else if (l == 3) {
    # For trigrams use D3 for calculation 
    
    if (high_order) {
      r <- DT.tri[t1 == x[1] & t2 == x[2] & t3 ==x[3],a1,]
    } else {
      r <- DT.tri[t1 == x[1] & t2 == x[2] & t3 ==x[3],a2,]
    }
    
    if (r<0) {
      #r <- (D3 / count_kn(x,high_order)) * N1plus_suc(x)
      r <- max( count_kn(x,high_order) - D3, 0) / count_kn(c(x[1],x[2]),high_order)
      
      
      if (high_order) {
        DT.tri[t1 == x[1] & t2 == x[2] & t3 ==x[3],a1:=r,]
      } else {
        DT.tri[t1 == x[1] & t2 == x[2] & t3 ==x[3],a2:=r,]
      }
    }
    
    
  } else if (l == 4) {
    # For quadgrams use D4 for calculation 
    
    if (high_order) {
      r <- DT.quad[t1 == x[1] & t2 == x[2] & t3 ==x[3] & t4 ==x[4],a1,]
    } else {
      r <- DT.quad[t1 == x[1] & t2 == x[2] & t3 ==x[3] & t4 ==x[4],a2,]
    }
    
    if (r<0) {
      #r <- (D4 / count_kn(x,high_order)) * N1plus_suc(x)
      r <- max( count_kn(x,high_order) - D4, 0) / count_kn(c(x[1],x[2],x[3]),high_order)
      
      #max( N1plus_pre(x) - D4, 0) / N1plus_pre(c(x[1],x[2],x[3]))
      
      if (high_order) {
        DT.quad[t1 == x[1] & t2 == x[2] & t3 ==x[3] & t4 ==x[4],a1:=r,]
      } else {
        DT.quad[t1 == x[1] & t2 == x[2] & t3 ==x[3] & t4 ==x[4],a2:=r,]
      }    
    }
  }
  r
}


####################################

lambda <- function(x,high_order) {
  # Lambda function for Kneser_ney
  # Parameters:
  #   lambda(x) = (D / N1+_prec(x)) * N1+_suc(x)
  #   x:  n-gram to be considered, with the following format
  #       c("t1") # unigram
  #       c("t1",t2") # bigram
  #   dt: data table that have the frequency for each ngram
  #     c("t1","t2","freq","prob") # bigram
  #     c("t1","t2","t3","freq","prob") # trigram
  l <- length(x)
  r <- -1
  if (l == 1) {
    # For unigrams use D2 for calculation
    
    if (high_order) {
      r <- DT.uni[t1 == x[1],l1,]
    } else {
      r <- DT.uni[t1 == x[1],l2,]
    }
    
    if (r<0)
    {
      r <- (D2 / count_kn(x,high_order)) * N1plus_suc(x)
      if (high_order) {
        DT.uni[t1 == x[1],l1:=r,]
      } else {
        DT.uni[t1 == x[1],l2:=r,]
      }
    }
    
    
  } else if (l == 2) {
    # For bigrams use D3 for calculation 
    
    if (high_order) {
      r <- DT.bi[t1 == x[1] & t2 == x[2],l1,]
    } else {
      r <- DT.bi[t1 == x[1] & t2 == x[2],l2,]
    }
    
    if (r<0) {
      r <- (D3 / count_kn(x,high_order)) * N1plus_suc(x)
      if (high_order) {
        DT.bi[t1 == x[1] & t2 == x[2],l1:=r,]
      } else {
        DT.bi[t1 == x[1] & t2 == x[2],l2:=r,]
      }
    }
    
    
  } else if (l == 3) {
    # For trigrams use D4 for calculation 
    
    if (high_order) {
      r <- DT.tri[t1 == x[1] & t2 == x[2] & t3 ==x[3],l1,]
    } else {
      r <- DT.tri[t1 == x[1] & t2 == x[2] & t3 ==x[3],l2,]
    }
    
    if (r<0) {
      r <- (D4 / count_kn(x,high_order)) * N1plus_suc(x)
      if (high_order) {
        DT.tri[t1 == x[1] & t2 == x[2] & t3 ==x[3],l1:=r,]
      } else {
        DT.tri[t1 == x[1] & t2 == x[2] & t3 ==x[3],l2:=r,]
      }    
    }
  }
  r
}


#########################################

prob_cont_kn <- function(x) {
  # For recursion purposes, when we call this function we are in low order
  l <- length(x)
  r <- -1
  
  if (l ==1) { 
    #unigram level
    r <- DT.uni[t1 == x[1],pkn1,]
    
    if (r < 0) {
      r <- N1plus_pre(x) / n.bi
      DT.uni[t1 == x[1],pkn1:=r,]
    }
  }
  else if (l==2) { 
    #bigram level
    
    r <- DT.bi[t1 == x[1] & t2 == x[2],pkn2,]
    if (r < 0) {
      r <- alpha(x,FALSE) + lambda(x[1],FALSE) * prob_cont_kn(x[2])
      DT.bi[t1 == x[1] & t2 == x[2],pkn2:=r,]
    }
    
  } else if (l==3) {
    #trigram level
    r <- DT.tri[t1 == x[1] & t2 == x[2] & t3 == x[3],pkn2,]
    if (r < 0) {
      r <- alpha(x,FALSE) + lambda(c(x[1],x[2]),FALSE) * prob_cont_kn(c(x[2],x[3]))
      DT.tri[t1 == x[1] & t2 == x[2] & t3 == x[3],pkn2:=r,]
    }
  } else if (l==4) {
    #quadgram level
    
    r <- DT.quad[t1 == x[1] & t2 == x[2] & t3 == x[3] & t4 == x[4],pkn2,]
    if (r < 0) {
      r <- alpha(x,FALSE) + lambda(c(x[1],x[2],x[3]),FALSE) * prob_cont_kn(c(x[2],x[3],x[4]))
      DT.quad[t1 == x[1] & t2 == x[2] & t3 == x[3] & t4 == x[4],pkn2:=r,]
    }
  }
  r
}

prob_kneser_ney <- function(x) {
  
  l <- length(x)
  r <- -1
  
  if (l == 1) {
    #uni-gram level
    r <- DT.uni[t1 == x[1],pkn1,]
    
    if (r < 0) {
      r <- N1plus_pre(x) / n.bi
      DT.uni[t1 == x[1],pkn1:=r,]
    }
  }
  else if (l == 2) {
    #bigram level
    r <- DT.bi[t1 == x[1] & t2 == x[2],pkn1,]
    if (r < 0) {
      r <- alpha(x,TRUE) + lambda(x[1],TRUE) * prob_cont_kn(x[2])
      DT.bi[t1 == x[1] & t2 == x[2],pkn1:=r,]
    }
  }
  else if (l == 3) {
    #trigram level
    r <- DT.tri[t1 == x[1] & t2 == x[2] & t3 == x[3],pkn1,]
    if (r < 0) {
      r <- alpha(x,TRUE) + lambda(c(x[1],x[2]),TRUE) * prob_cont_kn(c(x[2],x[3]))
      DT.tri[t1 == x[1] & t2 == x[2] & t3 == x[3],pkn1:=r,]
    }
  }
  else if (l == 4) {
    #quadgram level
    r <- DT.quad[t1 == x[1] & t2 == x[2] & t3 == x[3] & t4 == x[4],pkn1,]
    if (r < 0) {
      r <- alpha(x,TRUE) + lambda(c(x[1],x[2],x[3]),TRUE) * prob_cont_kn(c(x[2],x[3],x[4]))
      DT.quad[t1 == x[1] & t2 == x[2] & t3 == x[3] & t4 == x[4],pkn1:=r,]
    }
  }
}


save_dt_prob_temp <- function(n,lines=-1) {
  if (n >= 1) {
    file.name <- create_filename("DT_uni_prob_temp",lines)
    print(paste("... Saving DT probability temp file:",file.name,sep=""))
    save(DT.uni,file=file.name)
  } 
  if (n >= 2) {
    file.name <- create_filename("DT_bi_prob_temp",lines)
    print(paste("... Saving DT probability temp file:",file.name,sep=""))
    save(DT.bi,file=file.name)
  }
  if (n >= 3) {
    file.name <- create_filename("DT_tri_prob_temp",lines)
    print(paste("... Saving DT probability temp file:",file.name,sep=""))
    save(DT.tri,file=file.name)
  } 
  if (n == 4) {
    file.name <- create_filename("DT_quad_prob_temp",lines)
    print(paste("... Saving DT probability temp file:",file.name,sep=""))
    save(DT.quad,file=file.name)
  }
}
  

calculate_prob_kn_range <- function(n,lines=-1,init,final)  
{
  
  if (n == 1) {
    file.name <- create_filename("DT_uni_prob",lines)
  } else if (n == 2) {
    file.name <- create_filename("DT_bi_prob",lines)
  } else if (n == 3) {
    file.name <- create_filename("DT_tri_prob",lines)
  } else if (n == 4) {
    file.name <- create_filename("DT_quad_prob",lines)
  }
  
  print(paste("-----> calculate_prob_kn_range(",n,",",lines,",",init,",",final,").......",sep=""))
  tic1 <- proc.time()

  if (n==1) {
    
    for (i in init:final) {
      t1 <- as.character(DT.bi[i,t1,])
      t2 <- as.character(DT.bi[i,t2,])
      prob_kneser_ney(c(t1,t2))
    }
    # Saving DT prob temp files
    save_dt_prob_temp(n,lines)
    
  }
  tic2 <- proc.time()
  print(paste("-----> calculate_prob_kn_range: Running Time .......",
              elapsed_time(tic1,tic2)," seconds ...",sep=""))
}


  


  
calculate_prob_kn <- function(n,lines=-1,numlines=-1) {
    
  if (n == 1) {
    var.name <- "DT.uni.prob"
    file.name <- create_filename("DT_uni_prob",lines)
    file.name.final <- create_filename("DT_uni_prob_final",lines)
  } else if (n == 2) {
    var.name <- "DT.bi.prob"
    file.name <- create_filename("DT_bi_prob",lines)
    file.name.final <- create_filename("DT_bi_prob_final",lines)
  } else if (n == 3) {
    var.name <- "DT.tri.prob"
    file.name <- create_filename("DT_tri_prob",lines)
    file.name.final <- create_filename("DT_tri_prob_final",lines)
  } else if (n == 4) {
    var.name <- "DT.quad.prob"
    file.name <- create_filename("DT_quad_prob",lines)
    file.name.final <- create_filename("DT_quad_prob_final",lines)
  }
  
  #Validate if the DT with the probability exists in the enviroment
  if (!exists(var.name)) {
    #Validate if the file exists an load the value
    if (file.exists(file.name)) {
      print(paste("Loading DT with probability file: ",file.name, sep =""))
      load(file.name,.GlobalEnv) 
    } else {
      
      print(paste("-----> calculate_prob_kn(",n,",",lines,",",numlines,").......",sep=""))
      tic1 <- proc.time()
      
      print(paste("... Creating DT with prob:", var.name, sep=""))
      
      if (n == 1) {
        ## For each unigram lets calculate the kneser-ney prob
        if (numlines == -1) { numlines <- nrow(DT.uni) }
        
        percent_1 <- round(numlines/100)
        print(paste("... Calculating Knersey-ney prob for:",numlines," 1% is:",percent_1))
        k <- 1
        m <- 1
        
        nic1 <- proc.time()
        
        for (i in 1:numlines) 
        {
          t1 <- as.character(DT.uni[i,t1,])
          prob_kneser_ney(c(t1))
          
          ### To give some feedback, print a message each 1%
          if (k==percent_1) {
            nic2 <- proc.time()
            print(paste("......",m,"% done in ",
                        elapsed_time(nic1,nic2)," seconds ...",sep=""))
            
            k <- 1
            m <- m + 1
          } else { k <- k + 1}
          #print(paste(" i:",i," total:",numlines," percent10:",percent_10," k:",k," m:",m))
            
        }
        #print(paste("... Saving DT probability file:",file.name,sep=""))
        #save(DT.uni,file=file.name)
        
        # Saving DT prob temp files
        save_dt_prob_temp(n,lines)
        
        # Clean the DT with only two columns (t1,prob)
        DT.uni.prob <<- DT.uni[,list(t1,pkn1),]
        DT.uni.prob[,prob:=pkn1,]
        DT.uni.prob <<- DT.uni.prob[,list(t1,prob),]
        
        print(paste("... Saving DT probability final file:",file.name.final,sep=""))
        save(DT.uni.prob,file=file.name.final)
        
        #rm("DT.uni",envir =.GlobalEnv)
      } else if (n == 2) {
        ## For each bigram lets calculate the kneser-ney prob
        
        if (numlines == -1) { numlines <- nrow(DT.bi)}
        
        
        percent_1 <- round(numlines/100)
        
        print(paste("... Calculating Knersey-ney prob for:",numlines," 1% is:",percent_1))
        
        
        k <- 1
        m <- 1
        
        nic1 <- proc.time()
        
        for (i in 1:numlines) {
          t1 <- as.character(DT.bi[i,t1,])
          t2 <- as.character(DT.bi[i,t2,])
          prob_kneser_ney(c(t1,t2))
          
          ### To give some feedback, print a message each 10%
          if (k==percent_1) {
            nic2 <- proc.time()
            print(paste("......",m,"% done in ",
                        elapsed_time(nic1,nic2)," seconds ...",sep=""))
            
            k <- 1
            m <- m + 1
          } else { k <- k + 1}
          print(paste(" i:",i," total:",numlines," percent1:",percent_1," k:",k," m:",m))
          
        }
        #print(paste("... Saving DT probability file:",file.name,sep=""))
        #save(DT.bi,file=file.name)
        
        # Saving DT prob temp files
        save_dt_prob_temp(n,lines)
        
        # Clean the DT with only two columns (t1,t2,prob)
        DT.bi.prob <<- DT.bi[,list(t1,t2,pkn1),]
        DT.bi.prob[,prob:=pkn1,]
        DT.bi.prob <<- DT.bi.prob[,list(t1,t2,prob),]
        
        print(paste("... Saving DT probability final file:",file.name.final,sep=""))
        save(DT.bi.prob,file=file.name.final)
        #rm("DT.bi",envir =.GlobalEnv)

      } else if (n == 3) {
        ## For each trigram lets calculate the kneser-ney prob
        

        if (numlines == -1) { numlines <- nrow(DT.tri) }
        
        percent_1 <- round(numlines/100)
        print(paste("... Calculating Knersey-ney prob for:",numlines," 1% is:",percent_1))
        k <- 1
        m <- 1
        
        nic1 <- proc.time()
        
        for (i in 1:numlines) {
          t1 <- as.character(DT.tri[i,t1,])
          t2 <- as.character(DT.tri[i,t2,])
          t3 <- as.character(DT.tri[i,t3,])
          prob_kneser_ney(c(t1,t2,t3))
          
          ### To give some feedback, print a message each 10%
          if (k==percent_1) {
            nic2 <- proc.time()
            print(paste("......",m,"% done in ",
                        elapsed_time(nic1,nic2)," seconds ...",sep=""))

            k <- 1
            m <- m + 1
          } else { k <- k + 1}
          print(paste(" i:",i," total:",numlines," percent10:",percent_10," k:",k," m:",m))
        }
        #print(paste("... Saving DT probability file:",file.name,sep=""))
        #save(DT.tri,file=file.name)
        
        # Saving DT prob temp files
        save_dt_prob_temp(n,lines)
        
        # Clean the DT with only four columns (t1,t2,t3,prob)
        DT.tri.prob <<- DT.tri[,list(t1,t2,t3,pkn1),]
        DT.tri.prob[,prob:=pkn1,]
        DT.tri.prob <<- DT.tri.prob[,list(t1,t2,t3,prob),]
        
        print(paste("... Saving DT probability final file:",file.name.final,sep=""))
        save(DT.tri.prob,file=file.name.final)
        
        #rm("DT.tri",envir =.GlobalEnv)
        
      } else if (n == 4) {
        ## For each quadgram lets calculate the kneser-ney prob
        
        if (numlines == -1) { numlines <- nrow(DT.quad) }
        
        percent_1 <- round(numlines/100)
        print(paste("... Calculating Knersey-ney prob for:",numlines," 1% is:",percent_1))
        k <- 1
        m <- 1
        
        nic1 <- proc.time()
        
        for (i in 1:numlines) {
          t1 <- as.character(DT.quad[i,t1,])
          t2 <- as.character(DT.quad[i,t2,])
          t3 <- as.character(DT.quad[i,t3,])
          t4 <- as.character(DT.quad[i,t4,])
          prob_kneser_ney(c(t1,t2,t3,t4))
          
          ### To give some feedback, print a message each 10%
          if (k==percent_1) {
            nic2 <- proc.time()
            
            print(paste("......",m,"% done in ",
                        elapsed_time(nic1,nic2)," seconds ...",sep=""))
            
            k <- 1
            m <- m + 10
          } else { k <- k + 1}
          print(paste(" i:",i," total:",numlines," percent1:",percent_1," k:",k," m:",m))
        }
        #print(paste("... Saving DT probability file:",file.name,sep=""))
        #save(DT.quad,file=file.name)
        
        # Saving DT prob temp files
        save_dt_prob_temp(n,lines)
        
        # Clean the DT with only five columns (t1,t2,t3,t4,prob)
        DT.quad.prob <<- DT.quad[,list(t1,t2,t3,t4,pkn1),]
        DT.quad.prob[,prob:=pkn1,]
        DT.quad.prob <<- DT.quad.prob[,list(t1,t2,t3,t4,prob),]
        
        print(paste("... Saving DT probability final file:",file.name.final,sep=""))
        save(DT.quad.prob,file=file.name.final)
        #rm("DT.quad",envir =.GlobalEnv)
      }
      tic2 <- proc.time()
      print(paste("-----> calculate_prob_kn: Running Time .......",
                  elapsed_time(tic1,tic2)," seconds ...",sep=""))
    }
  }
}