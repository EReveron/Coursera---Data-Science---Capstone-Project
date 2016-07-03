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
# on bigrams, trigrams and quadgrams calculations. To do that we also store some 
# values as N1plus_pre(x), N1plus_suc(x), alpha(x), lambda(x). 

library(data.table)

#setwd("D:/Coursera/Capstone Project/Coursera-SwiftKey/final/en_US")
#setwd("D:/001 -- Coursera/Capstone Project/Coursera-SwiftKey/final/en_US")

# For reproducibility
set.seed(12345)

####################################
elapsed_time <- function(tic1,tic2) {
  format((tic2-tic1)[3][[1]], digits = 3)
}

####################################
create_filename <- function(x,lines) {
  
  if (lines < 0) {
    paste(x,"_all.RData",sep= "")
  }
  else {
    paste(x,"_",lines,".RData",sep= "")
  }
}



####################################
N1plus_pre <- function(x) {
  # Count the number of unique words that precede the ngram x 
  # with at least 1 appareance (freq). This value will be stored
  # in the corresponding ngram table in the column with the name "n11"

  l <- as.character(length(x))
  r <- -1
  
  switch(l,
         "1" = { #Unigrams
           #Check if the value exists in the ngram table
           r <- DT.uni.prob[t1 == x[1],n11,]
           
           if (r < 0) {
             # Calculate the value and store it in the table
             r <- nrow(DT.bi.prob[t2 == x[1] & freq >= 1,,])
             DT.uni.prob[t1 == x[1],n11:=r,]
           }
         },
         "2" = { #Bigrams
           #Check if the value exists in the ngram table
           r <- DT.bi.prob[t1 == x[1] & t2 == x[2],n11,]
           
           if (r < 0) {
             # Calculate the value and store it in the table
             r <- nrow(DT.tri.prob[t2 == x[1] & t3 == x[2] & freq >= 1,,])
             DT.bi.prob[t1 == x[1] & t2 == x[2],n11:=r,]
           }
         },
         "3" = { #Trigrams
           #Check if the value exists in the ngram table
           r <- DT.tri.prob[t1 == x[1] & t2 == x[2] & t3 == x[3],n11,]
           
           if (r < 0) {
             # Calculate the value and store it in the table
             r <- nrow(DT.quad.prob[t2 == x[1] & t3 == x[2] & t4 == x[3] & freq >= 1,,])
             DT.tri.prob[t1 == x[1] & t2 == x[2] & t3 == x[3],n11:=r,]
           }
         }
  )
  r
}

####################################
N1plus_suc <- function(x) {
  # Count the number of unique words that succeed the ngram x 
  # with at least 1 appareance (freq). This value will be stored
  # in the corresponding ngram table in the column with the name "n12"
  
  l <- as.character(length(x))
  r <- -1
  
  switch(l,
         "1" = { #Unigram
           #Check if the value exists in the ngram table
           r <- DT.uni.prob[t1 == x[1],n12,]
           
           if (r < 0)
           {
             # Calculate the value and store it in the table
             r <- nrow(DT.bi.prob[t1 == x[1] & freq >= 1,,])
             DT.uni.prob[t1 == x[1],n12:=r,]
           }
         },
         "2" = { 
           #Check if the value exists in the ngram table
           r <- DT.bi.prob[t1 == x[1] & t2 == x[2],n12,]
           
           if (r < 0)
           {
             # Calculate the value and store it in the table
             r <- nrow(DT.tri.prob[t1 == x[1] & t2 == x[2] & freq >= 1,,])
             DT.bi.prob[t1 == x[1] & t2 == x[2],n12:=r,]
           }
         },
         "3" = {
           #Check if the value exists in the ngram table
           r <-  DT.tri.prob[t1 == x[1] & t2 == x[2] & t3 == x[3], n12,]
           
           if (r < 0) {
             # Calculate the value and store it in the table
             r <- nrow(DT.quad.prob[t1 == x[1] & t2 == x[2] & t3 == x[3] & freq >= 1,,])
             DT.tri.prob[t1 == x[1] & t2 == x[2] & t3 == x[3], n12:=r,]
           }
         }
  )
  r
}
         
         
         
    
####################################
ngram_count <- function(x) {
  # Count the number of times that the ngram x appears on the corresponding table

  l <- as.character(length(x))
  switch(l,
         "1" = DT.uni.prob[t1 == x[1],freq,],
         "2" = DT.bi.prob[t1 == x[1] & t2 == x[2],freq,],
         "3" = DT.tri.prob[t1 == x[1] & t2 == x[2] & t3 == x[3],freq,],
         "4" = DT.quad.prob[t1 == x[1] & t2 == x[2] & t3 == x[3] & t4 == x[4],freq,]
  )
}     
         
         
  

####################################
count_kn <- function(x,high_order) {
  # Count function for knersey-ney calculation. 
  
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
  # Alpha function for Kneser-ney:
  # alpha(x) = max( count_kn(x) - D2, 0) / count_kn(x[1],high_order)
  # This value will be stored in the corresponding ngram table in the column 
  # with the name "a1"/"a2" (high/low order)
  
  l <- as.character(length(x))
  r <- -1
  
  
  
  
  switch(l,
         "2" = { # bigrams, check the table for a1 or a2 values (high/low order)
           
           if (high_order) {
             r <- DT.bi.prob[t1 == x[1] & t2 == x[2],a1,]
           } else {
             r <- DT.bi.prob[t1 == x[1] & t2 == x[2],a2,]
           }
           
           
           if (r < 0) {
             # Calculate the value and store it in the table
             r <- max( count_kn(x,high_order) - D2, 0) / count_kn(x[1],high_order)
             
             if (high_order) {
               DT.bi.prob[t1 == x[1] & t2 == x[2],a1:=r,]
             } else {
               DT.bi.prob[t1 == x[1] & t2 == x[2],a2:=r,]
             }
           }
         },
         
         "3" = { # trigrams, check the table for a1 or a2 values (high/low order)
           if (high_order) {
             r <- DT.tri.prob[t1 == x[1] & t2 == x[2] & t3 ==x[3],a1,]
           } else {
             r <- DT.tri.prob[t1 == x[1] & t2 == x[2] & t3 ==x[3],a2,]
           }
           
           if (r < 0) {
             # Calculate the value and store it in the table
             r <- max( count_kn(x,high_order) - D3, 0) / count_kn(c(x[1],x[2]),high_order)
             
             if (high_order) {
               DT.tri.prob[t1 == x[1] & t2 == x[2] & t3 ==x[3],a1:=r,]
             } else {
               DT.tri.prob[t1 == x[1] & t2 == x[2] & t3 ==x[3],a2:=r,]
             }
           }
         },
         "4" = { # quadgrams, check the table for a1 or a2 values (high/low order)  
           if (high_order) {
             r <- DT.quad.prob[t1 == x[1] & t2 == x[2] & t3 ==x[3] & t4 ==x[4],a1,]
           } else {
             r <- DT.quad.prob[t1 == x[1] & t2 == x[2] & t3 ==x[3] & t4 ==x[4],a2,]
           }
           
           if (r < 0) {
             # Calculate the value and store it in the table
             r <- max( count_kn(x,high_order) - D4, 0) / count_kn(c(x[1],x[2],x[3]),high_order)
             
             if (high_order) {
               DT.quad.prob[t1 == x[1] & t2 == x[2] & t3 ==x[3] & t4 ==x[4],a1:=r,]
             } else {
               DT.quad.prob[t1 == x[1] & t2 == x[2] & t3 ==x[3] & t4 ==x[4],a2:=r,]
             }    
           }
         }
  )
  r
}          
      
  
 

####################################
lambda <- function(x,high_order=TRUE) {
  
  # Lambba function for Kneser-ney:
  #     lambda(x) = (D / count_kn(x) * N1+_suc(x)
  # This value will be stored in the corresponding ngram table in the column 
  # with the name "l1"/"l2" (high/low order)
  
  l <- as.character(length(x))
  r <- -1
  
  switch(l,
         "1" = { # Unigrams, check the table for l1 or l2 values (high/low order)
           if (high_order) {
             r <- DT.uni.prob[t1 == x[1],l1,]
           } else {
             r <- DT.uni.prob[t1 == x[1],l2,]
           }
           
           if (r < 0) {
             # Calculate the value and store it in the table
             r <- (D2 / count_kn(x,high_order)) * N1plus_suc(x)
             if (high_order) {
               DT.uni.prob[t1 == x[1],l1:=r,]
             } else {
               DT.uni.prob[t1 == x[1],l2:=r,]
             }
           }
         },
         
         "2" = { # Bigrams, check the table for l1 or l2 values (high/low order)
           if (high_order) {
             r <- DT.bi.prob[t1 == x[1] & t2 == x[2],l1,]
           } else {
             r <- DT.bi.prob[t1 == x[1] & t2 == x[2],l2,]
           }
           
           if (r < 0) {
             # Calculate the value and store it in the table
             r <- (D3 / count_kn(x,high_order)) * N1plus_suc(x)
             if (high_order) {
               DT.bi.prob[t1 == x[1] & t2 == x[2],l1:=r,]
             } else {
               DT.bi.prob[t1 == x[1] & t2 == x[2],l2:=r,]
             }
           }
         },
         "3" = { # Trigrams, check the table for l1 or l2 values (high/low order)
           if (high_order) {
             r <- DT.tri.prob[t1 == x[1] & t2 == x[2] & t3 ==x[3],l1,]
           } else {
             r <- DT.tri.prob[t1 == x[1] & t2 == x[2] & t3 ==x[3],l2,]
           }
           
           if (r < 0) {
             r <- (D4 / count_kn(x,high_order)) * N1plus_suc(x)
             if (high_order) {
               DT.tri.prob[t1 == x[1] & t2 == x[2] & t3 ==x[3],l1:=r,]
             } else {
               DT.tri.prob[t1 == x[1] & t2 == x[2] & t3 ==x[3],l2:=r,]
             }    
           }
         }
  )
  r
}
         
         
         
         
         


####################################
prob_kneser_ney <- function(x,high_order = TRUE) {
  
  l <- as.character(length(x))
  r <- -1
  
  switch(l,
         
         "1" = { # Unigrams, check the table for pkn1 values (high/low order)
           r <- DT.uni.prob[t1 == x[1],pkn1,]
    
           if (r < 0) {
             # Calculate the value and store it in the table
             r <- N1plus_pre(x) / n.bi
             DT.uni.prob[t1 == x[1],pkn1:=r,]
           }
         },
         
         "2" = { # Bigrams, check the table for pkn1 or pkn2 values (high/low order)
           if (high_order) {
             r <- DT.bi.prob[t1 == x[1] & t2 == x[2],pkn1,]
           } else {
             r <- DT.bi.prob[t1 == x[1] & t2 == x[2],pkn2,]
           }
           
           if (r < 0) {
             # Calculate the value and store it in the table
             r <- alpha(x,high_order) + 
               lambda(x[1],high_order) * prob_kneser_ney(x[2], FALSE)
             
             if (high_order) {
               DT.bi.prob[t1 == x[1] & t2 == x[2],pkn1:=r,]
             } else {
               DT.bi.prob[t1 == x[1] & t2 == x[2],pkn2:=r,]
             }
           } 
         },
         
         "3" = { # Trigrams, check the table for pkn1 or pkn2 values (high/low order)
           if (high_order) {
             r <- DT.tri.prob[t1 == x[1] & t2 == x[2] & t3 == x[3],pkn1,]
           } else {
             r <- DT.tri.prob[t1 == x[1] & t2 == x[2] & t3 == x[3],pkn2,]
           }
           if (r < 0) {
             # Calculate the value and store it in the table
             r <- alpha(x,high_order) + 
               lambda(c(x[1],x[2]),high_order) * prob_kneser_ney(c(x[2],x[3]),FALSE)
             if (high_order) {
               DT.tri.prob[t1 == x[1] & t2 == x[2] & t3 == x[3],pkn1:=r,]
             } else {
               DT.tri.prob[t1 == x[1] & t2 == x[2] & t3 == x[3],pkn2:=r,]
             }
           }
         },
         
         "4" = { # Quadgrams, check the table for pkn1 or pkn2 values (high/low order)
           if (high_order) {
             r <- DT.quad.prob[t1 == x[1] & t2 == x[2] & t3 == x[3] & t4 == x[4],pkn1,]
           } else {
             r <- DT.quad.prob[t1 == x[1] & t2 == x[2] & t3 == x[3] & t4 == x[4],pkn2,]
           }
           if (r < 0) {
             # Calculate the value and store it in the table
             r <- alpha(x,high_order) + 
               lambda(c(x[1],x[2],x[3]),high_order) * prob_kneser_ney(c(x[2],x[3],x[4]),FALSE)
             if (high_order) {
               DT.quad.prob[t1 == x[1] & t2 == x[2] & t3 == x[3] & t4 == x[4],pkn1:=r,]
             } else {
               DT.quad.prob[t1 == x[1] & t2 == x[2] & t3 == x[3] & t4 == x[4],pkn2:=r,]
             }
           }
         } 
  )
  r
}
  

####################################
load_DT_table <- function (n,lines=-1) {
  
  switch(n,
         "1" = {
           var.name <- "DT.uni"
           file.name <- create_filename("DT_uni",lines)
         },
         "2" = {
           var.name <- "DT.bi"
           file.name <- create_filename("DT_bi",lines)
         },
         "3" = {
           var.name <- "DT.tri"
           file.name <- create_filename("DT_tri",lines)
         },
         "4" = {
           var.name <- "DT.quad"
           file.name <- create_filename("DT_quad",lines)
         }
  )
  
  #Validate if the DT exists in the enviroment
  if (!exists(var.name)) {
    #Validate if the file exists an load the value
    if (file.exists(file.name)) {
      print(paste("-----> load_DT_table(",n,").......",sep=""))
      t1 <- proc.time()
      
      print(paste("Loading DT file: ",file.name, sep =""))
      load(file.name,.GlobalEnv) 
      
      print(paste("Creating DT Table from:",var.name, sep =""))
      
      switch(n,
             "1" = DT.uni <<- as.data.table(DT.uni, key = "t1"),
             "2" = DT.bi <<- as.data.table(DT.bi, key = "t1,t2"),
             "3" = DT.tri <<- as.data.table(DT.tri, key = "t1,t2,t3"),
             "4" = DT.quad <<- as.data.table(DT.quad, key = "t1,t2,t3,t4")
      )
      
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

####################################
load_DT_prob_table <- function (n,lines=-1,p1=5) {
  
  # Values needed for Knersey-ney prob calculations
  p1 <<- p1
  p2 <<- p1 + 1
  
  switch(n,
         "1" = {
           var.name <- "DT.uni.prob"
           file.name.temp <- create_filename("DT_uni_prob_temp",lines)
         },
         "2" = {
           var.name <- "DT.bi.prob"
           file.name.temp <- create_filename("DT_bi_prob_temp",lines)
         },
         "3" = {
           var.name <- "DT.tri.prob"
           file.name.temp <- create_filename("DT_tri_prob_temp",lines)
         },
         "4" = {
           var.name <- "DT.quad.prob"
           file.name.temp <- create_filename("DT_quad_prob_temp",lines)
         }
  )
  
  #Validate if the DT prob temp exists in the enviroment
  if (!exists(var.name)) {
    #Validate if the file exists an load the value
    if (file.exists(file.name.temp)) {
      print(paste("Loading DT Prob temp file: ",file.name.temp, sep =""))
      load(file.name.temp,.GlobalEnv) 
    } else {
      
      
      load_DT_table(n,lines)
      print(paste("-----> load_DT_prob_table(","n:=",n," p1:=",p1," lines:=",lines,").......",sep=""))
      t1 <- proc.time()
      
      switch(n,
             "1" = {
               DT.uni.prob <<- as.data.table(DT.uni, key = "t1")
               DT.uni.prob <<- DT.uni.prob[ freq >= p1,
                                            c("n11","n12","l1","l2","a1","a2","pkn1","pkn2") :=
                                              list(-1,-1,-1,-1,-1,-1,-1,-1),
                                            ]
               rm(DT.uni,envir =.GlobalEnv)
             },
             "2" = {
               DT.bi.prob <<- as.data.table(DT.bi, key = "t1,t2")
               DT.bi.prob <<- DT.bi.prob[ freq >= p1,
                                c("n11","n12","l1","l2","a1","a2","pkn1","pkn2") :=
                                  list(-1,-1,-1,-1,-1,-1,-1,-1),
                                ]
               rm(DT.bi,envir =.GlobalEnv)
             },
             "3" = {
               DT.tri.prob <<- as.data.table(DT.tri, key = "t1,t2,t3")
               DT.tri.prob <<- DT.tri.prob[ freq >= p1,
                                            c("n11","n12","l1","l2","a1","a2","pkn1","pkn2") :=
                                              list(-1,-1,-1,-1,-1,-1,-1,-1),
                                            ]
               rm(DT.tri,envir =.GlobalEnv)
             },
             "4" = {
               DT.quad.prob <<- as.data.table(DT.quad, key = "t1,t2,t3,t4")
               DT.quad.prob <<- DT.quad.prob[ freq >= p1,
                                              c("n11","n12","l1","l2","a1","a2","pkn1","pkn2") :=
                                                list(-1,-1,-1,-1,-1,-1,-1,-1),
                                              ]
               rm(DT.quad,envir =.GlobalEnv)
             }
      )
      t2 <- proc.time()
      print(paste("-----> load_DT_prob_table: Running Time .......",
                  elapsed_time(t1,t2)," seconds ...",sep=""))
    }
    
  }
  
  switch(n, # Calculate important values for Knersey-ney, including discount values 
         # (D2, D3, D4) for lambda function
         
         "1" = n.uni <<- nrow(DT.uni.prob),
         
         "2" = {
           
           #   n1: number of bigrams that occurs exactly p1 times
           #   n2: number of bigrams that occurs exactly p1+1 times
           #   D2 = n1 / (n1 + 2 * n2)
           print("Creando D values")
           
           n.bi <<- nrow(DT.bi.prob)
           n1.bi <<- nrow(DT.bi.prob[freq == p1,,])
           n2.bi <<- nrow(DT.bi.prob[freq == p2,,])
           D2 <<- n1.bi / (n1.bi + 2 * n2.bi) 
         },
         
         "3" = {
           #   n1: number of trigrams that occurs exactly p1 times
           #   n2: number of trigrams that occurs exactly p1+1 times
           #   D3 = n1 / (n1 + 2 * n2)
           n.tri <<- nrow(DT.tri.prob)
           n1.tri <<- nrow(DT.tri.prob[freq == p1,,])
           n2.tri <<- nrow(DT.tri.prob[freq == p2,,])
           D3 <<- n1.tri / (n1.tri + 2 * n2.tri) 
         },
         
         "4" = {
           #   n1: number of quadgrams that occurs exactly p1 times
           #   n2: number of quadgrams that occurs exactly p1+1 times
           #   D4 = n1 / (n1 + 2 * n2)
           n.quad <<- nrow(DT.quad.prob)
           n1.quad <<- nrow(DT.quad.prob[freq == p1,,])
           n2.quad <<- nrow(DT.quad.prob[freq == p2,,])
           D4 <<- n1.quad / (n1.quad + 2 * n2.quad) 
         }
         
  )
}    


####################################
load_DT_prob_tables <- function (n,lines=-1,p1=5) { 
  
  print(paste("-----> load_DT_prob_tables(n:=",n," p1:=",p1," lines:=",lines,").......",sep=""))
  t1 <- proc.time()
  
  if (n >= 1) {
    load_DT_prob_table(1,lines,p1)
    load_DT_prob_table(2,lines,p1)
  } 
    
  if (n >= 2) {
      load_DT_prob_table(3,lines,p1)
  }
  
  if (n >= 3) {
    load_DT_prob_table(4,lines,p1)
  } 
  t2 <- proc.time()
  
  print(paste("-----> load_DT_prob_tables: Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
}


####################################
save_DT_prob_temp <- function(n,lines=-1) {
  print(paste("-----> save_DT_prob_temp(n:=",n," lines:=",lines,").......",sep=""))
  t1 <- proc.time()
  
  if (n >= 1) {
    file.name <- create_filename("DT_uni_prob_temp",lines)
    print(paste("... Saving DT probability temp file:",file.name,sep=""))
    save(DT.uni.prob,file=file.name)
  } 
  if (n >= 2) {
    file.name <- create_filename("DT_bi_prob_temp",lines)
    print(paste("... Saving DT probability temp file:",file.name,sep=""))
    save(DT.bi.prob,file=file.name)
  }
  if (n >= 3) {
    file.name <- create_filename("DT_tri_prob_temp",lines)
    print(paste("... Saving DT probability temp file:",file.name,sep=""))
    save(DT.tri.prob,file=file.name)
  } 
  if (n == 4) {
    file.name <- create_filename("DT_quad_prob_temp",lines)
    print(paste("... Saving DT probability temp file:",file.name,sep=""))
    save(DT.quad.prob,file=file.name)
  }
  t2 <- proc.time()
  
  print(paste("-----> save_DT_prob_temp: Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
}
  
####################################  
calculate_prob_kn <- function(n,lines=-1,p1=5,numlines=-1) {
    
  switch(n,
         "1" = {
           var.name <- "DT.uni.prob.final"
           file.name <- create_filename("DT_uni_prob_final",lines)
           file.name.final <- create_filename("DT_uni_prob_final",lines)
         },
         "2" = {
           var.name <- "DT.bi.prob.final"
           file.name <- create_filename("DT_bi_prob_final",lines)
           file.name.final <- create_filename("DT_bi_prob_final",lines)
         },
         "3" = {
           var.name <- "DT.tri.prob.final"
           file.name <- create_filename("DT_tri_prob_final",lines)
           file.name.final <- create_filename("DT_tri_prob_final",lines)
         },
         "4" = {
           var.name <- "DT.quad.prob.final"
           file.name <- create_filename("DT_quad_prob_final",lines)
           file.name.final <- create_filename("DT_quad_prob_final",lines)
         }
  )
  
  #Validate if the DT with the probability exists in the enviroment
  if (!exists(var.name)) {
    #Validate if the file exists an load the value
    if (file.exists(file.name)) {
      print(paste("Loading DT with probability file: ",file.name, sep =""))
      load(file.name,.GlobalEnv) 
    } else {
      
      load_DT_prob_tables(n,lines,p1)
      
      print(paste("-----> calculate_prob_kn(",n,",",lines,",",numlines,").......",sep=""))
      tic1 <- proc.time()
      
      print(paste("Calculating DT Prob Table:", var.name, sep=""))
      
      
      switch(n,
             "1" = { # For each unigram lets calculate the kneser-ney prob
               if (numlines == -1) { numlines <- nrow(DT.uni.prob) }
               percent_1 <- round(numlines/100)
               print(paste("Calculating Knersey-ney prob for:",numlines," 1% is:",percent_1))
            
               k <- 1
               m <- 1
               nic1 <- proc.time()
               
               for (i in 1:numlines) 
               {
                 t1 <- as.character(DT.uni.prob[i,t1,])
                 prob_kneser_ney(c(t1))
                 
                 ### To give some feedback, print a message each 1%
                 if (k==percent_1) {
                   nic2 <- proc.time()
                   print(paste("......",m,"% done in ",
                               elapsed_time(nic1,nic2)," seconds ...",sep=""))
                   # Saving DT prob temp files
                   #save_DT_prob_temp(n,lines)
                   k <- 1
                   m <- m + 1
                 } else { k <- k + 1}  
               }
               save_DT_prob_temp(n,lines)
               # Clean the DT with only two columns (t1,prob)
               DT.uni.prob.final <<- DT.uni.prob[,list(t1,pkn1),]
               DT.uni.prob.final[,prob:=pkn1,]
               DT.uni.prob.final <<- DT.uni.prob.final[,list(t1,prob),]
               #rm(DT.uni.prob,envir =.GlobalEnv)
               
               print(paste("Saving DT probability final file:",file.name.final,sep=""))
               save(DT.uni.prob.final,file=file.name.final)
             },
             
             "2" = {
               ## For each bigram lets calculate the kneser-ney prob
               if (numlines == -1) { numlines <- nrow(DT.bi.prob)}
               percent_1 <- round(numlines/100)
               print(paste("Calculating Knersey-ney prob for:",numlines," 1% is:",percent_1))
               
               k <- 1
               m <- 1
               nic1 <- proc.time()
               
               for (i in 1:numlines) {
                 t1 <- as.character(DT.bi.prob[i,t1,])
                 t2 <- as.character(DT.bi.prob[i,t2,])
                 prob_kneser_ney(c(t1,t2))
                 
                 ### To give some feedback, print a message each 1%
                 if (k==percent_1) {
                   nic2 <- proc.time()
                   print(paste("......",m,"% done in ",
                               elapsed_time(nic1,nic2)," seconds ...",sep=""))
                   # Saving DT prob temp files
                   #save_DT_prob_temp(n,lines)
                   k <- 1
                   m <- m + 1
                 } else { k <- k + 1}
               }
               save_DT_prob_temp(n,lines)
               # Clean the DT with only two columns (t1,t2,prob)
               DT.bi.prob.final <<- DT.bi.prob[,list(t1,t2,pkn1),]
               DT.bi.prob.final[,prob:=pkn1,]
               DT.bi.prob.final <<- DT.bi.prob.final[,list(t1,t2,prob),]
               #rm(DT.bi.prob,envir =.GlobalEnv)
               
               print(paste("Saving DT probability final file:",file.name.final,sep=""))
               save(DT.bi.prob.final,file=file.name.final)
             },
             
             "3" = {
               ## For each trigram lets calculate the kneser-ney prob
               if (numlines == -1) { numlines <- nrow(DT.tri.prob) }
               
               percent_1 <- round(numlines/100)
               print(paste("Calculating Knersey-ney prob for:",numlines," 1% is:",percent_1))
               k <- 1
               m <- 1
               
               nic1 <- proc.time()
               
               for (i in 1:numlines) {
                 t1 <- as.character(DT.tri.prob[i,t1,])
                 t2 <- as.character(DT.tri.prob[i,t2,])
                 t3 <- as.character(DT.tri.prob[i,t3,])
                 prob_kneser_ney(c(t1,t2,t3))
                 
                 ### To give some feedback, print a message each 1%
                 if (k==percent_1) {
                   nic2 <- proc.time()
                   print(paste("......",m,"% done in ",
                               elapsed_time(nic1,nic2)," seconds ...",sep=""))
                   #save_DT_prob_temp(n,lines)
                   
                   k <- 1
                   m <- m + 1
                 } else { k <- k + 1}
               }
               save_DT_prob_temp(n,lines)
               # Clean the DT with only four columns (t1,t2,t3,prob)
               DT.tri.prob.final <<- DT.tri.prob[,list(t1,t2,t3,pkn1),]
               DT.tri.prob.final[,prob:=pkn1,]
               DT.tri.prob.final <<- DT.tri.prob.final[,list(t1,t2,t3,prob),]
               #rm(DT.tri.prob,envir =.GlobalEnv)
               
               print(paste("Saving DT probability final file:",file.name.final,sep=""))
               save(DT.tri.prob.final,file=file.name.final)
             },
             "4" = {
               ## For each quadgram lets calculate the kneser-ney prob
               
               if (numlines == -1) { numlines <- nrow(DT.quad.prob) }
               
               percent_1 <- round(numlines/100)
               print(paste("... Calculating Knersey-ney prob for:",numlines," 1% is:",percent_1))
               k <- 1
               m <- 1
               
               nic1 <- proc.time()
               
               for (i in 1:numlines) {
                 
                 n1 <- proc.time()
                 
                 t1 <- as.character(DT.quad.prob[i,t1,])
                 t2 <- as.character(DT.quad.prob[i,t2,])
                 t3 <- as.character(DT.quad.prob[i,t3,])
                 t4 <- as.character(DT.quad.prob[i,t4,])
                 prob_kneser_ney(c(t1,t2,t3,t4))
                 
                 n2 <- proc.time()
                 
                 ### To give some feedback, print a message each 10%
                 
                 
                 if (k==percent_1) {
                   nic2 <- proc.time()
                   
                   print(paste("......",m,"% done in ",
                               elapsed_time(nic1,nic2)," seconds ...",sep=""))
                   #save_DT_prob_temp(n,lines)
                   
                   k <- 1
                   m <- m + 1
                 } else { k <- k + 1}
                 n3 <- proc.time()
                 
               }
              
               save_DT_prob_temp(n,lines)
               # Clean the DT with only five columns (t1,t2,t3,t4,prob)
               DT.quad.prob.final <<- DT.quad.prob[,list(t1,t2,t3,t4,pkn1),]
               DT.quad.prob.final[,prob:=pkn1,]
               DT.quad.prob.final <<- DT.quad.prob.final[,list(t1,t2,t3,t4,prob),]
              #rm(DT.quad.prob,envir =.GlobalEnv)
               
               print(paste("... Saving DT probability final file:",file.name.final,sep=""))
               save(DT.quad.prob.final,file=file.name.final)
             
               
             }
      )
      tic2 <- proc.time()
      print(paste("-----> calculate_prob_kn: Running Time .......",
                  elapsed_time(tic1,tic2)," seconds ...",sep=""))
    }
  }
}