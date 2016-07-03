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
load_DT_prob_table_opt <- function (n,lines=-1,p1=5) {
  
  # Values needed for Knersey-ney prob calculations
  p1 <<- p1
  p2 <<- p1 + 1
  
  switch(n,
         "1" = {
           var.name <- "DT.uni.prob.opt"
           file.name <- create_filename("DT_uni_prob_opt",lines)
         },
         "2" = {
           var.name <- "DT.bi.prob.opt"
           file.name <- create_filename("DT_bi_prob_opt",lines)
         },
         "3" = {
           var.name <- "DT.tri.prob.opt"
           file.name <- create_filename("DT_tri_prob_opt",lines)
         },
         "4" = {
           var.name <- "DT.quad.prob.opt"
           file.name <- create_filename("DT_quad_prob_opt",lines)
         }
  )
  
  #Validate if the DT prob temp exists in the enviroment
  if (!exists(var.name)) {
    #Validate if the file exists an load the value
    if (file.exists(file.name)) {
      print(paste("Loading DT Prob file: ",file.name, sep =""))
      load(file.name,.GlobalEnv) 
    } else {
      
      
      load_DT_table(n,lines)
      print(paste("-----> load_DT_prob_table(","n:=",n," p1:=",p1," lines:=",lines,").......",sep=""))
      t1 <- proc.time()
      
      switch(n,
             "1" = {
               DT.uni.prob.opt <<- as.data.table(DT.uni, key = "t1")
               DT.uni.prob.opt[,c("t11","freq1") := list(t1,freq)]
               DT.uni.prob.opt <<- DT.uni.prob.opt[,list(t11,freq1),]
               rm(DT.uni,envir =.GlobalEnv)
             },
             "2" = {
               
               DT.bi.prob.opt <<- as.data.table(DT.bi, key = "t1,t2")
               DT.bi.prob.opt[,c("t21","t22","freq2") := list(t1,t2,freq)]
               DT.bi.prob.opt <<- DT.bi.prob.opt[,list(t21,t22,freq2),]
               
               rm(DT.bi,envir =.GlobalEnv)
             },
             "3" = {
               DT.tri.prob.opt <<- as.data.table(DT.tri, key = "t1,t2,t3")
               DT.tri.prob.opt[,c("t31","t32","t33","freq3") := list(t1,t2,t3,freq)]
               DT.tri.prob.opt <<- DT.tri.prob.opt[,list(t31,t32,t33,freq3),]
               
               rm(DT.tri,envir =.GlobalEnv)
             },
             "4" = {
               DT.quad.prob.opt <<- as.data.table(DT.quad, key = "t1,t2,t3,t4")
               DT.quad.prob.opt[,c("t41","t42","t43","t44","freq4") := list(t1,t2,t3,t4,freq)]
               DT.quad.prob.opt <<- DT.quad.prob.opt[,list(t41,t42,t43,t44,freq4),]
               
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
         
         "1" = n.uni <<- nrow(DT.uni.prob.opt),
         
         "2" = {
           
           #   n1: number of bigrams that occurs exactly p1 times
           #   n2: number of bigrams that occurs exactly p1+1 times
           #   D2 = n1 / (n1 + 2 * n2)
           
           n.bi <<- nrow(DT.bi.prob.opt)
           n1.bi <<- nrow(DT.bi.prob.opt[freq2 == p1,,])
           n2.bi <<- nrow(DT.bi.prob.opt[freq2 == p2,,])
           D2 <<- n1.bi / (n1.bi + 2 * n2.bi) 
         },
         
         "3" = {
           #   n1: number of trigrams that occurs exactly p1 times
           #   n2: number of trigrams that occurs exactly p1+1 times
           #   D3 = n1 / (n1 + 2 * n2)
           n.tri <<- nrow(DT.tri.prob.opt)
           n1.tri <<- nrow(DT.tri.prob.opt[freq3 == p1,,])
           n2.tri <<- nrow(DT.tri.prob.opt[freq3 == p2,,])
           D3 <<- n1.tri / (n1.tri + 2 * n2.tri) 
         },
         
         "4" = {
           #   n1: number of quadgrams that occurs exactly p1 times
           #   n2: number of quadgrams that occurs exactly p1+1 times
           #   D4 = n1 / (n1 + 2 * n2)
           n.quad <<- nrow(DT.quad.prob.opt)
           n1.quad <<- nrow(DT.quad.prob.opt[freq4 == p1,,])
           n2.quad <<- nrow(DT.quad.prob.opt[freq4 == p2,,])
           D4 <<- n1.quad / (n1.quad + 2 * n2.quad) 
         }
         
  )
}    


####################################
load_DT_prob_tables_opt <- function (n,lines=-1,p1=5) { 
  
  print(paste("-----> load_DT_prob_tables_opt(n:=",n," p1:=",p1," lines:=",lines,").......",sep=""))
  t1 <- proc.time()
  
  if (n >= 1) {
    load_DT_prob_table_opt(1,lines,p1)
    load_DT_prob_table_opt(2,lines,p1)
  } 
    
  if (n >= 2) {
    load_DT_prob_table_opt(3,lines,p1)
  }
  
  if (n >= 3) {
    load_DT_prob_table_opt(4,lines,p1)
  } 
  t2 <- proc.time()
  
  print(paste("-----> load_DT_prob_tables_opt: Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
}



####################################  
calculate_prob_kn_opt <- function(n,lines=-1,p1=5,numlines=-1) {
    
  switch(n,
         "1" = {
           var.name <- "DT.uni.prob.opt"
           file.name <- create_filename("DT_uni_prob_opt",lines)
           var.name.final <- "DT.uni.prob.final.opt"
           file.name.final <- create_filename("DT_uni_prob_final_opt",lines)
         },
         "2" = {
           var.name <- "DT.bi.prob.opt"
           file.name <- create_filename("DT_bi_prob_opt",lines)
           var.name.final <- "DT.bi.prob.opt.final"
           file.name.final <- create_filename("DT_bi_prob_final_opt",lines)
         },
         "3" = {
           var.name <- "DT.tri.prob.opt"
           file.name <- create_filename("DT_tri_prob_opt",lines)
           var.name.final <- "DT.tri.prob.final.opt"
           file.name.final <- create_filename("DT_tri_prob_final_opt",lines)
         },
         "4" = {
           var.name <- "DT.quad.prob.final.opt"
           file.name <- create_filename("DT_quad_prob_final_opt",lines)
           file.name.final <- create_filename("DT_quad_prob_final_opt",lines)
         }
  )
  
  #Validate if the DT with the probability exists in the enviroment
  if (!exists(var.name.final)) {
    #Validate if the file exists an load the value
    if (file.exists(file.name.final)) {
      print(paste("Loading DT Prob Final file: ",file.name, sep =""))
      load(file.name.final,.GlobalEnv) 
    } else {
      
      load_DT_prob_tables_opt(n,lines,p1)
      
      print(paste("-----> calculate_prob_kn_opt(",n,",",lines,",",numlines,").......",sep=""))
      tic1 <- proc.time()
      
      print(paste("Calculating DT Prob Table Opt:", var.name, sep=""))
      
      
      switch(n,
             "1" = { # For each unigram lets calculate the kneser-ney prob
               
               print("Calculating Knersey-ney prob for unigrams ...")
               
               # Calculate n11 
               print("Calculating N11")
               
               DT.uni.prob.opt[,n11:=nrow(DT.bi.prob.opt[t22==t11 & freq2 >=p1]), by = t11]
               
               # Calculate n12 
               print("Calculating N12")
               
               DT.uni.prob.opt[,n12:=nrow(DT.bi.prob.opt[t21==t11 & freq2 >=p1]), by = t11]
               
               # Calculate pkn1
               print("Calculating PKN1")
               
               DT.uni.prob.opt[,pkn11:= n11 / n.bi]
               
               # Calculate lamba high and low order
               print("Calculating L11 and L12")
               
               DT.uni.prob.opt[,c("l11","l12") := 
                                 list((D2 / freq1) * n12, (D2 / n11) * n12)]
               
               tic2 <- proc.time()
               
               #save_DT_prob_temp(n,lines)
               # Clean the DT with only two columns (t1,prob)
               DT.uni.prob.opt.final <<- DT.uni.prob.opt[,list(t11,pkn11),]
               DT.uni.prob.opt.final[,c("t1","prob","t11","pkn11") := list(t11,pkn11,NULL,NULL)]
               
               
               #rm(DT.uni.prob.opt,envir =.GlobalEnv)
               
               print(paste("Saving DT probability final file:",file.name.final,sep=""))
               save(DT.uni.prob.opt.final,file=file.name.final)
               
               
             },
             
             "2" = {
               
               
               print("Calculating Knersey-ney prob for Bigrams ...")
               
               # Calculate n21 N1Plus_pre
               
               print("Calculating N21")
               
               DT.bi.prob.opt[,n21:=nrow(DT.tri.prob.opt[t32==t21 & t33==t22 & freq3 >=p1]), by = list(t21,t22)]
               
               # Calculate n22 N1Plus_suc
               
               print("Calculating N22")
               
               DT.bi.prob.opt[,n22:=nrow(DT.tri.prob.opt[t31==t21 & t32==t22 & freq3 >=p1]), by = list(t21,t22)]
               
               
               # Calculate lamba high and low order
               
               print("Calculating l21")
               
               DT.bi.prob.opt[, l21:= D3 * DT.uni.prob.opt[t11 == t21, n12/freq1,], by = list(t21,t22)]
               
               print("Calculating l22")
               
               DT.bi.prob.opt[, l22:= D3 * DT.uni.prob.opt[t11 == t21, n12/n11,], by = list(t21,t22) ]
               
               # Calculate alpha high and low order
               
               print("Calculating a21")
               
               DT.bi.prob.opt[, a21:= ifelse(D2>freq2,0,freq2 - D2) /  DT.uni.prob.opt[t11 == t22, pkn11,], by = list(t21,t22) ]
               
               print("Calculating a22")
               
               DT.bi.prob.opt[, a22:= ifelse(D2>n21,0,n21 - D2) /  DT.uni.prob.opt[t11 == t22, pkn11,], by = list(t21,t22) ]
               
               # Calculate pkn for high and low order
               
               print("Calculating pkn21")
               
               DT.bi.prob.opt[,pkn21:= a21 + DT.uni.prob.opt[t11 == t22, l21 * pkn11,], by = list(t21,t22) ]
               
               print("Calculating pkn22")
               
               DT.bi.prob.opt[,pkn22:= a22 + DT.uni.prob.opt[t11 == t22, l22 * pkn11,], by = list(t21,t22) ]
               
               
               tic2 <- proc.time()
               
               #save_DT_prob_temp(n,lines)
               # Clean the DT with only two columns (t1,prob)
               
               DT.bi.prob.opt.final <<- DT.bi.prob.opt[,list(t21,t22,pkn21),]
               DT.bi.prob.opt.final[,c("t1","t2","prob","t21","t22","pkn21") := list(t21,t22,pkn21,NULL,NULL,NULL)]
               
               
               
               #rm(DT.uni.prob,envir =.GlobalEnv)
               
               print(paste("Saving DT probability final file:",file.name.final,sep=""))
               save(DT.bi.prob.opt.final,file=file.name.final)
               
               
               
             },
             
             "3" = {
               ## For each trigram lets calculate the kneser-ney prob
               
               
               print("Calculating Knersey-ney prob for Trigrams ...")
               
               # Calculate n31 N1Plus_pre
               
               print("Calculating n31")
               
               DT.tri.prob.opt[,n31:=nrow(DT.quad.prob.opt[t42==t31 & t43==t32 & t44 == t33 & freq4 >=p1]), by = list(t31,t32,t33)]
               
               # Calculate n22 N1Plus_suc
               
               print("Calculating N32")
               
               DT.tri.prob.opt[,n32:=nrow(DT.quad.prob.opt[t41==t31 & t42==t32 & t43 == t33 & freq4 >=p1]), by = list(t31,t32,t33)]
               
               
               # Calculate lamba high and low order
               
               print("Calculating l31")
               
               DT.tri.prob.opt[, l31:= D4 * DT.bi.prob.opt[t21 == t31 & t22 == t32, n22/freq2,], by = list(t31,t32,t33)]
               
               print("Calculating N32")
               
               DT.tri.prob.opt[, l32:= D4 * DT.bi.prob.opt[t21 == t31 & t22 == t32, n22/n21,], by = list(t31,t32,t33) ]
               
               # Calculate alpha high and low order
               
               print("Calculating a31")
               
               DT.tri.prob.opt[, a31:= ifelse(D3>freq3,0,freq3 - D3) /  DT.bi.prob.opt[t21 == t32 & t22 == t33, pkn21,], by = list(t31,t32,t33) ]
               
               print("Calculating a32")
               
               DT.tri.prob.opt[, a32:= ifelse(D3>n31,0,n31 - D3) /  DT.bi.prob.opt[t21 == t32 & t22 == t33, pkn22,], by = list(t31,t32,t33) ]
               
               # Calculate pkn for high and low order
               
               print("Calculating pkn31")
               
               DT.tri.prob.opt[,pkn31:= a31 + DT.bi.prob.opt[t21 == t32 & t22 == t33, l21 * pkn21,], by = list(t31,t32,t33) ]
               
               print("Calculating pkn32")
               DT.tri.prob.opt[,pkn32:= a32 + DT.bi.prob.opt[t21 == t32 & t22 == t33, l22 * pkn22,], by = list(t31,t32,t33) ]
               
               
               tic2 <- proc.time()
               
               #save_DT_prob_temp(n,lines)
               # Clean the DT with only two columns (t1,prob)
               
               DT.tri.prob.opt.final <<- DT.tri.prob.opt[,list(t31,t32,t33,pkn31),]
               DT.tri.prob.opt.final[,c("t1","t2","t3","prob","t31","t32","t33","pkn31") := list(t31,t32,t33,pkn31,NULL,NULL,NULL,NULL)]
               
               
               
               #rm(DT.uni.prob,envir =.GlobalEnv)
               
               print(paste("Saving DT probability final file:",file.name.final,sep=""))
               save(DT.tri.prob.opt.final,file=file.name.final)
               
               
               
               
               
             },
             "4" = {
               ## For each quadgram lets calculate the kneser-ney prob
               
               
               
               print("Calculating Knersey-ney prob for Quadgrams ...")
               
               # Calculate alpha high and low order
               
               print("Calculating a41")
               
               DT.quad.prob.opt[, a41:= ifelse(D4>freq4,0,freq4 - D4) /  DT.tri.prob.opt[t31 == t42 & t32 == t43 & t33 == t44, pkn31,], by = list(t41,t42,t43,t44) ]
               
               
               print("Calculating pkn41")
               
               # Calculate pkn for high and low order
               
               DT.quad.prob.opt[,pkn41:= a41 + DT.tri.prob.opt[t31 == t42 & t32 == t43 & t33 == t44, l21 * pkn21,], by = list(t41,t42,t43,t44) ]
               
               
               
               tic2 <- proc.time()
               
               #save_DT_prob_temp(n,lines)
               # Clean the DT with only two columns (t1,prob)
               
               DT.quad.prob.opt.final <<- DT.quad.prob.opt[,list(t41,t42,t43,t44,pkn41),]
               DT.quad.prob.opt.final[,c("t1","t2","t3","t4","prob","t41","t42","t43","pkn41") := list(t41,t42,t43,t44,pkn41,NULL,NULL,NULL,NULL)]
               
               
               
               #rm(DT.uni.prob,envir =.GlobalEnv)
               
               print(paste("Saving DT probability final file:",file.name.final,sep=""))
               save(DT.quad.prob.opt.final,file=file.name.final)
               
               
               
               
               
             }
      )
      tic2 <- proc.time()
      print(paste("-----> calculate_prob_kn_opt: Running Time .......",
                  elapsed_time(tic1,tic2)," seconds ...",sep=""))
    }
  }
}