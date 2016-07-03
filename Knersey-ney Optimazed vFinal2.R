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
load_DT_prob_table_opt <- function (n,lines=-1,p1=5) {
  
  # Values needed for Knersey-ney prob calculations
  p1 <<- p1
  p2 <<- p1 + 1
  
  switch(n,
         "1" = {
           var.name <- "DT.uni.prob.opt"
           file.name <- create_filename("DT_uni_prob_opt",lines)
           file.name.temp <- create_filename("DT_uni_prob_temp_opt", lines)
           
         },
         "2" = {
           var.name <- "DT.bi.prob.opt"
           file.name <- create_filename("DT_bi_prob_opt",lines)
           file.name.temp <- create_filename("DT_bi_prob_temp_opt", lines)
         },
         "3" = {
           var.name <- "DT.tri.prob.opt"
           file.name <- create_filename("DT_tri_prob_opt",lines)
           file.name.temp <- create_filename("DT_tri_prob_temp_opt", lines)
         },
         "4" = {
           var.name <- "DT.quad.prob.opt"
           file.name <- create_filename("DT_quad_prob_opt",lines)
           file.name.temp <- create_filename("DT_quad_prob_temp_opt", lines)
         }
  )
  
  #Validate if the DT prob temp exists in the enviroment
  if (!exists(var.name)) {
    #Validate if the file exists an load the value
    if (file.exists(file.name.temp)) {
      print(paste("Loading DT Prob Temp File: ",file.name, sep =""))
      load(file.name.temp,.GlobalEnv) 
    } else {
      
      
      load_DT_table(n,lines)
      print(paste("-----> load_DT_prob_table(","n:=",n," p1:=",p1," lines:=",lines,").......",sep=""))
      t1 <- proc.time()
      
      switch(n,
             "1" = {
               DT.uni.prob.opt <<- as.data.table(DT.uni, key = "t1")
               DT.uni.prob.opt <<- DT.uni.prob.opt[,freq1:=freq,][,list(t1,freq1),]
               rm(DT.uni,envir =.GlobalEnv)
             },
             "2" = {
               
               DT.bi.prob.opt <<- as.data.table(DT.bi, key = "t1,t2")
               DT.bi.prob.opt <<- DT.bi.prob.opt[,freq2:=freq,][,list(t1,t2,freq2),]
               
               #DT.bi.prob.opt[,c("t21","t22","freq2") := list(t1,t2,freq)]
               #DT.bi.prob.opt <<- DT.bi.prob.opt[,list(t21,t22,freq2),]
               
               rm(DT.bi,envir =.GlobalEnv)
             },
             "3" = {
               DT.tri.prob.opt <<- as.data.table(DT.tri, key = "t1,t2,t3")
               DT.tri.prob.opt <<- DT.tri.prob.opt[,freq3:=freq,][,list(t1,t2,t3,freq3),]
               #DT.tri.prob.opt[,c("t31","t32","t33","freq3") := list(t1,t2,t3,freq)]
               #DT.tri.prob.opt <<- DT.tri.prob.opt[,list(t31,t32,t33,freq3),]
               
               rm(DT.tri,envir =.GlobalEnv)
             },
             "4" = {
               DT.quad.prob.opt <<- as.data.table(DT.quad, key = "t1,t2,t3,t4")
               DT.quad.prob.opt <<- DT.quad.prob.opt[,freq4:=freq,][,list(t1,t2,t3,t4,freq4),]
               #DT.quad.prob.opt[,c("t41","t42","t43","t44","freq4") := list(t1,t2,t3,t4,freq)]
               #DT.quad.prob.opt <<- DT.quad.prob.opt[,list(t41,t42,t43,t44,freq4),]
               
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
         
         "1" = {
           n.uni <<- nrow(DT.uni.prob.opt)
           numwords.uni <<- sum(DT.uni.prob.opt$freq1)
         },
         
         "2" = {
           
           #   n1: number of bigrams that occurs exactly p1 times
           #   n2: number of bigrams that occurs exactly p1+1 times
           #   D2 = n1 / (n1 + 2 * n2)
           
           numwords.bi <<- sum(DT.bi.prob.opt$freq2)
           
           n.bi <<- nrow(DT.bi.prob.opt)
           n1.bi <<- nrow(DT.bi.prob.opt[freq2 == p1,,])
           n2.bi <<- nrow(DT.bi.prob.opt[freq2 == p2,,])
           
           print(paste0("Calculando D2","n.bi=",n.bi," n1.bi=",n1.bi," n2.bi=",n2.bi," p1=",p1," p2=",p2))
           D2 <<- n1.bi / (n1.bi + 2 * n2.bi) 
         },
         
         "3" = {
           #   n1: number of trigrams that occurs exactly p1 times
           #   n2: number of trigrams that occurs exactly p1+1 times
           #   D3 = n1 / (n1 + 2 * n2)
           numwords.tri <<- sum(DT.tri.prob.opt$freq3)
           
           n.tri <<- nrow(DT.tri.prob.opt)
           n1.tri <<- nrow(DT.tri.prob.opt[freq3 == p1,,])
           n2.tri <<- nrow(DT.tri.prob.opt[freq3 == p2,,])
           D3 <<- n1.tri / (n1.tri + 2 * n2.tri) 
         },
         
         "4" = {
           #   n1: number of quadgrams that occurs exactly p1 times
           #   n2: number of quadgrams that occurs exactly p1+1 times
           #   D4 = n1 / (n1 + 2 * n2)
           numwords.quad <<- sum(DT.quad.prob.opt$freq4)
           
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
           file.name.temp <- create_filename("DT_uni_prob_temp_opt",lines)
           
         },
         "2" = {
           var.name <- "DT.bi.prob.opt"
           file.name <- create_filename("DT_bi_prob_opt",lines)
           var.name.final <- "DT.bi.prob.opt.final"
           file.name.final <- create_filename("DT_bi_prob_final_opt",lines)
           file.name.temp <- create_filename("DT_bi_prob_temp_opt",lines)
         },
         "3" = {
           var.name <- "DT.tri.prob.opt"
           file.name <- create_filename("DT_tri_prob_opt",lines)
           var.name.final <- "DT.tri.prob.final.opt"
           file.name.final <- create_filename("DT_tri_prob_final_opt",lines)
           file.name.temp <- create_filename("DT_tri_prob_temp_opt",lines)
         },
         "4" = {
           var.name <- "DT.quad.prob.final.opt"
           file.name <- create_filename("DT_quad_prob_final_opt",lines)
           file.name.final <- create_filename("DT_quad_prob_final_opt",lines)
           var.name.final <- "DT.quad.prob.final.opt"
           file.name.temp <- create_filename("DT_quad_prob_temp_opt",lines)
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
               DT.uni.temp <- copy(DT.uni.prob.opt)
               DT.bi.temp <- copy(DT.bi.prob.opt)

               # Calculate n11 
               print("Calculating N1+_Pred (n11)")
               
               #DT.bi.n.temp <- DT.bi.temp[freq >= 1]
               
               setkey(DT.uni.temp,"t1")
               setkey(DT.bi.temp,"t2")
               
               DT.n11 <- DT.bi.temp[DT.uni.temp, .N, by = .EACHI] [,c("t1","n11","t2","N"):=list(t2,N,NULL,NULL)]
               
               # Calculate n12 
               print("Calculating N1+_Suc (n12)")
               
               setkey(DT.uni.temp,"t1")
               setkey(DT.bi.temp,"t1")
               
               DT.n12 <- DT.bi.temp[DT.uni.temp, .N, by = .EACHI] [,c("t1","n12","N"):=list(t1,N,NULL)]
               
               DT.uni.prob.opt <- merge(DT.uni.prob.opt, DT.n11 , by = "t1")
               
               DT.uni.prob.opt <- merge(DT.uni.prob.opt, DT.n12 , by = "t1")
               
               
               # Calculate MLE for Unigrams
               print("Calculating MLE for Unigrams (pkn11)")
               
               DT.uni.prob.opt[,pkn11:= freq1 / numwords.uni]
               
               # Calculate lamba high and low order
               print("Calculating Lambda for High and Low Order (l11,l12)")
               
               DT.uni.prob.opt[,l11:= (D2 / freq1) * n12]
               DT.uni.prob.opt[,l12:= (D2 / n11) * n12]
               
               # Calculate pkn12
               print("Calculating Probability Knersey-ney (pkn12)")
               
               DT.uni.prob.opt[,pkn12:= n11 / n.bi]
               
               tic2 <- proc.time()
               
               print(paste("Saving DT probability Temp file:",file.name.temp,sep=""))
               save(DT.uni.prob.opt,file=file.name.temp)
               
               
               
               # Clean the DT with only two columns (t1,prob)
               DT.uni.prob.opt[,prob:=pkn11]
               DT.uni.prob.opt.final <<- DT.uni.prob.opt[,list(t1,prob),]

               rm(DT.uni.prob.opt,envir =.GlobalEnv)
               
               print(paste("Saving DT probability final file:",file.name.final,sep=""))
               save(DT.uni.prob.opt.final,file=file.name.final)
               
               
             },
             
             "2" = {
               
               
               print("Calculating Knersey-ney prob for Bigrams ...")
               
               DT.bi.temp <- copy(DT.bi.prob.opt)
               DT.tri.temp <- copy(DT.tri.prob.opt)
               
               # Calculate n21 
               print("Calculating N1+_Pred (n21)")
               
               #DT.tri.n.temp <- DT.tri.temp[freq >= 1]
               
               setkeyv(DT.bi.temp,c("t1","t2"))
               setkeyv(DT.tri.temp,c("t2","t3"))
               
               DT.n21 <- DT.tri.temp[DT.bi.temp, .N, by = .EACHI] [,c("t1","t2","n21","t3","N"):=list(t2,t3,N,NULL,NULL)]
               
               # Calculate n22 
               print("Calculating N1+_Suc (n22)")
               
               setkeyv(DT.bi.temp,c("t1","t2"))
               setkeyv(DT.tri.temp,c("t1","t2"))
               
               DT.n22 <- DT.tri.temp[DT.bi.temp, .N, by = .EACHI] [,c("t1","t2","n22","N"):=list(t1,t2,N,NULL)]
               
               DT.bi.prob.opt <- merge(DT.bi.prob.opt, DT.n21 , by = c("t1","t2"))
               
               DT.bi.prob.opt <- merge(DT.bi.prob.opt, DT.n22 , by = c("t1","t2"))
               
               # Calculate alpha high order
               
               print("Calculating Alpha High Order (a22)")
               
               ##### To calculate this we need freq1(t1) and n11(t1), let's added to the table
               
               DT.uni.temp1 <- copy(DT.uni.prob.opt)
               DT.uni.temp1 <- DT.uni.temp1[,list(t1,freq1,n11,l11,l12),]
               DT.bi.prob.opt <- merge(DT.bi.prob.opt, DT.uni.temp1 , by = "t1")
   
               DT.bi.prob.opt[, a21:= ifelse(D2>freq2,0,freq2 - D2) /  freq1,]
               
               DT.bi.prob.opt[, a22:= ifelse(D2>n21,0,n21 - D2) /  n11,]
               
               print("Calculating Knersey-ney Probability for High Order (pkn21)")
               
               # In order to calculate this we need to get pkn12(t2) let's added to the table
               
               DT.uni.temp2 <- copy(DT.uni.prob.opt)
               DT.uni.temp2[,t2:=t1]
               DT.uni.temp2 <- DT.uni.temp2[,list(t2,pkn12),]
               DT.bi.prob.opt <- merge(DT.bi.prob.opt, DT.uni.temp2 , by = "t2")
               
               DT.bi.prob.opt[, pkn21 := a21 + l11 * pkn12,]
               DT.bi.prob.opt[, pkn22 := a22 + l12 * pkn12,]
               
               # Calculate lamba high order
               print("Calculating Lambda for High Order (l21)")
               # Calculate lamba high and low order
               print("Calculating Lambda for High and Low Order (l21 and l22")
               
               
               DT.bi.prob.opt[,l21:= (D3 / freq2) * n22]
               DT.bi.prob.opt[,l22:= (D3 / n21) * n22]
               
              
               tic2 <- proc.time()
               
               print(paste("Saving DT probability Temp file:",file.name.temp,sep=""))
               save(DT.bi.prob.opt,file=file.name.temp)
               
               # Clean the DT with only three columns (t1,t2,prob)
               
               DT.bi.prob.opt[,prob:=pkn21]
               DT.bi.prob.opt.final <<- DT.bi.prob.opt[,list(t1,t2,prob),]
              
               rm(DT.bi.prob.opt,envir =.GlobalEnv)
               
               print(paste("Saving DT probability final file:",file.name.final,sep=""))
               save(DT.bi.prob.opt.final,file=file.name.final)
         

             },
             
             "3" = {
               ## For each trigram lets calculate the kneser-ney prob
               
               
               
               print("Calculating Knersey-ney prob for Trigrams ...")
               
               DT.tri.temp <- copy(DT.tri.prob.opt)
               DT.quad.temp <- copy(DT.quad.prob.opt)
               
               # Calculate n31 
               print("Calculating N1+_Pred (n31)")
               
               #DT.tri.n.temp <- DT.tri.temp[freq >= 1]
               
               setkeyv(DT.tri.temp,c("t1","t2","t3"))
               setkeyv(DT.quad.temp,c("t2","t3","t4"))
               
               DT.n31 <- DT.quad.temp[DT.tri.temp, .N, by = .EACHI] [,c("t1","t2","t3","n31","t4","N"):=list(t2,t3,t4,N,NULL,NULL)]
               
               # Calculate n32 
               print("Calculating N1+_Suc (n32)")
               
               setkeyv(DT.tri.temp,c("t1","t2","t3"))
               setkeyv(DT.quad.temp,c("t1","t2","t3"))
               
               DT.n32 <- DT.quad.temp[DT.tri.temp, .N, by = .EACHI] [,c("t1","t2","t3","n32","N"):=list(t1,t2,t3,N,NULL)]
               
               DT.tri.prob.opt <- merge(DT.tri.prob.opt, DT.n31 , by = c("t1","t2","t3"))
               
               DT.tri.prob.opt <- merge(DT.tri.prob.opt, DT.n32 , by = c("t1","t2","t3"))
               
               
               # Calculate alpha high order
               
               print("Calculating Alpha High Order (a31)")
               
               
               
               ##### To calculate this we need freq2(t1,t2), l21(t1,t2) and l22(t1,t2) let's added to the table
               
               DT.bi.temp1 <- copy(DT.bi.prob.opt)
               DT.bi.temp1 <- DT.bi.temp1[,list(t1,t2,freq2,n21,l21,l22),]
               DT.tri.prob.opt <- merge(DT.tri.prob.opt, DT.bi.temp1 , by = c("t1","t2"))

               DT.tri.prob.opt[, a31:= ifelse(D3>freq3,0,freq3 - D3) /  freq2,]
               DT.tri.prob.opt[, a32:= ifelse(D3>n31,0,n31 - D3) /  n21,]
               
               
               print("Calculating Knersey-ney Probability for High Order (pkn31)")
               
               
               
               # In order to calculate this we need to get pkn22(t2,t3) let's added to the table
               
               DT.bi.temp2 <- copy(DT.bi.prob.opt)
               DT.bi.temp2[,c("t3","t2") := list(t2,t1),] 
               DT.bi.temp2 <- DT.bi.temp2[,list(t2,t3,pkn22),]
               DT.tri.prob.opt <- merge(DT.tri.prob.opt, DT.bi.temp2 , by = c("t2","t3"))
               
               DT.tri.prob.opt[, pkn31 := a31 + l21 * pkn22,]
               DT.tri.prob.opt[, pkn32 := a32 + l22 * pkn22,]
               
               # Calculate lamba high Order
               print("Calculating Lambda for High Order (l31)")
               
               
               DT.tri.prob.opt[,l31:= (D4 / freq3) * n32]
               
               tic2 <- proc.time()
               
               print(paste("Saving DT probability Temp file:",file.name.temp,sep=""))
               save(DT.tri.prob.opt,file=file.name.temp)
               
               # Clean the DT with only three columns (t1,t2,prob)
               
               DT.tri.prob.opt[,prob:=pkn31]
               DT.tri.prob.opt.final <<- DT.tri.prob.opt[,list(t1,t2,t3,prob),]
               
               rm(DT.tri.prob.opt,envir =.GlobalEnv)
               
               print(paste("Saving DT probability final file:",file.name.final,sep=""))
               save(DT.tri.prob.opt.final,file=file.name.final)
               
               
             },
             "4" = {
               ## For each quadgram lets calculate the kneser-ney prob
               
               
               
               print("Calculating Knersey-ney prob for Quadgrams ...")
               
               DT.quad.temp <- copy(DT.quad.prob.opt)
               
               # We need l31 for this calculation, let's added to the table
               
               DT.tri.temp1 <- copy(DT.tri.prob.opt)
               DT.tri.temp1 <- DT.tri.temp1[,list(t1,t2,t3,freq3,l31),]
               DT.quad.prob.opt <- merge(DT.quad.prob.opt, DT.tri.temp1 , by = c("t1","t2","t3"))
               
               DT.quad.prob.opt[, a41:= ifelse(D4>freq4,0,freq4 - D4) /  freq3,]
               
               # Calculate pkn for high and low order
               
               DT.tri.temp2 <- copy(DT.tri.prob.opt)
               DT.tri.temp2[,c("t4","t3","t2") := list(t3,t2,t1),] 
               DT.tri.temp2 <- DT.tri.temp2[,list(t2,t3,t4,pkn32),]
               DT.quad.prob.opt <- merge(DT.quad.prob.opt, DT.tri.temp2 , by = c("t2","t3","t4"))
               
               
               print("Calculating Knersey-ney Probability for High Order (pkn41)")
               
               DT.quad.prob.opt[, pkn41 := a41 + l31 * pkn32,]
               
        
               
               
               tic2 <- proc.time()
               
               print(paste("Saving DT probability Temp file:",file.name.temp,sep=""))
               save(DT.quad.prob.opt,file=file.name.temp)
               
               # Clean the DT with only five columns (t1,t2,t3,t4,prob)
               
               DT.quad.prob.opt[,prob:=pkn41]
               DT.quad.prob.opt.final <<- DT.quad.prob.opt[,list(t1,t2,t3,t4,prob),]
               
               rm(DT.quad.prob.opt,envir =.GlobalEnv)
               
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