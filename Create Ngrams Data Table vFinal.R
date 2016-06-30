#####################################################################
# Create Ngram Data Table VFinal:
#          Generate the information about the frequency of 
#          the differents ngrams (unigram, bigram, trigram and quadgrams)
#          to be used later with Kneser-ney algortihm to calculate the
#          probability of each ngram for predict the next word.
#          This script was run on my PC and takes long time to finish,
#          at the end this script they produce the following files:
#
#             DT_uni_x.Rdata: data table that include all the unigrams and the
#                          frequency for each one.
#             DT_bi_x.Rdata: data table that include all the bigrams and the
#                          frequency for each one.
#             DT_tri_x.Rdata: data table that include all the trigrams and the
#                          frequency for each one.
#             DT_quad_x.Rdata: data table that include all the quadgrams and the
#                          frequency for each one.
#      Where X represent the number of lines of each file (twitter, news and blogs)
#      that was readed. In case of all lines, x = "all"


library(quanteda)
library(data.table)
library(stringi)

#setwd("D:/Coursera/Capstone Project/Coursera-SwiftKey/final/en_US")
setwd("D:/001 -- Coursera/Capstone Project/Coursera-SwiftKey/final/en_US")

# For reproducibility
set.seed(12345)

# For print elapsed time
elapsed_time <- function(tic1,tic2) {
  format((tic2-tic1)[3][[1]], digits = 2)
}

# For file name creation
create_filename <- function(x,lines) {
  
  if (lines < 0) {
    paste(x,"_all.RData",sep= "")
  }
  else {
    paste(x,"_",lines,".RData",sep= "")
  }
}

# Load, filter and transform the data (twitter, news, blogs). The
# lines parameter define the number of lines per each dataset to consider.
# If lines = -1, read all lines.

create_mydata <- function(lines=-1) {
  
  var.name <- "mydata"
  file.name <- create_filename("mydata",lines)

  #Validate if "mydata" exists in the enviroment
  if (!exists(var.name)) {

    #Validate if "mydata.Rdata" file exists an load the value
    if (file.exists(file.name)) {
      print(paste("Loading file",file.name,"...."))
      load(file.name,.GlobalEnv)
    }  
    else {
      print(paste("-----> create_mydata(",lines,").......",sep=""))
      t1 <- proc.time()
      ## Load the data and save the info into the file
      filename_twitter <- "en_US.twitter.txt"
      filename_news <- "en_US.news.txt" 
      filename_blogs <- "en_US.blogs.txt"
      
      print("Loading the Data from the files ...")
      
      print("... twitter data ....")
      
      twitter.data <- readLines(filename_twitter, n = lines, encoding="UTF-8", warn = FALSE)
      print("... news data .....")
      news.data <- readLines(filename_news, n = lines, encoding="UTF-8", warn = FALSE)
      print("... blogs data ....")
      blogs.data <- readLines(filename_blogs, n = lines, encoding="UTF-8", warn = FALSE)
      
      # Remove emojies and other characters.
      print("... Removing emojies and other characters ....")
      twitter.data <- iconv(twitter.data, "latin1", "ASCII", sub="")
      twitter.data <- iconv(twitter.data, "ISO-8859-2", "ASCII", sub="")
      
      news.data <- iconv(news.data, "latin1", "ASCII", sub="")
      news.data <- iconv(news.data, "ISO-8859-2", "ASCII", sub="")
      
      blogs.data <- iconv(blogs.data, "latin1", "ASCII", sub="")
      blogs.data <- iconv(blogs.data, "ISO-8859-2", "ASCII", sub="")
      
      # toLower
      print("... toLower Data ....")
      twitter.data <- toLower(twitter.data)
      blogs.data <- toLower(blogs.data)
      news.data <- toLower(news.data)
      
      # Replace punctutation for a special word "eeee" in order to 
      # avoid some ngrams that doesn't exists
      print("... Replacing punctuation for special characters ....")   
      twitter.data <- stri_replace_all_regex(twitter.data,"[.,;:]", " eeee ", 
                                             vectorize_all=FALSE)
      
      blogs.data <- stri_replace_all_regex(blogs.data,"[.,;:]", " eeee ", 
                                           vectorize_all=FALSE)
      news.data <- stri_replace_all_regex(news.data,"[.,;:]", " eeee ", 
                                          vectorize_all=FALSE)

      # Replace the rest of punctutation 
      print("... Replacing rest of punctuation ....")   
      twitter.data <- stri_replace_all_regex(twitter.data,"[:punct:]", " ", 
                                             vectorize_all=FALSE)
      blogs.data <- stri_replace_all_regex(blogs.data,"[:punct:]", " ", 
                                           vectorize_all=FALSE)
      news.data <- stri_replace_all_regex(news.data,"[:punct:]", " ", 
                                          vectorize_all=FALSE)
      # Final Data    
      mydata <<- c(twitter.data, news.data, blogs.data)
      
      print(paste("... Saving mydata file:",file.name,sep=""))
      save(mydata,file=file.name)
      
      rm("twitter.data","news.data","blogs.data",
         "filename_twitter", "filename_news", "filename_blogs")
      
      t2 <- proc.time()
      print(paste("-----> create_mydata: Running Time .......",
                  elapsed_time(t1,t2)," seconds ...",sep=""))
    }
  }
  gc()
}


create_alltokens <- function(lines=-1) {
  
  var.name <- "alltokens"
  file.name <- create_filename("alltokens",lines)
  
  #Validate if "alltokens" exists in the enviroment
  if (!exists(var.name)) {
    #Validate if "alltokens.Rdata" file exists an load the value
    if (file.exists(file.name)) {
      print(paste("Loading file",file.name," ...."))
      load(file.name,.GlobalEnv) 
    }  
    else {
      ## Load "mydata" 
      create_mydata(lines)
      
      print(paste("-----> create_alltokens(",lines,").......",sep=""))
      t1 <- proc.time()
      
      print("Creating alltokens ...")
      # Create "alltokens" and save it into the file
      alltokens <<- tokenize(mydata, what = "fastestword", 
                        removeNumbers = TRUE, 
                        removePunct = TRUE,
                        removeSymbols = TRUE, 
                        removeSeparators = TRUE, 
                        removeTwitter = TRUE,
                        removeHyphens = TRUE, 
                        removeURL = TRUE, 
                        verbose = TRUE)
      
      print(paste("... Saving alltokens file:",file.name,sep=""))
      save(alltokens,file=file.name)
      rm("mydata",envir =.GlobalEnv)
      
      t2 <- proc.time()
      print(paste("-----> create_alltokens: Running Time .......",
                  elapsed_time(t1,t2)," seconds ...",sep=""))
      
    }
  }
  gc()
}


create_ngram <- function(n,lines=-1)
{
  
  switch(n,
         "1" = {
           var.name <- "uni.ngram"
           file.name <- create_filename("uni_ngram",lines)
         },
         "2" = {
           var.name <- "bi.ngram"
           file.name <- create_filename("bi_ngram",lines)
         },
         "3"={
           var.name <- "tri.ngram"
           file.name <- create_filename("tri_ngram",lines)
         },
         "4"={
           var.name <- "quad.ngram"
           file.name <- create_filename("quad_ngram",lines)
         }
  )

  #if (n == 1) {
  #  var.name <- "uni.ngram"
  #  file.name <- create_filename("uni_ngram",lines)
  #} else if (n == 2) {
  #  var.name <- "bi.ngram"
  #  file.name <- create_filename("bi_ngram",lines)
  #} else if (n == 3) {
  #  var.name <- "tri.ngram"
  #  file.name <- create_filename("tri_ngram",lines)
  #} else if (n == 4) {
  #  var.name <- "quad.ngram"
  #  file.name <- create_filename("quad_ngram",lines)
  #}
  
  #Validate if the "var.name" exists in the enviroment
  if (!exists(var.name)) {
    #Validate if "file.name" file exists an load the value
    if (file.exists(file.name)) {
      print(paste("Loading ngram file: ",file.name, sep =""))
      load(file.name,.GlobalEnv) 
    }  else {
      ## Load "alltokens" and create the ngrams
      create_alltokens(lines)
      print(paste("-----> create_ngram(",n,",",lines,").......",sep=""))
      t1 <- proc.time()
      
      print(paste("... Creating Ngram:",var.name,sep=""))
      
      switch(n,
             "1"= { #Unigrams
               uni.ngram <<- ngrams(alltokens, 1)
               print(paste("... Saving Ngram file:",file.name, sep=""))       
             },
             "2"= { #Bigrams
               bi.ngram <<- ngrams(alltokens, 2)
               print(paste("... Saving Ngram file:",file.name,sep=""))
             },
             "3"= { #Trigrams
               tri.ngram <<- ngrams(alltokens, 3)
               print(paste("... Saving Ngram file:",file.name,sep=""))
               save(tri.ngram,file=file.name)
             },
             "4"= { #Quadgrams
               quad.ngram <<- ngrams(alltokens, 4)
               print(paste("... Saving Ngram file:",file.name,sep=""))
               save(quad.ngram,file=file.name)
             }
      )
      
      #if (n == 1) {
      #  uni.ngram <<- ngrams(alltokens, 1)
      #  print(paste("... Saving Ngram file:",file.name, sep=""))
      #  save(uni.ngram,file=file.name)
      #} else if (n == 2) {
      #  bi.ngram <<- ngrams(alltokens, 2)
      #  print(paste("... Saving Ngram file:",file.name,sep=""))
      #  save(bi.ngram,file=file.name)
      #} else if (n == 3) {
      #  tri.ngram <<- ngrams(alltokens, 3)
      #  print(paste("... Saving Ngram file:",file.name,sep=""))
      #  save(tri.ngram,file=file.name)
      #} else if (n == 4) {
      #  quad.ngram <<- ngrams(alltokens, 4)
      #  print(paste("... Saving Ngram file:",file.name,sep=""))
      #  save(quad.ngram,file=file.name)
      #}
      rm("alltokens",envir =.GlobalEnv)
      t2 <- proc.time()
      print(paste("-----> create_ngram: Running Time .......",
                  elapsed_time(t1,t2)," seconds ...",sep=""))
    }
    
  }
  gc()
}
    
   
clean_ngram <- function(n,lines=-1)
{
  # List of Profanity words to be removed
  profanityList <- c("shit","piss","fuck","cunt","cocksucker","motherfucker","tits")
  
  if (n == 1) {
    var.name <- "uni.ngram.clean"
    file.name <- create_filename("uni_ngram_clean",lines)
    ngram.name <- "uni.ngram"
  } else if (n == 2) {
    var.name <- "bi.ngram.clean"
    file.name <- create_filename("bi_ngram_clean",lines)
    ngram.name <- "bi.ngram"
  } else if (n == 3) {
    var.name <- "tri.ngram.clean"
    file.name <- create_filename("tri_ngram_clean",lines)
    ngram.name <- "tri.ngram"
  } else if (n == 4) {
    var.name <- "quad.ngram.clean"
    file.name <- create_filename("quad_ngram_clean",lines)
    ngram.name <- "quad.ngram"
  }  
  
  #Validate if the "var.name" exists in the enviroment
  if (!exists(var.name)) {
    #Validate if "file.name" file exists an load the value
    if (file.exists(file.name)) {
      print(paste("Loading ngram cleaned file: ",file.name, sep =""))
      load(file.name,.GlobalEnv) 
    }  else {
      ## Load the ngrams and cleaned it  
     
      create_ngram(n,lines)
      print(paste("-----> clean_ngram(",n,",",lines,").......",sep=""))
      t1 <- proc.time()
      
      print(paste("... Cleaning Ngram: ",ngram.name, sep =""))
      if (n == 1) {
        uni.ngram.clean <<- 
          selectFeatures(uni.ngram, c(profanityList,"eeee"),
                         selection = "remove", valuetype = "regex")
        rm(uni.ngram,envir =.GlobalEnv)
        print(paste("... Saving Ngram Cleaned file: ",file.name, sep =""))
        save(uni.ngram.clean,file=file.name)
      } else if (n == 2) {
        bi.ngram.clean <<- 
          selectFeatures(bi.ngram, c(profanityList,"eeee"),
                         selection = "remove", valuetype = "regex")
        rm(bi.ngram,envir =.GlobalEnv)
        print(paste("... Saving Ngram Cleaned file: ",file.name, sep =""))
        save(bi.ngram.clean,file=file.name)
      } else if (n == 3) {
        tri.ngram.clean <<- 
          selectFeatures(tri.ngram, c(profanityList,"eeee"),
                         selection = "remove", valuetype = "regex")
        rm(tri.ngram,envir =.GlobalEnv)
        print(paste("... Saving Ngram Cleaned file: ",file.name, sep =""))
        save(tri.ngram.clean,file=file.name)
      } else if (n == 4) {
        quad.ngram.clean <<- 
          selectFeatures(quad.ngram, c(profanityList,"eeee"),
                         selection = "remove", valuetype = "regex")
        rm(quad.ngram,envir =.GlobalEnv)
        print(paste("... Saving Ngram Cleaned file: ",file.name, sep =""))
        save(quad.ngram.clean,file=file.name)
      }
      t2 <- proc.time()
      print(paste("-----> clean_ngram: Running Time .......",
                  elapsed_time(t1,t2)," seconds ...",sep=""))
    }
  }
  gc() 
}
  
  
create_dfm <- function(n,lines=-1) {
  
  if (n == 1) {
    var.name <- "uni.dfm"
    file.name <- create_filename("uni_dfm",lines)
    ngram.clean.name <- "uni.ngram.clean"
  } else if (n == 2) {
    var.name <- "bi.dfm"
    file.name <- create_filename("bi_dfm",lines)
    ngram.clean.name <- "bi.ngram.clean"
  } else if (n == 3) {
    var.name <- "tri.dfm"
    file.name <- create_filename("tri_dfm",lines)
    ngram.clean.name <- "tri.ngram.clean"
  } else if (n == 4) {
    var.name <- "quad.dfm"
    file.name <- create_filename("quad_dfm",lines)
    ngram.clean.name <- "quad.ngram.clean"
  }  
  

  #Validate if the "var.name" exists in the enviroment
  if (!exists(var.name)) {
    #Validate if "file.name" file exists an load the value
    if (file.exists(file.name)) {
      print(paste("Loading dfm file:", file.name, sep=""))
      load(file.name,.GlobalEnv) 
    }  else {
      ## Load the ngrams and cleaned it 
      
      
      clean_ngram(n,lines)
      
      print(paste("-----> create_dfm(",n,",",lines,").......",sep=""))
      t1 <- proc.time()
      
      print(paste("... Creating dfm:", var.name, sep=""))
      if (n == 1) {
        uni.dfm <<- dfm(uni.ngram.clean,toLower = FALSE)
        rm(uni.ngram.clean,envir =.GlobalEnv)
        print(paste("... Saving dfm file:", file.name, sep=""))
        save(uni.dfm,file=file.name)
      } else if (n == 2) {
        bi.dfm <<- dfm(bi.ngram.clean,toLower = FALSE)
        rm(bi.ngram.clean,envir =.GlobalEnv)
        print(paste("... Saving dfm file:", file.name, sep=""))
        save(bi.dfm,file=file.name)
      } else if (n == 3) {
        tri.dfm <<- dfm(tri.ngram.clean,toLower = FALSE)
        rm(tri.ngram.clean,envir =.GlobalEnv)
        print(paste("... Saving dfm file:", file.name, sep=""))
        save(tri.dfm,file=file.name)
      } else if (n == 4) {
        quad.dfm <<- dfm(quad.ngram.clean,toLower = FALSE)
        rm(quad.ngram.clean,envir =.GlobalEnv)
        print(paste("... Saving dfm file:", file.name, sep=""))
        save(quad.dfm,file=file.name)
      }
      t2 <- proc.time()
      print(paste("-----> create_dfm: Running Time .......",
                  elapsed_time(t1,t2)," seconds ...",sep=""))
    }
  }  
  gc()
}


trim_dfm <- function(n,lines=-1,mincount=5) {
  
  if (n == 1) {
    var.name <- "uni.dfm.trim"
    file.name <- create_filename("uni_dfm_trim",lines)
    dfm.name <- "uni.dfm"
  } else if (n == 2) {
    var.name <- "bi.dfm.trim"
    file.name <- create_filename("bi_dfm_trim",lines)
    dfm.name <- "bi.dfm"
  } else if (n == 3) {
    var.name <- "tri.dfm.trim"
    file.name <- create_filename("tri_dfm_trim",lines)
    dfm.name <- "tri.dfm"
  } else if (n == 4) {
    var.name <- "quad.dfm.trim"
    file.name <- create_filename("quad_dfm_trim",lines)
    dfm.name <- "quad.dfm"
  }  
  
  #Validate if the "var.name" exists in the enviroment
  if (!exists(var.name)) {
    #Validate if "file.name" file exists an load the value
    if (file.exists(file.name)) {
      print(paste("Loading dfm trim file:",file.name,sep=""))
      load(file.name,.GlobalEnv) 
    }  else {
      ## Load the dfm  
      create_dfm(n,lines)
      print(paste("-----> trim_dfm(",n,",",lines,").......",sep=""))
      t1 <- proc.time()
      
      print(paste("... trim dfm:", var.name, sep=""))
      if (n == 1) {
        
        uni.dfm.clean <<- trim(uni.dfm,minCount = mincount)
        rm("uni.dfm",envir =.GlobalEnv)
        gc()
        print("... Saving dfm clean: uni.dfm.clean ..")
        save(uni.dfm.clean,file=file.name)

      } else if (n == 2) {
        
        bi.dfm.clean <<- trim(bi.dfm,minCount = mincount)
        rm("bi.dfm",envir =.GlobalEnv)
        gc()
        print("... Saving dfm clean: bi.dfm.clean ..")
        save(bi.dfm.clean,file=file.name)
        
      } else if (n == 3) {
        
        tri.dfm.clean <<- trim(tri.dfm,minCount = mincount)
        rm("tri.dfm",envir =.GlobalEnv)
        gc()
        print("... Saving dfm clean: tri.dfm.clean ..")
        save(tri.dfm.clean,file=file.name)
       
      } else if (n == 4) {
        
        quad.dfm.clean <<- trim(quad.dfm,minCount = mincount)
        rm("quad.dfm",envir =.GlobalEnv)
        gc()
        print("... Saving dfm clean: quad.dfm.clean ..")
        save(quad.dfm.clean,file=file.name)
      }
      t2 <- proc.time()
      print(paste("-----> trim_dfm: Running Time .......",
                  elapsed_time(t1,t2)," seconds ...",sep=""))
    }
  } 
  gc()
} 

  
      
create_DT <- function(n,lines=-1,mincount=5) {
  
  if (n == 1) {
    var.name <- "DT.uni"
    file.name <- create_filename("DT_uni",lines)
    dfm.name <- "uni.dfm.trim"
  } else if (n == 2) {
    var.name <- "DT.bi"
    file.name <- create_filename("DT_bi",lines)
    dfm.name <- "bi.dfm.trim"
  } else if (n == 3) {
    var.name <- "DT.tri"
    file.name <- create_filename("DT_tri",lines)
    dfm.name <- "tri.dfm.trim"
  } else if (n == 4) {
    var.name <- "DT.quad"
    file.name <- create_filename("DT_quad",lines)
    dfm.name <- "quad.dfm.trim"
  }  
  
  #Validate if the "var.name" exists in the enviroment
  if (!exists(var.name)) {
    #Validate if "file.name" file exists an load the value
    if (file.exists(file.name)) {
      print(paste("Loading DT file:",file.name,sep=""))
      load(file.name,.GlobalEnv) 
    }  else {
      ## Load the dfm trim 
      trim_dfm(n,lines,mincount)
      
      print(paste("-----> create_DT(",n,",",lines,").......",sep=""))
      t1 <- proc.time()
      
      print(paste("... Creating DT:", var.name, sep=""))
      if (n == 1) {
        #
        # Create DT for unigrams:
        #     t1  freq  
        n.uni <- nfeature(uni.dfm.clean)
        top.uni <- topfeatures(uni.dfm.clean,n.uni)
        rm("uni.dfm.clean",envir =.GlobalEnv)
        gc()
        DT.uni <<- data.table(t1=names(top.uni),freq=top.uni)
        print("... Saving DT.uni ..")
        save(DT.uni,file=file.name)
        rm("top.uni","n.uni")        
  
      } else if (n == 2) {

        #
        # DT for bigrams:
        #     t1  t2  freq
        n.bi <- nfeature(bi.dfm.clean)
        top.bi <- topfeatures(bi.dfm.clean,n.bi)
        rm("bi.dfm.clean",envir =.GlobalEnv)
        gc()
        DT.bi <<- data.table(V1=names(top.bi),freq=top.bi)
        rm("top.bi","n.bi")
        gc()
        DT.bi[,c("t1","t2") := tstrsplit(V1, "_", fixed=TRUE),]
        DT.bi <<- DT.bi[,list(t1,t2,freq),]
        print("... Saving DT.bi ..")
        save(DT.bi,file=file.name)
      } else if (n == 3) {

        #
        # DT for trigrams:
        #     t1  t2  t3 freq  

        n.tri <- nfeature(tri.dfm.clean)
        top.tri <- topfeatures(tri.dfm.clean,n.tri)
        rm("tri.dfm.clean",envir =.GlobalEnv)
        gc()
        DT.tri <<- data.table(V1=names(top.tri),freq=top.tri)
        rm("top.tri","n.tri")
        gc()
        DT.tri[,c("t1", "t2","t3") := tstrsplit(V1, "_", fixed=TRUE),]
        DT.tri <<- DT.tri[,list(t1,t2,t3,freq),]
        print("... Saving DT.tri ..")
        save(DT.tri,file=file.name)
      } else if (n == 4) {

        #
        # DT for quadgrams:
        #     t1  t2  t3 t4 freq
        
        n.quad <- nfeature(quad.dfm.clean)
        top.quad <- topfeatures(quad.dfm.clean,n.quad)
        rm("quad.dfm.clean", envir =.GlobalEnv)
        gc()
        DT.quad <<- data.table(V1=names(top.quad),freq=top.quad)
        rm("top.quad","n.quad")
        gc()
        DT.quad <- DT.quad[,c("t1", "t2","t3","t4") := tstrsplit(V1, "_", fixed=TRUE),]
        DT.quad <<- DT.quad[,list(t1,t2,t3,t4,freq),]
        print("... Saving DT.quad ..")
        save(DT.quad,file=file.name)
      }
      t2 <- proc.time()
      
      print(paste("-----> create_DT: Running Time .......",
                  elapsed_time(t1,t2)," seconds ...",sep=""))
    }
      
  } 
  gc()
} 