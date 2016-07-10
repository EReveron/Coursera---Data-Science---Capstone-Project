#####################################################################
# Generate data: generate the information about the frequency of 
#          the differents ngrams (unigram, bigram, trigram and quadgrams)
#          to be used later with Kneser-ney algortihm to calculate the
#          probability of each ngram for predict the next word.
#          This script was run on my PC and takes long time to finish,
#          at the end this script saves the data into four files:
#
#             DT_uni.Rdata: data table that include all the unigrams and the
#                          frequency for each one.
#             DT_bi.Rdata: data table that include all the bigrams and the
#                          frequency for each one.
#             DT_tri.Rdata: data table that include all the trigrams and the
#                          frequency for each one.
#             DT_quad.Rdata: data table that include all the quadgrams and the
#                          frequency for each one.

library(quanteda)
library(data.table)
library(ggplot2)
library(microbenchmark)

setwd("D:/001 -- Coursera/Capstone Project/Coursera---Data-Science---Capstone-Project")
#setwd("D:/Coursera/Capstone Project/Coursera---Data-Science---Capstone-Project")

# For reproducibility
set.seed(12345)

source("Create Ngrams Data Table vFinal 2.R")
#source("Knersey-ney vFinal.R")
#source("Knersey-ney Optimazed vFinal2.R")
source("Knersey-ney Optimazed vFinal 5.R")
source("Main Predict Word vFinal.R")
source("Pred Next Word Regex vFinal.R")
source("Pred Next Word vFinal.R")

setwd("D:/001 -- Coursera/Capstone Project/Coursera-SwiftKey/final/en_US")
#setwd("D:/Coursera/Capstone Project/Coursera-SwiftKey/final/en_US")


mydata <- "this is a demo of games.this is a game demo a game. its a demo of games . a real a demo if games"


mydata <- stri_replace_all_regex(mydata,"[.,;:]", " eeee ", 
                                 vectorize_all=FALSE)


alltokens <<- tokenize(mydata, what = "fastestword", 
                       removeNumbers = TRUE, 
                       removePunct = TRUE,
                       removeSymbols = TRUE, 
                       removeSeparators = TRUE, 
                       removeTwitter = TRUE,
                       removeHyphens = TRUE, 
                       removeURL = TRUE, 
                       verbose = TRUE)

uni.ngram <<- ngrams(alltokens, 1)

bi.ngram <<- ngrams(alltokens, 2)

tri.ngram <<- ngrams(alltokens, 3)

quad.ngram <<- ngrams(alltokens, 4)

profanityList <- c("shit","piss","fuck","cunt","cocksucker","motherfucker","tits")

uni.ngram.clean <<- 
  selectFeatures(uni.ngram, c(profanityList,"eeee"),
                 selection = "remove", valuetype = "regex")

bi.ngram.clean <<- 
  selectFeatures(bi.ngram, c(profanityList,"eeee"),
                 selection = "remove", valuetype = "regex")

tri.ngram.clean <<- 
  selectFeatures(tri.ngram, c(profanityList,"eeee"),
                 selection = "remove", valuetype = "regex")

quad.ngram.clean <<- 
  selectFeatures(quad.ngram, c(profanityList,"eeee"),
                 selection = "remove", valuetype = "regex")


uni.dfm.clean <<- dfm(uni.ngram.clean,toLower = FALSE)
bi.dfm.clean <<- dfm(bi.ngram.clean,toLower = FALSE)
tri.dfm.clean <<- dfm(tri.ngram.clean,toLower = FALSE)
quad.dfm.clean <<- dfm(quad.ngram.clean,toLower = FALSE)




n.uni <- nfeature(uni.dfm.clean)
top.uni <- topfeatures(uni.dfm.clean,n.uni)
DT.uni <<- data.table(t1=names(top.uni),freq=top.uni)


p1 <- 1
p2 <- p1 +1


n.bi <- nfeature(bi.dfm.clean)
top.bi <- topfeatures(bi.dfm.clean,n.bi)
DT.bi <<- data.table(V1=names(top.bi),freq=top.bi)
DT.bi[,c("t1","t2") := tstrsplit(V1, "_", fixed=TRUE),]
DT.bi <<- DT.bi[,list(t1,t2,freq),]


n.tri <- nfeature(tri.dfm.clean)
top.tri <- topfeatures(tri.dfm.clean,n.tri)
DT.tri <<- data.table(V1=names(top.tri),freq=top.tri)
DT.tri[,c("t1", "t2","t3") := tstrsplit(V1, "_", fixed=TRUE),]
DT.tri <<- DT.tri[,list(t1,t2,t3,freq),]


n.quad <- nfeature(quad.dfm.clean)
top.quad <- topfeatures(quad.dfm.clean,n.quad)
DT.quad <<- data.table(V1=names(top.quad),freq=top.quad)
DT.quad <- DT.quad[,c("t1", "t2","t3","t4") := tstrsplit(V1, "_", fixed=TRUE),]
DT.quad <<- DT.quad[,list(t1,t2,t3,t4,freq),]



DT.uni.prob <<- as.data.table(DT.uni, key = "t1")
DT.uni.prob <<- DT.uni.prob[,freq1:=freq,][,list(t1,freq1),]

DT.bi.prob <<- as.data.table(DT.bi, key = "t1,t2")
DT.bi.prob <<- DT.bi.prob[,freq2:=freq,][,list(t1,t2,freq2),]

DT.tri.prob <<- as.data.table(DT.tri, key = "t1,t2,t3")
DT.tri.prob <<- DT.tri.prob[,freq3:=freq,][,list(t1,t2,t3,freq3),]

DT.quad.prob <<- as.data.table(DT.quad, key = "t1,t2,t3,t4")
DT.quad.prob <<- DT.quad.prob[,freq4:=freq,][,list(t1,t2,t3,t4,freq4),]


n.uni <<- nrow(DT.uni.prob)
numwords.uni <<- sum(DT.uni.prob$freq1)

n.bi <<- nrow(DT.bi.prob)
n1.bi <<- nrow(DT.bi.prob[freq2 == p1,,])
n2.bi <<- nrow(DT.bi.prob[freq2 == p2,,])


calculate_prob_kn_opt(1,50,1)
calculate_prob_kn_opt(2,50,1)
calculate_prob_kn_opt(3,50,1)
calculate_prob_kn_opt(4,50,1)
























































create_DT(1,50,1)
create_DT(2,50,1)
create_DT(3,50,1)
create_DT(4,50,1)

load_DT_prob_tables_opt(1,50,1)
load_DT_prob_tables_opt(2,50,1)


DT.uni.temp <- copy(DT.uni)
DT.bi.temp <- copy(DT.bi)

DT.uni.temp <- DT.uni.temp[1:100]
DT.bi.temp <- DT.bi.temp[1:100]




########### Calculte N11 and N12

DT.bi.n.temp <- DT.bi.temp[freq >= 1]

setkey(DT.uni.temp,"t1")
setkey(DT.bi.n.temp,"t2")

DT.n11 <- DT.bi.n.temp[DT.uni.temp, .N, by = .EACHI][,c("t1","n11","t2","N"):=list(t2,N,NULL,NULL)]

setkey(DT.uni.temp,"t1")
setkey(DT.bi.n.temp,"t1")

DT.n12 <- DT.bi.n.temp[DT.uni.temp, .N, by = .EACHI][,c("t1","n12","N"):=list(t1,N,NULL)]

DT.uni.temp <- merge(DT.uni.temp, DT.n11 , by = "t1")

DT.uni.temp <- merge(DT.uni.temp, DT.n12 , by = "t1")



######### Calculate pkn1

n.bi = 4

DT.uni.temp[,pkn11:= n11 / n.bi]

######### Calculate Lambda

D2 = 5

## n11 es Cero ??

DT.uni.temp[,c("l11","l12") := 
                  list((D2 / freq) * n12, (D2 / n11) * n12)]








#setkey(DT.bi.temp,"t22")

#DT.join2 <- DT.bi.temp[DT.uni.temp]


#DT.join1[,n11:= nrow(t11), by = t11]






create_DT(1,100,1)
create_DT(2,100,1)
create_DT(3,100,1)
create_DT(4,100,1)

create_DT(1,150,1)
create_DT(2,150,1)
create_DT(3,150,1)
create_DT(4,150,1)

create_DT(1,250,1)
create_DT(2,250,1)
create_DT(3,250,1)
create_DT(4,250,1)


calculate_prob_kn(1,250,1)
calculate_prob_kn(2,250,1)
calculate_prob_kn(3,250,1)
calculate_prob_kn(4,250,1)


create_DT(1,-1,1)
create_DT(2,-1,1)
create_DT(3,-1,1)
create_DT(4,-1,1)



calculate_prob_kn_opt(1,-1,5,-1)
calculate_prob_kn_opt(2,-1,5,-1)
calculate_prob_kn_opt(3,-1,5,-1)
calculate_prob_kn_opt(4,-1,5,-1)




microbenchmark(
  cond_sum_cpp(x, y, z),
  cond_sum_r(x, y, z),
  unit = "ms"
)








source("Pred_Next_Wordv2.R")

predict_nextword(c("this"),100,0,5)

predict_nextword(c("this","is"),100,0,5)

predict_nextword(c("this","is","not"),100,0,5)

  
######################################################################

source("Pred_Next_Word_Regexv1.R")

predict_nextword_regex(c("thi"),100,0,5)

predict_nextword_regex(c("this","is"),100,0,5)

predict_nextword_regex(c("this","is","not"),100,0,5)



################################## TEST FINAL ################################


create_DT(1,-1,0)
create_DT(2,-1,0)
create_DT(3,-1,0)
create_DT(4,-1,0)

calculate_prob_kn(n=1,lines=-1,p1=1)
calculate_prob_kn(n=2,lines=-1,p1=1)
calculate_prob_kn(n=3,lines=-1,p1=1)
calculate_prob_kn(n=4,lines=-1,p1=1)


