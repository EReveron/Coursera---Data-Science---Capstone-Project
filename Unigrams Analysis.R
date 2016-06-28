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

#setwd("D:/Coursera/Capstone Project/Coursera-SwiftKey/final/en_US")
setwd("D:/001 -- Coursera/Capstone Project/Coursera-SwiftKey/final/en_US")

# For reproducibility
set.seed(12345)

source("Create Ngrams Data Table v4.R")

create_DT(1,100,1)
create_DT(2,100,1)
create_DT(3,100,1)
create_DT(4,100,1)

DT.analysis <- DT.uni

total.words <- DT.analysis[,sum(freq),]
number.words <- nrow(DT.analysis)

DT.analysis <- DT.analysis[,acumfreq:=cumsum(freq) ,]

DT.analysis <- DT.analysis[,perfreq:= freq / total.words * 100,]
DT.analysis <- DT.analysis[,acumperfreq:= acumfreq / total.words * 100,]

DT.analysis <- DT.analysis[,numword:= 1:number.words,]



head(DT.analysis[acumperfreq >= 50 & acumperfreq <= 51,,],1)
head(DT.analysis[acumperfreq >= 90 & acumperfreq <= 91,,],1)


ggplot(data=DT.analysis[1:100000], aes(x=numword, y=acumperfreq)) +
  geom_line(stat ="identity", position= "identity",size=1.2, colour="black") +
  #geom_text(data=subset(all.words, numword == 820 | numword == 13493),
  #          aes(label=paste("(",acumperfreq,",",numword,")")), 
  #          hjust = 1.2, vjust = -0.4) +
  geom_vline(xintercept = 820, color="red") +
  geom_hline(aes(yintercept=50), color="red") +
  geom_vline(xintercept = 13493, color="blue") +
  geom_hline(aes(yintercept=90), color="blue") +
  labs(title="Number of Words needed to Cover all Words Instances") +
  #scale_x_continuous( trans = "log10") +   
  labs(x="Number of Words",y="%Coverage")


#DT.analysis2 <- as.data.table()

create_dfm(3,-1)

DT.freq <- data.table(num.words=0, freq=0, per=0)
DT.freq <- DT.freq[0]
max.freq <- head(DT.analysis[,freq,],1)

j <- 1

for (i in 1:max.freq) {
  r <- nrow(DT.analysis[freq == i,,])  
  if (r > 0 ) {
    print(paste("r vale:",r," j vale:",j))
    DT.freq[j,num.words:=r,]
    j <<- j +1
  }
}



for (i in 1:l.tri) 
  DT.tri[i,prob:=prob_kneser_ney(
    c(as.character(t1),as.character(t2),as.character(t3)))]










freq1 <- nrow(DT.analysis[, freq == 1,])
freq2 <- nrow(DT.analysis[, freq == 2,])


#######################################################################

source("Knersey-ney v8.R")

init_DT_tables(100,1)
calculate_prob_kn(1,100,-1)
calculate_prob_kn(2,100,-1)
calculate_prob_kn(3,100,-1)
calculate_prob_kn(4,100,-1)

#######################################################################

source("Pred_Next_Wordv2.R")

predict_nextword(c("this"),100,0,5)

predict_nextword(c("this","is"),100,0,5)

predict_nextword(c("this","is","not"),100,0,5)

  
######################################################################

source("Pred_Next_Word_Regexv1.R")

predict_nextword_regex(c("thi"),100,0,5)

predict_nextword_regex(c("this","is"),100,0,5)

predict_nextword_regex(c("this","is","not"),100,0,5)



