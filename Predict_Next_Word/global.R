# This is the global.R file for a Shiny web application.
# Author: Enrique Reveron
# Date: 2016-06-27
# Content: This Shiny Apps predict the next word for english language using
#          Knersey-ney Algoritm and Backoff

library(shiny)
library(googleVis)
library(RColorBrewer)
library(wordcloud)
library(DT)
library(markdown)


#setwd("D:/Coursera/Capstone Project/Coursera---Data-Science---Capstone-Project")
setwd("D:/001 -- Coursera/Capstone Project/Coursera---Data-Science---Capstone-Project")

source("Pred Next Word vFinal.R")
source("Pred Next Word Regex vFinal.R")
source("Main Predict Word vFinal.R")
setwd("D:/001 -- Coursera/Capstone Project/Coursera-SwiftKey/final/en_US")

#setwd("D:/Coursera/Capstone Project/Coursera-SwiftKey/final/en_US/FINAL2/Final 27-06")

load_DT_prob_final_table(1,50)
#DT.uni.prob.final <- DT.uni.prob.final[freq1 >=5,,]
load_DT_prob_final_table(2,50)
#DT.bi.prob.final <- DT.bi.prob.final[freq2 >=5,,]
load_DT_prob_final_table(3,50)
#DT.tri.prob.final <- DT.tri.prob.final[freq3 >=5,,]
load_DT_prob_final_table(4,50)
#DT.quad.prob.final <- DT.quad.prob.final[freq4 >=5,,]





