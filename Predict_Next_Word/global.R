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
library(ggplot2)
library(grid)
library(gridExtra)


#wd.R <- "D:/Coursera/Capstone Project/Coursera---Data-Science---Capstone-Project"
wd.R <- "D:/001 -- Coursera/Capstone Project/Coursera---Data-Science---Capstone-Project"

#wd.RData <- "D:/Coursera/Capstone Project/Coursera-SwiftKey/final/en_US/"
wd.RData <- "D:/001 -- Coursera/Capstone Project/Coursera-SwiftKey/final/en_US"

setwd(wd.R)


source("Pred Next Word vFinal.R")
source("Pred Next Word Regex vFinal.R")
source("Main Predict Word vFinal.R")

#setwd(wd.RData)

#load_DT_prob_final_table(1,50)
#load_DT_prob_final_table(2,50)
#load_DT_prob_final_table(3,50)
#load_DT_prob_final_table(4,50)





