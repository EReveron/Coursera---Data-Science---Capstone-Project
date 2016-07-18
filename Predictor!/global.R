# This is the global.R file for a Shiny web application.
# Author: Enrique Reveron
# Date: 2016-07-17
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

source("Pred Next Word vFinal.R")
source("Pred Next Word Regex vFinal.R")
source("Main Predict Word vFinal.R")