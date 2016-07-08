# Capstone Project - Final Report
Enrique Reveron  
June 26, 2016  




## Executive Summary

This is a Milestone report related with the Coursera Capstone Project, the target is show initial exploratory data analysis about the US dataset that include three kind of files:

* Twitter
* News
* Blogs


## 1. Optimizations 

$$\sum_{i=1}^n X_i$$

$$\alpha, \beta,  \gamma, \Gamma$$

$$a \pm b$$
$$x \ge 15$$
$$a_i \ge 0~~~\forall i$$

$$\sum_{i=1}^{n}\left( \frac{X_i}{Y_i} \right)$$

$$\sum_{i=1}^{n} X^3_i$$

### 1.1 Create Ngrams Tables

###	1.1.1 quanteda Library

#####		1.1.1.1 no corpus

####		1.1.1.2 tokenizer + remove bad words before dfm
		
####		1.1.1.3 tokenizer unigrams + to create tokenizer of ngrams
				
####		1.1.1.4 dfm + dfm clean

### 1.1.2 data frame vs data table

### 1.2 Method to Calculate Probabilities of Words to make Prediction

#### 1.2.1 MLE

#### 1.2.2 Knersey-ney

#### 1.2.2.1 Recursive

#### 1.2.2.2 Recursive + saving important values

#### 1.2.2.3 Iterative whithout joins

#### 1.2.2.4 Iterative + joins

### 1.3 Predict Next Words

#### 1.3.1 Backoff Implementation

#### 1.3.2 Complete words

#### 1.3.3 Incomplete words

#### 1.3.4 Number of words to predict

#### 1.3.5 How to reduce the number of words / Minimun Probability to Consider

### 1.4 Shiny App 

### 1.4.1 Only use the nth most important words (higher probability)

### 1.4.2 Remove which words?

### 1.4.3 App Parameters

#### 1.4.3.1 Number of Words to Predict

#### 1.4.3.2 Minimun Probability to Consider

#### 1.4.3.3 Predict next words considering Complete / Incomplete Words

#### 1.4.3.4 Choose the Ngrams (20%,40%,60%,80%) of the total




## 1. Load the Neccesary Libraries

For this project we will use basicly the **quanteda**,**ggplot2**, **knitr** and **RColorBrewer**.


```r
library(quanteda)
```

```
## quanteda version 0.9.6.9
```

```
## 
## Attaching package: 'quanteda'
```

```
## The following object is masked from 'package:base':
## 
##     sample
```

```r
library(data.table)
library(ggplot2)
library(knitr)

# To load the local files

setwd("D:/Coursera/Capstone Project/Coursera---Data-Science---Capstone-Project")
#setwd("D:/001 -- Coursera/Capstone Project/Coursera-SwiftKey/final/en_US")

source("Create Ngrams Data Table vFinal.R")
source("Knersey-ney vFinal.R")
source("Pred Next Word vFinal.R")

# For reproducibility
set.seed(12345)

# To load the Data
setwd("D:/Coursera/Capstone Project/Coursera-SwiftKey/final/en_US")
```

## 2. Create Ngram Data Table

In order to create the Ngrams we will do several steps

### 2.1 Load All the Data

The files that we will use in the project are bigger than **150Mbytes** each one. In order to do the exploratory data analysis and to have an acceptable runtime, I will use only **10%** of the data. 


```r
#create_mydata() 
```

### 2.2 Create All Tokens


```r
#create_alltokens()
```


### 2.3 Create Ngrams 

#### 2.3.1 Create and Clean Unigrams


```r
#create_ngram(n=1)
```



```r
#clean_ngram(n=1)
```



#### 2.3.2 Create and Clean Bigrams


```r
#create_ngram(n=2)
```




```r
#clean_ngram(n=2)
```


#### 2.3.3 Create and Clean Trigrams


```r
#create_ngram(n=3)
```



```r
#clean_ngram(n=2)
```


#### 2.3.4 Create and Clean Quadgrams


```r
#create_ngram(n=4)
```



```r
#clean_ngram(n=4)
```


### 2.4 Create and Trim DFM 

#### 2.4.1 Create and Trim Uni-dfm


```r
#create_dfm(n=1)
```



```r
#trim_dfm(n=1)
```


#### 2.4.2 Create and Trim Bi-dfm


```r
#create_dfm(n=2)
```



```r
#trim_dfm(n=2)
```


#### 2.4.3 Create and Trim Tri-dfm


```r
#create_dfm(n=3)
```



```r
#trim_dfm(n=3)
```

#### 2.4.4 Create and Trim Quad-dfm


```r
#create_dfm(n=4)
```



```r
#trim_dfm(n=4)
```


### 2.5 Create Data Table with Tokens and Frequency

#### 2.5.1 Create Unigram Data Table with Tokens and Frequency


```r
#create_DT(n=1)
```


#### 2.5.2 Create Bigram Data Table with Tokens and Frequency


```r
#create_DT(n=2)
```

#### 2.5.3 Create Trigram Data Table with Tokens and Frequency


```r
#create_DT(n=3)
```

#### 2.5.4 Create Quadgram Data Table with Tokens and Frequency


```r
#create_DT(n=4)
```

### 3. Calculate Probability for Each Ngram

### 3.1 Create Unigram Knersey-ney Probability Table

The files that we will use in the project are bigger than **150Mbytes** each one. In order to



```r
#init_DT_tables() 
#calculate_prob_kn(1)
```


Those are information in the Unigram DT:


```r
# Print the basic information about the files. 
#kable(DT.uni[1:10])
```

The prob table has:


```r
# Print the basic information about the files. 
#kable(DT.uni.prob[1:10])
```

### 3.2 Create Bigram Knersey-ney Probability Table

The files that we will use in the project are bigger than **150Mbytes** each one. In order to



```r
#init_DT_tables() 
#calculate_prob_kn(2)
```

Those are information in the Unigram DT:


```r
# Print the basic information about the files. 
#kable(DT.bi[1:10])
```

The prob table has:


```r
# Print the basic information about the files. 
#kable(DT.bi.prob[1:10])
```


### 3.3 Create Trigram Knersey-ney Probability Table

The files that we will use in the project are bigger than **150Mbytes** each one. In order to



```r
#calculate_prob_kn(3)
```

Those are information in the Unigram DT:


```r
# Print the basic information about the files. 
#kable(DT.tri[1:10])
```

The prob table has:


```r
# Print the basic information about the files. 
#kable(DT.tri.prob[1:10])
```

### 3.4 Create Quadgram Knersey-ney Probability Table

The files that we will use in the project are bigger than **150Mbytes** each one. In order to



```r
#calculate_prob_kn(4)
```

Those are information in the Unigram DT:


```r
# Print the basic information about the files. 
#kable(DT.quad[1:10])
```

The prob table has:


```r
# Print the basic information about the files. 
#kable(DT.quad.prob[1:10])
```


## 4. Prediction of next word

### 4.1 Example with Unigram


```r
#source("Pred Next Word vFinal.R")
#prediction1 <- predict_nextword(c("how"),100,0,5) 
```


```r
# Print the basic information about the files.  
#kable(prediction1)
```

### 4.2 Example with Bigram


```r
#prediction2 <- predict_nextword(c("how","are"),100,0,5) 
```


```r
# Print the basic information about the files.  
#kable(prediction2)
```

### 4.3 Example with Trigram


```r
#prediction3 <- predict_nextword(c("how","are","you"),100,0,5) 
```


```r
# Print the basic information about the files. 
#kable(prediction3)
```


## 4. Prediction of next word using Regex


```r
#source("Pred Next Word vFinal.R")
#source("Pred Next Word Regex vFinal.R")
#prediction1 <- predict_nextword_regex(c("how","are",""),100,0,5)
#prediction2 <- predict_nextword(c("how","are"),100,0,5)
```



```r
# Print the basic information about the files. 
#kable(prediction1)
#kable(prediction2)
```
