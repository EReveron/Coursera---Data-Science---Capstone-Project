# Capstone Project - Final Report
Enrique Reveron  
July 16, 2016  




## Executive Summary

This is a Final Report related with the Switfkey Coursera Capstone Project, the target was create a Shiny App to predict next word using a US dataset that include three kind of files:

* Twitter
* News
* Blogs

This Final Report will explain the most relevant considerations to finally build the application. As a Natural Language Processing (NLP) Application, the core is define how to calculate the probability to each word that could be used in the prediction task.

As was show in the **Milestone Report**, the dataset (Twitter, News and Blogs US files) is considerable big to be handle by a normal PC Computer, also as a final target, the prediction application must run on Shiny servers on the Internet, so is very important to consider how to simplify and optimize the process to provide a solution that suits the enviroment. The main assumptions, simplifications and optimizations are described in the next section. 

## 1. Cleaning and Transform the Dataset (Corpora)

One special and important step is how to clean and transform the Dataset. In NLP, the dataset is named the **corpus** (or **corpora** in plural).
This project only consider to use the English language corpora, as a part of the cleaning stage, we consider the following:

### 1.1. Subsetting the Corpora

Following the idea of Machine Learning Algorithms and the already mentioned limited computational resources, we consider to take a subset of the corpora, **80% of the total** to be used as a **training data**.

### 1.2. Transform ISO8859-2 and Latin Characters

Some special characters (**like emojies and latin characters**) was found in the corpora, so was neccesary to convert **all the text first to ASCII** to remove it.

### 1.3. Lowering the Characters

To simplify the process, we consider to lowering all the corpora characters.

### 1.4 Remove Special Words

Into the corpora was found several special words that we remove it:

* URL's: http://www.coursera.org, https://www.123.com
* Email Addresses: enrique.reveron@gmail.com
* Twitter Addresses and Hashtag: @aes, #SavetheWorld
* Words that start with Digits: *123end, 1up, 2less*
* Words that finish with Digits: *end123, up2, less4*
* Digits: *12345, 12, 1345* and
* Symbols: *+,-,=,[,],{,},¿,*,*

### 1.5 Strategy to Handle Special Characters

After severals tests, we identify the neccesity to establish a strategy to handle two kind of special characters: **sentence separators ( , ; . ! ? : )** and **apostrophe ( ' )**. 

The idea to handle the **sentence separators** raised because in the process to create the ngrams tables (called **tokenization**) appears fake ngrams that could result in a wrong prediction result. Let's make an example, if we consider a sentence into the corpora like this: 

   *"how are you? i'm ok, i want to talk to you about $$$"*
   
If we consider as a cleaning strategy remove all the punctuation symbols (including the ones identified previously as a sentence separators), the sentence will be transform to:

   *"how are you i'm ok i want to talk to you about"*

The **bigram tokenization** process will create the following **bigrams**:

* (how are)
* (are you)
* (you i'm)
* (i'm ok)
* (ok i)
* (i want)
* ....

We can see that the bigrams *(you i'm)* and *(ok i)* are fake ones (are not real bigrams into the corpora) and could affect the prediction task. To avoid this, we **substitute first the sentence separators by a special character** (**"eeee"**, that is not a regular english word and should not appear into the corpora), and then **substitute all the remaining symbols**. Using the previous example, after substitute the **sentence separators** by *"eeee"* we will have:

   *"how are you eeee i'm ok eeee i want to talk to you about"*

And the **bigrams** will be:

* (how are)
* (are you)
* (you eeee)
* (eeee i'm)
* (i'm ok)
* (ok eeee)
* (eeee i)
* (i want)
* ....

And later in the **clean ngram process** (we will discuss about it later), we will remove any bigram that include **eeee** in order to remove those fake ngrams. This process also works for unigrams, trigrams and quadgrams.

Regarding **apostrophe ( ' )**, the objective was the opposite, avoid to remove it from the corpora to have words like **"i'm" "you're" "did'nt"** into the ngrams tables. To do that we consider to do something similar as sentence separators. First we **substitute apostrophe by a special character**, **then remove all the symbols** and **finally put apostrophe back**. Considering our previous example, after substitute **sentence separators by eeee** we will **substitute apostrophe by ffff**, so we will have:

   *"how are you eeee i ffff m ok eeee i want to talk to you about $$$"*

Then we will **remove all the symbols**, the previous example will be transform to:

   *"how are you eeee i ffff m ok eeee i want to talk to you about"*
   
And finally **put apostrofe back**:

   *"how are you eeee i'm ok eeee i want to talk to you about"*



## 2. Ngrams Model Selection

The basic of a Natural Language Processing (NLP) Application, is create a ngram model and calculate the probability for each word on it. A **ngram** is a sequence of words that appears into the dataset with n-length (unigram, bigram, trigram, quadgram and ngram for 1,2,3,4 and n-length). Using those ngrams and the probabilities calculated, we can "predict" the next word. Using the Markov Chains principle with a ngram model, **we can use the last n-1 words in order to predict the next n-word**.

One important step is **choose the ngram model that will be created**. Based on the application enviroment limitations (my local PC with only 16GB RAM and finally Shiny Servers on Internet), we consider to implement a **quadgram model**.

Based on this we will create: **Unigrams, Bigrams, Trigrams and Quadgrams Tables; calculate the probability for each ngram and use it to predict next word**.

With this model, **we can use the last three words to predict the next word using Quadgram Probabilities Tables**.


## 3. Build a Quadgram Model

To **build the quadgram model** we choose the **quanteda** library that provide good tools to create and handle ngrams and the corpora.

###	3.1. Inside **quanteda** Library

The **quanteda** library provide several good tools that help us to build our quadgram model. In fact, the library offer different ways (using different types of objects class) to do the same tasks. In order to optimize the running time and resources, I did some test with different object types to choose the best way to use it.    

#### 3.1.1. Corpus vs Tokenizedtexts

In order to build the quadgram model, we finally want to know the frequency that each unigram, bigram, trigram and quadgram have in the corpora. To get that with **quanteda** we must create a **Document Feature Matrix (dfm)** (using **quanteda 'dfm' object**). 

We have several options to create a **'dfm'** object in **quanteda**:

* Create a **'corpus'** object first (using **corpus()** function) and then create a **'dfm'** object (using **dfm()** function)
* Create a **'tokenizedtext'** object first (using **tokenize()** function), then create a **'ngrams'** (using **ngrams()** function) and finally a **'dfm'** object (using **dfm()** function) 

In this step I got a lot of problems regarding the memory limits of my PC (16GB), so I decided to do several test and use the **profvis()** library to find a solution.

After check the results, I saw that the first option **(corpus() --> dfm())** takes 30% more **running time** that the second option **(tokenize() --> ngrams() --> dfm()) (52780 ms vs 74770 ms)**, in terms of **peak memory allocation requirements** the first option needs almost the double **(456.5MB vs 220.7MB)** as you can see in the following picture:

<div style="width:800px; height=600px">
![Image DFM with Corpus Flame](Coursera - Capstone - profvis dfm with corpus flame.jpg)
![Image DFM with Token Flame](Coursera - Capstone - profvis dfm with tokenize flame.jpg)
</div>

Was even more interesting to see that in the first option **(corpus() --> dfm())** the **dfm()** function make a internal call to **tokenize()** function, that also make a internal call to **ngrams()** as you can see in the following picture:

<div style="width:800px; height=600px">
![Image DFM with Corpus Data](Coursera - Capstone - profvis dfm with corpus data.jpg)
![Image DFM with Token Data](Coursera - Capstone - profvis dfm with tokenize data.jpg)
</div>

Based on those results, we choose the second option **(tokenize() --> ngrams() --> dfm())** and was possible to create the dfm's for all ngrams (unigrams, bigrams, trigrams and quadgrams) in my computer.


#### 3.1.2. Strategy to Remove Bad Words and Fake Ngrams  

To remove bad words and avoid to create fake ngrams, we have a similar approach to **sentence separators** **(please check 1.1.5 Strategy to Handle Special Characters, about "Sentence Separators")**, we will substitute into the corpora the bad words for the **sentence separator** special character **'eeee'** and finally remove all the ngrams that include it.

To remove those ngrams we also have several options in **quanteda**:

* At **'ngram'** object creation time using **ngrams()** function with **ignoredFeatures** option 
* At **'dfm'** object creation time using **dfm()** function with **ignoredFeatures** option
* After **'ngram'** object creation using **selectFeatures()** function with **selection = "remove"** and  **valuetype = "regex"** options

Based on test results and memory restrictions, we choose the last option: **after ngrams() creation using selectFeatures() function with selection = "remove" and valuetype = "regex" options**.

#### 3.1.3. Ngrams Creation

In order to create the Ngrams in **quanteda** we can also have several choices:

* With **tokenize()** function using a 'character' object (the all corpora) with **ngrams** option
* With **ngrams()** function using a 'character' object (the all corpora)
* With **ngrams()** function using a 'tokenizedTexts' object 

Because on **1.3.1.1 Corpus vs Tokenizedtexts** we already choose to create the **tokenizedTexts**, we use the third option. 

To create each ngram (bigrams, trigrams, quadgrams) we **first create a 'tokenizedTexts' object** with ngram=1 and use this object to create the others. 


#### 3.1.4. Data Feature Matrix Trim

Because the big size of the different **'dfm' objects** (unigram, bigram, trigram and quadgram) we consider how to reduce the **'dfm' object size**, one option that **quanteda** provides is use the **trim()** function, is very flexible and you can choose to trim a dfm object based on document and term frequency, and/or subsampling.

We consider to **trim the dfm before** the Ngram probability calculation at the beggining, but because some wrong calculations appears (probability values larger than 1) we didn't use it at this level.

### 4. Pre-compute Probability Tables

One obvius consideration is **pre-compute all ngrams probabilites** to avoid that the Shiny Application need to handle that, running faster on the enviroment. The next step is choose the probability calculation algorithm.

### 5. Algorithm to be used to Calculate Ngram Probabilities

This is a very important step and maybe the most important one in the all process, we consider two options: **Maximun Likelihood (ML)** and **Kneser-ney Smoothing**.

#### 5.1. Maximum Likelihood (ML)

This is the most simple one, it only consider the relationship between the frequency of each ngram with the total amount of ngrams.

$$P_{ML}(w_{i}{\mid}w_{i-n+1}^{i}) = \frac{c(w_{i-n+1}^{i})}{\sum_{w'_{i}} c(w_{i-n+1}^{i-1} w'_{i}) } = \frac{c(w_{i-n+1}^{i})}{ c(w_{i-n+1}^{i-1})}$$

Where $c(w_{i-n+1}^{i})$ means the count (number of times) that the ngram $w_{i-n+1}^{i}$ appears on the corpora.

The calculations results are not so good to be used for prediction because the probability calculation only takes into consideration the same order ngram table, whithout taking into consideration the history (sequence).  

For example, suppose that we have the bigram **(in the)** with a frequence of **100**, and the frequency of the unigrams **(in)**  is *1000*, the **Maximun Likelihood (ML) Probability** is:

$$P_{ML}(the {\mid}in) = \frac{c(in,the)}{\sum_{w'_{i}} c(in, w'_{i}) } = \frac{c(in,the)}{ c(in)} = \frac{100}{1000} = 0,1$$

After a research and some test, we consider to implement a better algorithm: Kneser-ney Smoothing Algorithm.

#### 5.2. Kneser-ney Smoothing Algorithm

The Kneser-ney algorithm consider a smoothing interpolation mecanism and include into the calculation the low order ngrams table on that. 

According with the Kneser-ney algorithm, they define three different cases:

#### 5.2.1. Lowest Order Equation

For lowest order **(n=1, unigrams)** the probability is:

$${P_{KN}^{1}(w_{i})} = \frac{N_{1+}({\bullet} w_{i})} { N_{1+}({\bullet} {\bullet})}$$

where:

$$N_{1+}({\bullet} w_{i}) = {\mid}\left\{w_{i-1}:c(w_{i-1}^{i}) >0  \right\} {\mid}$$
$$N_{1+}({\bullet} {\bullet}) = {\mid}\left\{(w_{i-1},w_{i}):c(w_{i-1}^{i}) >0  \right\} {\mid} = \sum_{w_{i}}N_{1+}({\bullet} w_{i})$$

In other words, $N_{1+}({\bullet} w_{i})$ is the number of words that **precede** $w_{i}$ at least once in the corpus.

#### 5.2.2. Second Highest Ngrams to Bigrams Equation

From second highest ngrams **(n-1)** to bigrams **(n=2)**,the following equation is used:

$$P_{KN}^{n}(w_{i}{\mid}w_{i-n+1}^{i}) = 
\frac{max\left\{ N_{1+}(\bullet w_{i-n+1}^{i} )-{\delta}_{n},0\right\} }
{ N_{1+}(\bullet w_{i-n+1}^{i-1}\bullet)} 
+ \frac{{\delta}_{n}} {N_{1+}(\bullet w_{i-n+1}^{i-1}\bullet)} 
N_{1+}(w_{i-n+1}^{i-1}\bullet)P_{KN}^{n-1}(w_{i}{\mid}w_{i-n+2}^{i-1})$$

where:

$$N_{1+}({\bullet} w_{i-n+1}^{i}) = {\mid}\left\{w_{i-n}:c(w_{i-n}^{i}) >0  \right\} {\mid}$$

$$N_{1+}(w_{i-n+1}^{i}{\bullet} ) = {\mid}\left\{w_{i-n+1}:c(w_{i-n+1}^{i+1}) >0  \right\} {\mid}$$

$$N_{1+}({\bullet}w_{i-n+1}^{i-1} {\bullet}) = 
{\mid}\left\{(w_{i-n},w_{i}):c(w_{i-n}^{i}) >0  \right\} {\mid} = \sum_{{w'}_{i}}N_{1+}({\bullet} {w}_{i-n+1}^{i-1},{w'_{i}})$$

$$\delta_{n}= \frac{n_{1}^{n}}{n_{1}^{n} +2n_{2}^{n}}$$

$$n_{1}^{n} = {\mid}\left\{w_{n}:c(w_{1}^{n}) = 1  \right\} {\mid}$$
$$n_{2}^{n} = {\mid}\left\{w_{n}:c(w_{1}^{n}) = 2  \right\} {\mid}$$

In other words, $N_{1+}(w_{i-n+1}^{i}{\bullet})$ is the number of words that **succeed** $w_{i-n+1}^{i}$ at least once in the corpus.

#### 5.2.3. Highest Order Ngrams Equation

And finally for highest order ngrams **(n)** the following one:

$$P_{KN}^{n}(w_{i}{\mid}w_{i-n+1}^{i}) = 
\frac{max\left\{c(w_{i-n+1}^{i}) -{\delta}_{n},0\right\} } 
{ \sum_{w'_{i}} c(w_{i-n+1}^{i-1},{w'_{i}})} + \frac{{\delta}_{n}} {\sum_{w'_{i}} c(w_{i-n+1}^{i-1},{w'_{i}})} 
N_{1+}(w_{i-n+1}^{i-1}\bullet )P_{KN}^{n-1}(w_{i}{\mid}w_{i-n+2}^{i-1})$$

#### 5.2.4. Example

For example, suppose that we want to calculate the Kneser-ney probability of the quadgram **(in the next future)**.

Using **5.2.3 equation for highest order (quadgrams)**, $P_{KN}^{4}(future{\mid}in,the,next)$ is equal to:

$$P_{KN}^{4}(future{\mid}in,the,next) = 
\frac{max\left\{c(in,the,next,future) -{\delta}_{4},0\right\} } 
{ \sum_{w'_{i}} c(in,the,next,{w'_{i}})} + $$
$$\frac{{\delta}_{4}} {\sum_{w'_{i}} c(in,the,next,{w'_{i}})} 
N_{1+}(in,the,next,\bullet )
P_{KN}^{3}(future{\mid}the,next)$$

where:

$$\delta_{4}= \frac{n_{1}^{4}}{n_{1}^{4} +2n_{2}^{4}}$$
$$n_{1}^{4} = {\mid}\left\{w_{4}:c(w_{1}^{4}) = 1  \right\} {\mid}$$
$$n_{2}^{4} = {\mid}\left\{w_{4}:c(w_{1}^{4}) = 2  \right\} {\mid}$$

In order to calculate $P_{KN}^{3}(future{\mid}the,next)$ we use the **5.2.2 equation for trigrams**:  

$$P_{KN}^{3}(future{\mid}the,next) = 
\frac{max\left\{ N_{1+}(\bullet the,next,future)-{\delta}_{3},0\right\} }
{ N_{1+}(\bullet the,next, \bullet)} + $$
$$
\frac{{\delta}_{3}} { N_{1+}(\bullet the,next \bullet)}
N_{1+}(the,next \bullet)P_{KN}^{2}(future{\mid}next)$$

where:

$$\delta_{3}= \frac{n_{1}^{3}}{n_{1}^{3} +2n_{2}^{3}}$$
$$n_{1}^{3} = {\mid}\left\{w_{3}:c(w_{1}^{3}) = 1  \right\} {\mid}$$
$$n_{2}^{3} = {\mid}\left\{w_{3}:c(w_{1}^{3}) = 2  \right\} {\mid}$$

In a similar way, to calculate $P_{KN}^{2}(future{\mid}next)$ we use again the **5.2.2 equation for bigrams**:

$$P_{KN}^{2}(future{\mid}next) = 
\frac{max\left\{ N_{1+}(\bullet next,future)-{\delta}_{2},0\right\} }
{ N_{1+}(\bullet next \bullet)} + $$
$$
\frac{{\delta}_{2}} { N_{1+}(\bullet next \bullet)}
N_{1+}(next,\bullet)P_{KN}^{1}(future)$$

where:

$$\delta_{2}= \frac{n_{1}^{2}}{n_{1}^{2} +2n_{2}^{2}}$$
$$n_{1}^{2} = {\mid}\left\{w_{2}:c(w_{1}^{2}) = 1  \right\} {\mid}$$
$$n_{2}^{2} = {\mid}\left\{w_{2}:c(w_{1}^{2}) = 2  \right\} {\mid}$$

And finally, using **5.2.1 equation for lowest order (unigrams)**, $P_{KN}^{1}(future)$ is equal to:

$$P_{KN}^{1}(future) = \frac{N_{1+}({\bullet} future)} { N_{1+}({\bullet} {\bullet})}$$

In the following sections we will describe the different aspects considered to create the Kneser-ney algoritm.

#### 5.2.1. Recursive Kneser-Ney Implementation

We consider to implement the recursion notation based on the previous section. Because the algoritm was written in a recursive way, at the beggining we decide to implement it in the same way.
The running time of the recursive Kneser-ney implementation was almost imposible to finish, at one moment the Bigrams table will take more than 4 days to finish (based on some debugging values provided when was running), so I believe that was an issue related with the processing capacity, so I decided to explore different ways to improve the performace by parallelize. I try different things:

* Use some libraries (like **parallel**)
* Use a R version version from Microsoft that support multithreading (**R Open**)

But I didn't get any good results. After that and make a lot of research I found and clear statement that explain that R is not a good platform for a recursive algorithm. So I decided to change the approach and create a non-recursive algorithm.

#### 5.2.2. Non-Recursive Implementation

I start working in the non-recursive (iterative) version, getting a reduction on running time but that even will take more than 7 hours to run for bigrams, so I start to check other alternatives and I found one of my biggest issues:

#### 5.2.2.1. Data Frame vs Data Table

At the beggining I create an algorithm using **data.frame** objects to store and make the calculations. After several research I found in several blogs different articles that show the big running time difference between **data.table** and **data.frame**, so I decided to use that.
The change was exponential, the running time was reduced from hours to minutes.

#### 5.2.2.2. Join vs Add the values needed on the Same Table

In Kneser-ney algorithm is neccesary to calculate some special values like **N1+(* w)** and also low order probability calculation that involve several ngrams table. 

For example, to calculate a **trigram probability** we need also to calculate the **low order bigram probability**, that also needs the **lowest order probability value (unigram)**, to do that at the beginning I was taking the values **from different tables at the calculation moment**. A big running time difference appears when the calculation is made with all the values on the same table. I mean, add all the values needed to make the calculation from bigram and unigram using **merge** to the trigram table.

Once again the change was exponential, the running time was reduced from minutes to seconds.

#### 5.2.2.3. RAM Memory Management

It was very important to manage the amount of memory that the algorithm use, removing from the memory the non-needed objects (using the function **rm()**) and calling the garbage collector after the removing process (function **gc()**). 


### 6. How to Predict Next Word

Once we got the ngrams tables with the Kneser-ney calculation for our model (unigrams, bigrams, trigrams and quadgrams), the next step is how to use it to predict next word.

As we said mention on **1.2 Ngrams Model Selection**, this quagram models give to us the ability to predict the next word based on the last three words that should be our best scenario. The main idea is use the last three words and search in the quadgram table that sequence of words and select the ones with higher probability.

Using this approach and how many words we got, the algorithm will be:

* If exists **at least three words**, pick up the **last three**, search that sequence in the **quadgrams table** and then choose the one with **higher probability** (based on Kneser-ney)

* Else If exists **at least two words**, pick up the **last two**, search that sequence in the **trigrams table** and then choose the one with **higher probability** (based on Kneser-ney).

* Else If exists **at least one word**, search that word in the **bigrams table** and then choose the one with **higher probability** (based on Kneser-ney).

* Else (if doesnt exists words) choose in the **unigrams table** the one with **higher probability** (based on MLE).

This works if we found the sequence of words (last three, last two or last one) in the related ngram table (quadgrams, trigrams and bigrams) but, what will happend if we can not find this sequence? this is something possible because we can miss some sequence in our corpora. To handle this we should use a **backoff** strategy.

**Backoff** means that you go back to a n-1 gram level to calculate the probabilities when you encounter a word with prob=0 (doesn't exists). For example we will use a trigram model (that is based only in the last two words) to calculate the probability of a sequence of three words that doesn't exists in the quadgram table.

#### 6.1. A Backoff Strategy

We will use the **Stupid Backoff** scheme, that states that whenever you go back 1 level you multiply the probabilities by **0.4**. The algorithm will be:

* If the **last three words sequence** doesn't exists in the **quadgrams table** let's **backoff to a trigrams model** considering only the **last two words** and a new probability equal to **0.4 * trigrams prob**.

* If the **last two words sequence** doesn't exists in the **trigrams table** let's **backoff to a bigrams model** considering only the **last words** and a new probability equal to **0.4 * 0.4 * bigrams prob**.

* If the **last word** doesn't exists in the **bigrams table** let's **backoff to a unigrams model** with a new probability equal to ** 0.4 * 0.4 * 0.4 * unigrams prob**.

As an example, let's consider that we want to predict the next word based on the sequence **"this is a paper"** using our **quadgram model**, we will pick up the last three words **"is a paper"** and search that sequence in the **trigram table**, suppose that this sequence doesn't exists, so we must **backoff to trigram model**, that means use the sequence **"a paper"** and calculate the probability for each **trigram = 0.4 * current trigram probability**. 

If the sequence **"a paper"** doesn't exists in the bigram table, we must **backoff to bigram model**, considering only **"paper"** and calculate the probability for each **bigram = 0.4 * 0.4 * current bigram probability**. 

Again, if **"paper"** doesn't exists in the bigrams table we must **backoff to unigram model** calculating the probability for each **unigram = 0.4 * 0.4 * 0.4 * current unigram probability**. 

#### 6.2. Prediction based on Incomplete Word

To provide some interesting features to the Shiny App we consider to implement a prediction algorithm that consider the last word in the sequence as a **incomplete word**, that means will be used as a **regex** to search words in the highest ngram table.

As an example, let's consider that the following **trigrams** exists into the **trigrams table**:

* (is a paper)
* (is a paperless)
* (is a paperline)
* (is a paperpop)

Let's consider that we want to predict the next word of the sequence **"this is a paper"** considering the last word as **incomplete** using our **quadgram model**. We will pick up the last three words **"is a paper"** consider the last word **"paper"** as regex to search into the trigrams table.  Our predicting algorithm will provide the previous trigrams as results. 

If the regex search of **"is a paper"** in trigrams table didn't provide any results, we should **backoff to bigram model** so we will search **"a paper"** considering **"paper"** as regex in bigrams table. If we didn't find any match, the final step is **backoff to unigram level** using **"paper"** as a regex to search into unigram table.  

Thinking on the final Shiny App, using this method we can predict next word every time that the user introduce a key an will be very interactive to the end user. 

#### 6.3. Number of Predicted Words

Thinking on the final Shiny App, we consider to implement a prediction algorithm that consider as a parameter the **amount of predicted words**, so we will make the prediction and choose the top-n words according with this. With this idea, we will also use the **backoff** strategy if we didn't find enought words to cover the ones needed. At the end we choose the top-n words based on the related probability value.


#### 6.4. Minimun Probability of Predicted Words

With the same objective that the previous idea, we consider to implement a prediction algorithm that consider as a parameter the **minimun probability of predicted words**, so we will make the prediction and choose the top-n words that **have a probability equal or higher than a provided value**. With this idea, we will also use the **backoff** strategy if we didn't find enought words that comply with the requirements. At the end we choose the top-n words based on the related probability value.


### 7. How to reduce the size of Probability Ngrams Tables?

To improve the running speed and produce a suitable final solution, we consider to reduce the size of probability ngrams tables based on the frequency of each ngram.

To do this we validate in each ngram table (unigrams, bigrams, trigrams and quadgrams) the amount of ngrams that have frequency equal to 1, 2, 3, 4 and 5.

We got interesting results that shows that a big amounts of ngrams have very low frequency (1, 2, 3 or 4) that could be removed because it will not affect the results (because those ngrams are not so frequent, was mainly part of some typing mistakes). We choose to reduce the size of the ngrams tables considering two types:

* Ngrams with frequency higher than 1 (remove singletons)
* Ngrams with frequency higher than 4.

This idea reduce a big the size of each ngram probability table as show in the next code:

### 8. The Shiny App

The Shiny App is very simple, was named **Predictor!** as has:

* A textarea when the end user will introduce the text to be used to predict next word. This text must will be input on lowercase. The application run every time that the user input a key, so is not neccesary to push a button to get results. 

* A panel that have several parameters/options to choose/select, this panel will be described later.

* A label where the top word is selected and show it as a result of the prediction. The text input by the user will be showed in black color and the predicted word (or the portion of that word in case of **incomplete prediction** will be showed in blue color. 

* A result table that provide the information of the top-n words (the result of the prediction algorithm)

* A wordcloud of the top-n words. 

We also include several tabpanels with additional interesting information like:

* The Ngrams Probability Tables

* The **Middle End** report

* A Probability Data Table Frequency Analisys

* The "Final Report" (this document)

#### 8.1. User Panel Parameters

As parameters the user could choose/select the following: 

* **Ngram Probability Table**: the user could choose to use two types of probability tables: **with Freq >= 5** and **with Freq > 1** in order to evaluate if this affect the prediction results. The default is **Freq >= 5**. 

* **Prediction Method**: the user could choose to use two types of prediction method: **Complete Words** and **Incomplete Words**. When **Complete Words** method is selected (default), the application only will show a prediction when the user press the space bar. If the **Incomplete Words** method is selected, after any key input the prediction will be shown. The default is **Incomplete Words**.

* **Probability Range**: the user could choose to minimun probability to consider for the prediction result. The range is between **0** and **1**. The predicted words provided as result must have a probability **higher of equal to value selected**. The default value is **0**.

* **Maximun Number of Words**: the user could choose the amount of words that the prediction algorithm will provide as results. The range is between **1** to **20**. The default value is **5**.



