PredictText - the future of text prediction is here...
========================================================
author: 
date: 
autosize: true

Why should I use it?
========================================================

There are several reasons, why you should use PredictText to predict text.

- it's a great app for predicting text
- using text prediction may actually save your marriage (89% of people find it annoying or very annoying when their spouse spends too much time in front of the computer)
- it spent a lot of time in front of the computer in order to develop it and my wife found that very annoying

Why is it so fast?
========================================================

In the process of cleaning data, exploratory analysis and development of a model, I learned a lot about optimizing R code. I found that...

- the data.table is quite fast, especially if you use the setkey function

```r
summary(cars)
```

```
     speed           dist       
 Min.   : 4.0   Min.   :  2.00  
 1st Qu.:12.0   1st Qu.: 26.00  
 Median :15.0   Median : 36.00  
 Mean   :15.4   Mean   : 42.98  
 3rd Qu.:19.0   3rd Qu.: 56.00  
 Max.   :25.0   Max.   :120.00  
```

- the hashmap function is extremely efficient when you replace all words that are not in a dictionary

Why did I make the choices that I made?
========================================================

- I did not eliminate stopwords. Although they are useless for text classification, I thought that a tool for text prediction should also predict stopwords. 
- I restricted the words in the training corpus to a dictionary of the 35000 most common words. This helped to save memory and to reduce time.
- I excluded ngrams that occured only once. Again, this helped to reduce memory. However, I would consider including them for longer ngrams (n = 5), because a majority of ngrams occur only once,
- I did not caluclate exact probabilities nor perplexities, because these values did not seem relevant for text prediction. Only the ranks of the ngrams matter.
- I used a model where predictions from all ngrams are included in a list and are included according to weighting factors. However, I choose these to be so large that I ended up using a backoff model in the end.

How could the model and the app be improved in the future?
========================================================

- I would consider skip-n-grams in order to get more frequent long n-grams
- I'm thinking about eliminating stopwords

How do I use the app?
========================================================

- I would consider skip-n-grams in order to get more frequent long n-grams
- I'm thinking about eliminating stopwords