PredictText - the future of text prediction is (almost) here...
========================================================
author: CH
date: 01/21/2018
autosize: true

Main features 
========================================================

- based on a dictionary of 35000 words
- unknown words are removed without replacement
- contains n-grams with more than one occurence in the training corpus
  - more than 1.4 million 5-grams
  - more than 3.0 million 4-grams
  
The last four words of the sentence are taken for prediction - these are split into 1-grams, 2-grams, ..., 4-grams. All of these n-grams are used for prediction, even the more distant 2-grams. Weighting factors are used to strengthen the influence of longer n-grams.


How does it predict text?
========================================================



The data.table package can be used to create at table of predictions. It is quite fast when the n-grams used for prediction are set as keys with the setkey-function.


```
                   key prediction freq n
1: a big brazilian fan          i    2 5
2:      a big bro that          i    2 5
3:         a big but i       have    2 5
4: a big challenge but          i    2 5
```


```r
ngrams["tear down this"]
```

```
              key prediction freq n
1: tear down this       wall    5 4
```


Quality of prediction
========================================================



The exact next word was predicted in 18%. The distribution of ranks that the acutal next words had in the prediction table can be seen in the plot below.

![plot of chunk unnamed-chunk-5](presentation-figure/unnamed-chunk-5-1.png)

One idea that improved efficiency
========================================================




The hashmap-package can be used to remove words that are not in a dictionary in a preprocessed text. Techniques based on regular expressions have problems with the large number of words.


```r
dictionaryHash <- hashmap(names(dictionary), names(dictionary))
text <- tokenize_words("do you know vlad vukovic")[[1]]
textVector <- dictionaryHash[[text]]
paste(textVector[!is.na(textVector)], collapse = " ")
```

```
[1] "do you know"
```

How do I use the app?
========================================================

- Enter a word or a sentence into the input box on the left side

- Use one of the add buttons to add this text to the sentence on the right.

- The lower add button adds the word from the dictionary that is closest to the word in the input box.

- Once text has been added to the sentence on the right, the five most likely words to follow are shown on the left and can be added.

- Use the buttons ".", "?", or "!" on the right to add the sentence to a paragraph.
