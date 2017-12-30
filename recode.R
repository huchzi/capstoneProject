# Recode text

sentences <- training[[1]]$content[1:200] %>% 
  removeAbbreviations %>%
  divideSentences %>%
  removePunctuation %>%
  tolower() %>%
  dealWithNumbers %>%
  replaceProfanity

tfm <- termFreq(sentences)

tokenList <- pblapply(sentences, function(x) tokenize_ngrams(x, n = 1) %>% unlist)

factorList <- pblapply(tokenList, function(x) factor(x, levels = names(tfm)) %>% as.integer)

dictionary <- c("this", "is", "it")
dictHash <- setNames(seq(dictionary), dictionary)
names(dictHash) <- dictionary
sentence <- c("this is not it")

test <- dictHash[strsplit("is this or is this not it", split = " ")[[1]]]
names(dictHash[test])
lapply(list(1:3, 2:4, 3:5, 4:6, 5:7), function(x) dictHash[test[x]])


strsplit(sentence, split = " ")


createHash <- function(sentenceList) {
  pblapply(sentenceList,
           function(x) {
             rValue <- dictHash[strsplit(x, split = " ")[[1]]]
             names(rValue) <- NULL
             rValue
           }
  )
}