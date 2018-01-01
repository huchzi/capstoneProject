preprocessText <- function(text) {
  text %>%
    replace_abbreviation() %>%
    replace_contraction() %>% 
    tolower() %>%
    replace_ordinal() %>%
    removeWords(words = profanity) %>%
    removeFunnyCharacters() %>%
    stripWhitespace()
    # keepDictionaryWords()
}

getLastNGram <- function(text, n) {
  tokenList <- tokenize_skip_ngrams(text, n = n, k = 2)
  
  lapply(tokenList, function(x) x[length(x)])
  
}

fiveGramHash <- hashmap(getLastNGram(names(fiveGrams), 4), fiveGrams)

firstFour <- function(fg) paste(tokenize_words(fg)[[1]][1:4], collapse = " ")

library(data.table)
fg <- data.table(ngram = names(fiveGrams), 
                 freq = fiveGrams)
fg <- fg[, .(key = gsub(" [[:alpha:]]+$", "", names(fiveGrams)),
             prediction = gsub("([[:alpha:]]+ ){4}", "", names(fiveGrams)),
             freq)]
setorder(fg, -freq)
setkey(fg, key)

getLastFour <- function(text) {
  rm <- gregexpr("([[:alpha:]]+ ){3}([[:alpha:]]+$)", text) 
  regmatches(x = text, m = rm)
}

quiz2 %>%
  preprocessText() %>%
  getLastFour() %>%
  lapply(function(x) fg[x])
