# Evaluate models

SRC_validation <- DirSource("Corpus/Validation/")
validation <- PCorpus(SRC_validation, dbControl = list(dbName = "validation.db",
                                                       dbType = "DB1"))

preProcessTime <- system.time(preProcess(validation)) # took 22 minutes

# somehow there were problems when the variable was reloaded from a saved workspace
#  Fehler in x$`[[`(i) : external pointer is not valid 
dictionaryHash <- hashmap(names(dictionary), names(dictionary)) # somehow there were problems

removeNonDictWordsTime <- system.time(
  tm_map(validation, content_transformer(keepDictionaryWords))
) # took 92 sec

ngram_model <- list(
  wheights = data.frame(n = c(1:5), 
                        w = c(1, 
                              100^1, 
                              100^2, 
                              100^3, 
                              100^4)) %>% 
    data.table(),
  n_max = 4L,
  n_min = 1L,
  ngrams = 2:5
)


# validate with or without removing non-dict words?
precisionNL <- function(text, n = 1, verbose = F) {
  
  weightsNGrams <- data.frame(n = c(1:5), w = c(1, 100^1, 100^2, 100^3, 100^4)) %>% data.table()
  
  text <- preprocessText(text)
  pMatch <- gregexpr("([[:alpha:]]+ ){1,6}([[:alpha:]]+$)", text)
  text <- regmatches(x = text, m = pMatch)[[1]]
  if(verbose) print(text)
  pMatch <- gregexpr("([[:alpha:]]+$)", text)
  toPredict <- regmatches(x = text, m = pMatch)[[1]]
  
  text <- gsub(" [[:alpha:]]+$", "", text)

  predictTable <-
    tokenize_ngrams(text, n = 4, n_min = 1) %>%
    ngrams[.]

  predictTable <- predictTable[weightsNGrams, on = "n", nomatch = 0]
  predictTable <- predictTable[, .(key, prediction, wFreq = freq * w)]
  predictTable <- predictTable[, .(cFreq = sum(wFreq)), by = prediction]
  setorder(predictTable, -cFreq)
  
  if (verbose) print(predictTable)
  
  if(is.na(predictTable[1, prediction])) return(0) else
    if(toPredict %in% predictTable[1:n, prediction] ) return(1) else
      return(0)
}

rankNL <- function(text, verbose = F) {
  
  weightsNGrams <- data.frame(n = c(1:5), w = c(1, 100^1, 100^2, 100^3, 100^4)) %>% data.table()
  
  text <- preprocessText(text)
  pMatch <- gregexpr("([[:alpha:]]+ ){1,6}([[:alpha:]]+$)", text)
  text <- regmatches(x = text, m = pMatch)[[1]]
  if(verbose) print(text)
  pMatch <- gregexpr("([[:alpha:]]+$)", text)
  toPredict <- regmatches(x = text, m = pMatch)[[1]]
  
  text <- gsub(" [[:alpha:]]+$", "", text)
  
  predictTable <-
    tokenize_ngrams(text, n = 4, n_min = 1) %>%
    ngrams[.]
  
  predictTable <- predictTable[weightsNGrams, on = "n", nomatch = 0]
  predictTable <- predictTable[, .(key, prediction, wFreq = freq * w)]
  predictTable <- predictTable[, .(cFreq = sum(wFreq)), by = prediction]
  setorder(predictTable, -cFreq)
  
  if (verbose) print(predictTable)
  
  setkey(predictTable, prediction)
  rValue <- predictTable[toPredict, which = T]
  
  if (verbose) print(predictTable[rValue, ])
  
  return(rValue)
}

validationText <- tokenize_ngrams(validation[[1]]$content[1:500], n = 10) %>%
  lapply(function(x) x[length(x)]) %>%
  c(recursive = T)
  
sapply(validationText[1:10], precisionNL, n = 1) %>%
  mean(.)

sapply(validationText[1:10], precisionNL, n = 3) %>%
  mean(.)

sapply(validationText[1:10], rankNL) %>%
  mean(.)
  
print(validationText[1])
test <- tokenize_ngrams(validationText[1], n = 4, n_min = 2) %>% ngrams[.]
test2 <- test[weightsNGrams, on = "n", nomatch = 0]
print(test2)
test2[, .(key, prediction, wFreq = freq * w)]


  
# main aspects of the model
# which ngrams to use, how to wheigth them, how many words to use


