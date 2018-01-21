predict1.ngramModel <- function(model, text) {
  
  regEx <- paste("([[:alpha:]]+ ){0,", model$useWords - 1, "}([[:alpha:]]+$)", sep = "")
  text <- regmatches(x = text, m = gregexpr(regEx, text))[[1]]
  
  predictTable <-
    tokenize_ngrams(text, n = model$n_max, n_min = model$n_min) %>%
    ngrams[.]
  
  predictTable <- predictTable[model$weights, on = "n", nomatch = 0]
  predictTable <- predictTable[, .(cFreq = sum(freq * w)), by = prediction]
  setorder(predictTable, -cFreq)
  
  predictTable
  
}

predict2.ngramModel <- function(model, text) {
  
  number_of_ngrams <- 1 + model$n_max - model$n_min
  ngramList <- tokenize_ngrams(text, n = model$n_max, n_min = model$n_min)[[1]]
  index <- length(ngramList) - c(0, 1, 3, 6, 10)[seq(number_of_ngrams)]
  ngramList <- ngramList[index[index > 0]]
  
  predictTable <-
    ngrams[ngramList]
  
  nmax <- max(predictTable[, "n"], na.rm = T)
  predictTable <- predictTable[n == nmax]
  setorder(predictTable, -freq)
  
  predictTable
  
}

predict3.ngramModel <- function(model, text) {
  
  text <- regmatches(x = text, m = gregexpr("([[:alpha:]]+ ){0,3}([[:alpha:]]+$)", text))[[1]]
  
  number_of_ngrams <- 1 + model$n_max - model$n_min
  ngramList <- tokenize_ngrams(text, n = model$n_max, n_min = model$n_min)[[1]]
  index <- length(ngramList) - c(0, 1, 3, 6, 10)[seq(number_of_ngrams)]
  ngramList <- ngramList[index[index > 0]]
  
  predictTable <-
    ngrams[ngramList]
  
  for (i in 2:5)
      predictTable[n == i, w := model$weights[n == i, w]]

  predictTable[, wFreq := (freq * w)]
  predictTable <- predictTable[, .(cFreq = sum(wFreq)), by = prediction][order(-cFreq)]
  
  predictTable[!is.na(prediction), ]
  
}

predict.ngramModel <- function(model, text) {
  
  text <- strsplit(text, split = " ")[[1]]
  l <- length(text)
  ngramList <- character(l)
  
  for (i in seq(l - 4, l)) ngramList[i] <- paste(text[i:l], collapse = " ")

  predictTable <-
    ngram_model$ngrams[ngramList]
  
  predictTable <- predictTable[, .(cFreq = sum(wFreq)), by = prediction][order(-cFreq)]
  
  na.omit(predictTable[cFreq > 0, ])
  
}

ngram_model$ngrams <- ngrams
for (i in 2:5)
  ngram_model$ngrams[n == i, w := ngram_model$weights[n == i, w]]
ngram_model$ngrams[, wFreq := (freq * w)]

textPred <- "mister tear down that"
system.time(replicate(10, predict1.ngramModel(ngram_model, textPred)))
system.time(replicate(10, predict2.ngramModel(ngram_model, textPred)))
system.time(replicate(10, predict3.ngramModel(ngram_model, textPred)))
system.time(replicate(10, predict4.ngramModel(ngram_model, textPred)))

