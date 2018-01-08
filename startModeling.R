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

Sys.time()
load("dtm5.rda")
fiveGrams <- colSums(as.matrix(dtm5))
rm(dtm5)
gc()

fiveGrams <- fiveGrams[fiveGrams > 1]
ngrams5 <- data.table(ngram = names(fiveGrams),
                      freq = fiveGrams,
                      n = 5)
ngrams5 <- ngrams5[, .(key = gsub(" [[:alpha:]]+$", "", names(fiveGrams)),
                       prediction = gsub("([[:alpha:]]+ ){4}", "", names(fiveGrams)),
                       freq,
                       n)]
rm(fiveGrams)
gc()

Sys.time()
load("dtm4.rda")
fourGrams <- colSums(as.matrix(dtm4))
rm(dtm4)
gc()

fourGrams <- fourGrams[fourGrams > 1]
ngrams4 <- data.table(ngram = names(fourGrams),
                      freq = fourGrams,
                      n = 4)
ngrams4 <- ngrams4[, .(key = gsub(" [[:alpha:]]+$", "", names(fourGrams)),
                       prediction = gsub("([[:alpha:]]+ ){3}", "", names(fourGrams)),
                       freq,
                       n)]
rm(fourGrams)
gc()

Sys.time()
load("dtm3.rda")
threeGrams <- colSums(as.matrix(dtm3))
rm(dtm3)
gc()

threeGrams <- threeGrams[threeGrams > 1]
ngrams3 <- data.table(ngram = names(threeGrams),
                      freq = threeGrams,
                      n = 3)
ngrams3 <-
  ngrams3[, .(
    key = gsub(" [[:alpha:]]+$", "", names(threeGrams)),
    prediction = gsub("([[:alpha:]]+ ){2}", "", names(threeGrams)),
    freq,
    n
  )]
rm(threeGrams)
gc()

Sys.time()
load("dtm2.rda")
twoGrams <- colSums(as.matrix(dtm2))
rm(dtm2)
gc()

twoGrams <- twoGrams[twoGrams > 1]
ngrams2 <- data.table(ngram = names(twoGrams),
                      freq = twoGrams,
                      n = 2)
ngrams2 <-
  ngrams2[, .(
    key = gsub(" [[:alpha:]]+$", "", names(twoGrams)),
    prediction = gsub("([[:alpha:]]+ ){1}", "", names(twoGrams)),
    freq,
    n
  )]
rm(twoGrams)
gc()

ngrams <- rbind(ngrams5, ngrams4, ngrams3)
rm(ngrams5, ngrams4, ngrams3, ngrams2)
gc()

ngrams1 <- data.frame(key = c(""), prediction = names(dictionary), freq = dictionary, n = 1) %>% data.table()
ngrams <- rbind(ngrams, ngrams1)
rm(ngrams1)

setorder(ngrams, -n, -freq)
setkey(ngrams, key)


predictions <-
  quiz2 %>%
  preprocessText() %>%
  tokenize_ngrams(n = 4, n_min = 2) %>%
  lapply(function(x) ngrams[x])

predictions[[1]][prediction %in% c("die", "give", "eat", "sleep"), sum(freq), by = .(n, prediction)]
predictions[[2]][prediction %in% c("spiritual", "financial", "horticultural", "marital"), sum(freq), by = .(n, prediction)]
predictions[[3]][prediction %in% c("month", "morning", "weekend", "decade"), sum(freq), by = .(n, prediction)]
predictions[[4]][prediction %in% c("hunger", "sleepiness", "happiness", "stress"), sum(freq), by = .(n, prediction)]
predictions[[5]][prediction %in% c("picture", "walk", "look", "minute"), sum(freq), by = .(n, prediction)]
predictions[[6]][prediction %in% c("matter", "account", "incident", "case"), sum(freq), by = .(n, prediction)]
predictions[[7]][prediction %in% c("toe", "arm", "hand", "finger"), sum(freq), by = .(n, prediction)]
predictions[[8]][prediction %in% c("center", "middle", "top", "side"), sum(freq), by = .(n, prediction)]
predictions[[9]][prediction %in% c("weekly", "daily", "inside", "outside"), sum(freq), by = .(n, prediction)]
predictions[[10]][prediction %in% c("novels", "pictures", "movies", "stories"), sum(freq), by = .(n, prediction)]

