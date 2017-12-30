library(data.table)
library(dplyr)

load("dtm5.rda")
freq5 <- as.matrix(dtm5) %>% colSums()
rm(dtm5)

freq5 <- data.frame(ngram = names(freq5), freq = freq5) %>% data.table

preProcess(validation)

freq1 <- as.matrix(dtm1) %>% colSums()
freq2 <- as.matrix(dtm2) %>% colSums()
freq5 <- as.matrix(dtm5) %>% colSums()
freq2 <- log(freq2 / sum(freq2))

freq <- list()

freq[1] <- as.matrix(dtm1) %>% colSums()
freq[5] <- as.matrix(dtm5) %>% colSums()

testSentences <- validation[[1]]$content[1:10]

testSentences <-
  testSentences %>%
  replace_abbreviation %>%
  replace_contraction %>%
  divideSentences %>%
  tolower %>%
  replace_ordinal %>%
  replace_number %>%
  replaceProfanity %>%
  removeFunnyCharacters %>%
  removePunctuation %>%
  stripWhitespace

perplexity2 <- function(text) {
  
  first_ngram <- function(ngram) tokenize_ngrams(ngram, n = 1)[[1]][1]
  secnd_ngram <- function(ngram) tokenize_ngrams(ngram, n = 1)[[1]][2]
  first_ngram <- Vectorize(first_ngram)
  
  tokens <- text %>% tokenizer2 %>%
    data.frame(token = ., freq = freq2[.])
  
  tokens %>% 
    filter(is.na(freq)) %>%
    mutate(freqUG1 = first_ngram(token)) %>%
    print
  
  rValue <- tokens %>% summarize(perplexity = exp(sum(freq, na.rm = T) / -n()))
                                  
  return(rValue$perplexity)
}

test <- pblapply(testSentences[1], perplexity2)
