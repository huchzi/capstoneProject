library(tm)
library(ngram)
library(dplyr)
library(pbapply)
library(tokenizers)
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(rJava)
library(qdap)
library(hashmap)
library(data.table)

Sys.time()

# download.file("http://www.bannedwordlist.com/lists/swearWords.txt", "profanity.txt")
profanity <- read.csv("profanity.txt", header = F, stringsAsFactors = F)
profanity <- c(profanity, recursive = T)

SRC_training <- DirSource("Corpus/Training/")
SRC_validation <- DirSource("Corpus/Validation/")
SRC_testing <- DirSource("Corpus/Testing/")

training <- PCorpus(SRC_training, dbControl = list(dbName = "training.db",
                                        dbType = "DB1"))
validation <- PCorpus(SRC_validation, dbControl = list(dbName = "validation.db",
                                          dbType = "DB1"))
testing <- PCorpus(SRC_testing, dbControl = list(dbName = "testing.db",
                                          dbType = "DB1"))


divideSentences <- function(text) {
  tokenize_sentences(paste(text, collapse = " ")) %>%
    c(recursive = T)
}

removeFunnyCharacters <- function(text) {
  gsub("[^[:alpha:] ]", "", text)
}

keepDictionaryWords <- function(text) {
  FUN <- function(text)
  {
    textVector <- dictionaryHash[[text]]
    textVector <- paste(textVector[!is.na(textVector)], collapse = " ")
  }
  text <- tokenize_words(text)
  lapply(text, FUN) %>% c(recursive = T)
}

preProcess <- function(corpus)  {
  print("Step 1/7: Remove abbreviations")
  Sys.time()
  tm_map(corpus, content_transformer(replace_abbreviation))
  tm_map(corpus, content_transformer(replace_contraction))
  
  print("Step 2/7: Divide sentences")
  Sys.time()
  tm_map(corpus, content_transformer(divideSentences))
  
  print("Step 3/7: Set to lowercase")
  Sys.time()
  tm_map(corpus, content_transformer(tolower))
  
  print("Step 4/7: Deal with numbers")
  Sys.time()
  tm_map(corpus, content_transformer(replace_ordinal))
  
  print("Step 5/7: Replace profanity")
  Sys.time()
  tm_map(corpus, removeWords, words = profanity)
  
  print("Step 6/7: Remove funny characters (and punctuation)")
  Sys.time()
  tm_map(corpus, content_transformer(removeFunnyCharacters))
  
  print("Step 7/7: Strip whitespace")
  Sys.time()
  tm_map(corpus, stripWhitespace)
}

print("start preprocessing")
Sys.time()

preProcessTime <- system.time(preProcess(training))

getTokenizer <- function(n) {
  function(text) {
    if ("TextDocument" %in% class(text)) text <- text$content
    pblapply(text, function(x) tokenize_ngrams(x, n = n)) %>%
      c(recursive = T)
  }
}

tokenizer1 <- getTokenizer(1)
tokenizer2 <- getTokenizer(2)
tokenizer3 <- getTokenizer(3)
tokenizer4 <- getTokenizer(4)
tokenizer5 <- getTokenizer(5)
tokenizer6 <- getTokenizer(6)

print("extractDictionary")
Sys.time()

dtm1 <- 
  DocumentTermMatrix(training, 
                     control = list(bounds = list(local = c(2, Inf)),
                                    wordLengths = c(1, 25),
                                    tokenize = tokenizer1))
save(dtm1, file = "dtm1.rda")

gc()

dictionary <- colSums(as.matrix(dtm1))
dictionary <- dictionary[order(dictionary, decreasing = T)]
dictionary <- dictionary[1:35000]
dictionaryHash <- hashmap(names(dictionary), names(dictionary))

save(dictionary, file = "dictionary.rda")

print("removeNonDictionary")
Sys.time()

tm_map(training, content_transformer(keepDictionaryWords))

print("extractNGrams")
Sys.time()

dtm2 <- DocumentTermMatrix(training, 
                           control = list(tokenize = tokenizer2))

save(dtm2, file = "dtm2.rda")
remove(dtm2)
gc()

dtm3 <- DocumentTermMatrix(training, control = list(tokenize = tokenizer3))

save(dtm3, file = "dtm3.rda")
remove(dtm3)
gc()

dtm4 <- DocumentTermMatrix(training, control = list(tokenize = tokenizer4))

save(dtm4, file = "dtm4.rda")
remove(dtm4)
gc()

dtm5 <- DocumentTermMatrix(training, control = list(tokenize = tokenizer5))

save(dtm5, file = "dtm5.rda")

print("done")
Sys.time()

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