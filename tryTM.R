library(tm)
library(ngram)
library(dplyr)
library(pbapply)
library(tokenizers)

dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(rJava)
library(qdap)

# download.file("http://www.bannedwordlist.com/lists/swearWords.txt", "profanity.txt")
# profanity <- read.csv("profanity.txt", header = F, stringsAsFactors = F)
# profanity <- c(profanity, recursive = T)
# pasteOR <- function(x) paste(x, sep = "|")
# profanityList <- as.character(profanity) %>% 
#   paste("\\<", ., "\\>", sep = "") %>% 
#   as.list
# profanityRegEx <- do.call(paste, c(profanityList, sep = "|"))
# rm(profanity, profanityList)

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
  
  ## is a little bit better; too slow for me
  # alternative_function <- function (x) {
  #   x <- as.String(x)
  #   x[annotate(x, Maxent_Sent_Token_Annotator())]
  # }
  
  pblapply(text, function(x) tokenize_sentences(x)) %>%
    c(recursive = T) %>%
    return()
}

# removeSentenceFragments <- function(text) {
#   
#   fun <- function(x) {
#     n <- 0
#     try(n <- ngram(x, n = 3) %>% get.ngrams() %>% length(), silent = T)
#     if (n > 2) return(x) else return(NULL)
#   }
#   
#   pblapply(text, fun) %>% c(recursive = T)
#   
# }

# removeAbbreviations <- function(text) {
#   
#   gsub("(?<=Dr|Mr|Mrs|St)\\.", "", text, perl = T)
#   
# } 

removeFunnyCharacters <- function(text) {
  
  gsub("[^[:alnum:] ]", "", text)
  
}

# removeParenthesis <- function(text) {
#   
#   gsub("\\(.*\\)", "", text)
#   
# }

# dealWithNumbers <- function(text) {
#   
#   gsub("[1-9]{2,}", "NUMBER", text) %>%
#     gsub("0", "zero", .) %>%
#     gsub("1", "one", .) %>%
#     gsub("2", "two", .) %>%
#     gsub("3", "three", .) %>%
#     gsub("4", "four", .) %>%
#     gsub("5", "five", .) %>%
#     gsub("6", "six", .) %>%
#     gsub("7", "seven", .) %>%
#     gsub("8", "eight", .) %>%
#     gsub("9", "nine", .)
#   
# }

# replaceProfanity <- function(text) {
#   
#   gsub(profanityRegEx, "PROFANITY", text)
#   
# }

preProcess <- function(corpus)  {
  print("Step 1/7: Remove abbreviations")
  tm_map(corpus, content_transformer(replace_abbreviation))
  tm_map(corpus, content_transformer(replace_contraction))
  
  print("Step 2/7: Divide sentences")
  tm_map(corpus, content_transformer(divideSentences))
  
  print("Step 3/7: Set to lowercase")
  tm_map(corpus, content_transformer(tolower))
  
  print("Step 4/7: Deal with numbers")
  # tm_map(corpus, content_transformer(dealWithNumbers))
  tm_map(corpus, content_transformer(replace_ordinal))
  tm_map(corpus, content_transformer(replace_number))
  
  print("Step 5/7: Replace profanity")
  #tm_map(corpus, content_transformer(replaceProfanity))
  tm_map(corpus, removeWords, word = profanity)
  
  print("Step 6/7: Remove funny characters (and punctuation)")
  tm_map(training, removePunctuation)
  # tm_map(corpus, content_transformer(removeFunnyCharacters))
  
  print("Step 7/7: Strip whitespace")
  tm_map(corpus, stripWhitespace)
}

system.time(preProcess(training))

#system.time(tm_map(corpus, content_transformer(replaceNonDictionaryWords)))

# getTokenizer <- function(n) {
#   function(text) {
#     if ("TextDocument" %in% class(text)) text <- text$content
#     pblapply(text, function(x) ngram_asweka(x, min = n, max = n)) %>%
#       c(recursive = T)
#   }
# }

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

dtm1 <- 
  DocumentTermMatrix(training, 
                     control = list(tokenize = tokenizer1,
                                    tolower = F,
                                    wordLengths = c(1, Inf),
                                    bounds = list(local = c(c, Inf))))

save(dtm1, file = "dtm1.rda")
remove(dtm1)
gc()

dictionary <- findMostFreqTerms(dtm1, 95000)

dtm2 <- DocumentTermMatrix(training, control = list(tokenize = tokenizer2,
                                                    tolower = F,
                                                    wordLengths = c(1, Inf)))

save(dtm2, file = "dtm2.rda")
remove(dtm2)
gc()

dtm3 <- DocumentTermMatrix(training, control = list(tokenize = tokenizer3,
                                                    tolower = F,
                                                    wordLengths = c(1, Inf)))

save(dtm3, file = "dtm3.rda")
remove(dtm3)
gc()

dtm4 <- DocumentTermMatrix(training, control = list(tokenize = tokenizer4,
                                                    tolower = F,
                                                    wordLengths = c(1, Inf)))

save(dtm4, file = "dtm4.rda")
remove(dtm4)
gc()

dtm5 <- DocumentTermMatrix(training, control = list(tokenize = tokenizer5,
                                                    tolower = F,
                                                    wordLengths = c(1, Inf)))

save(dtm5, file = "dtm5.rda")
 
# dictionary <- names(findMostFreqTerms(dtm1, 95000)$en_US.news_training.txt)
# 
# 
# dtm5 <- DocumentTermMatrix(training, control = list(tokenize = tokenizer5,
#                                                     tolower = F,
#                                                     wordLengths = c(1, Inf)))
# 
