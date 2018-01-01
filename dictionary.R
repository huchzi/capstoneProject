dictionaryList <- as.character(dictionary) %>% paste("\\b", ., "\\b", sep = "") %>% as.list
dictionaryRegEx <- do.call(paste, c(dictionaryList, sep = "|")) %>%
  paste("(", ., ")", sep = "")

dictionary <- findMostFreqTerms(dtm1.keep, 950)$en_US.news_training.txt
dictionary <- setNames(names(dictionary), names(dictionary))
dictionaryList <- as.character(dictionary) %>% 
  #paste("\\<", ., "\\>", sep = "") %>% 
  as.list
dictionaryRegEx <- do.call(paste, c(dictionaryList, sep = "|")) %>%
  paste("\\b(^", ., ")\\b", sep = "")
regmatches("test: this is a test", gregexpr(dictionaryRegEx, "test: this is a test"))

keepDictionaryWords <- function(text) {
  
  FUN <- function(text)
  {
    textVector <- tokenize_words(text)[[1]]
    textVector <- dictionary[textVector]
    textVector <- paste(textVector[!is.na(textVector)], collapse = " ")
  }
  
  pblapply(text, FUN)
}

replaceNonDictionaryWords <- function(text) {
  
  textVector <- strsplit(text, split = " ")[[1]]
  textVector[!(textVector %in% dictionary)] <- "UNKNOWN"
  do.call("paste", as.list(textVector))
  
}

test <- "this is a test"
for (i in seq(dictionary)) test <- gsub(dictionary[i], "", test)

replaceNonDictionaryWords <- function(text) {
  textList <- tokenize_ngrams(text, n = 1)
  fun <- function(textVector){
    textVector[!(textVector %in% dictionary)] <- "UNKNOWN"
    do.call("paste", as.list(textVector))}
  pblapply(textList, function(x) fun(x)) %>% c(recursive = T)
}


dict <- paste("\\b", dictionary, "\\b", sep = "")

replaceNonDictionaryWords <- function(text) {
  
  textCopy <- text
  
  for (i in seq(dict)) text <- gsub(dict[i], "", text)
  
  newRegEx <- text %>% 
    stripWhitespace() %>% 
    gsub("^( )+", "", .) %>%
    gsub("( )+$", "", .) %>%
    # paste("|", ., sep = "") %>%
    as.list() %>%
    do.call("paste", .) %>%
    gsub(" ", "|", .) %>%
    #gsub("\\(|", "(", .) %>%
    #gsub("|\\)", ")", .) %>%
    gsub("||", "", .) %>%
    paste("\\b(", ., ")\\b", sep = "")
  
  gsub(newRegEx, "UNKNOWN", textCopy)
}


replaceNonDictionaryWords <- function(text) {
  
  FUN <- function(txt)
  {  
    textVector <- dictHash[strsplit(text, split = " ")[[1]]]
    # textVector[!(textVector %in% dictionary)] <- "UNKNOWN"
    do.call("paste", as.list(names(textVector)))
  }
  
  pblapply(text, FUN)
  
}

# lapply(findMostFreqTerms(dtm1, 95000), sum)
# rowSums(as.matrix(dtm1))

trainingProcSource <- DirSource("processedCorpus/")

trainingProc <- PCorpus(trainingProcSource, dbControl = list(dbName = "trainingProc.db",
                                                   dbType = "DB1"))

dictionary <- findMostFreqTerms(dtm1.keep, 2000)$en_US.news_training.txt
dictionary <- setNames(names(dictionary), names(dictionary))

tm_map(trainingProc, content_transformer(keepDictionaryWords))

dtm2 <- DocumentTermMatrix(trainingProc, control = list(tokenize = tokenizer2,
                                                    tolower = F,
                                                    wordLengths = c(1, Inf)
))

save(dtm2, file = "dtm2.rda")
remove(dtm2)
gc()

dtm3 <- DocumentTermMatrix(trainingProc, control = list(tokenize = tokenizer3,
                                                    tolower = F,
                                                    wordLengths = c(1, Inf)))

save(dtm3, file = "dtm3.rda")
remove(dtm3)
gc()

dtm4 <- DocumentTermMatrix(trainingProc, control = list(tokenize = tokenizer4,
                                                    tolower = F,
                                                    wordLengths = c(1, Inf)))

save(dtm4, file = "dtm4.rda")
remove(dtm4)
gc()

systimeDTM5 <- system.time(dtm5 <- DocumentTermMatrix(trainingProc, control = list(tokenize = tokenizer5,
                                                    tolower = F,
                                                    wordLengths = c(1, Inf))))

save(dtm5, file = "dtm5.rda")
