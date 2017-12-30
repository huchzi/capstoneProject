dictionaryList <- as.character(dictionary) %>% paste("\\b", ., "\\b", sep = "") %>% as.list
dictionaryRegEx <- do.call(paste, c(dictionaryList, sep = "|")) %>%
  paste("(", ., ")", sep = "")

dictionaryList <- as.character(dictionary) %>% 
  #paste("\\<", ., "\\>", sep = "") %>% 
  as.list
dictionaryRegEx <- do.call(paste, c(dictionaryList, sep = "|")) %>%
  paste("\\b(^", ., ")\\b", sep = "")

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
  
  textVector <- dictHash[strsplit(text, split = " ")[[1]]]
  # textVector[!(textVector %in% dictionary)] <- "UNKNOWN"
  do.call("paste", as.list(names(textVector)))
  
}

# lapply(findMostFreqTerms(dtm1, 95000), sum)
# rowSums(as.matrix(dtm1))