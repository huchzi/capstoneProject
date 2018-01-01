
test <- testing[[1]]$content[1:100]

keepDictionaryWords <- function(text) {

  FUN <- function(text)
  {
    textVector <- HH[[text]]
    textVector <- paste(textVector[!is.na(textVector)], collapse = " ")
  }
  
  text <- tokenize_words(text)
  lapply(text, FUN) %>% c(recursive = T)

}

divideSentences <- function(text) {
  
  tokenize_sentences(paste(text, collapse = " ")) %>%
    c(recursive = T)
  
}

gc()
Rprof("keepDict")
replicate(n = 20, test2 <- keepDictionaryWords(test))
Rprof(NULL)

summaryRprof("keepDict")

gc()
Rprof("divideSents")
replicate(n = 100, divideSentences(test))
Rprof(NULL)

summaryRprof("divideSents")