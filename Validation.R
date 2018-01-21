# Evaluate models

SRC_validation <- DirSource("Corpus/Validation/")
validation <- PCorpus(SRC_validation, dbControl = list(dbName = "validation.db",
                                                       dbType = "DB1"))

preProcess(validation)

# somehow there were problems when the variable was reloaded from a saved workspace
#  Fehler in x$`[[`(i) : external pointer is not valid 
dictionaryHash <- hashmap(names(dictionary), names(dictionary))

removeNonDictWordsTime <- system.time(
  tm_map(validation, content_transformer(keepDictionaryWords))
) # took 92 sec



separateLastWord <- function(text, verbose = F) {
  
  pMatch <- gregexpr("([[:alpha:]]+$)", text)
  toPredict <- regmatches(x = text, m = pMatch)[[1]]
  if(verbose) print(pMatch)
  
  text <- gsub(" [[:alpha:]]+$", "", text)
  if(verbose) print(text)
  
  return(list(text = text, toPredict = toPredict))
  
}

validateModel <- function(model, text, verbose = F) {
  
  sepText <- separateLastWord(text)
  
  predictTable <- predict(model, sepText$text)
  if (verbose) print(predictTable)
  
  list(precision = precisionNL(predictTable, sepText$toPredict, verbose),
       precision10 = precisionNL(predictTable, sepText$toPredict, topN = 10, verbose),
       rank = rankNL(predictTable, sepText$toPredict, verbose)) %>%
    return()
  
}

precisionNL <- function(predictTable, toPredict, topN = 1, verbose = F) {
  if(is.na(predictTable[1, prediction])) return(NA) else
    if(toPredict %in% predictTable[1:topN, prediction] ) return(1) else
      return(0)
}

rankNL <- function(predictTable, toPredict, verbose = F) {
  rValue <- predictTable[prediction == toPredict, which = T]
  if (verbose) print(predictTable[rValue, ])
  
  if(length(rValue) == 0) return(NA) else return(rValue)
}




