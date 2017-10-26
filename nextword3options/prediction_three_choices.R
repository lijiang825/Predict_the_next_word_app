library(shiny)
library(data.table)
library(tm)

# Expand contractions function 
extend_contraction <- function(txt){
  contraction = c("won't", "can't", "'m", "'ll", "'d", "'ve", "'re", "'s", "n't")
  full = c("will not", "cannot", " am", " will", " had", " have", " are", " is"," not")
  corrected_txt = NULL
  for (i in 1:length(txt)){
    temp = txt[i]
    for (j in 1:length(contraction)){
      temp = gsub(contraction[j],full[j],temp)
    }
    corrected_txt = c(corrected_txt,temp)
  }
  corrected_txt
}

ngramExtraction <- function(sentence, n, removeStopwords = FALSE){
  txt = extend_contraction(sentence)
  txt = Corpus(VectorSource(txt))
  txt = tm_map(txt,content_transformer(tolower))
  txt = tm_map(txt,content_transformer(removePunctuation))
  txt = tm_map(txt,content_transformer(removeNumbers))
  if (removeStopwords){
    txt = tm_map(txt, removeWords, stopwords("english"))
  }
  txt = tm_map(txt,content_transformer(stripWhitespace))
  txt = txt[[1]]$content
  wordList = unlist(strsplit(txt, " "))
  if (length(wordList) <= n){
    ngram = paste(wordList,collapse="_")
  }
  else{
    ngram = paste(tail(wordList,n), collapse="_")
  }
  return(ngram)
}

splitNgram <- function(ngram){
  wordList = unlist(strsplit(ngram,split="_"))
  n = length(wordList)
  n_1gram = paste(wordList[1:n-1],collapse="_")
  unigram = wordList[n]
  c(n_1gram,unigram)
}

lastN_1gram <- function(ngram){
  wordList = unlist(strsplit(ngram,split="_"))
  n = length(wordList)
  last = paste(wordList[2:n],collapse="_")
  last
}

wordPrediction <- function(ngram, ngram_table_list){
  prefix = lastN_1gram(ngram)
  ngram_table_list = ngram_table_list[1:length(unlist(strsplit(ngram,split="_")))]
  listSize = length(ngram_table_list)
  wordPrediction_recursion(prefix,ngram_table_list,listSize)
}

wordPrediction_recursion <- function(n_1gram, ngram_table_list, n){
  ngram_table = ngram_table_list[[n]]
  
  if (n == 1){
    sorted = ngram_table[order(ngram_table[,freq],decreasing=T),]
    return(sorted[1:3,ngram])
  }
  
  n_1gram_list = ngram_table[,n_1gram]
  if (n_1gram %in% n_1gram_list){
    target_expr = paste0("^",n_1gram,"$")
    target = grep(target_expr,n_1gram_list)
    targetNgrams = ngram_table[target,ngram]
    targetSize = length(targetNgrams)
    if (targetSize <= 3){
      result = character(targetSize)
      for (i in 1:targetSize){
        result[i] = splitNgram(targetNgrams[i])[2]
      }
      for (j in (targetSize+1):3){
        result[j] = ""
      }
      return(result)
    }
    else{
      df = data.frame(ngram=rep("asdfg",3),freq=rep(0,3), stringsAsFactors = F)
      df = df[order(df$freq,decreasing=F),]
      for (ngram in targetNgrams){
        prob = ngram_table[ngram,freq]
        for (i in 1:3){
          if (prob > df[i,"freq"]){
            df[1,"ngram"] = splitNgram(ngram)[2]
            df[1,"freq"] = prob  
            df = df[order(df$freq,decreasing=F),]
            break
          }
        }
      }
      return(df[,"ngram"])
    }
  }
  else{
    return(bestWord = wordPrediction_recursion(lastN_1gram(n_1gram), ngram_table_list, n-1))
  }
}

prediction <- function(sentence, ngram_table_list, removeStopwords = FALSE){
  listSize = length(ngram_table_list)
  ngram = ngramExtraction(sentence, listSize, removeStopwords)
  pred = wordPrediction(ngram, ngram_table_list)
  pred
}

## Read in ngrams tables with ngrams frequencies  
unigram = fread("unigram2_withStopwords.csv")
bigram = fread("bigram2_withStopwords.csv")
trigram = fread("trigram2_withStopwords.csv")
fourgram = fread("fourgram2_withStopwords.csv")

ngram_table_list = list(unigram,bigram,trigram,fourgram)
rm(unigram)
rm(bigram)
rm(trigram)
rm(fourgram)
for (ngram_table in ngram_table_list){
  setkey(ngram_table,ngram)
}
rm(ngram_table)
gc()