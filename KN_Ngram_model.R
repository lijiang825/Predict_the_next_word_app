setwd("/Users/li/Documents/Education/Coursera/Data_Science_Track/10_Capstone_project/final/en_US")
library(quanteda)
library(readtext)
library(data.table)

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

# Preprocess texts: convert to lower text, remove punctuation and numbers
clean_text <- function(txt,removeStopwords = F){
  txt = extend_contraction(txt)
  txt = Corpus(VectorSource(txt))
  txt = tm_map(txt,content_transformer(tolower))
  txt = tm_map(txt,content_transformer(removePunctuation))
  txt = tm_map(txt,content_transformer(removeNumbers))
  if (removeStopwords){
    txt = tm_map(txt, removeWords, stopwords("english"))
  }
  txt = tm_map(txt,content_transformer(stripWhitespace))
  txt = txt[[1]]$content
  txt
}

## Remove foreign words
foreignWords <- function(ngram_list){
  result = logical(length(ngram_list))
  for (i in 1:length(ngram_list)){
    temp = unlist(strsplit(ngram_list[i],split="_"))
    non.ASCII <- grep("asdfg", iconv(temp, "latin1", "ASCII", sub="asdfg"))
    result[i] = length(non.ASCII) == 0
  }
  result
}

## Build tokens 
build_tokens <- function(clean_txt, removeStopwords = FALSE){
  ## Keep hyphen 
  ## Consider remove stopwords: remove=stopwords("english")
  ## No stemming 
  tokensAll <- tokens(clean_txt)
  if (removeStopwords){
    tokensNoStopwords <- removeFeatures(tokensAll,stopwords("english"))
  }
  else{
    tokensNoStopwords <- tokensAll
  }
  tokensNoStopwords 
}

## Build ngrams table 
build_ngram_tables <- function(tokensNoStopwords,ngram,removeStopwords=FALSE){
  # Build ngram tokens
  tokensNgramsNoStopwords <- tokens_ngrams(tokensNoStopwords, ngram)
  temp <- dfm(tokensNgramsNoStopwords, stem=FALSE)
  ## convert to df, then to dt
  temp <- data.frame(temp,check.names=F)
  temp <- temp[,foreignWords(colnames(temp))]
  temp <- t(temp)
  temp <- data.frame(temp)
  colnames(temp) = "counts"
  temp$ngram <- rownames(temp)
  temp = temp[c("ngram","counts")]
  temp = data.table(temp)
  setkey(temp,ngram)
  temp
}

## Build corpus 
myft1 <- readtext("en_US.subset5_training.txt")
corpus_myft1 <- corpus(myft1)
txt <- texts(corpus_myft1)
## Expand contraction 
library(tm)
clean_txt <- clean_text(txt)
library(quanteda)
tokensNoStopwords = build_tokens(clean_txt,removeStopwords = F)
dfm_ngram = list()
n = 4
for (i in 1:n){
  dfm_ngram[[i]] = build_ngram_tables(tokensNoStopwords, i, removeStopwords = T)
  cat("ngram table with n=", i, "finished\n")
}
unigram = dfm_ngram[[1]]
bigram = dfm_ngram[[2]]
trigram = dfm_ngram[[3]]
fourgram = dfm_ngram[[4]]
rm(dfm_ngram)
gc()

## Split ngram into n-1-gram and last unigram 
splitNgram <- function(ngram){
  wordList = unlist(strsplit(ngram,split="_"))
  n = length(wordList)
  n_1gram = paste(wordList[1:n-1],collapse="_")
  unigram = wordList[n]
  c(n_1gram,unigram)
}

n_1gram <- function(ngram){
  wordList = unlist(strsplit(ngram,split="_"))
  n = length(wordList)
  n_1gram = paste(wordList[1:n-1],collapse="_")
  n_1gram
}

lastN_1gram <- function(ngram){
  wordList = unlist(strsplit(ngram,split="_"))
  n = length(wordList)
  last = paste(wordList[2:n],collapse="_")
  last
}

initiate_env <- function(ngram_table,aspect=NULL){
  env <- new.env()
  df = data.frame(ngram_table)
  n = dim(ngram_table)[1]
  base = n%/%10
  for (i in 1:n){
    if (i%%base == 0){
      progress = i%/%base*10
      cat("Finished:", progress, "%\n")
    }
    ngram = df[i,"ngram"]
    if (is.null(aspect)){
      assign(ngram,0,env)
    }
    else{
      assign(ngram,df[i,aspect],env)
    }
  }
  env
}

update_env <- function(ngram,env,value=1){
  if (ngram == ""){
    print("Blank founded")
    return(env)
  }
  if (!exists(ngram,envir=env)){
    assign(ngram,0,env)
  }
  assign(ngram,env[[ngram]]+value,env)
  env
}

# Compute # of suffix and prefix, lambda and Pcontinuation for each word simutaneously and store in unigram 
prefix_suffix_lambda_Pcon_unigram <- function(bigram_table,unigram_table,d=0.75){
  ## Suffix stores # of words that follow the given word
  ## Prefix stores # of words that precede the given word 
  print(Sys.time())
  # Initialize suffix and prefix columns 
  n = dim(unigram_table)[1]
  unigram_table$suffix = numeric(n)
  unigram_table$prefix = numeric(n)
  ## Initiate new environments for suffix and prefix 
  print("Initiate unigram suffix hash table")
  suffix_env <- initiate_env(unigram_table)
  print("Initiate unigram prefix hash table")
  prefix_env <- initiate_env(unigram_table)
  
  ## Count suffix and prefix from bigram table and store in the suffix/prefix environment 
  print("Building suffix and prefix hash tables")
  base = dim(bigram_table)[1]%/%10
  step = 0
  for (bigram in bigram_table$ngram){
    step = step + 1
    if (step%%base == 0){
      cat("Finished:", step%/%base*10, "%\n")
    }
    word12 = unlist(strsplit(bigram,split="_"))
    suffix_env = update_env(word12[1],suffix_env)
    prefix_env = update_env(word12[2],prefix_env)
  }
  
  ## Store suffix and prefix into the data table 
  print("Store suffix and prefix into data tables")
  base = dim(unigram_table)[1]%/%10
  step = 0
  unigram_list = unigram[,ngram]
  for (unigram in unigram_list){
    step = step + 1
    if (step%%base == 0){
      cat("Finished:", step%/%base*10, "%\n")
    }
    unigram_table[unigram, "suffix"] = suffix_env[[unigram]]
    unigram_table[unigram, "prefix"] = prefix_env[[unigram]]
  }
  
  ## Use vector to compute lambda and Pcon 
  unigram_table$lambda = d/(unigram_table[,"counts"]) * unigram_table[, "suffix"]
  unigram_table$freq = unigram_table[,"prefix"]/dim(bigram_table)[1]
  
  print(Sys.time())
  unigram_table
}

# KN freq for one bigram
KN_freq_bigram <- function(bigram, bigram_counts, unigram_counts, unigram_lambda, unigram_freq, d=0.75){
  word1 = splitNgram(bigram)[1]
  word2 = splitNgram(bigram)[2]
  Pkn = max(bigram_counts[[bigram]]-d,0)/unigram_counts[[word1]] + unigram_lambda[[word1]] * unigram_freq[[word2]]
  Pkn
}

# KN freq for all bigrams
KN_freq_bigram_all <- function(bigram_table, unigram_par_list, d=0.75){
  print(Sys.time())
  n = dim(bigram_table)[1]
  base = n%/%10
  KN_freq = numeric(n)
  print("Initiating bigram_table counts hash table")
  bigram_counts = initiate_env(bigram_table,"counts")
  unigram_counts = unigram_par_list[["counts"]]
  unigram_lambda = unigram_par_list[["lambda"]]
  unigram_freq = unigram_par_list[["freq"]]
  for (i in 1:n){
    if (i%%base == 0){
      cat("Finished:", i%/%base*10, "%\n")
    }
    KN_freq[i] = KN_freq_bigram(bigram_table[i,ngram], bigram_counts, unigram_counts, unigram_lambda, unigram_freq, d)
  }
  print(Sys.time())
  KN_freq
}

## Define some functions for KN frequency of ngram(n>=3)
# Compute lambda
lambda_n_1gram <- function(ngram_table,n_1gram_table,n_1gram_par_list, d=0.75){
  print(Sys.time())
  ## Initiate new environments for suffix and lambda 
  print ("Initiating suffix hash maps")
  suffix_env <- initiate_env(n_1gram_table)
  print ("Initiating lambda hash maps")
  lambda_env <- initiate_env(n_1gram_table)
  
  ## Computing suffix hash maps 
  n = dim(ngram_table)[1]
  base = n%/%10
  print ("Buliding suffix hash maps")
  for (i in 1:n){
    if (i%%base == 0){
      cat("Finished:", i%/%base*10, "%\n")
    }
    n_1gram = splitNgram(ngram_table[i,ngram])[1]
    suffix_env = update_env(n_1gram,suffix_env)
  }
  
  ## Computing lambda hash maps 
  n_1gram_counts = n_1gram_par_list[["counts"]]
  n = dim(n_1gram_table)[1]
  base = n%/%10
  print ("Building lambda hash maps")
  for (i in 1:n){
    if (i%%base == 0){
      cat("Finished:", i%/%base*10, "%\n")
    }
    n_1gram = n_1gram_table[i,ngram]
    lambda = d/(n_1gram_counts[[n_1gram]]) * suffix_env[[n_1gram]]
    lambda_env = update_env(n_1gram,lambda_env,value=lambda)
  }
  
  print(Sys.time())
  lambda_env
}

KN_freq_ngram <- function(ngram, ngram_counts, n_1gram_counts, n_1gram_lambda, n_1gram_freq, d=0.75){
  n_1gram = splitNgram(ngram)[1]
  Pkn = max(ngram_counts[[ngram]]-d,0)/n_1gram_counts[[n_1gram]] + n_1gram_lambda[[n_1gram]] * n_1gram_freq[[n_1gram]]
  Pkn
}

KN_freq_ngram_all <- function(ngram_table, n_1gram_par_list, d=0.75){
  print(Sys.time())
  n = dim(ngram_table)[1]
  base = n%/%10
  KN_freq = numeric(n)
  print("Initiating ngram_table counts hash table")
  ngram_counts = initiate_env(ngram_table,"counts")
  n_1gram_counts = n_1gram_par_list[["counts"]]
  n_1gram_lambda = n_1gram_par_list[["lambda"]]
  n_1gram_Pcon = n_1gram_par_list[["freq"]]
  for (i in 1:n){
    if (i%%base == 0){
      cat("Finished:", i%/%base*10, "%\n")
    }
    KN_freq[i] = KN_freq_ngram(ngram_table[i,ngram], ngram_counts, n_1gram_counts, n_1gram_lambda, n_1gram_Pcon, d)
  }
  print(Sys.time())
  KN_freq
}

## Generate unigram_par_list
unigram = prefix_suffix_lambda_Pcon_unigram(bigram,unigram)
unigram_par_list = list()
for (par in c("counts", "lambda", "freq")){
  unigram_par_list[[par]] = initiate_env(unigram,par)
}

## Generate bigram_par_list
bigram$freq = KN_freq_bigram_all(bigram, unigram_par_list, d=0.75)
bigram$n_1gram = sapply(bigram[,ngram],FUN=n_1gram)
bigram_par_list = list()
for (par in c("counts", "freq")){
  bigram_par_list[[par]] = initiate_env(bigram,par)
}
bigram_par_list[["lambda"]] = lambda_n_1gram(trigram,bigram,bigram_par_list)

trigram$freq = KN_freq_ngram_all(trigram,bigram_par_list, d=0.75)
trigram$n_1gram = sapply(trigram[,ngram],FUN=n_1gram)
trigram_par_list = list()
for (par in c("counts", "freq")){
  trigram_par_list[[par]] = initiate_env(trigram,par)
}
trigram_par_list[["lambda"]] = lambda_n_1gram(fourgram,trigram,trigram_par_list)

## Generate fourgram_par_list
fourgram$freq = KN_freq_ngram_all(fourgram,trigram_par_list)
fourgram$n_1gram = sapply(fourgram[,ngram],FUN=n_1gram)

fwrite(unigram[,c("ngram","counts","freq")],file="unigram5_withStopwords.csv")
fwrite(bigram,file="bigram5_withStopwords.csv")
fwrite(trigram,file="trigram5_withStopwords.csv")
fwrite(fourgram,file="fourgram5_withStopwords.csv")
