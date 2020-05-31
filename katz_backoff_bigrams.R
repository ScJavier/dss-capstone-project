# Load important libraries

library(tm)
library(RWeka)
library(ggplot2)
library(gridExtra)
library(dplyr)

# Load corpora

df_us_blogs <- readLines('../Coursera-SwiftKey/final/en_US/en_US.blogs.txt', skipNul=TRUE, encoding = 'UTF-8')
df_us_news <- readLines('../Coursera-SwiftKey/final/en_US/en_US.news.txt', skipNul=TRUE, encoding = 'UTF-8')
df_us_twits <- readLines('../Coursera-SwiftKey/final/en_US/en_US.twitter.txt', skipNul=TRUE, encoding = 'UTF-8')

# Select a sample of data a declare VCorpus

set.seed(1304)
df_us_sample <- c(sample(df_us_news, round(0.05*length(df_us_news))),
                  sample(df_us_blogs, round(0.05*length(df_us_blogs))),
                  sample(df_us_twits, round(0.05*length(df_us_twits))))

rm(df_us_blogs, df_us_news, df_us_twits)

corpus_sample <- VCorpus(VectorSource(df_us_sample))


fix_contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won['’‘]t", "will not", doc)
  doc <- gsub("couldn['’‘]t", "could not", doc)
  doc <- gsub("n['’‘]t", " not", doc)
  doc <- gsub("['’‘]ll", " will", doc)
  doc <- gsub("['’‘]re", " are", doc)
  doc <- gsub("['’‘]ve", " have", doc)
  doc <- gsub("['’‘]d", " had", doc)
  doc <- gsub("['’‘]m", " am", doc)
  doc <- gsub("['’‘]em", " them", doc)
  doc <- gsub("he['’‘]s", "he is", doc)
  doc <- gsub("she['’‘]s", "she is", doc)
  doc <- gsub("it['’‘]s", "it is", doc)
  # 's could be is or possessive: it has no expansion
  doc <- gsub("['’‘]s", "", doc) 
  return(doc)
}


corpus_sample <- tm_map(corpus_sample, tolower)
corpus_sample <- tm_map(corpus_sample, removeNumbers)
corpus_sample <- tm_map(corpus_sample, removePunctuation)
corpus_sample <- tm_map(corpus_sample, fix_contractions)
corpus_sample <- tm_map(corpus_sample, PlainTextDocument)
removeSpecials <- content_transformer(function (x, pattern) gsub(pattern, ' ', x))
corpus_sample <- tm_map(corpus_sample, removeSpecials, "[^a-z ]")
corpus_sample <- tm_map(corpus_sample, stripWhitespace)
corpus_sample <- tm_map(corpus_sample, PlainTextDocument)

tdm <- TermDocumentMatrix(corpus_sample)
tdm <- removeSparseTerms(tdm, 0.99999)
tdm_freq <- row_sums(tdm)

# Esta función calcula las probabilidades de los unigramas con suavizamiento de Laplace
probs_unigrams <- function(x, freq_table){
  V <- length(freq_table)
  test <- is.na(freq_table[x])
  if(test)
  {
    res <- 1/V
  } else{
    res <- (freq_table[x] + 1) / (sum(freq_table) + V)
  }
  return(res)    
}

probs_unigrams('not', tdm_freq)


bigram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
bi_tdm <- TermDocumentMatrix(corpus_sample, control=list(tokenize=bigram_tokenizer))
bi_tdm <- removeSparseTerms(bi_tdm, 0.99999)
bi_tdm_freq <- row_sums(bi_tdm)

probs_bigrams <- function(x, y, freq_table){
  bigram <- paste(x, y, sep = ' ')
  V <- length(freq_table)
  test <- is.na(freq_table[bigram])
  if(test)
  {
    res <- 1/V
  } else{
    res <- (freq_table[bigram] + 1) / (sum(freq_table) + V)
  }
  return(res)    
}

probs_bigrams('bless', 'you', bi_tdm_freq)


trigram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
tri_tdm <- TermDocumentMatrix(corpus_sample, control=list(tokenize=trigram_tokenizer))
tri_tdm <- removeSparseTerms(tri_tdm, 0.99999)
tri_tdm_freq <- row_sums(tri_tdm)

rm(corpus_sample)

probs_trigrams <- function(x, y, z, freq_table){
  trigram <- paste(x, y, z, sep = ' ')
  V <- length(freq_table)
  test <- is.na(freq_table[trigram])
  if(test)
  {
    res <- 1/V
  } else{
    res <- (freq_table[trigram] + 1) / (sum(freq_table) + V)
  }
  return(res)    
}

probs_trigrams('i', 'will', 'not', tri_tdm_freq)




predice_dos <- function(x)
{
  index_temp <- grepl(paste0('^(', x, ')[ ]+'), names(bi_tdm_freq))
  match_bigrams <- bi_tdm_freq[index_temp]
  res <- names(sort(match_bigrams, decreasing = TRUE))[1]
  return(res)
}

predice_dos('love')

predice_tres <- function(x)
{
  index_temp <- grepl(paste0('^(', x, ')[ ]+'), names(tri_tdm_freq))
  match_trigrams <- tri_tdm_freq[index_temp]
  res <- names(sort(match_trigrams, decreasing = TRUE))[1:5]
  return(res)
}


# prepare tables

unigram_table <- data.frame(unigram = names(tdm_freq),
                            frequency = tdm_freq, stringsAsFactors = FALSE)

bigram_table <- data.frame(bigram = names(bi_tdm_freq),
                           frequency = bi_tdm_freq, stringsAsFactors = FALSE)

trigram_table <- data.frame(trigram = names(tri_tdm_freq),
                            frequency = tri_tdm_freq, stringsAsFactors = FALSE)

rownames(unigram_table) <- NULL
rownames(bigram_table) <- NULL
rownames(trigram_table) <- NULL


# Stupid Back-Off

example <- 'you francisco'

predice_new_word <- function(x)
{
  example <- x
  regexp <- paste0('^', example,'[ ]+')
  index_trigrams <- grepl(regexp, trigram_table$trigram)
  matched_trigrams <- filter(trigram_table, index_trigrams)
  
  if(dim(matched_trigrams)[1] > 0)
  {
    matched_trigrams <- arrange(matched_trigrams, desc(frequency))
    size <- min(5, dim(matched_trigrams)[1])
    res <- head(matched_trigrams, n = size)
    res$trigram <- sapply(strsplit(res$trigram, split = ' '), function(x) x[3])
  } else{
    last_word <- strsplit(example, split = ' ')[[1]][2]
    regexp <- paste0('^', last_word,'[ ]+')
    index_bigrams <- grepl(regexp, bigram_table$bigram)
    matched_bigrams <- filter(bigram_table, index_bigrams)
    if(dim(matched_bigrams)[1] > 0){
      matched_bigrams <- arrange(matched_bigrams, desc(frequency))
      size <- min(5, dim(matched_bigrams)[1])
      res <- head(matched_bigrams, n = size)
      res$bigram <- sapply(strsplit(res$bigram, split = ' '), function(x) x[2])
    } else{
      matched_unigrams <- arrange(unigram_table, desc(frequency))
      res <- head(matched_unigrams, n = 5)
    }
  }
  return(res)  
}

predice_new_word('you are')









  