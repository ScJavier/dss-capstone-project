
# Load important libraries

library(tm)
library(RWeka)
library(ggplot2)
library(gridExtra)
library(dplyr)

# Load corpora

df_us_blogs <- readLines('Coursera-SwiftKey/final/en_US/en_US.blogs.txt', skipNul=TRUE, encoding = 'UTF-8')
df_us_news <- readLines('Coursera-SwiftKey/final/en_US/en_US.news.txt', skipNul=TRUE, encoding = 'UTF-8')
df_us_twits <- readLines('Coursera-SwiftKey/final/en_US/en_US.twitter.txt', skipNul=TRUE, encoding = 'UTF-8')

# Select a sample of data a declare VCorpus

set.seed(1304)
df_us_sample <- c(sample(df_us_news, 2500),
                  sample(df_us_blogs, 2500),
                  sample(df_us_twits, 2500))

corpus_news <- VCorpus(VectorSource(df_us_sample))

corpus_sample <- tm_map(corpus_news, tolower)
corpus_sample <- tm_map(corpus_news, removeNumbers)
corpus_sample <- tm_map(corpus_news, removePunctuation)
corpus_sample <- tm_map(corpus_news, stripWhitespace)
corpus_sample <- tm_map(corpus_news, PlainTextDocument)

# Count words and select top 15

tdm <- TermDocumentMatrix(corpus_news)
tdm_fqt <- findFreqTerms(tdm, lowfreq=500)
most_frequent_terms <- sort(rowSums(as.matrix(tdm[tdm_fqt,])), decreasing = TRUE)[1:15]

df_temp <- data.frame(words=names(most_frequent_terms),
                      freq=most_frequent_terms)
df_temp <- arrange(df_temp, desc(freq))
ggplot(df_temp, aes(reorder(words, freq), freq)) + geom_bar(stat = "identity") +
  xlab('') + ylab('') + coord_flip()

# Count 2-grams and select top 15

bigram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
bi_tdm <- TermDocumentMatrix(corpus_news, control=list(tokenize=bigram_tokenizer))

bidm_fqt <- findFreqTerms(bi_tdm, lowfreq=50)
most_frequent_bigrams <- sort(rowSums(as.matrix(bi_tdm[bidm_fqt,])), decreasing = TRUE)[1:15]

df_temp <- data.frame(words=names(most_frequent_bigrams),
                      freq=most_frequent_bigrams)
df_temp <- arrange(df_temp, desc(freq))
ggplot(df_temp, aes(reorder(words, freq), freq)) + geom_bar(stat = "identity") +
  xlab('') + ylab('') + coord_flip()

# Count 3-grams and select top 15

trigram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
tri_tdm <- TermDocumentMatrix(corpus_news, control=list(tokenize=trigram_tokenizer))

tridm_fqt <- findFreqTerms(tri_tdm, lowfreq=10)
most_frequent_trigrams <- sort(rowSums(as.matrix(tri_tdm[tridm_fqt,])), decreasing = TRUE)[1:15]

df_temp <- data.frame(words=names(most_frequent_trigrams),
                      freq=most_frequent_trigrams)
df_temp <- arrange(df_temp, desc(freq))
ggplot(df_temp, aes(reorder(words, freq), freq)) + geom_bar(stat = "identity") +
  xlab('') + ylab('') + coord_flip()
