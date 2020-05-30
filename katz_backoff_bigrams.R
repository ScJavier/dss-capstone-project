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
df_us_sample <- c(sample(df_us_news, 2500),
                  sample(df_us_blogs, 2500),
                  sample(df_us_twits, 2500))

corpus_sample <- VCorpus(VectorSource(df_us_sample))

corpus_sample <- tm_map(corpus_sample, tolower)
corpus_sample <- tm_map(corpus_sample, removeNumbers)
corpus_sample <- tm_map(corpus_sample, removePunctuation)
corpus_sample <- tm_map(corpus_sample, stripWhitespace)
corpus_sample <- tm_map(corpus_sample, PlainTextDocument)


tdm <- TermDocumentMatrix(corpus_sample)
tdm_freq <- rowSums(as.matrix(tdm))
head(tdm_freq)

