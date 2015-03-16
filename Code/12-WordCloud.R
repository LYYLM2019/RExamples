install.packages("twitteR")
install.packages("wordcloud")
install.packages("tm")
install.packages("RCurl")
install.packages("stringr")
library(stringr)
library(base)
library(RCurl)
library("twitteR")
library("wordcloud")
library("tm")

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'L0kdjEltuZkN9ue9Kdg1A0Ioh'
consumer_secret <- 'yQOZCuZzWCW9MU0gOPlca6B3cLTaDJ17ARkOK7ZwzxJMfi5iVG'
access_token <- '2574584934-lNkcHbNIt1v2cUrgR11A0gXPHKeoDkIEmrblRN9'
access_secret <- 'CTfjXOiLBSfc5OyXLSMvvErZbsvnvkkXVEPu27ROqrlXJ'
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

r_stats = searchTwitter('cheap gas', since='2014-06-01', until='2015-02-23', n=1500, lang="en")

r_stats_text <- sapply(r_stats, function(x) x$getText())
r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))

r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower), lazy = TRUE) 
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation, lazy = TRUE)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()), lazy = TRUE)

wordcloud(r_stats_text_corpus, lazy = TRUE)