library(stringr)
library(base)
library(RCurl)
library(twitteR)
library(wordcloud)
library(tm)
library(SnowballC)

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'L0kdjEltuZkN9ue9Kdg1A0Ioh'
consumer_secret <- 'yQOZCuZzWCW9MU0gOPlca6B3cLTaDJ17ARkOK7ZwzxJMfi5iVG'
access_token <- '2574584934-lNkcHbNIt1v2cUrgR11A0gXPHKeoDkIEmrblRN9'
access_secret <- 'CTfjXOiLBSfc5OyXLSMvvErZbsvnvkkXVEPu27ROqrlXJ'
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

r_stats = searchTwitter('gas', since = '2014-06-01', until = '2015-02-23', n=1500)
r_stats_text <- sapply(r_stats, function(x) iconv(x$getText(), sub = "byte"))

r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))

# removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
# r_stats_text_corpus <- tm_map(r_stats_text_corpus , removeURL)
# r_stats_text_corpus2 <-tm_map(r_stats_text_corpus, function(x) iconv(enc2utf8(x), sub = "byte")) 

# inspect(r_stats_text_corpus)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower), mc.cores=1)

r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation)
# r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()), mc.cores = 1)


ap.tdm <- TermDocumentMatrix(r_stats_text_corpus, control = list(stopwords = TRUE))

ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
wordcloud(ap.d$word, ap.d$freq)

#============================================================

text = readLines(
  textConnection("Ол арман – әлем елдерімен терезесі тең қатынас құрып, әлем картасынан ойып тұрып орын алатын Тәуелсіз Мемлекет атану еді. Ол арман – тұрмысы бақуатты, түтіні түзу ұшқан, ұрпағы ертеңіне сеніммен қарайтын бақытты Ел болу еді. Біз армандарды ақиқатқа айналдырдық. Мәңгілік Елдің іргетасын қаладық. Мен қоғамда «Қазақ елінің ұлттық идеясы қандай болуы керек?» деген сауал жиі талқыға түсетінін көріп жүрмін. Біз үшін болашағымызға бағдар ететін, ұлтты ұйыстырып, ұлы мақсаттарға жетелейтін идея бар. Ол – Мәңгілік Ел идеясы. Тәуелсіздікпен бірге халқымыз Мәңгілік Мұраттарына қол жеткізді."), 
  , encoding="UTF-8")