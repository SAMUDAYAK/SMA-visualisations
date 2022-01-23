#libraries required - Network effects

library(dplyr) # supportive library for functions required
library(rtweet) # extraction of twitter time line data
library(ggplot2) # for plotting
library(tidytext) # supportive library for functions required 
library(forestmangr) # supportive library for functions required
library(writexl)# for writing excel files
library(readxl) # for reading excel file
library(wordcloud) # for visualization
library(tm) # for text mining
library(sentimentr) # scoring for sentiment analysis

# twitter time line extraction

#frimi_tl_data <- get_timeline("@FriMi_lk", n= 3200) # Getting time line data for 3200 (twitter cap) time line entries of frimi
#write_xlsx(frimi_tl_data, "C:\\Users\\samud\\Desktop\\SMA\\tweenet\\frimi_tl_data.xlsx") # writing time line data to an excel file for storage

frimi_followers <- get_followers("@FriMi_LK", n = 5000, page = "-1",retryonratelimit = FALSE, parse = TRUE, verbose = TRUE, token = NULL) # list of FriMi followers
head(frimi_followers)
summary(frimi_followers)

FriMi <- read_excel("C:/Users/samud/Desktop/SMA/tweenet/frimi_tl_data.xlsx") # importing frimi time line data as at 21.01.2022
frimi <- FriMi # reassigning

# subsetting organic tweets

frimi_organic <- frimi[frimi$is_retweet==FALSE, ] # Removing retweets
frimi_organic <- subset(frimi_organic, is.na(frimi_organic$reply_to_status_id)) # removing replies


# subsetting retweets

frimi_retweets <- frimi[frimi$is_retweet==TRUE, ] ## Isolating retweets

# subsetting retweets

frimi_rep <- subset(frimi, !is.na(frimi$reply_to_status_id)) ## Isolating replies

# creating a summary of tweet types

frimi_sum <- data.frame(category=c("Organic", "Retweets", "Replies"), count=c(726, 106, 590))


# Adding columns for calculation of proportions for pie chart - Tweet type pie chart
frimi_sum$fraction = frimi_sum$count / sum(frimi_sum$count)
frimi_sum$percentage = frimi_sum$count / sum(frimi_sum$count) * 100
frimi_sum$ymax = cumsum(frimi_sum$fraction)
frimi_sum$ymin = c(0, head(frimi_sum$ymax, n=-1))
# Rounding the data to one decimal point
frimi_sum <- round_df(frimi_sum, 1)
# Specify what the legend should say
Type_of_tweets <- paste(frimi_sum$category, frimi_sum$percentage, "%")
# plotting the pie chart for tweet types
ggplot(frimi_sum, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_tweets)) + 
geom_rect() + coord_polar(theta="y") + xlim(c(2, 4)) + theme_void() + theme(legend.position = "left")


# Representation of tweet counts on Monthly basis
colnames(frimi)[colnames(frimi)=="screen_name"] <- "Twitter_Account"
ts_plot(dplyr::group_by(frimi, Twitter_Account), "months") + ggplot2::theme_minimal() +
ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +   ggplot2::labs(x = NULL, y = NULL,
title = "FriMi Tweet Frequency", subtitle = "Monthly aggregation of Tweet counts", caption = "\nSource: Data collected from twitter's REST API via rtweet"  )

# Representation of tweet sources 
tweet_sources <- frimi_organic %>% select(source) %>% group_by(source) %>% summarize(count=n())
tweet_sources <- subset(tweet_sources, count > 15)

tweet_sources_rep <- data.frame(category=tweet_sources$source, count=tweet_sources$count)
tweet_sources_rep$fraction = tweet_sources_rep$count / sum(tweet_sources_rep$count)
tweet_sources_rep$percentage = tweet_sources_rep$count / sum(tweet_sources_rep$count) * 100
tweet_sources_rep$ymax = cumsum(tweet_sources_rep$fraction)
tweet_sources_rep$ymin = c(0, head(tweet_sources_rep$ymax, n=-1))
tweet_sources_rep <- round_df(tweet_sources_rep, 1)
Source <- paste(tweet_sources_rep$category, tweet_sources_rep$percentage, "%")
ggplot(tweet_sources_rep, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) + geom_rect() +
coord_polar(theta="y") + xlim(c(2, 4)) + theme_void() +
  theme(legend.position = "left")

#Most retweeted accounts

wordcloud(frimi_retweets$retweet_screen_name, min.freq=1, scale=c(2, .5), random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Accent"))

#cleaning organic tweets, retweets and replies

frimi_organic$text <-  gsub("https\\S*", "", frimi_organic$text)
frimi_organic$text <-  gsub("@\\S*", "",frimi_organic$text) 
frimi_organic$text  <-  gsub("amp", "", frimi_organic$text) 
frimi_organic$text  <-  gsub("[\r\n]", "", frimi_organic$text)
frimi_organic$text  <-  gsub("[[:punct:]]", "",frimi_organic$text)
frimi_organic$text <- tolower(frimi_organic$text)
cln_twts_org <- frimi_organic %>% select(text) %>% unnest_tokens(word, text)
cln_twts_org <- cln_twts_org %>% anti_join(stop_words)


frimi_rep$text <-  gsub("https\\S*", "", frimi_rep$text)
frimi_rep$text <-  gsub("@\\S*", "",frimi_rep$text) 
frimi_rep$text  <-  gsub("amp", "", frimi_rep$text) 
frimi_rep$text  <-  gsub("[\r\n]", "", frimi_rep$text)
frimi_rep$text  <-  gsub("[[:punct:]]", "",frimi_rep$text)
frimi_rep$text <- tolower(frimi_rep$text)
cln_twts_rep <- frimi_rep %>% select(text) %>% unnest_tokens(word, text)
cln_twts_rep <- cln_twts_rep %>% anti_join(stop_words)

frimi_retweets$text <-  gsub("https\\S*", "", frimi_retweets$text)
frimi_retweets$text <-  gsub("@\\S*", "",frimi_retweets$text) 
frimi_retweets$text  <-  gsub("amp", "", frimi_retweets$text) 
frimi_retweets$text  <-  gsub("[\r\n]", "", frimi_retweets$text)
frimi_retweets$text  <-  gsub("[[:punct:]]", "",frimi_retweets$text)
frimi_retweets$text <- tolower(frimi_retweets$text)
cln_twts_ret <- frimi_retweets %>% select(text) %>% unnest_tokens(word, text)
cln_twts_ret <- cln_twts_ret %>% anti_join(stop_words)

#Barchart of frequent words - organic tweet words

cln_twts_org %>%  count(word, sort = TRUE) %>% top_n(30) %>% mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() + labs(y = "Count",
                                                                               x = "Words", title = "FriMi organic tweets unique words")
#Key organic tweet words
wordcloud(cln_twts_org$word, min.freq=10, scale=c(5.5, .5), random.order=FALSE, rot.per=0.45, colors=brewer.pal(12, "Paired"))

#Barchart of frequent words - retweet words

cln_twts_ret %>%  count(word, sort = TRUE) %>% top_n(30) %>% mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() + labs(y = "Count",
                                                                               x = "Words", title = "FriMi retweets unique words")
#Key retweet words
wordcloud(cln_twts_ret$word, min.freq=5, scale=c(5.5, .5), random.order=FALSE, rot.per=0.45, colors=brewer.pal(12, "Paired"))

#Barchart of frequent words - reply words

cln_twts_rep %>%  count(word, sort = TRUE) %>% top_n(30) %>% mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() + labs(y = "Count",
                                                                               x = "Words", title = "FriMi reply unique words")
#Key reply words
wordcloud(cln_twts_rep$word, min.freq=5, scale=c(5.5, .5), random.order=FALSE, rot.per=0.45, colors=brewer.pal(12, "Paired"))


#Sentiment analysis - organic tweets

frimi_org_sent <- frimi_organic
sent_text_org <- sentimentr::sentiment(frimi_org_sent$text) #calculating sentiment scored for FriMi twitter timeline organic texts
frimi_org_sent$sentiment <- sent_text_org$sentiment #Adding the sentiment score as a feature to the FriMi timeline organic tweets data set

pos_tweets_org <- head(frimi_org_sent[order(frimi_org_sent$sentiment, decreasing = TRUE),c(5,91)],100) # sorting texts according to the descending order of sentiment score and getting the 1st 100 positive observations.
neg_tweets_org <- head(frimi_org_sent[order(frimi_org_sent$sentiment),c(5,91)],100) # sorting text according to ascending order (default) of sentiment score and getting 1st 100 negative observations.

write.table(pos_tweets_org, file = "C:/Users/samud/Desktop/SMA/tweenet/corp/org/pos_tweets.txt", sep = "\n") #writing positive tweets as a text file to the local drive
write.table(neg_tweets_org, file = "C:/Users/samud/Desktop/SMA/tweenet/corp/org/neg_tweets.txt", sep = "\n") #writing negative tweets as a text file to the local drive

twts_corpus_org <- Corpus(DirSource(directory = "C:/Users/samud/Desktop/SMA/tweenet/corp/org")) # reading all files in the direct to a document corpus

# cleaning positive and negative tweets

cln_twts_corpus_org <- tm_map(twts_corpus_org, tolower) # transforming text to lower case
cln_twts_corpus_org <- tm_map(cln_twts_corpus_org, removePunctuation) # removing punctuation marks
cln_twts_corpus_org <- tm_map(cln_twts_corpus_org, removeWords, stopwords()) # removing stop words
cln_twts_corpus_org <- tm_map(cln_twts_corpus_org, removeNumbers)
cln_twts_corpus_org <- tm_map(cln_twts_corpus_org, stripWhitespace) # removing white space
cln_twts_corpus_org <- tm_map(cln_twts_corpus_org, stemDocument) # stemming
twt_tdm_org <- TermDocumentMatrix(cln_twts_corpus_org) # converting the corpus to a term document matrix

ctm_org <- as.matrix(twt_tdm_org) # converting the earlier to a standard matrix

colnames(ctm_org) <- c("Negative Tweets", "Positive Tweets") # Renaming the columns
head(ctm_org)
comparison.cloud(ctm_org, max.words = 100) # creating a comparison word cloud


#Sentiment analysis - replies (similar to organic tweet process)

frimi_rep_sent <- frimi_rep
sent_text_rep <- sentimentr::sentiment(frimi_rep_sent$text) 
frimi_rep_sent$sentiment <- sent_text_rep$sentiment 

pos_tweets_rep <- head(frimi_rep_sent[order(frimi_rep_sent$sentiment, decreasing = TRUE),c(5,91)],100) 
neg_tweets_rep <- head(frimi_rep_sent[order(frimi_rep_sent$sentiment),c(5,91)],100)

write.table(pos_tweets_rep, file = "C:/Users/samud/Desktop/SMA/tweenet/corp/rep/pos_tweets.txt", sep = "\n") 
write.table(neg_tweets_rep, file = "C:/Users/samud/Desktop/SMA/tweenet/corp/rep/neg_tweets.txt", sep = "\n") 

twts_corpus_rep <- Corpus(DirSource(directory = "C:/Users/samud/Desktop/SMA/tweenet/corp/rep")) 

# cleaning positive and negative tweets

cln_twts_corpus_rep <- tm_map(twts_corpus_rep, tolower) 
cln_twts_corpus_rep <- tm_map(cln_twts_corpus_rep, removePunctuation) 
cln_twts_corpus_rep <- tm_map(cln_twts_corpus_rep, removeWords, stopwords()) 
cln_twts_corpus_rep <- tm_map(cln_twts_corpus_rep, removeNumbers)
cln_twts_corpus_rep <- tm_map(cln_twts_corpus_rep, stripWhitespace)
cln_twts_corpus_rep <- tm_map(cln_twts_corpus_rep, stemDocument) 
twt_tdm_rep <- TermDocumentMatrix(cln_twts_corpus_rep)

ctm_rep <- as.matrix(twt_tdm_rep) 

colnames(ctm_rep) <- c("Negative Tweets", "Positive Tweets") 
head(ctm_rep)
comparison.cloud(ctm_rep, max.words = 100) 

#Sentiment analysis - retweets  (similar to organic tweet process)

frimi_ret_sent <- frimi_retweets
sent_text_ret <- sentimentr::sentiment(frimi_ret_sent$text) 
frimi_ret_sent$sentiment <- sent_text_ret$sentiment 

pos_tweets_ret <- head(frimi_ret_sent[order(frimi_ret_sent$sentiment, decreasing = TRUE),c(5,91)],100)
neg_tweets_ret <- head(frimi_ret_sent[order(frimi_ret_sent$sentiment),c(5,91)],100) 

write.table(pos_tweets_ret, file = "C:/Users/samud/Desktop/SMA/tweenet/corp/ret/pos_tweets.txt", sep = "\n")
write.table(neg_tweets_ret, file = "C:/Users/samud/Desktop/SMA/tweenet/corp/ret/neg_tweets.txt", sep = "\n") 

twts_corpus_ret <- Corpus(DirSource(directory = "C:/Users/samud/Desktop/SMA/tweenet/corp/ret"))

# cleaning positive and negative tweets

cln_twts_corpus_ret <- tm_map(twts_corpus_ret, tolower) 
cln_twts_corpus_ret <- tm_map(cln_twts_corpus_ret, removePunctuation) 
cln_twts_corpus_ret <- tm_map(cln_twts_corpus_ret, removeWords, stopwords())
cln_twts_corpus_ret <- tm_map(cln_twts_corpus_ret, removeNumbers)
cln_twts_corpus_ret <- tm_map(cln_twts_corpus_ret, stripWhitespace) 
cln_twts_corpus_ret <- tm_map(cln_twts_corpus_ret, stemDocument) 
twt_tdm_ret <- TermDocumentMatrix(cln_twts_corpus_ret)

ctm_ret <- as.matrix(twt_tdm_ret) 

colnames(ctm_ret) <- c("Negative Tweets", "Positive Tweets")
head(ctm_ret)
comparison.cloud(ctm_ret, max.words = 100) 

