rm(list=ls())
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(httr)
library(wordcloud)
library(sentimentr)
library(SentimentAnalysis)
library(rtweet)
library(corpus)
api_key <- "################"
api_secret_key <- "###############"
access_token <- "###################"
access_token_secret <- "##################"
setup_twitter_oauth(api_key, api_secret_key, access_token, access_token_secret)
Yestoken <- create_token(
  app = "Nonename",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)
library(RCurl)
library(syuzhet)
#-------------------------------------------------------
some_tweets<-searchTwitter("trump",n=2000,since="2019-09-01",lang="en")
length.some_tweets<-length(some_tweets)
#-----------------------------------------------------------
some_tweets.df<-ldply(some_tweets,function(t) t$toDataFrame())
write.csv(some_tweets.df,"D:/Coursera/trump_tweets.csv")
#-----------------------------------------------------------
###getting the data(stripping the text out)
some_txt<-sapply(some_tweets, function(x) x$getText())
#----------------------------------------------------------
#cleaning the text using gsub, remove people name, rt text etc
#gsub is a text cleaning function in r
#will find a particular pattern and will substitute with "" aka nothign and apply to all the texts

some_txt1<-gsub("(RT|via)((?:\\b\\W*A\\w+)+)","",some_txt)
#removing html tags
some_txt2<-gsub("http[^[:blank:]]+","",some_txt1)
#removing poeple name
some_txt3<-gsub("@\\w+","",some_txt2)
#remove punctuations
some_txt4<-gsub("[[:punct:]]"," ",some_txt3)
some_txt5<-gsub("[^[:alnum:]]"," ",some_txt4)
#------------------------------------------------------------
#exporting to excel
write.csv(some_txt5,"D:/Coursera/trumptweets1.csv")
#------------------------------------------------------------
#creating wordcorpus and cleaning
some_txt6<-Corpus(VectorSource(some_txt5))
library(tm)
library(SnowballC)
some_txt6<-tm_map(some_txt6,removePunctuation)
some_txt6<-tm_map(some_txt6, content_transformer(tolower))
some_txt6<-tm_map(some_txt6, removeWords,stopwords("english"))
some_txt6<-tm_map(some_txt6, stripWhitespace)
#------------------------------------------------------------
#building wordcloud
#defining palette
pal<-brewer.pal(8,"Dark2")
#consider only if occurs minimum 5 times
wordcloud(some_txt6, min.freq = 5, max.words = Inf, width=1000, height=1000, random.order = FALSE,col=pal)
#-------------------------------------------------------------
#sentiment analysis
mysentiment<-get_nrc_sentiment(some_txt5)
#sum of columns
SentimentScores<- data.frame(colSums(mysentiment[,]))
names(SentimentScores)<-"Scores"
#making another column
SentimentScores<-cbind("sentiment"=rownames(SentimentScores),SentimentScores)
rownames(SentimentScores)<-NULL
##############################_-------------------recheck
ggplot(data=SentimentScores, aes(x=sentiment, y=Scores))+
  geom_bar(aes(fill=sentiment), stat="identity")+
  theme(legend.position = "none")+
  xlab("Sentiment")+ylab("Score")+
  
  ggtitle("The sentiment score based on the tweets")
