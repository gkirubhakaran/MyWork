install.packages("twitteR")
install.packages("RCurl")
install.packages("tm")
install.packages("wordcloud")
install.packages("plyr")
install.packages("stringr")
install.packages("caret")
install.packages("leaflet")

library(twitteR)
library(RCurl)
library(tm)
library(wordcloud)
library(plyr)
library(stringr)
library(caret)
library(leaflet)

#Authenticate access to twitter account/api and save teh oauth cert file

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")


requestURL <-  "https://api.twitter.com/oauth/request_token"
accessURL <-  "https://api.twitter.com/oauth/access_token"
authURL      <-  "https://api.twitter.com/oauth/authorize"
consKey <-  "XoFiEUM7YhH25a15vPxl8HeJz" # See my blog Part1
consSecret <-  "xMMHeWutaMOIIQEcc758Vi19N3bNMV6EDdCtr9rfYgLWUNuDAk" 
accesstok <- "1454299591-Hav6MB86JTdXQcxzYOhrQKAVr0bnHUH1cQMkSgc"
authsec <- "b8NgHdkLmyIjZbIE95XeZunzk0jsGkcBa4RnfdnRRqVSa"

#data()

Cred <- setup_twitter_oauth(consKey,consSecret,access_token = accesstok,access_secret = authsec)

# receive top trendingtweets
f1 = function(receivelat, receivelong)
{
  val=closestTrendLocations(receivelat,receivelong)
 #val=closestTrendLocations(41.6,-93.3)
  v=val$woeid
  cc<-getTrends(v)
  a <- cc$name
 #a
  b<-do.call("rbind",lapply(a,as.data.frame))
  colnames(b)<-"List of Top Trending"
  return(b)
 # return(a)
}

f2 = function(receivelat, receivelong)
{
  val=closestTrendLocations(receivelat,receivelong)
  v=val$woeid
  cc<-getTrends(v)
  a=searchTwitter(cc$name[1])
  b<-do.call("rbind",lapply(a,as.data.frame))
  return(b)
}

#create word corpus for the word cloud
getTermMatrix = function(receivelat,receivelong) {
  ctl=closestTrendLocations(receivelat,receivelong)
  cc<-getTrends(ctl[3])
  cc_1<-searchTwitter( cc$name[1],n = 100,lang="en")
  
  tw.df=twListToDF(cc_1)
  
  CleanData <- function(tweet) {
    
    tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweet)
    tweet = gsub("@\\w+", "", tweet)
    tweet = gsub("[[:punct:]]", "", tweet)
    tweet = gsub("[[:digit:]]", "", tweet)
    tweet = gsub("http\\w+", "", tweet)
    tweet = gsub("[ \t]{2,}", "", tweet)
    tweet = gsub("^\\s+|\\s+$", "", tweet)
    tweet = gsub("amp", "", tweet)
    tweet = gsub("\n", "", tweet)
    tweet = gsub("^ ", "", tweet)
    tweet = gsub("$", "", tweet)
    
  }
  
  #Then  remove @'d names
  tw.df <- as.vector(sapply(tw.df$text, CleanData))
 
  #Install the textmining library
  require(tm)
  
  tw.corpus= Corpus(VectorSource(tw.df))
  
  tw.corpus = tm_map(tw.corpus, removePunctuation)
  tw.corpus = tm_map(tw.corpus, removeNumbers)
  tw.corpus = tm_map(tw.corpus, content_transformer(tolower))
  
  # remove stopwords
  tw.corpus = tm_map(tw.corpus, removeWords, stopwords('english'))
  #tw.corpus = tm_map(tw.corpus, removeWords, stopwords())
  tw.corpus = tm_map(tw.corpus, removeWords,c(substring(cc$name[1], 2),"http"))
  myDTM = TermDocumentMatrix(tw.corpus,
                             control = list(minWordLength=1))
  
  
  
  m = as.matrix(myDTM)
  v=sort(rowSums(m), decreasing = TRUE)
  d = data.frame(word=names(v),freq=v)
  return(d)
}






