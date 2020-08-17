install.packages("twitteR")
install.packages("ROAuth")
install.packages("base64enc")
install.packages("httpuv")
library(twitteR)
library(ROAuth)
library(base64enc)
library(httpuv)

cred <- OAuthFactory$new(consumerKey='HSJGohgbWfe4jJJ73nO6Q41zk',
                         consumerSecret='Y2sdfoMBSmGIjTSeamejqmL6njetxfafwNTIJBETmwWPVZdpCd', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")
setup_twitter_oauth("HSJGohgbWfe4jJJ73nO6Q41zk", # Consumer Key (API Key)
                    "Y2sdfoMBSmGIjTSeamejqmL6njetxfafwNTIJBETmwWPVZdpCd", #Consumer Secret (API Secret)
                    "725669652899176448-xroZbOUUGtJ86FrKluem6FoNpCwg6D6",  # Access Token
                    "xJzgyobIajAg0xo7bvMy3LzK7PaGVXRr7B1yfjUVos76Y")  #Access Token Secret
Tweets <- userTimeline('@BillGates', n = 1000)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)
write.csv(TweetsDF,"TweetsBG.csv")
getwd()

x=read.csv(file.choose())
View(x)
x1=x[2]
View(x1)
write.table(x1,"TweetsBg1.txt",row.names = F)
getwd()
x2=as.character(x1)
View(x2)
library(tm)
install.packages("SnowballC")
library("SnowballC")
library("wordcloud")
install.packages("RColorBrewer")
library("RColorBrewer")
x2=Corpus(VectorSource(x2))
inspect(x2[1])
#Data cleaning
x3=tm_map(x2,tolower)
x3=tm_map(x3,removePunctuation)
x3=tm_map(x3,removeNumbers)
x3=tm_map(x3,removeWords,stopwords("english"))
x3 = tm_map( x3, removeWords, "(f|ht)tp(s?)://(.*)[.][a-z]+")
inspect(x3[1])

#striping white space
x3=tm_map(x3,stripWhitespace)
inspect(x3[1])

#Term document matrix
#converting unstructured data into structured format using TDM
tdm=TermDocumentMatrix(x3)
#dtm=t(tdm)
tdm=as.matrix(tdm)
tdm

#Bar plot
w=rowSums(tdm)
w
w_sub=subset(w,w>=20)
w_sub
barplot(w_sub,las=3,col = rainbow(20))

#Term which repeats in allmost all documetns
x3=tm_map(x3,removeWords,c("s","m","...","can","cant","ever","every","like","make","new","one","will","see","re","ve","books"))
x3=tm_map(x3,removePunctuation)
x3=tm_map(x3,stripWhitespace)
tdm=TermDocumentMatrix(x3)
tdm=as.matrix(tdm)
w1=rowSums(tdm)
w1
w_sub1=subset(w1,w1>=20)
w_sub1
barplot(w_sub1,las=3,col = rainbow(30))



# Bi gram word clouds
#install.packages("quanteda")
library(quanteda)
#install.packages("Matrix")
library(Matrix)

# Bi gram document term frequency 
dtm0_2 <- dfm(unlist(x3),ngrams=3,verbose = F)
tdm0_2 <- t(dtm0_2)
a0 = NULL
for (i1 in 1:ncol(tdm0_2)){ if (sum(tdm0_2[, i1]) == 0) {a0 = c(a0, i1)} }
length(a0)		# no. of empty docs in the corpus
if (length(a0) >0) { tdm0_2 = tdm0_2[, -a0]} else {tdm0_2 = tdm0_2};	dim(tdm0_2)	
a0 <- NULL;i1 <- NULL

dtm0_2 <- t(tdm0_2)
makewordc = function(x3){	
  freq = sort(rowSums(as.matrix(x3)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  windows()
  wordcloud(freq.df$word[1:120], freq.df$freq[1:120],scale = c(4,.5),random.order = F, colors=1:10)
} 
# Bi gram word cloud
makewordc(tdm0_2) # We have too see warnings to edit few words
title(sub = "BIGRAM - Wordcloud using TF")

words_bar_plot <- function(x3){
  freq = sort(rowSums(as.matrix(x3)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  head(freq.df, 20)
  library(ggplot2)
  windows()
  ggplot(head(freq.df,50), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Words") + ylab("Frequency") +
    ggtitle("Most frequent words")
}

# Bi gram barplot on TF
words_bar_plot(tdm0_2)



