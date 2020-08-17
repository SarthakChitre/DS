library(rvest)
library(XML)
library(magrittr)

movies_reviews=NULL
murl="https://www.imdb.com/title/tt2713180/reviews?ref_=tt_urv"
a=10
for(i in 0:22){
  curl <- read_html(as.character(paste(murl,i*a,sep="")))
  rev <- curl %>%
    html_nodes("tn15content div+ p") %>%
    html_text()
  movies_reviews <- c(movies_reviews,rev)
}
write.table(movies_reviews,"Fury.txt",row.names = F)
getwd()

x=as.character(movies_reviews)
#Corpus
library(tm)
x=Corpus(VectorSource(x))
x1=tm_map(x,tolower)
x1=tm_map(x1,removePunctuation)
x1=tm_map(x1,removeNumbers)
x1=tm_map(x1,removeWords,stopwords("english"))
inspect(x1[1])

#striping white space
x1=tm_map(x1,stripWhitespace)
inspect(x1[1])

#Term document matrix
#converting unstructured data into structured format using TDM
tdm=TermDocumentMatrix(x1)
dtm=t(tdm)
tdm=as.matrix(tdm)
tdm

#Bar plot
w=rowSums(tdm)
w
w_sub=subset(w,w>=25)
w_sub
barplot(w_sub,las=3,col = rainbow(20))

#Term which repeats in allmost all documetns
x1=tm_map(x1,removeWords,c("can","even","feel","mind","keep","sure","end","much","though","well","isnt","say","wants"))
x1=tm_map(x1,stripWhitespace)
tdm=TermDocumentMatrix(x1)
tdm=as.matrix(tdm)
w1=rowSums(tdm)
w1
w_sub1=subset(w1,w>=25)
w_sub1
barplot(w_sub1,las=3,col = rainbow(20))


#Word Cloud
install.packages("wordcloud")
library(wordcloud)
wordcloud(words=names(w1),freq=w1)
w_sub1=sort(rowSums(tdm),decreasing = TRUE)
w_sub1
wordcloud(words = names(w_sub1), freq = w_sub1, 
          random.order = F, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(1.5,0.5))

#wordcloud(words=names(w_sub1),freq=w_sub1)

#Loading postive and negative dictionaries
pos.words=scan(file.choose(),what="character",comment.char=";")
neg.words=scan(file.choose(),what="character",comment.char=";")

#postive wordcloud
pos.matches=match(names(w_sub1), c(pos.words))
pos.matches=!is.na(pos.matches)
freq_pos=w_sub1[pos.matches]
p_names=names(freq_pos)
wordcloud(p_names,freq_pos,scale=c(3,0.5),colors=rainbow(20))


#negative wordcloud
neg.matches=match(names(w_sub1), c(neg.words))
neg.matches=!is.na(neg.matches)
freq_neg=w_sub1[neg.matches]
n_names=names(freq_neg)
wordcloud(n_names,freq_neg,scale=c(2,1.0),colors=rainbow(20))

# Bi gram word clouds
#install.packages("quanteda")
library(quanteda)
#install.packages("Matrix")
library(Matrix)

# Bi gram document term frequency 
dtm0_2 <- dfm(unlist(x1),ngrams=3,verbose = F)
tdm0_2 <- t(dtm0_2)
a0 = NULL
for (i1 in 1:ncol(tdm0_2)){ if (sum(tdm0_2[, i1]) == 0) {a0 = c(a0, i1)} }
length(a0)		# no. of empty docs in the corpus
if (length(a0) >0) { tdm0_2 = tdm0_2[, -a0]} else {tdm0_2 = tdm0_2};	dim(tdm0_2)	
a0 <- NULL;i1 <- NULL

dtm0_2 <- t(tdm0_2)
makewordc = function(x1){	
  freq = sort(rowSums(as.matrix(x1)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  windows()
  wordcloud(freq.df$word[1:120], freq.df$freq[1:120],scale = c(4,.5),random.order = F, colors=1:10)
} 
# Bi gram word cloud
makewordc(tdm0_2) # We have too see warnings to edit few words
title(sub = "BIGRAM - Wordcloud using TF")

words_bar_plot <- function(x1){
  freq = sort(rowSums(as.matrix(x1)),decreasing = TRUE)
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


#Emotion mining
y=readLines(file.choose())
gs=get_sentences(y)

install.packages("syuzhet")
library("syuzhet")

sentiment_vector <- get_sentiment(gs, method = "bing")


afinn_s_v <- get_sentiment(gs, method = "afinn")


sum(sentiment_vector)

# plot
plot(sentiment_vector, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")


ft_values <- get_transformed_values(
  sentiment_vector, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values, 
  type ="h", 
  main ="LOTR using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

# To extract the sentence with the most negative emotional valence
negative <- gs[which.min(sentiment_vector)]
negative

# and to extract the most positive sentence
positive <- gs[which.max(sentiment_vector)]
positive

# categorize each sentence by eight emotions
###This might take time####
nrc_data <- get_nrc_sentiment(gs)



# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data[, 1:8]))), horiz = T, cex.names = 0.7,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:8)
