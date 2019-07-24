#Sentimental Analysis
#  Install Requried Packages
install.packages("SnowballC")         #(used for putting all the workds in one place for better vocabulary comparision)
install.packages("tm")                #(framework used for text mining)
install.packages("twitteR")
install.packages("syuzhet")           #(contains 4 sentiment dictionaries)

# Load Requried Packages
library("SnowballC")
library("tm")	
library("twitteR")
library("syuzhet")
library(MASS)

#Choosing the netflix shows csv file
datamining<-read.csv(choose.files(),header=TRUE)

#Head means top 6 rows
head(datamining$ratingLevel)

#this is calculate something please check internet
word.df <- as.vector(datamining$ratingLevel)
emotion.df <- get_nrc_sentiment(word.df)            #Calls the NRC sentiment dictionary to calculate the presence of eight different emotions and their corresponding valence in a text file.
emotion.df2 <- cbind(datamining$ratingLevel, emotion.df) 
head(emotion.df2)
sent.value <- get_sentiment(word.df)

#calculating most positive accoding to the vaue
most.positive <- word.df[sent.value == max(sent.value)]
most.positive

most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative


positive.review <- word.df[sent.value > 1]
head(positive.review)
positive.review


negative.review <- word.df[sent.value < 0] 
head(negative.review)
negative.review

neutral.review <- word.df[sent.value == 0] 
head(neutral.review)

# Alternate way to classify as Positive, Negative or Neutral reviews

category_senti <- ifelse(sent.value < 1, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
head(category_senti)
category_senti

ratinglevel<-(datamining$ratingLevel)
category_senti2 <- cbind.data.frame(ratinglevel,category_senti,sent.value) 
category_senti2


