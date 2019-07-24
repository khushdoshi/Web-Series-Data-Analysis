#1.	First the data is chosen or read in the system using a built in function.

library(MASS)
datamining<-read.csv(choose.files(),header=TRUE)

#2.   The N/A values that are in the data are omitted.
datamining[datamining=='NA']<-NA
datamining<-na.omit(datamining)


#3.   Creating the model
model<-lm(ratingDescription~user.rating.size+user.rating.score,data=datamining)
print(model)

#here, ratingDescription is the dependent or predicted variable

#4.  Finding the coefficients 
Xsize<-coef(model)[2]
Xrating<-coef(model)[3]
Xintercept<-coef(model)[1]
print(Xsize)
print(Xrating)
print(Xintercept)

#5.   Assigning values
user_rating_score<-(datamining$user.rating.score)
summary(user_rating_score)
user_rating_size<-(datamining$user.rating.size)
summary(user_rating_size)


serialname<-(datamining$title)
summary(serialname)


#6.   . Applying the equation of Regression Model for predicting New Values

Y=Xintercept+Xsize*user_rating_score[1]+Xrating*user_rating_size[1]
print(Y)


#7.  Defining class
#a)Class A:(Y value between 0 to 74.99):
Y=Xintercept+Xsize*user_rating_score[1]+Xrating*user_rating_size[1]

#b)Class B:(Y value betize[1]ween 75 to 79.99)
Y=Xintercept+Xsize*user_rating_score[1]+Xrating*user_rating_size[1]

#c)Class C:(Y values more then 80 )
Y=Xintercept+Xsize*user_rating_score[1]+Xrating*user_rating_size[1]


#8. code
for(i in 1:495)
{
  Y=167.10+(-1.5029)*user_rating_score[i]+(0.3516)*user_rating_size[i]
  if(Y>=0 && Y<75)
  {
    cat(sprintf("%s is less popular serial \n\n", serialname[i]));
  }
  else if(Y>=75 && Y<80)
  {
    cat(sprintf("%s is hit but not extreme popular serial\n\n", serialname[i]));
  }
  else
  {
    cat(sprintf("%s is extremly popular serial among viewers\n\n", serialname[i]));
  }
}
