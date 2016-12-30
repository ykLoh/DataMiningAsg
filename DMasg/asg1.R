setwd("/Users/natalie/Desktop/DMasg")

#read imdb file and convert blank cells to NA
movie <- read.csv("imdb.csv", header=T, na.strings=c("","NA"))

#remove rows with values 0 and 1
movie<-movie[movie$fn != 0,]
movie<-movie[movie$fn != 1,]

#remove rows with tv series
movie<-movie[movie$type == "video.movie",]

#remove column fn and tid
movie$fn <- NULL
movie$tid <- NULL
movie$wordsInTitle <- NULL

#convert year from factor to numeric
movie$year <- as.numeric(as.character(movie$year))

#remove rows before 2000
movie<-movie[movie$year > 2000,]

#remove rows with null values
movie<-movie[complete.cases(movie),]

#convert imdbRating from factor to numeric
movie$imdbRating <- as.numeric(as.character(movie$imdbRating))

#classify imdbRating into new column Recommendation
movie$Recommendation[movie$imdbRating>=0 & movie$imdbRating<3.0]<-"Poor"
movie$Recommendation[movie$imdbRating>=3.0 & movie$imdbRating<5.0]<-"Fair"
movie$Recommendation[movie$imdbRating>=5.0 & movie$imdbRating<7.0]<-"Average"
movie$Recommendation[movie$imdbRating>=7.0 & movie$imdbRating<8.0]<-"Good"
movie$Recommendation[movie$imdbRating>=8.0 & movie$imdbRating<=10]<-"Excellent"

#remove year and strings in the brackets from title 
movie$title<-gsub("\\s\\(.....*","",movie$title)

#remove noisy data in the title
movie$title<-gsub("\\Ã"," ",movie$title)
movie$title<-gsub("\\¼"," ",movie$title)
movie$title<-gsub("\\¤"," ",movie$title)
movie$title<-gsub("\\¶"," ",movie$title)
movie$title<-gsub("\\Ÿ"," ",movie$title)
movie$title<-gsub("\\!:"," ",movie$title)
movie$title<-gsub("\\©"," ",movie$title)
movie$title<-gsub("\\ª"," ",movie$title)
movie$title<-gsub("\\»"," ",movie$title)
movie$title<-gsub("\\..."," ",movie$title)
movie$title<-gsub("\\·"," ",movie$title)
movie$title<-gsub("\\+"," ",movie$title)
movie$title<-gsub("\\â"," ",movie$title)
movie$title<-gsub("\\¨"," ",movie$title)
movie$title<-gsub("\\®"," ",movie$title)
movie$title<-gsub("\\´"," ",movie$title)
movie$title<-gsub("\\§"," ",movie$title)
movie$title<-gsub("\\ "," ",movie$title)
movie$title<-gsub("\\(|\\)"," ",movie$title)
movie$title<-gsub("\\ "," ",movie$title)

library(ggplot2)
library(plyr)
recommendation = count(movie, 'Recommendation')
recommendation <- recommendation[with(recommendation, order(-freq)),]
recommendation <- head(recommendation,5)
recommendation.count <- table(movie$imdbRating)

p<-ggplot(data=recommendation, aes(x=Recommendation, y=freq, fill=Recommendation)) +
  geom_bar(stat="identity") + labs(title="Number of Movies for Each Recommendation", 
                                    x="Recommendation", y = "Number of Movies") +
  geom_text(aes(label=freq), color="black", hjust=0.5, size=3) 
p + scale_x_discrete(limits=c("Excellent", "Good", "Average", "Fair", "Poor"))

p<-ggplot(movie, aes(x=year, y=imdbRating, fill=year)) + geom_bar(stat="identity") + 
  labs(title="Number of Movie Ratings from Year 2001 to 2014", 
       x="Year", y="Number of Ratings")
p

year = count(movie, 'year')
year <- year[with(year, order(-freq)),]
year <- head(year,14)
year.count <- table(movie$title)

p<-ggplot(data=year, aes(x=year, y=freq, fill=year)) +
  geom_bar(stat="identity") + labs(title="Number of Movies Produced from Year 2001 to 2014", 
                                     x="Year", y = "Number of Movies")
p

#save file
write.csv(movie,"movie.csv")