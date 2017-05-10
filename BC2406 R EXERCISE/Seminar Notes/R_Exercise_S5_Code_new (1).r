#### R Exercise_S5 ####
rm(list=ls())

## Data Import ##
Apps_data <- read.csv('Apps.csv')
Ratings_data <- read.csv('Apps_Ratings.csv')

## Merging Datasets ##
head(Apps_data)
head(Ratings_data)
Apps <- merge(Apps_data, Ratings_data, by='AppID')
head(Apps)

##Understand Data##
str(Apps)
summary(Apps[,c('Rank','Price','Screenshots','Rating_Score','Rating_Num')])

## Variable Conversion ##
class(Apps$Title)
Apps$Title <- as.character(Apps$Title)
str(Apps)

## Missing Values ##
# Identify Missing Values # 
missing <- subset(Apps, is.na(Apps$Price) == 'TRUE'); missing
nrow(missing)

# Impute Missing Values # 
for (i in 1:nrow(Apps)) {
  ifelse(is.na(Apps$Price[i]) == 'TRUE', Apps$Price[i] <-mean(Apps$Price, na.rm=TRUE), Apps$Price[i])
}

# Alternative#
Apps$Price[is.na(Apps$Price)] <- mean(Apps$Price,na.rm = TRUE)


## Handling Outliers ##
# Identify outliers #
Mean_Num <- mean(Apps$Rating_Num)
SD_Num <- sd(Apps$Rating_Num)

Upper <- Mean_Num+3*SD_Num 
Lower <- Mean_Num -3*SD_Num # This is a negative number, but Rating_Num >= 0

Outliers <- subset(Apps, Rating_Num > Upper | Rating_Num < Lower)
nrow(Outliers)

# Remove Outliers #
for (i in 1:nrow(Apps)) {
  ifelse(Apps$Rating_Num[i] > Upper, Apps$Rating_Num[i] <- NA, Apps$Rating_Num[i])
}

# Alternative#
Apps$Rating_Num[Apps$Rating_Num > Upper] <- NA


# Identify Missing Values #
missing <- subset(Apps, is.na(Apps$Rating_Num == 'TRUE'))
nrow(missing)

summary(Apps[,c('Rank','Price','Screenshots','Rating_Score','Rating_Num')])

## Normalization ##
Normalize <- function(x) {return((x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE) - min(x, na.rm=TRUE)))}

Apps$Rating_Num <-Normalize(Apps$Rating_Num)

# Alternative#
Apps$Rating_Num[!is.na(Apps$Rating_Num)] <- Normalize(Apps$Rating_Num[!is.na(Apps$Rating_Num)])

summary(Apps[,c('Rank','Price','Screenshots','Rating_Score','Rating_Num')])




## Variable Transformation
Sales <- -log(Apps$Rank)
Apps <-cbind(Apps, Sales)
head(Apps)


## Data Summary ##
# Summary Statistices #
summary(Apps[,c('Rank', 'Sales','Price','Screenshots','Rating_Score','Rating_Num')])

# A Correlation Matrix with variables having missing values #
cor(Apps[,c('Rank', 'Sales','Price','Screenshots','Rating_Score','Rating_Num')], use="complete.obs")


######### Visialization ###############
#install.packages('ggplot2')
library(ggplot2)

## Plot ##
plot(Apps$Rank,Apps$Sales)

## Histogram ##
ggplot(Apps, aes(factor(Rating_Score))) +
  geom_bar(stat = 'count', width =0.3) +
  ggtitle('User Review Socres')+
  xlab('Review Score')+ ylab('Num of Apps')



## Scatter Plots ##
ggplot(Apps, aes(x=Price , y=Sales )) +
  geom_point()
  ggtitle('Sales and Price')
  
## Matrix Plot ##
#install.packages('ggcorrplot')
library(ggcorrplot)
corr <- cor(Apps[,c('Sales','Price','Rating_Score','Rating_Num')], use="complete.obs")
ggcorrplot(corr, type="lower", lab=TRUE)


## Data Export ##
write.csv(Apps, file="Apps_new.csv", row.names=FALSE)
             
             