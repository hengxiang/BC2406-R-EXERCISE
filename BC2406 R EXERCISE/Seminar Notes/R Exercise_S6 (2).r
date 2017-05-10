## Assignment 1 ##
rm(list=ls())

## Step 2 ##
Apps <- read.csv("07192013Apps.csv")


## Step 3 ## 
summary(Apps[,c('Rank','Price','Screenshots','Rating_Score','Rating_Num', 'Business', 'Finance', 'Games', 'Health', 'Utilities')])

# Variable Transformation #
Sales <- -log(Apps$Rank)
Log_Rating_Num <- log(Apps$Rating_Num+1)

Apps <-cbind(Apps, Sales)
Apps <-cbind(Apps, Log_Rating_Num)
head(Apps) 
summary(Apps[,c('Sales', 'Log_Rating_Num')])

# Correlation Matrix #
# Lower Triangular Part only #
# Up to three decimal digits #
corr<- round(cor(Apps[,c('Sales','Price','Screenshots','Rating_Score','Log_Rating_Num', 'Business', 'Finance', 'Games', 'Health', 'Utilities')]), 3)
corr[upper.tri(corr)] <- NA; corr


## Steps 5 & 6 ##
Apps_Reg <- lm(Sales ~ Price+ Screenshots + Rating_Score + Log_Rating_Num + Business + Finance + Health + Utilities, data=Apps)
summary(Apps_Reg)







