rm(list=ls())


##Activate Packages##
library(NLP)
library(tm)
library(SnowballC)
library(wordcloud)
library(stats)


### Text-Mining ###
##Data Loading##
Movies_data <- read.csv("BC2406-S02-G04-Data.csv", header=T, na.strings="")
# Remove Records that do not have at least 1 plot keywords#
Movies_data <- Movies_data[!(is.na(Movies_data$plot_keywords)),]
# Remove Duplicated Records#
Movies_data <- Movies_data[!duplicated(Movies_data$movie_title),]


Movies_plot <-Movies_data[,17] #plotkeywords recorded in the 17th column
n_plot <- length(Movies_plot) #Check the number of Movie plotkeywords
n_plot


## Convert Vector/List to Corpus##
Movies <- Corpus(VectorSource(Movies_plot))
Movies
Movies[[1]]
as.character(Movies[[1]])


## Remove | ##
for (j in 1:n_plot) Movies[[j]] <- gsub("\\|"," ", Movies[[j]]) #|
as.character(Movies[[1]])


# Convert phrase to words/terms #
for (j in 1:n_plot) Movies[[j]] <- gsub("new york","newyork", Movies[[j]])
for (j in 1:n_plot) Movies[[j]] <- gsub("new jersey","newjersey", Movies[[j]])
for (j in 1:n_plot) Movies[[j]] <- gsub("box office hit","boxofficehit", Movies[[j]])
for (j in 1:n_plot) Movies[[j]] <- gsub("box office flop","boxofficeflop", Movies[[j]])
for (j in 1:n_plot) Movies[[j]] <- gsub("high school","highschool", Movies[[j]])
for (j in 1:n_plot) Movies[[j]] <- gsub("word title","wordtitle", Movies[[j]])
# Remove less important terms: title-related terms #
for (j in 1:n_plot) Movies[[j]] <- gsub("wordtitle","", Movies[[j]]) #wordtitle
for (j in 1:n_plot) Movies[[j]] <- gsub("title","", Movies[[j]]) #title
# Remove less important terms: movie setting/core elements related terms #
for (j in 1:n_plot) Movies[[j]] <- gsub("american","", Movies[[j]]) #american
for (j in 1:n_plot) Movies[[j]] <- gsub("newyork","", Movies[[j]]) #newyork
for (j in 1:n_plot) Movies[[j]] <- gsub("newjersey","", Movies[[j]]) #newjersey
for (j in 1:n_plot) Movies[[j]] <- gsub("city","", Movies[[j]]) #city
for (j in 1:n_plot) Movies[[j]] <- gsub("highschool","", Movies[[j]]) #highschool
for (j in 1:n_plot) Movies[[j]] <- gsub("school","", Movies[[j]]) #school
for (j in 1:n_plot) Movies[[j]] <- gsub("character","", Movies[[j]]) #character
for (j in 1:n_plot) Movies[[j]] <- gsub("protagonist","", Movies[[j]]) #protagonist
for (j in 1:n_plot) Movies[[j]] <- gsub("student","", Movies[[j]]) #student
for (j in 1:n_plot) Movies[[j]] <- gsub("teenage","", Movies[[j]]) #teenage
for (j in 1:n_plot) Movies[[j]] <- gsub("animation","", Movies[[j]]) #animation
for (j in 1:n_plot) Movies[[j]] <- gsub("train","", Movies[[j]]) #train


# Remove less important terms: movie origin-related terms#
for (j in 1:n_plot) Movies[[j]] <- gsub("novel","", Movies[[j]]) #novel
for (j in 1:n_plot) Movies[[j]] <- gsub("book","", Movies[[j]]) #book
for (j in 1:n_plot) Movies[[j]] <- gsub("based","", Movies[[j]]) #based
for (j in 1:n_plot) Movies[[j]] <- gsub("reference","", Movies[[j]]) #reference
# Remove less important terms: box-office related terms #
for (j in 1:n_plot) Movies[[j]] <- gsub("film","", Movies[[j]]) #film
for (j in 1:n_plot) Movies[[j]] <- gsub("boxofficehit","", Movies[[j]]) #boxofficehit
for (j in 1:n_plot) Movies[[j]] <- gsub("boxofficeflop","", Movies[[j]]) #boxofficeflop
for (j in 1:n_plot) Movies[[j]] <- gsub("movie","", Movies[[j]]) #movie
# Remove less important terms: gender-related terms #
for (j in 1:n_plot) Movies[[j]] <- gsub("female","", Movies[[j]]) #female
for (j in 1:n_plot) Movies[[j]] <- gsub("male","", Movies[[j]]) #male
for (j in 1:n_plot) Movies[[j]] <- gsub("boy","", Movies[[j]]) #boy
for (j in 1:n_plot) Movies[[j]] <- gsub("girl","", Movies[[j]]) #girl
for (j in 1:n_plot) Movies[[j]] <- gsub("man","", Movies[[j]]) #man
for (j in 1:n_plot) Movies[[j]] <- gsub("woman","", Movies[[j]]) #woman


# Remove stopwords #
Movies <- tm_map(Movies, removeWords, stopwords("english"))
inspect(Movies[1:5])
newstopwords <- c("and","for","the","to","in","when","then","he","she","than","can","get","one");
Movies <- tm_map(Movies, removeWords, newstopwords)
inspect(Movies[1:24])


# Remove Extra White Space #
Movies <- tm_map(Movies,stripWhitespace)
inspect(Movies[1:3])
as.character(Movies[[1]])


##Return the first 10 plotkeywords after parsing##
inspect(Movies[1:10])


#Stemming#
Movies <- tm_map(Movies, PlainTextDocument) #Remove common word endings ("es","ed","s","ing")
Movies <- tm_map(Movies,stemDocument)


##Present the first plotkeywords after stemming##
as.character(Movies[[1]])


# Original Document Term Matrix (DTM) Conver Texts to Number#
dtm_Movies <- DocumentTermMatrix(Movies)
dtm_Movies
#DTM with controls#
dtm_Movies_Ctrl <- DocumentTermMatrix(Movies, control=list(wordLength=c(3,20), bounds=list(global=c(50,1000))))
dtm_Movies_Ctrl
inspect(dtm_Movies_Ctrl) #Display DTM for the plotkeywords
inspect(dtm_Movies_Ctrl[1:10,1:10]) #DTM for the first 10 plotkeywords with the first 10 terms


#Find the terms that occur at least 70 times#
findFreqTerms(dtm_Movies_Ctrl,70)


#Frequency of terms#
Freq_term<- colSums(as.matrix(dtm_Movies_Ctrl))
Order_Freq_term <- order(Freq_term, decreasing = TRUE)
Freq_term[Order_Freq_term]


#Frequency Diagram#
library(grDevices) #for colours
Movies_DTM_DF = as.data.frame(as.matrix(dtm_Movies_Ctrl))
numwords <- 30; #the most frequent 30 terms


#sum each column and sort by decreasing order#
Terms_Freq <- as.matrix(sort(sapply(Movies_DTM_DF, FUN=sum), decreasing=TRUE)[1:numwords],colnames=count)
x <- sort(Terms_Freq[1:numwords,], decreasing = FALSE)
barplot(x, horiz=TRUE, cex.names = 0.5, space=1, las=1, col=grey.colors(10),main="Frequency of Terms")


#For Original DTM#
set.seed(2406) #set the same seed each time ensures consistent look across clouds
m <- as.matrix(t(dtm_Movies)) # Convert it to a matrix
v <- sort(rowSums(m),decreasing = TRUE) #Sort the terms in a decreasing order
w <- data.frame(word = names(v),freq=v)
WC_colour <- brewer.pal(8,"Set2")
wordcloud(w$word,w$freq, scale=c(3,.1), min.freq=1,max.words = 200,random.order = F,rot.per=.3,colors = WC_colour)


#For DTM with Controls#
dev.off()
set.seed(2406) #set the same seed each time ensures consistent look across clouds
m <- as.matrix(t(dtm_Movies_Ctrl)) # Conver it to a matrix
v <- sort(rowSums(m),decreasing = TRUE) #Sort the terms in a decreasing order
w <- data.frame(word = names(v),freq=v)
WC_colour <- brewer.pal(8,"Set2")
wordcloud(w$word,w$freq, scale=c(3,.1), min.freq=1,max.words = 200,random.order = F,rot.per=.3,colors = WC_colour)


## Clustering ##
##Select the number of Terms for Clustering##
nrow(dtm_Movies_Ctrl);ncol(dtm_Movies_Ctrl) 
dtm_Movies_Ctrl
inspect(dtm_Movies_Ctrl)
dtm_Movies_cluster<-as.matrix(dtm_Movies_Ctrl)


### Two Other Models (K-Mean and Hierarchical Clustering) to compare the prediction performance###
##Select the number of Terms for Clustering##
dtm_Movies_Sparse <- removeSparseTerms(dtm_Movies_Ctrl,0.986)
#remove the terms which have at least a 98.6 percent of sparse
nrow(dtm_Movies_Sparse);ncol(dtm_Movies_Sparse) #27 Terms
dtm_Movies_Sparse
colnames(dtm_Movies_Sparse)
inspect(dtm_Movies_Sparse)


##K-means Clustering##
dtm_Movies_cluster1<-as.matrix(dtm_Movies_Sparse)
library(stats) #Clustering


#Model 1#
set.seed(1234)
Movies_KM<-kmeans(t(dtm_Movies_cluster1),3)
Movies_KM


Movies_KM$size
sort(Movies_KM$cluster)


#Model 2#
set.seed(2406)
Movies_KM2<-kmeans(t(dtm_Movies_cluster1),4)
Movies_KM2


Movies_KM2$size
sort(Movies_KM2$cluster)


##Hierarchical Clustering##
dtm_Movies_cluster1<-as.matrix(dtm_Movies_Sparse)


#Calculate the Distance between Terms#
distance<- dist(t(dtm_Movies_cluster1),method="euclidean")
distance


#Model 3#
Movies_HC1 <-hclust(distance,method="complete")
plot(Movies_HC1)


#Draw dendrogram with red borders around the 4 clusters#
rect.hclust(Movies_HC1,k=4,border="red")


Movies_HC_Cut<- cutree(Movies_HC1, k=4) #cut tree into 4 clusters
Movies_HC_Cut


sort(Movies_HC_Cut)


#Model 4#
Movies_HC2 <-hclust(distance,method="ward.D")
plot(Movies_HC2)


#Draw dendrogram with blue borders around the 4 clusters#
rect.hclust(Movies_HC2,k=4,border="blue")


Movies_HC_Cut2<- cutree(Movies_HC2, k=4) #cut tree into 4 clusters
Movies_HC_Cut2


sort(Movies_HC_Cut2)


###Regression ###
##Compute Cluster Scores##
#Identify the terms for each cluster#
cluster1 <- dtm_Movies_cluster[,c("nuditi","sex","frontal")]
cluster2 <- dtm_Movies_cluster[,c("love","friend","relationship","friendship","marriag","gay","father","parti","famili","dog")]
cluster3 <- dtm_Movies_cluster[,c("polic","death","murder","drug","car","war","fbi","prison","secret","agent","killer","reveng","detect","money")]
cluster4 <- dtm_Movies_cluster[,c("alien","time","travel")]


head(cluster1)
head(cluster2)
head(cluster3)
head(cluster4)


#Sums#
C1_Sum <- rowSums(cluster1)
C2_Sum <- rowSums(cluster2)
C3_Sum <- rowSums(cluster3)
C4_Sum <- rowSums(cluster4)


# Create a Score table#
Score <- matrix(data=0, n_plot,4);
Score[,1] <- as.matrix(C1_Sum)
Score[,2] <- as.matrix(C2_Sum)
Score[,3] <- as.matrix(C3_Sum)
Score[,4] <- as.matrix(C4_Sum)


# Name the Columns/Clusters #
colnames(Score) <- c("Cluster1", "Cluster2", "Cluster3","Cluster4")
head(Score)


## Add a Score matrix to the original Data##
Movies_new <- cbind(Movies_data,Score)
str(Movies_new)


## Variable Transformation ##
Movies_Age <- (2016-Movies_new$title_year) +1
Movies_Age
Movies_new <- cbind(Movies_new, Movies_Age)


## Missing Values ##
summary(Movies_new)
summary(Movies_new[,c('gross','imdb_score','num_critic_for_reviews','duration','num_voted_users','facenumber_in_poster','num_user_for_reviews','Movies_Age','movie_facebook_likes','Cluster1','Cluster2','Cluster3','Cluster4')])




# Replace num_critic_for_reviews Missing Value with Mean #
table(is.na(Movies_new$num_critic_for_reviews)) #26 Missing
for(i in 1:nrow(Movies_new)) {
  ifelse(is.na(Movies_new$num_critic_for_reviews[i]) == 'TRUE',Movies_new$num_critic_for_reviews[i] <-mean(Movies_new$num_critic_for_reviews, na.rm=TRUE), Movies_new$num_critic_for_reviews[i])
}
table(is.na(Movies_new$num_critic_for_reviews))


# Replace duration Missing Value with Mean #
table(is.na(Movies_new$duration)) #8 Missing
for(i in 1:nrow(Movies_new)) {
  ifelse(is.na(Movies_new$duration[i]) == 'TRUE',Movies_new$duration[i] <-mean(Movies_new$duration, na.rm=TRUE), Movies_new$duration[i])
}
table(is.na(Movies_new$duration))


# Replace facenumber_in_poster Missing Value with Mean #
table(is.na(Movies_new$facenumber_in_poster)) #11 Missing
for(i in 1:nrow(Movies_new)) {
  ifelse(is.na(Movies_new$facenumber_in_poster[i]) == 'TRUE',Movies_new$facenumber_in_poster[i] <-mean(Movies_new$facenumber_in_poster, na.rm=TRUE), Movies_new$facenumber_in_poster[i])
}
table(is.na(Movies_new$facenumber_in_poster))


# Replace Movies_Age Missing Value with Mean #
table(is.na(Movies_new$Movies_Age)) ##93 missing values
for(i in 1:nrow(Movies_new)) {
  ifelse(is.na(Movies_new$Movies_Age[i]) == 'TRUE',Movies_new$Movies_Age[i] <-mean(Movies_new$Movies_Age, na.rm=TRUE), Movies_new$Movies_Age[i])
}
table(is.na(Movies_new$Movies_Age))


# Replace num_user_for_reviews Missing Value with Mean #
table(is.na(Movies_new$num_user_for_reviews)) ##7 missing values
for(i in 1:nrow(Movies_new)) {
  ifelse(is.na(Movies_new$num_user_for_reviews[i]) == 'TRUE',Movies_new$num_user_for_reviews[i] <-mean(Movies_new$num_user_for_reviews, na.rm=TRUE), Movies_new$num_user_for_reviews[i])
}
table(is.na(Movies_new$num_user_for_reviews))


summary(Movies_new[,c('imdb_score','num_critic_for_reviews','duration','num_voted_users','facenumber_in_poster','num_user_for_reviews','Movies_Age','movie_facebook_likes','Cluster1','Cluster2','Cluster3','Cluster4')])


## Handling Outliers ##
# Identify & remove outliers: num_critic_for_reviews #
Mean1<- mean(Movies_new$num_critic_for_reviews)
SD1 <- sd(Movies_new$num_critic_for_reviews)
Upper1 <- Mean1+3*SD1
Lower1 <- Mean1 -3*SD1 # This is a negative number, but num_critic_for_reviews >= 0
Outliers1 <- subset(Movies_new, num_critic_for_reviews > Upper1 | num_critic_for_reviews < Lower1)
nrow(Outliers1)
Movies_new$num_critic_for_reviews[Movies_new$num_critic_for_reviews > Upper1] <- NA
summary(Movies_new$num_critic_for_reviews)


# Identify & remove outliers: duration #
Mean2 <- mean(Movies_new$duration)
SD2 <- sd(Movies_new$duration)
Upper2 <- Mean2+3*SD2
Lower2 <- Mean2 -3*SD2
Outliers2 <- subset(Movies_new, duration > Upper2 | duration < Lower2)
nrow(Outliers2)
Movies_new$duration[Movies_new$duration > Upper2] <- NA
Movies_new$duration[Movies_new$duration < Lower2] <- NA
summary(Movies_new$duration)


# Identify & remove outliers: num_voted_users #
Mean3<- mean(Movies_new$num_voted_users)
SD3 <- sd(Movies_new$num_voted_users)
Upper3 <- Mean3+3*SD3
Lower3 <- Mean3 -3*SD3 # This is a negative number, but num_voted_users >= 0
Outliers3 <- subset(Movies_new, num_voted_users > Upper3 | num_voted_users < Lower3)
nrow(Outliers3)
Movies_new$num_voted_users[Movies_new$num_voted_users > Upper3] <- NA
summary(Movies_new$num_voted_users)


# Identify & remove outliers: facenumber_in_poster #
Mean4<- mean(Movies_new$facenumber_in_poster)
SD4 <- sd(Movies_new$facenumber_in_poster)
Upper4 <- Mean4+3*SD4
Lower4 <- Mean4 -3*SD4 # This is a negative number, but facenumber_in_poster >= 0
Outliers4 <- subset(Movies_new, facenumber_in_poster > Upper4 | facenumber_in_poster < Lower4)
nrow(Outliers4)
Movies_new$facenumber_in_poster[Movies_new$facenumber_in_poster > Upper4] <- NA
summary(Movies_new$facenumber_in_poster)


# Identify & remove outliers: num_user_for_reviews #
Mean5<- mean(Movies_new$num_user_for_reviews)
SD5 <- sd(Movies_new$num_user_for_reviews)
Upper5 <- Mean5+3*SD5
Lower5 <- Mean5 -3*SD5 # This is a negative number, but num_user_for_reviews >= 0
Outliers5 <- subset(Movies_new, num_user_for_reviews > Upper5 | num_user_for_reviews < Lower5)
nrow(Outliers5)
Movies_new$num_user_for_reviews[Movies_new$num_user_for_reviews > Upper5] <- NA
summary(Movies_new$num_user_for_reviews)


# Identify & remove outliers: Movies_Age #
Mean6<- mean(Movies_new$Movies_Age)
SD6 <- sd(Movies_new$Movies_Age)
Upper6 <- Mean6+3*SD6
Lower6 <- Mean6 -3*SD6 # This is a negative number, but Movies_Age >= 0
Outliers6 <- subset(Movies_new, Movies_Age > Upper6 | Movies_Age < Lower6)
nrow(Outliers6)
Movies_new$Movies_Age[Movies_new$Movies_Age > Upper6] <- NA
summary(Movies_new$Movies_Age)


# Identify & remove outliers: movie_facebook_likes #
Mean7<- mean(Movies_new$movie_facebook_likes)
SD7 <- sd(Movies_new$movie_facebook_likes)
Upper7 <- Mean7+3*SD7
Lower7 <- Mean7 -3*SD7 # This is a negative number, but movie_facebook_likes >= 0
Outliers7 <- subset(Movies_new, movie_facebook_likes > Upper7 | movie_facebook_likes < Lower7)
nrow(Outliers7)
Movies_new$movie_facebook_likes[Movies_new$movie_facebook_likes > Upper7] <- NA
summary(Movies_new$movie_facebook_likes)


summary(Movies_new[,c('imdb_score','num_critic_for_reviews','duration','num_voted_users','facenumber_in_poster','num_user_for_reviews','Movies_Age','movie_facebook_likes','Cluster1','Cluster2','Cluster3','Cluster4')])




### Normalisation ###
# For: num_critic_for_reviews #
mean(Movies_new$num_critic_for_reviews, na.rm=TRUE)
sd(Movies_new$num_critic_for_reviews, na.rm=TRUE) # Since sd<mean, no normalisation required


# For: duration #
mean(Movies_new$duration, na.rm=TRUE)
sd(Movies_new$duration, na.rm=TRUE) # Since sd<mean, no normalisation required


# For: num_voted_users #
mean(Movies_new$num_voted_users, na.rm=TRUE)
sd(Movies_new$num_voted_users, na.rm=TRUE) # Since sd>mean, normalisation is required
Log_num_voted_users <-log(Movies_new$num_voted_users+1)
head(Log_num_voted_users)


# For: facenumber_in_poster #
mean(Movies_new$facenumber_in_poster, na.rm=TRUE)
sd(Movies_new$facenumber_in_poster, na.rm=TRUE) # Since sd>mean, normalisation is required
Log_facenumber_in_poster <-log(Movies_new$facenumber_in_poster+1)
head(Log_facenumber_in_poster)


# For: num_user_for_reviews #
mean(Movies_new$num_user_for_reviews, na.rm=TRUE)
sd(Movies_new$num_user_for_reviews, na.rm=TRUE) # Since sd>mean, normalisation is required
Log_num_user_for_reviews <-log(Movies_new$num_user_for_reviews+1)
head(Log_num_user_for_reviews)


# For: Movies_Age #
mean(Movies_new$Movies_Age, na.rm=TRUE)
sd(Movies_new$Movies_Age, na.rm=TRUE) # Since sd<mean, no normalisation required


# For: movie_facebook_likes #
mean(Movies_new$movie_facebook_likes, na.rm=TRUE)
sd(Movies_new$movie_facebook_likes, na.rm=TRUE) # Since sd>mean, normalisation is required
Log_movie_facebook_likes <-log(Movies_new$movie_facebook_likes+1)
head(Log_movie_facebook_likes)


Movies_new <- cbind(Movies_new, Log_num_voted_users)
Movies_new <- cbind(Movies_new, Log_facenumber_in_poster)
Movies_new <- cbind(Movies_new, Log_num_user_for_reviews)
Movies_new <- cbind(Movies_new, Log_movie_facebook_likes)


summary(Movies_new[,c('imdb_score','num_critic_for_reviews','duration','Log_num_voted_users','Log_facenumber_in_poster','Log_num_user_for_reviews','Movies_Age', 'Log_movie_facebook_likes','Cluster1','Cluster2','Cluster3','Cluster4')])


cormatrix <- cor(Movies_new[,c('imdb_score','num_critic_for_reviews','duration','Log_num_voted_users','Log_facenumber_in_poster','Log_num_user_for_reviews','Movies_Age', 'Log_movie_facebook_likes','Cluster1','Cluster2','Cluster3','Cluster4')],use="complete.obs")
write.csv(cormatrix, file="cormatrix2.csv")
# Build a regression model #
Movies_Reg <- lm(imdb_score ~ num_critic_for_reviews + duration + Log_facenumber_in_poster + Movies_Age + Log_movie_facebook_likes + Cluster1 + Cluster2 + Cluster3 + Cluster4, data=Movies_new)
summary(Movies_Reg)
