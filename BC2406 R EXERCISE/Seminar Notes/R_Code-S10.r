rm(list=ls(all=TRUE)) # Clear all 

##  Required Packages  ##
install.packages("NLP") #NLP Functions
install.packages("tm")  #Text-mining
install.packages("SnowballC") #Stemming
install.packages("wordcloud") #WordCloud
install.packages("RColorBrewer")  #Colours for WordCloud

##  Activate Packages ##
library(NLP) 
library(tm)
library(SnowballC) 
library(wordcloud)
library(RColorBrewer)  
library(stats)     # Clustering

## Data Loading ##
Apps_data <- read.csv("App_Games_Desc.csv") #The original dataset is stored in your project folder. 
Apps_desc <- Apps_data[,10]                 #Descriptions are recorded in the 10th column.  
n_desc <- length(Apps_desc)                 #Check the number of app descriptions

## Use the First Two Sentences ##
# Select the first two sentences #
Apps_Sent <- list()
for (i in 1:n_desc) { 
temp <- unlist(strsplit(as.character(Apps_desc[i]), split = "[.!?]+"))  
Apps_Sent[[i]] <- temp[1:2]           #Use the first 2 sentences
}  

Apps_Sent[[1]];Apps_Sent[[2]]

## Convert Vector/List to Corpus ## 
Apps   <- Corpus(VectorSource(Apps_Sent)) 
as.character(Apps[[1]])

###Data Pre-Processing ###

## Upper-case Letters to Lower-case Letters ##
Apps  <- tm_map(Apps, content_transformer(tolower)) 
as.character(Apps[[1]])

### Remove unnecessary terms and symbols ###
  
# Delete HTML Tags #
for (j in 1:n_desc) Apps[[j]] <- gsub("u2019", " ", Apps[[j]]) #delete "u2019"
for (j in 1:n_desc) Apps[[j]] <- gsub("u'", " ", Apps[[j]])  #delete u'
for (j in 1:n_desc) Apps[[j]] <- gsub("u\"", " ", Apps[[j]]) #delete u"
as.character(Apps[[1]])
inspect(Apps[1:3])
   
# Delete HTML Tags #
for (j in 1:n_desc) Apps[[j]] <- gsub("u2605", " ", Apps[[j]]) #delete "u2605"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2606", " ", Apps[[j]]) #delete "u2606"
for (j in 1:n_desc) Apps[[j]] <- gsub("u201c", " ", Apps[[j]]) #delete "u201c"
for (j in 1:n_desc) Apps[[j]] <- gsub("u201d", " ", Apps[[j]]) #delete "u201d"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2011", " ", Apps[[j]]) #delete "u2011"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2013", " ", Apps[[j]]) #delete "u2013"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2014", " ", Apps[[j]]) #delete "u2014"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2022", " ", Apps[[j]]) #delete "u2022"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2122", " ", Apps[[j]]) #delete "u2122"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2026", " ", Apps[[j]]) #delete "u2026
for (j in 1:n_desc) Apps[[j]] <- gsub("u2028", " ", Apps[[j]]) #delete "u2028"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2729", " ", Apps[[j]]) #delete "u2729"
for (j in 1:n_desc) Apps[[j]] <- gsub("u20ac", " ", Apps[[j]]) #delete "u20ac"
for (j in 1:n_desc) Apps[[j]] <- gsub("amp", " ", Apps[[j]]) #amp
for (j in 1:n_desc) Apps[[j]] <- gsub("xae", " ", Apps[[j]]) #xae
for (j in 1:n_desc) Apps[[j]] <- gsub("xa0", " ", Apps[[j]]) #xa0
for (j in 1:n_desc) Apps[[j]] <- gsub("xa3", " ", Apps[[j]]) #xa3
inspect(Apps[1:3])
  
# Remove less important terms: Device names #
for (j in 1:n_desc) Apps[[j]] <- gsub("apple", " ", Apps[[j]]) #Apple
for (j in 1:n_desc) Apps[[j]] <- gsub("iphone", " ", Apps[[j]]) #iphone
for (j in 1:n_desc) Apps[[j]] <- gsub("touch", " ", Apps[[j]]) #touch
for (j in 1:n_desc) Apps[[j]] <- gsub("ipod", " ", Apps[[j]]) #ipod
for (j in 1:n_desc) Apps[[j]] <- gsub("ipad", " ", Apps[[j]]) #ipad
for (j in 1:n_desc) Apps[[j]] <- gsub("3gs", " ", Apps[[j]]) #iPohne 3GS 
for (j in 1:n_desc) Apps[[j]] <- gsub("3rd", " ", Apps[[j]]) #3rd Gen. iPod
for (j in 1:n_desc) Apps[[j]] <- gsub("2nd", " ", Apps[[j]]) #2nd Gen. iPod
for (j in 1:n_desc) Apps[[j]] <- gsub("4th", " ", Apps[[j]]) #4th Gen. iPod
inspect(Apps[1:3])
  
# Remove less important terms: App, Game, Play # 
for (j in 1:n_desc) Apps[[j]] <- gsub("app", " ", Apps[[j]]) #app
for (j in 1:n_desc) Apps[[j]] <- gsub("store", " ", Apps[[j]]) #store
for (j in 1:n_desc) Apps[[j]] <- gsub("game", " ", Apps[[j]]) #game 
for (j in 1:n_desc) Apps[[j]] <- gsub("play", " ", Apps[[j]]) #play
for (j in 1:n_desc) Apps[[j]] <- gsub("mobile", " ", Apps[[j]]) #mobile
for (j in 1:n_desc) Apps[[j]] <- gsub("free", " ", Apps[[j]]) #Free
for (j in 1:n_desc) Apps[[j]] <- gsub("new", " ", Apps[[j]]) #new
for (j in 1:n_desc) Apps[[j]] <- gsub("world", " ", Apps[[j]]) #world
inspect(Apps[1:3])
  
# Convert Important numbers to words/terms #
for (j in 1:n_desc) Apps[[j]] <- gsub("#1", "numberone", Apps[[j]]) #1
for (j in 1:n_desc) Apps[[j]] <- gsub("99", "nintyninecent", Apps[[j]]) #$0.99
for (j in 1:n_desc) Apps[[j]] <- gsub("%", "percent", Apps[[j]]) #percent
inspect(Apps[1:3])
  
  
# Remove stopwords #
Apps <- tm_map(Apps, removeWords, stopwords("english"))
inspect(Apps[1:3])
  
# You can define stopwords #
newstopwords <-c("and", "for", "the", "to", "in", "when", "then", "he", "she", "than", "can", "get", "one");
Apps <- tm_map(Apps, removeWords, newstopwords)
  
for (j in 1:n_desc) Apps[[j]] <- gsub("don", " ", Apps[[j]]) #don
for (j in 1:n_desc) Apps[[j]] <- gsub("won", " ", Apps[[j]]) #won't
for (j in 1:n_desc) Apps[[j]] <- gsub("ing", " ", Apps[[j]]) # -ing
for (j in 1:n_desc) Apps[[j]] <- gsub("http", " ", Apps[[j]]) # http
for (j in 1:n_desc) Apps[[j]] <- gsub("'ll", " ", Apps[[j]]) #'ll   
for (j in 1:n_desc) Apps[[j]] <- gsub("www", " ", Apps[[j]]) # www
for (j in 1:n_desc) Apps[[j]] <- gsub("com", " ", Apps[[j]]) # com
inspect(Apps[1:3])
  
# Remove Numbers #
Apps <- tm_map(Apps, removeNumbers) 
inspect(Apps[1:3])
  
# Remove Punctuations and Symbols #
Apps <- tm_map(Apps, removePunctuation) 
inspect(Apps[1:3])
  
# You can also manually delete non-characters #
for (j in 1:n_desc) Apps[[j]] <- gsub("['*|&|-|/|\\|()|\\.,!-_]", " ", Apps[[j]]) # remove non-characters

# Remove Extra White Space #
Apps <- tm_map(Apps, stripWhitespace)
inspect(Apps[1:3])
  
## Stemming ##
Apps <- tm_map(Apps, PlainTextDocument)  # Remove common word endings ("es", "ed", "s", "ing")
Apps <- tm_map(Apps, stemDocument)

as.character(Apps[[1]])
as.character(Apps[[3]]) 
 
## Create a DTM ##
# Original DTM #
dtm_Apps <- DocumentTermMatrix(Apps)
dtm_Apps
inspect(dtm_Apps)

# DTM with Controls #
dtm_Apps_Ctrl <- DocumentTermMatrix(Apps, control=list(wordLength=c(3,20), bounds=list(global=c(10,200))))
dtm_Apps_Ctrl
inspect(dtm_Apps_Ctrl) # Display DTM for the descriptions 

inspect(dtm_Apps_Ctrl[1:5,1:15]) # DTM for the first 5 descriptions with the first 15 terms


## Frequency of Terms in DTM ##

# Find the terms that occur at least 30 times #
findFreqTerms(dtm_Apps_Ctrl, 30)

# Frequency of terms #  
Freq_term <-colSums(as.matrix(dtm_Apps_Ctrl))
Order_Freq_term <- order(Freq_term, decreasing = TRUE)
Freq_term[Order_Freq_term]

# Frequency Diagram #
library(grDevices); # for colours  
Apps_DTM_DF = as.data.frame(as.matrix(dtm_Apps_Ctrl))
numwords <- 30; # the most frequent 30 terms  

# sum each column and sort by descending order # 
Terms_Freq <- as.matrix(sort(sapply(Apps_DTM_DF, FUN=sum),decreasing=TRUE)[1:numwords], colnames=count)
x <- sort(Terms_Freq[1:numwords,], decreasing=FALSE) 
barplot(x, horiz=TRUE, cex.names=0.5, space=1, las=1, col=grey.colors(10), main="Frequency of Terms")

## Word Cloud ##
# For Original DTM #
 set.seed(2406) #set the same seed each time ensures consistent look across clouds
 m <- as.matrix(t(dtm_Apps)) # Convert it to a matrix
 v <- sort(rowSums(m),decreasing=TRUE) # Sort the terms in a descending order 
 w <- data.frame(word = names(v),freq=v) # Create a data frame indicating the name&frequency of terms

 WC_color <- brewer.pal(8,"Set2")
 wordcloud(w$word,w$freq, scale=c(3,.1),min.freq=1, max.words=200, random.order=F, rot.per=.3, colors=WC_color)

# For DTM with Controls #
 dev.off()
 set.seed(2406) #set the same seed each time ensures consistent look across clouds
 m <- as.matrix(t(dtm_Apps_Ctrl)) # Convert it to a matrix
 v <- sort(rowSums(m),decreasing=TRUE) # Sort the terms in a descending order 
 w <- data.frame(word = names(v),freq=v) # Create a data frame indicating the name&frequency of terms

 WC_color <- brewer.pal(8,"Set2")
 wordcloud(w$word,w$freq, scale=c(3,.1),min.freq=1, max.words=200, random.order=F, rot.per=.3, colors=WC_color)

 
## Find Associated Terms ##
findAssocs(dtm_Apps_Ctrl, "time", .3);
findAssocs(dtm_Apps_Ctrl, "now", .2);
findAssocs(dtm_Apps_Ctrl, "best", .2);

 
 