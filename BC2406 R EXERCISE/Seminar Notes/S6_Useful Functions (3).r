## S6_Exercise ##
rm(list=ls())

## ifelse function vs. If Else Statements ##
adm <- read.csv("admission.csv")

# ifelse Function: Take a scalar & a vector #
GRE_Rank <- ifelse(adm$gre <= 400, "Low", ifelse(adm$gre >= 701, "High","Mid"))

# IF Else Statements: Take a scalar #
n_row <- nrow(adm)
GRE_Rank2 <- 1:n_row

for (i in 1:n_row){
if(adm$gre[i] <= 400) {GRE_Rank2[i] <- "Low"} else if(adm$gre[i] >= 701) {GRE_Rank2[i] <- "High"} else {GRE_Rank2[i] <- "Mid"}
}


## Load files in the sub-folder ##
# Read files in the subfolders#
# Assume you're working on the project folder #
# and your subfolder name is “sub” and the file name is ‘test.csv’# 

> test_csv <- read.csv(‘ ./sub/test.csv’)

# Also you can list the files in the subfolder #
> list.files(‘./sub’)


## apply, lapply, sapply, mapply ##
test <- matrix(1:15, nrow=5, ncol=3)
colnames(test) <- c('A', 'B', 'C'); test

# apply(df/matrix, row(1)/col(2), function) #
# Apply a function to the rows or columns of a matrix or a dataframe #
apply(test,1, sum)                  # sums by rows
apply(test,2, sum)                  # sums by columns
apply(test[,c('A', 'C')],2, mean)   # means in A and C

# lapply(list, function) # aaply in dply
# Apply a given function to every element of a vector/list and obtain a list as result #
lapply(test[,1], function(x){x^2})  # x^2 in the 1st column
lapply(test[2,], function(x){x*2})  # x*2 in the 2nd row  

# sapply(vector, function) # laply in dply
# Apply a given function to every element of a vector/list and obtain a vector as result #
sapply(test[,1], function(x){x^2})  # x^2 in the 1st column
sapply(test[2,], function(x){x*2})  # x*2 in the 2nd row

# mapply(function, vector1, vector2, vector3,...)# maply in dply
# Apply a function to the 1st elements of each, and then the 2nd elements of each,.. #
mapply(sum,test[,'A'], test[,'B'], test[,'C']);test #sums 

