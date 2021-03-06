######## R Exercise: Seminar 7 ##########
rm(list=ls())

#### STEP 2 ####
# Data Import #
credit <- read.csv("credit.csv")
str(credit)

# look at two characteristics of the applicant
table(credit$checking_balance)
table(credit$savings_balance)

# look at two characteristics of the loan
summary(credit$months_loan_duration)
summary(credit$amount)

# look at the class variable
table(credit$default)


#### STEP3 ####
# create a random sample for training and test data
# use set.seed to use the same random number sequence 
num_obs <- nrow(credit)
train_size <- num_obs * 0.8 
set.seed(1234)
train_sample <- sample(num_obs, train_size)

Credit_Train <- credit[train_sample, ]
Credit_Test  <- credit[-train_sample, ]

nrow(Credit_Train); nrow(Credit_Test)

table(Credit_Train$default)
table(Credit_Test$default)

# check the proportion of class variable
prop.table(table(Credit_Train$default))
prop.table(table(Credit_Test$default))


#### STEP 5 ####
# build the simplest decision tree
library(rpart) # activate the rpart package

# Train the model with training set# 
credit_model <- rpart(default ~ ., data = Credit_Train, method="class")

# Plot Tree #
#install.packages("partykit")
library(partykit) # activate the partykit package
plot(as.party(credit_model))

# display simple facts about the tree
credit_model

# display detailed information about the tree
summary(credit_model)

#### STEP 6 ####
# create a vector of predictions on test data
credit_pred <- predict(credit_model, Credit_Test, type="class")
mean(credit_pred == Credit_Test$default)

# Confusion Matrix #
#install.packages("caret")
library(caret) # activate the caret package
confusionMatrix(credit_pred, Credit_Test$default, positive = "yes")
