setwd("D:/Personal/Data Science/Git Hub Projects/Census Income")
#install.packages("data.table")
library(data.table) 
train <- fread("train.csv", na.strings = c(""," ","?","NA",NA))
test <- fread("test.csv", na.strings = c(""," ","?","NA",NA))
dim(train)
dim(test)
str(train)
head(train)
#Check the target variable
unique(train$income_level)
unique(test$income_level)

# Encode target variables
train[, income_level := ifelse(income_level == "-50000",0,1)]
test[, income_level := ifelse(income_level == "-50000",0,1)]

# Check the imbalance
round(prop.table(table(train$income_level))*100) #94% of -50000 and 6 % of 50000+

# Set Column Classes
factcols <- c(2:5,7,8:16,20:29,31:38,40,41) # Gathering all factorial representation cols
numcols <- setdiff(1:40,factcols) # Gathering non factor cols

?lapply
train[, (factcols) := lapply(.SD, factor), .SDcols = factcols]
train[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

test[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
test[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

# Subset Categorical variables
cat_train <- train[,factcols,with=FALSE]
cat_test <- test[, factcols, with = FALSE]

# Subset Numerical Variables
num_train <- train[, numcols, with =FALSE]
num_test <-  test[, numcols, with=FALSE]

rm(train,test) # To save the memory

str(cat_train)
str(num_train)
dim(cat_train)
dim(num_train)

library(ggplot2)
library(plotly)

# Write a plot function
tr <- function(a){
  ggplot(data = num_train, aes(x= a, y=..density..)) + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + geom_density()
  ggplotly()
}

# Age Variable
tr(num_train$age)

# capital_losses variable
tr(num_train$capital_losses)

# Add target variable to num_train set
num_train[, income_level := cat_train$income_level]

# Create a scatter plot
ggplot(data=num_train,aes(x = age, y=wage_per_hour))+geom_point(aes(colour=income_level))+scale_y_continuous("wage per hour", breaks = seq(0,10000,1000))

#dodged bar chart
all_bar <- function(i){
  ggplot(cat_train,aes(x=i,fill=income_level))+geom_bar(position = "dodge",  color="black")+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))
}
#variable class_of_worker
all_bar(cat_train$class_of_worker)

#variable education
all_bar(cat_train$education)

prop.table(table(cat_train$marital_status,cat_train$income_level),1)
prop.table(table(cat_train$class_of_worker, cat_train$income_level),1)


######################
# Data Cleaning 
######################

# Check missing values in numerical data
table(is.na(num_train))
table(is.na(num_test))

# Checking the correlation
library(caret)
# Set threshold as 0.7
str(num_train)
ax <-findCorrelation(x = cor(num_train[,0:7]), cutoff = 0.7)
num_train <- num_train[,-ax,with=FALSE]
num_test[, weeks_worked_in_year := NULL]

# Checking missing values per columns in Categorical Data
mvtr <- sapply(cat_train, function(x){sum(is.na(x)/length(x))})*100
mvte <- sapply(cat_test, function(x){sum(is.na(x)/length(x))})*100
mvtr
mvte

# Select Columns with missing values less than 5%
cat_train <-  subset(cat_train, select = mvtr < 5)
cat_test <- subset(cat_test, select = mvte < 5)

# Set NA as Unavailable - train data
# Convert to characters
cat_train <- cat_train[, names(cat_train) := lapply(.SD, as.character), .SDcols = names(cat_train)]
for(i in seq_along(cat_train)) set(cat_train, i = which(is.na(cat_train[[i]])), j=i, value = "Unavailable")

# Convert back to factors
cat_train <- cat_train[, names(cat_train) := lapply(.SD,factor), .SDcols = names(cat_train)]

#set NA as Unavailable - test data
cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, as.character), .SDcols = names(cat_test)]
for (i in seq_along(cat_test)) set(cat_test, i=which(is.na(cat_test[[i]])), j=i, value="Unavailable")
#convert back to factors
cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, factor), .SDcols = names(cat_test)]

###########################
## Data Manipulation
###########################
# Combine factor levels with less than 5% values
# In train data
for(i in names(cat_train)){
  p <- 5/100
  ld <- names(which(prop.table(table(cat_train[[i]])) < p))
  levels(cat_train[[i]])[levels(cat_train[[i]]) %in% ld] <- "Other"
}
# In test data
for(i in names(cat_test)){
  p <- 5/100
  ld <- names(which(prop.table(table(cat_test[[i]])) < p))
  levels(cat_test[[i]])[levels(cat_test[[i]]) %in% ld] <- "Other"
}

# Check columns with unequal levels
library(mlr)
summarizeColumns(cat_train)[,"nlevs"]
summarizeColumns(cat_test)[,"nlevs"]


num_train[,.N,age][order(age)]
num_train[,.N,num_train$wage_per_hour][order(-N)]

#bin age variable 0-30 31-60 61 - 90
num_train[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))]
num_train[,age := factor(age)]



num_test[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))]
num_test[,age := factor(age)]

#Bin numeric variables with Zero and MoreThanZero
num_train[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
num_train[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]
num_train[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]
num_train[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]

num_test[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
num_test[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]
num_test[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]
num_test[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]

num_train[,income_level := NULL]

#combine data and make test & train files
d_train <- cbind(num_train,cat_train)
d_test <- cbind(num_test,cat_test)

# Remove unwanted files
rm(num_train, num_test, cat_train, cat_test)

# Load library for machine learning
library(mlr)

# create task
train.task <- makeClassifTask(data = d_train,target = "income_level")
test.task <- makeClassifTask(data = d_test, target = "income_level")
train.task

#remove zero variance features
train.task <- removeConstantFeatures(train.task)
test.task <- removeConstantFeatures(test.task)