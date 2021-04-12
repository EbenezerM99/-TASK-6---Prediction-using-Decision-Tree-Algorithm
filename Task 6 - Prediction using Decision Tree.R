# # #clear the console
cat("\f")
# # 
# 
# The Sparks Foundation || GRIP_APRIL-21
# Data Science and Business Analytics
# TASK 6 - Prediction using Decision Tree Algorithm
# 
# Create the Decision Tree classifier and visualize it graphically.
# 
#The purpose is if we feed any new data to this classifier, it would be able to predict the right class accordingly.
# 
# set the working directory
setwd("E:/sparkfoundation")

#import the dataset 
data=read.csv("iris.csv")

# view the data set
head(data)
# structure of the data set
str(data)
# summary of the data set
summary(data)
#checking for Missing Values
print(any(is.na(data)))


#splite the dataset into train and test
set.seed(555)
ind <- sample(2,nrow(iris),replace=TRUE, prob=c(0.7,0.3))
train <- iris[ind==1,]
test <- iris[ind==2,]

# Decision tree model
library(party)
tree <- ctree(Species~.,data=train)

# Visualization of decision trees
plot(tree)

#Predict
predict(tree,test)

# Misclassification error - train data
p1 <- predict(tree, train)
(tab1 <- table(p1, train$Species))

ca=sum(diag(tab1)/sum(tab1))
cat("classification accuracy of train data=",ca)

# Misclassification error - test data
p2 <- predict(tree, test)
(tab2 <- table(p2,test$Species))

ca=sum(diag(tab2)/sum(tab2))
cat("classification accuracy of test data =",ca)