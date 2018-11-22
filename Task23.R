# Task 2.3
setwd("C:/Users/Elena/Desktop/Git/task2-3-profitability-ElenaBonan")
library(readr)
library(caret)
library(plotly)
library(corrplot)
library(rpart)
library(rpart.plot)
library(rlist)
library(randomForest)
MyData <- read.csv(file="existingproductattributes2017.2.csv")
###############PREPROCESSING #############################
# check the structure of data 
str(MyData)
MyData$ProductNum <- NULL # I don't need the number of the product 
# Check the distribution of my variables
for(i in 1:(ncol(MyData))){
  cl = MyData[,i]
  if (is.numeric( cl )) 
  {hist( cl, xlab= names(MyData)[i], main = (paste('Frequency of',names(MyData)[i])))}
  else if (is.factor(cl))
  {barplot(table(cl),main = (paste('Frequency of',names(MyData)[i])))}}

# Plot the independent variable against the volume
for (i in 1:(ncol(MyData)-1)){
  print(
  plot_ly(data = MyData, x = MyData[,i], y = MyData$Volume, type = 'scatter', mode = 'markers')%>%
    layout(xaxis = list(title =names(MyData)[i]) , yaxis = list(title='Volume'))
  )
}

# elimination of some variables

MyData$x5StarReviews <- NULL # Otherwise we have overfitting 
MyData$BestSellersRank <- NULL # We don't know the meaning of this variable
MyData$ProductType <- NULL # We have too many different type of products with respect to the number of data 

# I eliminate the warrienties

MyData <- rbind( MyData[1:34,], MyData[42:80,])

# Decision tree
tree <- rpart(Volume~., data=MyData, cp=.001)
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)

# I delate the variables that are not relate with the volume by reasoning and by the tree  

MyData$ProductDepth <- NULL
MyData$ProductWidth <- NULL
MyData$ProductHeight <- NULL
MyData$ShippingWeight <- NULL

# Correlation matrix 
corrData <- cor(MyData) 
corrData
corrplot(corrData,tl.cex=0.5)

# Box plot

library(plotly)
for(i in 1:(ncol(MyData)) ){
  if (  is.numeric( MyData[,i] ) || is.integer(MyData[,i]) )
  {print( plot_ly( y = MyData[,i], name = names(MyData)[i], type = 'box') )}}
# I delate the outliers of the volume 
#MyData  <- subset(MyData, Volume != 11204)
MyData <- subset(MyData, Volume < 7036)
# I keep the outliers of the other variables because they are too much.

# I start trying the model with the most important variables. I want to try with 1, 2 and 3 variables.
# In order of importance I got X4 starreviews , Positive service reviews and x2stars.

set.seed(123)
inTraining <- createDataPartition(MyData$Volume, p = .80, list = FALSE)
training <- MyData[inTraining,]
testing <- MyData[-inTraining,]
#check the distribution of the two subsets.

p <- plot_ly(y = training$Volume, type = "box", name = 'training') %>%
  add_trace(y = testing$Volume, name='testing')


# I want to try different models using different subsets of variables

# Let us use the list given by the tree

tree <- rpart(Volume~., data=MyData, cp=.001)
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)

# Let us create a Data frame with the variables in order of importance 
variables = names(tree$variable.importance)
indicesVolume = which(names(training) == 'Volume')
indices = c(indicesVolume)
listtraining = list()

for (i in variables){
  var = which(names(training)== i)
  indices = c(indices, var)
  train = training[,indices]
  listtraining = list.append(listtraining,train)
}

#Train Control for the models
control <- trainControl(method="repeatedcv", number=10, repeats=3)

# MLR 
resultsMLR = c()
modelsMLR = c()
name = c()
for (i in 1:length(listtraining)){
  if (i == 1){
    resultsMLR <- c()
    modelsMLR <-c()
  }
  set.seed(123)
  mod <- train(Volume ~ ., data = data.frame(listtraining[i]), method = "lm", preProcess=c("center","scale"))
  modelsMLR <- cbind(modelsMLR,mod)
  prediction1 <- predict(mod,testing)
  performance <- postResample(prediction1,testing$Volume)
  resultsMLR <- cbind(resultsMLR,performance)
  name = c(name,paste("MLR Model ",i,"V",sep=""))
}
colnames(resultsMLR) <- name
colnames(modelsMLR) <- name
modelsMLR

#Random forest
resultsRF = c()
modelsRF = c()
name = c()
for (i in 1:length(listtraining)){
  if (i == 1){
    resultsRF <- c()
    modelsRF <-c()
  }
  set.seed(123)
  mod <- train(Volume ~ ., data = data.frame(listtraining[i]), method = "rf", preProcess=c("center","scale"))
  modelsRF <- cbind(modelsRF,mod)
  prediction <- predict(mod,testing)
  performance <- postResample(prediction,testing$Volume)
  resultsRF <- cbind(resultsRF,performance)
  name = c(name,paste("RF Model ",i,"V",sep=""))
}
name
resultsRF
colnames(resultsRF) <- name
colnames(modelsRF) <- name


#Random forest with list 
resultsRF = c()
modelsRF = list()
name = c()
for (i in 1:length(listtraining)){
  if (i == 1){
    resultsRF <- c()
    modelsRF <- list()
  }
  set.seed(123)
  mod <- train(Volume ~ ., data = data.frame(listtraining[i]), method = "rf", preProcess=c("center","scale"))
  modelsRF <- list.append(modelsRF,mod)
  prediction <- predict(mod,testing)
  performance <- postResample(prediction,testing$Volume)
  resultsRF <- cbind(resultsRF,performance)
  name = c(name,paste("RF Model ",i,"V",sep=""))
}

colnames(resultsRF) <- name

#Check the results
print(resultsRF)
print(resultsMLR)
