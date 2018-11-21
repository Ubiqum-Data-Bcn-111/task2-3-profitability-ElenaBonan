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
  print(plot_ly(data = MyData, x = MyData[,i], y = MyData$Volume, type = 'scatter', mode = 'markers', name = names(MyData)[i]))
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

# Scatter plot at group of tree.


# All the variables against the volume

# Random forest to decide the variables

# Box plot

library(plotly)
for(i in 1:(ncol(MyData)) ){
  if (  is.numeric( MyData[,i] ) || is.integer(MyData[,i]) )
  {print( plot_ly( y = MyData[,i], name = names(MyData)[i], type = 'box') )}}
# I delate the outliers of the volume 
MyData  <- subset(MyData, Volume != 11204)
MyData <- subset(MyData, Volume != 7036)
# I keep the outliers of the other variables because they are too much.

# I eliminate the warranties because probably there was an error.

# I start trying the model with the most important variables. I want to try with 1, 2 and 3 variables.
# In order of importance I got X4 starreviews , Positive service reviews and x2stars.

set.seed(123)
inTraining <- createDataPartition(MyData$Volume, p = .80, list = FALSE)

training <- MyData[inTraining,]
testing <- MyData[-inTraining,]
#check the distribution of the two subsets.


p <- plot_ly(y = training$Volume, type = "box", name = 'training') %>%
  add_trace(y = testing$Volume, name='testing')
p
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
###############Try the models 
# I start trying the model with the most important variables. I want to try with 1, 2 and 3 variables.
# In order of importance I got X4 starreviews , Positive service reviews and x2stars.

#I create try and testing with the three subset I want to consider 

MyData1 <- MyData[,c(2,10)]
MyData2 <- MyData[,c(2,6,10)]
MyData3 <- MyData[,c(2,6,4,10)]

training1 <- MyData1[inTraining,]
training2 <- MyData2[inTraining,]
training3 <- MyData3[inTraining,]


training <- list(training1,training2,training3)
training[1]

testing1 <- MyData1[-inTraining,]
testing2 <- MyData2[-inTraining,]
testing3 <- MyData3[-inTraining,]


testing <- list(testing1, testing2, testing3)


linear1 = list()
for (i in 1:3){
mo <- train(Volume~., data = data.frame(training[i]), method = "rf", trControl=fitControl, preProcess=c("center", "scale"))
print(mo)
p <- predict(mo, data.frame(testing[i]))
print(p)
typeof(p)
list.append(linear1,p)
}

linear1
list.append(linear1,p)

linear
data.frame(training[1])


mo <- train(Volume~., data = data.frame(training[1]), method = "rf", trControl=fitControl)
p <- predict(mo, data.frame(testing[1]))


for (i in 1:3){
  mo <- train(Volume~., data = data.frame(training[i]), method = "lm", trControl=fitControl, preProcess=c("center", "scale"))
  print(mo)}


# check in the testing. 
#I took three variables for the random forest.

mo <- train(Volume~., data = data.frame(training[3]), method = "rf", trControl=fitControl, preProcess=c("center", "scale"))
print(mo)
p <- predict(mo, data.frame(testing[3]))
postResample(p,data.frame(testing[3])$Volume)
relativeerror <- ((p-data.frame(testing[3])$Volume)/data.frame(testing[3])$Volume)
meanrelativeerrror1 <- mean(relativeerror )
# For the linear I took 3 variables and 2.

mo <- train(Volume~., data = data.frame(training[2]), method = "lm", trControl=fitControl, preProcess=c("center", "scale"))
print(mo)
p <- predict(mo, data.frame(testing[2]))
postResample(p,data.frame(testing[2])$Volume)
meanrelativeerror2<- mean((p-data.frame(testing[2])$Volume)/data.frame(testing[2])$Volume)

#-------
#rm(list=ls(all=TRUE))
setwd("C:/Users/Elena/Desktop/Git/task2-3-profitability-ElenaBonan")
library(readr)
library(caret)
library(plotly)
MyData <- read.csv(file="existingproductattributes2017.2.csv")
# Preprocessing 
# check the structure of data 
str(MyData)
par(mfrow = c(1,1))
# check the distribution of the data 
# Using histogram
for(i in 1:(ncol(MyData))){
  cl = MyData[,i]
  if (is.numeric( cl )) 
    {hist( cl, xlab= names(MyData)[i], main = (paste('Frequency of',names(MyData)[i])))}
   else if (is.factor(cl))
  {barplot(table(cl),main = (paste('Frequency of',names(MyData)[i])))}}

# Using the histogram and the density
class(MyData$ProductType)

plot(MyData$Price,MyData$Volume)

# some plot 
qplot(MyData$ProductType,MyData$Volume,main='Sales Volume across Product Type', xlab= 'Product Type', ylab= 'Sales Volume' )


is.integer(MyData$ProductType)
is.factor(MyData$ProductType)

par(mfrow = c(4,4))
for(i in 1:(ncol(MyData))){
  cl = MyData[,i]
  if (is.numeric( cl ) || is.integer( cl )) 
  {hist( cl, xlab= names(MyData)[i], main = (paste('Frequency of',names(MyData)[i])))}
  else if (is.factor(cl))
  {print(ggplot(MyData, aes(x = cl))+geom_bar())}}
is.factor(MyData[,1])
MyData$ProductType =MyData[,1]
#Boxplot
# Put the variables together if they have the same range or use frame
p <- plot_ly(MyData, x = MyData$Price, type = "box")
p
if(is.factor(cl)) 
1:(ncol(MyData))
ggplot(MyData, aes (x = MyData[,1]))+ geom_bar()

#boxplot 
library(plotly)
for(i in 1:(ncol(MyData))){
  if (is.numeric( MyData[,i] ) || is.integer(MyData[,i]) ) 
  {boxplot(MyData[,i], xlab =names(MyData)[i])}
}





typeof(names(MyData))

names(MyData)[2]
is.numeric(MyData$Salary)==FALSE

for (i in names(MyData)) {print(i)}

for (i in names(MyData)){print(i)}

for (i = 2)

hist(MyData[,3])
hist(MyData$Pri)
MyData[2,]
str(MyData)
summary(MyData)
MyData1 <- dummyVars(" ~ .", data = MyData)
MyData1
MyData2 <- data.frame(predict(MyData1, newdata = MyData))
MyData2$BestSellersRank <- NULL
MyData2$ProductNum <- NULL
corrData <- cor(MyData2[,1-18]) 
corrData
#tl.cex=0.5
#tl.pos='n'
library(corrplot)
corrplot(corrData,tl.cex=0.5)


##-------------
#Cose interessanti
#names(MyData), ncol(MyData)
#for(i in names(MyData) ){
#if (is.numeric( MyData$i )==TRUE) {print(i)}
#}
# Check if I have duplicate

#dup.names <- as.numeric(MyData[which(duplicated(as.numeric(MyData$ProductWidth))),"ProductWidth"])
#MyData[which (MyData$ProductWidth %in% dup.names),]

#dup.names <- (MyData[which(duplicated(MyData$ProductNum)),"ProductNum"])

#MyData[which (MyData$ProductNum %in% dup.names),]
x = 7

if (x ==5 or x ==7)
{ print(x)}

|| is.integer(MyData[,i])

MyData$BestSellersRank <- NULL
MyData$ProductNum <- NULL
#Random forest
f <- train(Volume ~ ., data = MyData, method = "rf", importance = TRUE) 
varweights <- varImp(f) #save the results of the weighting after the first desicion tree varweights
varweights[1]
p = varweights[1]$importance
rownames(p)[13]
s = names(MyData)
which( s == 'Price')

#
if(is.factor(MyData[,1])) 
{ggplot(MyData, aes(x = MyData[,1] ))+geom_bar()}



###----------------
# How to check the variable better

Variables <- data.frame(MyData$Volume)
names(Variables)[1]<- 'Volume'
Variables$Star4 <- MyData$x4StarReviews
Variables$Positive <- MyData$PositiveServiceReview
Variables$Star2 <- MyData$x2StarReviews

for (i in 1:3){
  Prova = Variables[,1:(i+1)]
  
  train = Prova[inTraining,]
  mo <- train(Volume~., data = train, method = "rf", trControl=fitControl, preProcess=c("center", "scale"))
  print(mo)
}

train = Variables[inTraining,]
mo <- train(Volume~., data = train, method = "rf", trControl=fitControl, preProcess=c("center", "scale")


cor(MyData3)
library(rgl)
plot3d(MyData3$x4StarReviews, MyData3$PositiveServiceReview, MyData$Volume)
