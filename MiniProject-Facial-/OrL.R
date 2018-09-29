setwd("~/Desktop/ML-R/ORL")
## orl.csv is resizble version of ORl images dataset (25 X 25.8), converted to pixels 
Orldataset = read.csv('orl.csv')
View (Orldataset)

str(Orldataset)
dim(Orldataset)
library(caTools)
set.seed(123)
split = sample.split(Orldataset$label, SplitRatio = 0.8)
training_set = subset(Orldataset, split == TRUE)
test_set = subset(Orldataset, split == FALSE)
dim(test_set)
dim(training_set)
############# KNN
library(class)
attach(Orldataset)
Knn_Model = knn(train = training_set[, -645],
             test = test_set[, -645],
             cl = training_set[, 645],
             k = 5,
             prob = TRUE)
Confus_matrix = table(test_set[, 645], Knn_Model)
Confus_matrix
View (Confus_matrix)
set.seed(33)
Knn_Model!=label
sum(Knn_Model!=label)/length(label) # Taux de réussite 

table(Knn_Model==test_set$label)
 

########################
library(rpart)
library(rpart.plot)
attach(Orldataset)
RpartClassifier = rpart(formula = as.factor(label) ~ .,
                        data = training_set)
y_pred = predict(RpartClassifier,newdata = test_set[-645])
 
# Making the Confusion Matrix
dim(y_pred)
 
plot(RpartClassifier)
text(RpartClassifier)
rpart.plot(RpartClassifier,box.palette=0 ,trace=-1)
summary(RpartClassifier)
############## TREE Regression tree
library(tree)
attach(Orldataset)
labelFactor <- as.factor(training_set$label)
Orldataset$label <- as.factor(Orldataset$label )

class(labelFactor)
classifier2 = tree(as.factor(label)  ~ .,
                   data = training_set)


pred2 = predict(classifier2, test_set[-645] , type ="class")
summary(classifier2)
plot(classifier2)
text(classifier2)

#########  C50
library(C50)
attach(Orldataset)

classifier3 <- C5.0(as.factor(label)~ .,data=training_set )

classifier3
summary(classifier3)
plot(classifier3)

summary(classifier3)
######### Random Forest
library(randomForest)
set.seed(415)
RandFrst <- randomForest(label ~ .,
                         data=training_set, 
                         ntree=200)
plot(RandFrst)
summary(RandFrst)

RandFrst <- randomForest(label ~ .,
                         data=training_set, 
                         ntree=500)
plot(RandFrst)
####### Naive Bayes
library(e1071)

naiveBayesClassifier <- naiveBayes(label ~ . , data = training_set)
NaivePred = predict(naiveBayesClassifier , test_set[-645])
summary(naiveBayesClassifier)
print(naiveBayesClassifier)
 
#Prediction on the dataset
NB_Predictions=predict(Naive_Bayes_Model,Titanic_dataset)
#Confusion matrix to check accuracy
table(NB_Predictions,Titanic_dataset$Survived)

library(mlr)
install.packages('mlr')
task = makeClassifTask(data = Orldataset, target = "label")

#Initialize the Naive Bayes classifier
selected_model = makeLearner("classif.naiveBayes")

#Train the model
NB_mlr = train(selected_model, task)

#Read the model learned  
NB_mlr$learner.model

#Predict on the dataset without passing the target feature
predictions_mlr = as.data.frame(predict(NB_mlr, newdata = Orldataset[,1:645]))

##Confusion matrix to check accuracy
Confusion_matrix =table(predictions_mlr[,1],Orldataset$label) 
View(Confusion_matrix)  

