# Importing the dataset
dataset = read.csv('dataset2.csv')
dataset = dataset[2:11]

#data cleaning
clean <- function(ttt){
  as.numeric( gsub('[?]', '', ttt))
}
dataset[] <- sapply(dataset, clean)

for(i in 1:ncol(dataset)){
  dataset[is.na(dataset[,i]), i] <- mean(dataset[,i], na.rm = TRUE)
}
dataset$Class...2.for.benign...4.for.malignant.=as.factor(dataset$Class...2.for.benign...4.for.malignant.)

# Splitting the dataset into the Training set and Test set
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Class...2.for.benign...4.for.malignant., SplitRatio = 0.70)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

training_set[-10]=scale(training_set[-10])

test_set[-10]=scale(test_set[-10])

# Fitting SVM Classification to the Training set
#install.packages('rpart')
library(e1071)
classifier = svm(formula = Class...2.for.benign...4.for.malignant.  ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial'
      
                 )

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-10])

# Making the Confusion Matrix
cm = table(test_set[, 10], y_pred)

#print confusion matrix
print(cm, mode = cm$mode, digits = max(3,getOption("digits") - 3), printStats = TRUE)

#Result accuracy
accuracy<-sum(diag(cm))/sum(cm)

#print accuracy
print(accuracy*100)

#False Positive
FP<- cm[1,2]
print(FP)

#False Negative
FN<- cm[2,1]
print(FN)
  
#applying grid search to improve svm model
library(caret)
classifier = train(form = Class...2.for.benign...4.for.malignant. ~ ., data = training_set, method = 'svmRadial',
                   sigma=0.7459332
                   )

classifier
classifier$bestTune

#prediction
y_pred = predict(classifier, newdata = test_set[-10])

#confusion matrix
cm = table(test_set[, 10], y_pred)

#calculate accuracy
accuracy<-sum(diag(cm))/sum(cm)

#print accuracy
print(accuracy*100)

#False Positive
FP<- cm[1,2]
print(FP)

#False Negative
FN<- cm[2,1]
print(FN)