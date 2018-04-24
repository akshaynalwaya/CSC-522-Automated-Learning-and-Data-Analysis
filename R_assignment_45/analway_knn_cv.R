######
# kNN with cross validation
# Student Name: Akshay Nalwaya
# Student Unity ID: analway
######

# Do not clear your workspace


# load required libraries
require(class) # for KNN
require(caret) # for train, createDataPartition, createFolds...
# set seed to ensure reproducibility
set.seed(100)

data(iris)

# normalize all predictors, i.e., all but last column species
iris[, -ncol(iris)] <- scale(iris[, -ncol(iris)])

# split the data into training and test sets 70/30 split
trainIdx <- createDataPartition(1:nrow(iris), p = 0.7, list = FALSE)
testIdx <- setdiff((1:nrow(iris)), trainIdx)
train <- iris[trainIdx, -ncol(iris)]
test <- iris[testIdx, -ncol(iris)]

# convert class variable to type factor
cl <- factor(iris[trainIdx, ncol(iris)])

# use knn from class package to predict
knnPreds <- knn(train, test, cl, k = 3, prob = TRUE)

table(knnPreds, factor(iris[testIdx, ncol(iris)]))

# implement myknncv - knn with cross validation
# use the standard knn function for training/testing
# implement the cross validation section
# divide training data into 'numFolds' folds, use one fold for validation, others for training
# repeat this 'numFolds' times
myknncv <- function(train, test, cl, k, numFolds)
{
   # create k folds
   ###
   folds <- createFolds(cl,numFolds)
   fold_accuracies <- vector()
   for(i in 1:numFolds){
           # use ith fold for validation
           ###
           validation_set <- train[folds[[i]], ]
           # rest of the folds as training
           ###
           cv_train <- train[-folds[[i]], ]
           # train class variable
           ###
           cv_train_cl <- cl[-folds[[i]]]
           # predict using cv_train, cv_train_cl and validation_set
           ###
           knnpreds <- knn(cv_train, validation_set, cv_train_cl, k, prob = TRUE)
           # calculate accuracy
           tab <- table(knnpreds, factor(cl[folds[[i]]]))
           fold_accuracies <- c(fold_accuracies, sum(diag(tab)) / sum(tab))
   }
   #final accuracy is mean of accuracies of all folds
   cv_accuracy <- mean(fold_accuracies)
   return(cv_accuracy)
}

# perform KNN classification for k in 2:10 using 10-fold CV to estimate 
# test accuracy
accuracies <- vector()
for(nn in 2:10)
    accuracies <- c(accuracies, myknncv(train, test, cl, k = nn, numFolds = 10))

# plot k vs. 10-fold cv accuracy
plot(2:10, accuracies, type = 'l', xlab = 'Number of nearest neighbors',
     ylab = '10-fold cv accuracy',
     main = 'How K affects an estimate of test accuracy')


