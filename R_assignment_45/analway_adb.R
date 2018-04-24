######
# Adaboost Classifier
# Student Name: Akshay Nalwaya
# Student Unity ID: analway
######

# Do not clear your workspace

require(rpart) # for decision stump
require(caret)
require(mlbench) # for importing Ionosphere dataset
# require(rattle)
# require(rpart.plot)
# require(RColorBrewer)
# set seed to ensure reproducibility
set.seed(100)

# calculate the alpha value using epsilon
# Input params: epsilon
# epsilon: value from calculate_epsilon (or error, line 7 in algorithm 5.7 from Textbook)
# output: alpha value (single value) (from Line 12 in algorithm 5.7 from Textbook)
###
calculate_alpha <- function(epsilon){
    return (0.5*log((1-epsilon)/epsilon))
}

# calculate the epsilon value  
# input:
# weights: weights generated at the end of the previous iteration
# y_true: actual labels (ground truth)
# y_pred: predicted labels (from your newly generated decision stump)
# n_elements: number of elements in y_true or y_pred
# output:
# just the epsilon or error value (line 7 in algorithm 5.7 from Textbook)
###
calculate_epsilon <- function(weights, y_true, y_pred, n_elements){
    return (sum(weights[y_true!=y_pred])/n_elements)
}

# Calculate the weights using equation 5.69 from the textbook 
# Input:
# old_weights: weights from previous iteration
# alpha: current alpha value from Line 12 in algorithm 5.7 in the textbook
# y_true: actual class labels
# y_pred: predicted class labels
# n_elements: number of values in y_true or y_pred
# Output:
# a vector of size n_elements containing updated weights
###
calculate_weights <- function(old_weights, alpha, y_true, y_pred, n_elements){
    new_weights <- ifelse(y_pred==y_true,old_weights*exp(-alpha),old_weights*exp(alpha))
    return (new_weights/sum(new_weights))
}

# implement myadaboost - simple adaboost classification
# use the 'rpart' method from 'rpart' package to create a decision stump 
# Think about what parameters you need to set in the rpart method so that it generates only a decision stump, not a decision tree
# Input: 
# train: training dataset (attributes + class label)
# k: number of iterations of adaboost
# n_elements: number of elements in 'train'
# Output:
# a vector of predicted values for 'train' after all the iterations of adaboost are completed
###
myadaboost <- function(train, k, n_elements){
    # initliazing the vectors
    weights <- rep(1/n_elements,n_elements)
    pred_log <- rep(0,n_elements)
    for(i in 1:k){
        train_set <- train[sample(1:n_elements, n_elements, replace = TRUE, prob = weights),]
        decision_stump <- rpart(Label ~ .,data = train_set,control = rpart.control(maxdepth = 1))
        #print(decision_stump)
        #fancyRpartPlot(decision_stump)
        predictions <- predict(decision_stump,train)
        
        epsilon <- calculate_epsilon(weights,cl,predictions,n_elements)
        while(epsilon > 0.5){
            # Re-initializing the weights
            weights <- rep(1/n_elements,n_elements)
            train_set <- train[sample(1:n_elements, n_elements, replace = TRUE, prob = weights),]
            decision_stump <- rpart(Label ~ .,data = train_set,control = rpart.control(maxdepth = 1))
            predictions <- predict(decision_stump,train)
            epsilon <- calculate_epsilon(weights,cl,predictions,n_elements)
        }
        alpha <- calculate_alpha(epsilon)
        pred_log <- pred_log + predictions*alpha
        weights <- calculate_weights(weights,alpha,train$Label,predictions,n_elements)
    }
    pred_log <- ifelse(pred_log<0,-1,1)
    return (pred_log)
}

# Code has already been provided here to preprocess the data and then call the adaboost function
# Implement the functions marked with ### before this line
data("Ionosphere")
Ionosphere <- Ionosphere[,-c(1,2)]
# lets convert the class labels into format we are familiar with in class
# -1 for bad, 1 for good (create a column named 'Label' which will serve as class variable)
Ionosphere$Label[Ionosphere$Class == "good"] = 1
Ionosphere$Label[Ionosphere$Class == "bad"] = -1
# remove unnecessary columns
Ionosphere <- Ionosphere[,-(ncol(Ionosphere)-1)]
# class variable
cl <- Ionosphere$Label

# train and predict on training data using adaboost
predictions <- myadaboost(Ionosphere, 5, nrow(Ionosphere))
# generate confusion matrix
print(table(cl, predictions))
# Not asked in the question
#print(paste0("Accuracy: ",(table(cl, predictions)[1,1]+table(cl, predictions)[2,2])/351))
