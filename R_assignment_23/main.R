# Main file
# All function calls will happen here
# DO NOT submit this file - TA will use his own version of this file, with 
# function calls using the same format as shown here, with different parameter values

# clear memory
rm(list=ls(all=T))
# clear terminal screen 
cat('\014')

# Enter your unity id here, after renaming your_unity_id.R 
your_unity_id <- "analway"
functions_file_name <- paste('./', your_unity_id, '.R', sep='')
source(functions_file_name)

################################################
#   TA defined functions (load_data_clustering, load_data_classification)
#   Code has already been provided for you - you don't need to modify these functions 
###############################################

# Some useful functions to load data and plot data
# Load data for KMeans and KNN
load_data_clustering <- function(){
  data(iris)
  iris$ID <- c(1:nrow(iris)) # Create an id column for easy identification
  return(iris[,c('ID','Sepal.Length','Sepal.Width','Petal.Length', 'Petal.Width')])
}

load_data_classification <- function(){
  data(iris)
  # normalize all predictors, i.e., all but last column species
  iris[, -ncol(iris)] <- scale(iris[, -ncol(iris)])
  trainIdx <- createDataPartition(1:nrow(iris), p = 0.7, list = FALSE) # separate data into train, test (70, 30)
  testIdx <- setdiff((1:nrow(iris)), trainIdx)
  train <- iris[trainIdx, -ncol(iris)]
  test <- iris[testIdx, -ncol(iris)]
  train_cl <- factor(iris[trainIdx, ncol(iris)])
  test_cl <- factor(iris[testIdx, ncol(iris)])
  return(list(train, test, train_cl, test_cl))
}


# load the data for clustering
data.clustering <- load_data_clustering()

# k value for KMeans and Bisecting KMeans
# you can play around with the clustering_k variable to see how bisecting kmeans behaves as k value changes
clustering_k <- 3

# run bisecting kmeans
bkm_result <- bkm(data.clustering, 25, clustering_k)

# plot bisecting kmeans result
plot_clustering_result(data.clustering, bkm_result, "Bisecting KMeans outcome", clustering_k)

# compare bisecting kmeans vs Kmeans
km_centers <- c(15,16,19)
kmeans_comparison(data.clustering, bkm_result, clustering_k, km_centers)


################################
# Classification
################################
# load the data for classification
data.classification <- load_data_classification()
train <- data.classification[[1]]
test <- data.classification[[2]]
train_cl <- as.factor(data.classification[[3]])
test_cl <- as.factor(data.classification[[4]])

# k value for KNN - Play around with different k values to see which 
# k gives best accuracy
classification_k <- 10

# run KNN classifier
knn_result <- my_knn(train, test, train_cl, classification_k)

# get the accuracy measures for K
my_knn_result <- my_knn_accuracy(test_cl, knn_result)

