######
# Bisecting K-Means and KNN Classifier
# Rename this file before submitting 
#####

require(RColorBrewer)
require(class)
require(caret)
require(data.table)

# set seed to ensure consistent results
set.seed(100)


################################################
#   TA defined functions
#   Code has already been provided for you - you don't need to modify this 
###############################################
# Plot clustering result
plot_clustering_result <- function(data.df, result, title, k){
  # Plot using two columns in the data
  plot(data.df[, c(2,4)], col = brewer.pal(k, "Set1")[result[[1]]], pch = '.',
       cex = 3, main = title)
}

################################################
# GRADED FUNCTIONS
# Write your code using the given function definitions
# Input and output types are defined for each function
###############################################

# Implement bisecting k means.
# Input:
# data.df: data frame loaded using load_data() function
# iter.max: Max. number of trials for kmeans, as per Algorithm 8.2 in textbook. 
# You are allowed to use the pre-defined kmeans() function.
# k: Number of clusters to find in bisecting k-means

# Output:
# Your output/function return value will be a list containing 2 elements
# first element of the list is a vector containing cluster assignments (i.e., values 1 to k assigned to each data point)
# second element of the list is a vector containing SSE of each cluster
# Additional Information:
# When identifying which cluster to split, choose the one with maximum SSE
# When performing kmeans, pick two random points the cluster with largest SSE as centers at every iteration. 
# Ensure that the two centers being randomly picked are not the same.
# terminating condition: when k clusters have been found
# bisecting k means 
bkm <- function(data.df, iter.max, k){
    
    # removing the ID field from given data 
    data.df$ID <- NULL
    fit_kmeans <- kmeans(data.df,centers = 1,algorithm = "Lloyd")
    sse <- fit_kmeans$tot.withinss
    data.df$cluster <- 1
    clusters <- data.df
    # to store the cluster ID to SSE mapping
    cluster_map <- data.frame(cluster_id = numeric(),sse = numeric())
    for(i in 1:k){
        
        if(i==1){
            cluster_map[nrow(cluster_map)+1,] = c(1,sse)
        }
        max_record <- subset(cluster_map,sse == max(cluster_map$sse))
        clust_id <- max_record[1,]$cluster_id
        # fetch and remove the cluster with maximum SSE
        cluster_map <- subset(cluster_map,cluster_id!=clust_id)
        temp_data <- subset(clusters,cluster == clust_id)
        temp_data$cluster <- NULL
        clusters <- subset(clusters,cluster!=clust_id)
        # applying kmeans to pick clustering with minimum overall SSE
        for(j in 1:iter.max){
            temp_fit <- kmeans(temp_data,centers = 2, algorithm = "Lloyd")
            if(j==1){
                min_sse <- temp_fit$tot.withinss
                best_fit <- temp_fit
            }
            if(temp_fit$tot.withinss < min_sse){
                min_sse = temp_fit$tot.withinss
                best_fit <- temp_fit
            }
        }
        
        temp_data$cluster <- best_fit$cluster
        if(i==1){
            max_val <- 0
            clusters <- temp_data
        }    
        else{
            max_val <- max(clusters$cluster) - 1
            clust <- clusters$cluster
            for(x in 1:length(clust)){
                clust[x] <- ifelse(clust[x] > clust_id,clust[x]-1,clust[x])
            }
            clusters$cluster <- clust
            
            clust <- cluster_map$cluster_id
            for(x in 1:length(clust)){
                clust[x] <- ifelse(clust[x] > clust_id,clust[x]-1,clust[x])
            }
            cluster_map$cluster_id <- clust
            clust <- temp_data$cluster
            for(x in 1:length(clust)){
                clust[x] <- clust[x] + max_val
            }
            temp_data$cluster <- clust
            clusters <- rbind(clusters,temp_data)
        }
        new_sse <- best_fit$withinss
        new_id <- unique(best_fit$cluster) + max_val

        cluster_map[nrow(cluster_map)+1,] = c(new_id[1],new_sse[1])
        cluster_map[nrow(cluster_map)+1,] = c(new_id[2],new_sse[2])
    }
    cluster_assigned <- list()
    cluster_assigned <- clusters$cluster
    cluster_sse <- list()
    cluster_sse <- cluster_map$sse
    
    return(list(cluster_assigned,cluster_sse))
}


# Write code for comparing kmeans with result from bisecting kmeans here - Part b
# Input:
# data.df:  Dataset used for kmeans/bisecting kmeans clustering 
# Result: Variable of type list, obtained on running bkm() function
# k : k value for k-means
# km_centers: ID values of centers for KMeans

#Returns:
# Nothing, just print the observations requested

kmeans_comparison <- function(data.df, result, k, km_centers){
    # First, run KMeans using km_centers and k. 
    # Compare outcomes of kmeans with bisecting kmeans in terms of:
    # 1. Overall SSE
    # 2. Plot the clusters and compare the size and shape of clusters generated
    # 3. Using the plot, also verify (visually) if points are being assigned to different clusters
    init_centers <- subset(data.clustering,ID %in% km_centers)
    init_centers$ID <- NULL
    # since default parameters have to be used, not specifying the algorithm as "Lloyd"
    fit.kmean <- kmeans(data.df[,2:5],centers = init_centers)
    kmeans_result <- fit.kmean$centers
    # plotting the result
    plot(data.df[,c(2,4)], col = brewer.pal(clustering_k,"Set1")[as.vector(fit.kmean$cluster)], 
         pch = '.', cex = 3, main = "kMeans Outcome")
    
    print(paste0("Total SSE (Bisecting kMeans): ",sum(result[[2]])))
    print(paste0("Total SSE (kMeans with default parameters): ",fit.kmean$tot.withinss))
}


################################
# Classification
################################
# gives the Euclidean distance between two instances
my_distance <- function(a,b){
    return (sqrt(sum((a$Sepal.Length - b$Sepal.Length)^2,(a$Sepal.Width - b$Sepal.Width)^2,(a$Petal.Length - b$Petal.Length)^2,(a$Petal.Width - b$Petal.Width)^2)))
}

# Write code for KNN algorithm
# implement my_knn with euclidean distance, majority vote and 
# randomly resolving ties
# you are NOT allowed to use the built-in knn function or the dist() function
# (i.e., implement the K Nearest Neighbors, also implement a function for euclidean distance calculation)

# Input: 
# train: training data frame
# test: test data frame
# cl: class labels for training data
# k: 'k' value in KNN

# Output:
# A vector of class labels. return variable should be of type factor
my_knn <- function(train, test, cl, k){
    temp_data <- data.frame(class = cl)
    temp_data$dist <- ""
    for (i in 1:nrow(test)){
        train$dist <- ""
        for (j in 1:nrow(train)){
            temp_data$dist[j] = my_distance(train[j,],test[i,])
        }
        # Sorting the data frame based on Euclidean distances
        ordered_data <- temp_data[order(temp_data[,2]),]
        # Getting the first k neighbours
        neighbours <- ordered_data[1:k,]
        # Counting frequency of the neighbours
        count_setosa <- nrow(neighbours[neighbours$class == "setosa",])
        count_versicolor <- nrow(neighbours[neighbours$class == "versicolor",])
        count_virginica <- nrow(neighbours[neighbours$class == "virginica",])
        if(count_setosa < count_versicolor){
            if(count_virginica < count_versicolor)
                test$class[i] <- "versicolor"
            else
                test$class[i] <- "virginica"
        }
        else{
            if(count_setosa < count_virginica)
                test$class[i] <- "virginica"
            else
                test$class[i] <- "setosa"
        }
    }
    return (as.factor(test$class))
}

# Generate accuracy measures for your KNN classification
# Input:
# test_cl: actual class labels for test data
# knn_result: predicted class labels for test data

# Output:
# A vector of size 4 in the following order: 
# (overall accuracy, precision for the class 'setosa', recall for the class 'setosa', F-measure for the class 'setosa')

# DONOT use predefined libraries to calculate the accuracy measures
# Implement the formulae by generating the confusion matrix using the table() function
my_knn_accuracy <- function(test_cl, knn_result){
    # Generating the confusion matrix
    confusion_matrix <- table(Predicted = knn_result,Actual = test_cl)
    #print(confusion_matrix)
    
    true_positive <- length(test_cl[test_cl == knn_result])
    setosa_true_positive <- sum(confusion_matrix[1,1])
    setosa_false_positive <- sum(confusion_matrix[1,2:3])
    setosa_false_negative <- sum(confusion_matrix[2:3,1])
    
    accuracy <- true_positive/sum(confusion_matrix[,])
    precision <- setosa_false_positive/sum(setosa_true_positive,setosa_false_positive)
    recall <- setosa_true_positive/sum(setosa_true_positive,setosa_false_negative)
    f_measure <- (2*recall*precision)/(precision + recall)
    return (c(accuracy,precision,recall,f_measure))
}
