################################################################################
##Team Names##
##############
#1-Ammar yasser farouk abuelnaga
#2-Amr ahmed mahmoud metwally
#3-Omar ibrahim ali abdelgani
#4-Ali ahmed taha ahmed
#5-Ahmed salama ibrahim salama
################################################################################
##necessary libraries##
#######################
library(dplyr)
library(factoextra)
#DB scan
library(fpc)
################################################################################
##Load the data and read it##
#############################
data <- read.csv("C:/Users/Lenovo/OneDrive/Desktop/INF_PRO/Heartdiseas.txt")
#View(data)
#data type
class(data)
#summary
str(data)
#columns name
names(data)
################################################################################
##cleaning the data##
#####################
# Remove rows with duplicated IDs
data <- data[!duplicated(data$id), ]
# Convert negative ages to positive values
data$age <- abs(data$age)
# Rearrange the id to start from 1
data$id <- seq_len(nrow(data))

data_outlier<-data

# Outlier Detection
cols <- c("age", "thalach", "trestbps", "oldpeak", "chol")

for (col in cols) {
  q1 <- quantile(data[[col]], 0.25)
  q3 <- quantile(data[[col]], 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  # Filtering outliers for each column
  data <- data[data[[col]] >= lower_bound & data[[col]] <= upper_bound, ]
}


################################################################################
##visualization##
#################
# 1- Pie #
##########
# pie for slope
slope<- table(data$slope)
pie(slope,names(slope),main = " summery of slope")
################
# 2- Histogram #
################
hist(data$age, col = "lightblue", xlab = "Age", main = "Age Histogram without outliers")
hist(data$thalach, col = "lightblue", xlab = "thalach", main = "thalach Histogram without outliers")
hist(data$trestbps, col = "lightblue", xlab = "trestbps", main = "trestbps Histogram without outliers")
hist(data$oldpeak, col = "lightblue", xlab = "oldpeak", main = "oldpeak Histogram without outliers")
hist(data$chol, col = "lightblue", xlab = "Chol", main = "Chol Histogram without outliers")
##############
# 3- Boxplot #
##############
boxplot(data_outlier$age, col = rainbow(6), ylab = "Ages Boxplot with outliers")
boxplot(data_outlier$thalach, col = rainbow(6), ylab = "thalach Boxplot with outliers")
boxplot(data_outlier$trestbps, col = rainbow(6), ylab = "trestbps Boxplot with outliers")
boxplot(data_outlier$oldpeak, col = rainbow(6), ylab = "oldpeak Boxplot with outliers")
boxplot(data_outlier$chol, col = rainbow(6), ylab = "Chol Boxplot with outliers")

boxplot(data$age, col = rainbow(6), ylab = "Ages Boxplot without outliers")
boxplot(data$thalach, col = rainbow(6), ylab = "thalach Boxplot without outliers")
boxplot(data$trestbps, col = rainbow(6), ylab = "trestbps Boxplot without outliers")
boxplot(data$oldpeak, col = rainbow(6), ylab = "oldpeak Boxplot without outliers")
boxplot(data$chol, col = rainbow(6), ylab = "Chol Boxplot without outliers")
################################################################################
##clustring techniques##
########################
############
##1-Kmeans##
############

# Selecting specific columns for clustering
selected_cols <- data[, c('age', 'thalach','oldpeak' ,'trestbps', 'chol')]

# Normalizing the data
normalized_data <- scale(selected_cols)

wss <- numeric(100)

for (i in 1:100) {
  km <- kmeans(normalized_data, centers = i, nstart = 100)
  wss[i] <- sum(km$withinss)
}

plot(1:10, wss[1:10], type = 'b', xlab = 'Number of clusters', ylab = 'Within-cluster sum of squares')
optimal_num_clusters <- 2

# Performing K-means clustering
kmeans_model <- kmeans(normalized_data, centers = optimal_num_clusters, nstart = 20)

# Adding cluster labels to data
data$cluster <- kmeans_model$cluster

# Viewing the clusters
table(data$cluster)

# Function to create a scatter plot with cluster centers 2x1
create_cluster_plot <- function(x_var, y_var) {
  plot(normalized_data[, c(x_var, y_var)], col = data$cluster, 
  main = "Cluster Visualization", xlab = x_var, ylab = y_var)
  points(kmeans_model$centers[, c(x_var, y_var)], col = 1:3, pch = 8, cex = 2)
}

# Generate scatter plots for different pairs of variables
create_cluster_plot("age", "chol")
create_cluster_plot("age", "trestbps")
create_cluster_plot("age", "thalach")

# Another visualization way for all data by using fviz_cluster
fviz_cluster(kmeans_model, data = normalized_data , geom = "point",
             main = "Kmeans Clustering Results",)

# view updating in my data
View(data)
################################################################################
##2- Hierarchical clustering##
##############################
## I know its not usable for large data and make bad visualization but i use it

distance_mat <- dist(normalized_data, method = 'euclidean')
hc <- hclust(distance_mat)

# Plotting dendrogram
plot(hc, main = "Hierarchical Clustering Dendrogram", hang = -1)

# Cutting tree by no. of clusters
fit <- cutree(hc, k = 3 )

rect.hclust(hc, k = 3, border = "green")

# Adding hierarchical cluster labels to data
data$cluster_hierarchical <- fit

View(data)
################################################################################
##3- DB scan##
##############
dbscan_result <- dbscan(normalized_data, eps = 0.7, MinPts = 2)
dbscan_result$cluster

# Function to create a scatter plot with cluster centers 2x1
create_cluster_plot_db <- function(x_var, y_var) {
  plot(normalized_data[, c(x_var, y_var)], col = dbscan_result$cluster, 
       main = "Cluster Visualization", xlab = x_var, ylab = y_var)
  points(dbscan_result$centers[, c(x_var, y_var)], col = 1:3, pch = 8, cex = 2)
}

# Generate scatter plots for different pairs of variables
create_cluster_plot_db("age", "chol")
create_cluster_plot_db("age", "trestbps")
create_cluster_plot_db("age", "thalach")

# Another visualization way for all data by using fviz_cluster
fviz_cluster(
  dbscan_result,
  normalized_data,
  geom = "point",
  main = "DBSCAN Clustering Results",
)

################################################################################
