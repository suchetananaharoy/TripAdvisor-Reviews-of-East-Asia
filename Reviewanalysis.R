# Load required packages
library(readr)
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)

# Load the dataset
dataset <- read_csv("D:/MYFile/12/tripadvisor_review_clean.csv")
dataset

# Examine the dataset
head(dataset)
summary(dataset)
str(dataset)


# Rename columns
colnames(dataset)[colnames(dataset) == "Category 1"] <- "Art Galleries"
colnames(dataset)[colnames(dataset) == "Category 2"] <- "Dance Clubs"
colnames(dataset)[colnames(dataset) == "Category 3"] <- "Juice Bars"
colnames(dataset)[colnames(dataset) == "Category 4"] <- "Restaurants"
colnames(dataset)[colnames(dataset) == "Category 5"] <- "Museums"
colnames(dataset)[colnames(dataset) == "Category 6"] <- "Resorts"
colnames(dataset)[colnames(dataset) == "Category 7"] <- "Parks/Picnic Spots"
colnames(dataset)[colnames(dataset) == "Category 8"] <- "Beaches"
colnames(dataset)[colnames(dataset) == "Category 9"] <- "Theaters"
colnames(dataset)[colnames(dataset) == "Category 10"] <- "Religious Institutions"


str(dataset)
summary(dataset)

#Get rid of UserId column and scale values even though they all range from 1 to 5.
reviews_data <- scale(dataset[,-1])
summary(reviews_data)



# Descriptive analysis
# Perform descriptive analysis on relevant columns
summary(dataset$`Final Rating`)
table(dataset$`Art Galleries`)


library(dplyr)
library(ggplot2)


# Load required packages
library(dplyr)
library(summarytools)

# Perform descriptive analysis
# Summary statistics
summary(dataset)

# Frequency table
freq(dataset$`Final Rating`)

# Cross-tabulation
cross_table(dataset$Category, dataset$Rating)

# Histogram of ratings
hist(dataset$`Final Rating`, breaks = 10, col = "lightblue", main = "Histogram of Ratings", xlab = "Rating")

# Bar plot of categories
barplot(table(dataset$`Art Galleries`), col = "lightblue", main = "Bar Plot of Categories", xlab = "Category", ylab = "Count")


















# Predictive analysis
#K-means clustering
#Trying k = 2
set.seed(44)
kmReviews <- kmeans(dataset$`Final Rating`, centers = 2)
#Plotting clusters
#Plot the data points according to the first two principal components that explain most of the variance

fviz_cluster(kmReviews, data = dataset$`Final Rating`, geom = 'point')



#Select best k

k_values <- seq(2, 10, 1)

set.seed(668822544)
withins_sum <- sapply(k_values, function(k){
  kmeans(dataset, centers = k, nstart = 25)$tot.withinss
})



#“Elbow” method
plot(k_values, withins_sum, type='b', xlab="Number of clusters", ylab="Within groups sum of squares", main="WCSS vs K")



set.seed(668822544)
km3 <- kmeans(dataset$`Final Rating`, centers = 3, nstart = 25)
km4 <- kmeans(dataset$`Final Rating`, centers = 4, nstart = 25)
km5 <- kmeans(dataset$`Final Rating`, centers = 5, nstart = 25)
km6 <- kmeans(dataset$`Final Rating`, centers = 6, nstart = 25)

p3 <- fviz_cluster(km3, dataset$`Final Rating`, geom='point') + ggtitle("3 Clusters")
p4 <- fviz_cluster(km4, dataset$`Final Rating`, geom='point') + ggtitle("4 Clusters")
p5 <- fviz_cluster(km5, dataset$`Final Rating`, geom='point') + ggtitle("5 Clusters")
p6 <- fviz_cluster(km6, dataset$`Final Rating`, geom='point') + ggtitle("6 Clusters")

grid.arrange(p3, p4, p5, p6, ncol = 2)


btw_df <- data.frame(k = 3:6, betweenss = sapply(list(km3, km4, km5, km6), function(km) km$betweenss))
kable(btw_df)



kable(data.frame(k=3:10, wcss_decrease=round(abs(diff(withins_sum)), 2)))


km4_centers <- as.data.frame(round(km4$centers, 3))
km4_centers <- km4_centers %>% gather(Activity, Value)
km4_centers$Cluster <- rep(paste0("Cluster", " ", 1:4), 10)
km4_centers <- arrange(km4_centers, Activity)


ggplot(km4_centers, aes(Activity, Value, group=Cluster, fill=Cluster)) +
  geom_bar(stat="identity", position = position_dodge()) +
  theme(axis.text.x = element_text(hjust = 1, angle = 60)) +
  labs(y = "Scaled Mean Rating", fill="") +
  ggtitle("Cluster means")



#Hierarchical clustering
distances <- dist(dataset, method = "euclidean")
reviews.hclust <- hclust(distances, method = "ward.D")
plot(reviews.hclust)


#Cut dendrogram/tree to obtain 4 clusters
reviews.hclusters <- cutree(reviews.hclust, k=4)
#Split data by cluster
spl <- split(as.data.frame(reviews_data), reviews.hclusters)

hclustDf <- as.data.frame(round(sapply(spl, colMeans), 3))
names(hclustDf) <- c("Cluster1", "Cluster2", "Cluster3", "Cluster4")
hclustDf$Activity <- rownames(hclustDf)

# Delete row names
rownames(hclustDf) <- NULL

# Reorder columns
hclustDf <- hclustDf[,c(5, 1:4)]
hclustDf <- gather(hclustDf, Cluster, Value, -Activity) %>% arrange(Activity)


#Interpreting clusters
ggplot(hclustDf, aes(Activity, Value, group=Cluster, fill=Cluster)) +
  geom_bar(stat="identity", position = position_dodge()) +
  theme(axis.text.x = element_text(hjust = 1, angle = 60)) +
  labs(y = "Scaled Mean Rating", fill="") +
  ggtitle("Cluster means")

