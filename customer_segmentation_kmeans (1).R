############################################################
# Customer Segmentation using K-Means Clustering
# Author : Ankur Pandey
############################################################

rm(list = ls())

library(ggplot2)

mall_data <- read.csv("Mall_Customers.csv")

head(mall_data)
summary(mall_data)
str(mall_data)

features <- mall_data[, c("Annual Income (k$)", "Spending Score (1-100)")]

features_scaled <- scale(features)

wcss <- numeric(10)

for(k in 1:10){
  set.seed(42)
  km <- kmeans(features_scaled,
               centers = k,
               nstart = 25,
               iter.max = 300)
  wcss[k] <- km$tot.withinss
}

plot(1:10, wcss,
     type="b",
     pch=19,
     col="steelblue",
     xlab="Number of Clusters (k)",
     ylab="Within Cluster Sum of Squares",
     main="Elbow Method")

abline(v=5, col="red", lty=2)

set.seed(42)

km_model <- kmeans(features_scaled,
                   centers=5,
                   nstart=25,
                   iter.max=300)

mall_data$Cluster <- factor(km_model$cluster)

print(km_model$centers)
print(km_model$size)
print(km_model$tot.withinss)

ggplot(mall_data,
       aes(x=`Annual Income (k$)`,
           y=`Spending Score (1-100)`,
           color=Cluster)) +
  geom_point(size=3.5, alpha=0.85) +
  labs(title="Customer Segmentation using K-Means",
       x="Annual Income (k$)",
       y="Spending Score (1-100)") +
  theme_minimal(base_size=13)

aggregate(mall_data[, c("Annual Income (k$)",
                        "Spending Score (1-100)")],
          by=list(Cluster=mall_data$Cluster),
          FUN=mean)
