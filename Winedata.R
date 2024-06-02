install.packages("readxl")
library(readxl)
file_path <- "D:/UOW/L5/ML/CW/Whitewine_v6.xlsx"
df <- read_excel(file_path)
df
library(tidyr)
library(ggplot2)
df_long <- gather(df, key = "feature", value = "value")
ggplot(df_long, aes(x = feature, y = value)) +
  geom_boxplot() +
  labs(x = "Feature", y = "Value") +
  ggtitle("Boxplot of 11 Features")
dfNormZ <- as.data.frame( scale(df[1:11] ))
dfNormZ
dfNormZ_long <- gather(dfNormZ, key = "feature", value = "value")
#ggplot(dfNormZ_long, aes(x = feature, y = value)) +
  geom_boxplot() +
  labs(x = "Feature", y = "Value") +
  #ggtitle("Boxplot of 11 Features")
remove_outliers <- function(data, k = 1.5) {
  Q1 <- apply(data, 2, quantile, probs = 0.25)
  Q3 <- apply(data, 2, quantile, probs = 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - k * IQR
  upper_bound <- Q3 + k * IQR
  return(data[apply(data, 1, function(x) all(x >= lower_bound & x <= upper_bound)), ])
}
dfNoOutliers <- remove_outliers(dfNormZ)
dfNoOutliers
dfNoOutliers_long <- gather(dfNoOutliers, key = "feature", value = "value")
ggplot(dfNoOutliers_long, aes(x = feature, y = value)) +
  geom_boxplot() +
  labs(x = "Feature", y = "Value") +
  ggtitle("Boxplot of 11 Features")
install.packages("NbClust")
library(NbClust)
nb <- NbClust(data = dfNoOutliers, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
plot(nb)
library(factoextra)
elbow <- fviz_nbclust(dfNoOutliers, kmeans, method = "wss")
plot(elbow, frame = FALSE, xlab="Number of Clusters (k)", ylab="Within-cluster Sum of Squares (WSS)")
library(cluster)
gapstat <- fviz_nbclust(dfNoOutliers, kmeans, method="gap_stat")
plot(gapstat)
silhouette <- fviz_nbclust(dfNoOutliers, kmeans, method = "silhouette", k.max = 10)
plot(silhouette, frame = FALSE,  xlab = "Number of clusters K", ylab = "Average Silhouettes")
kc <- kmeans(dfNoOutliers,2)
kc
center <- kc$centers
center
bss = kc$betweenss
bss
tss = kc$totss
tss
bss_tss_ratio <- bss / tss
bss_tss_ratio
wss = kc$tot.withinss
wss
sil <- silhouette(kc$cluster, dist(dfNoOutliers))
fviz_silhouette(sil)

wine.cov <- cov(dfNoOutliers)
wine.eigen <- eigen(wine.cov)
str(wine.eigen)

phi <- wine.eigen$vectors[, 1:11]
row.names(phi) <- c("fixed acidity", "volatile acidity", "citric acid", "residual sugar","chlorides", "free sulfur dioxide", "total sulfur dioxide:", "density","pH", "sulphates", "alcohol")
colnames(phi) <- c("PC1", "PC2","PC3", "PC4","PC5", "PC6","PC7", "PC8", "PC9", "PC10", "PC11")
phi

pca_result <- prcomp(dfNoOutliers, scale. = TRUE)
summary_pca <- summary(pca_result)
VE <- summary_pca$sdev^2 / sum(summary_pca$sdev^2)
cumulative_score <- cumsum(VE)
print(cumulative_score)

wine_transform = as.data.frame(pca_result$x[,1:7])
nb2 <- NbClust(data = wine_transform, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
plot(nb2)
elbow2 <- fviz_nbclust(wine_transform, kmeans, method = "wss")
plot(elbow2, frame = FALSE, xlab="Number of Clusters (k)", ylab="Within-cluster Sum of Squares (WSS)")
best_k_elbow <- as.numeric(elbow2$data[which.min(elbow$data$y), "clusters"])
plot(best_k_elbow)
plot(elbow, frame = FALSE, xlab="Number of Clusters (k)", ylab="Within-cluster Sum of Squares (WSS)")
gapstat <- fviz_nbclust(wine_transform, kmeans, method="gap_stat")
plot(gapstat)
silhouette2 <- fviz_nbclust(wine_transform, kmeans, method = "silhouette", k.max = 10)
plot(silhouette2, frame = FALSE,  xlab = "Number of clusters K", ylab = "Average Silhouettes")
kc2 <- kmeans(wine_transform,2)
kc2
center2 <- kc2$centers
center2
bss2 = kc2$betweenss
bss2
tss2 = kc2$totss
tss2
bss_tss_ratio2 <- bss / tss
bss_tss_ratio2
wss2 = kc2$tot.withinss
wss2
sil2 <- silhouette(kc2$cluster, dist(wine_transform))
fviz_silhouette(sil2)

calinski_harabasz <- calinski_harabasz_index(wss2, bss2, kc2)

# Print the Calinski-Harabasz Index
print(calinski_harabasz)
library(cluster)


kmeans_model <- kmeans(wine_transform, centers = 3)  

# Calculate the Calinski-Harabasz Index
calinski_harabasz <- calinski.harabasz(wine_transform, kmeans_model$cluster)

# Print the Calinski-Harabasz Index
print(calinski_harabasz)