
library(dplyr)

# read data
wine <- readRDS("small_data/wine.RDS")

wine %>% head

## check correlations
cor(wine)

## https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
library(corrplot)                       
corrplot(wine %>% cor, method="ellipse", order="FPC")

# kmeans() is included in base R

# specify K, and number of random starts (nstart)
kk <- 3
kmeans <- kmeans(wine, centers=kk, nstart = 100)

kmeans

# cluster labels for each row
kmeans$cluster

# size of cluster
kmeans$size

kmeans$centers

library(cluster)

# daisy(): Compute all the pairwise dissimilarities (distances) between observations in the data set.
# The original variables may be of mixed types.
dissimilarity <- daisy(wine)
silhouette <- silhouette(kmeans$cluster, dissimilarity)

plot(silhouette)

clusplot(wine, kmeans$cluster, color=TRUE, labels = 5)

# princomp() is included in base R

#wine_pca <- princomp(wine, cor=FALSE)
#wine_pca %>% summary                    #why?
#wine_pca %>% loadings

wine_pca <- princomp(wine, cor=TRUE)

wine_pca %>% summary

wine_pca %>% loadings

## visualize lambdas
wine_pca$sdev**2
wine_pca %>% screeplot

#wine_pca$sdev %>% barplot

## visualize first 2 components
wine_predict <- predict(wine_pca)
df <- as_data_frame(wine_predict)

library(ggplot2)
ggplot(df, aes(x=Comp.1, y=Comp.2, label=row.names(df))) +
    geom_text() +
    ggtitle("First 2 principal components\n(with row labels)") +
    labs(x="First PC", y="Second PC")

# showing variable loadings
wine_pca %>% biplot

## create a bootstrapped confidence interval for % variance explained
value <- 0
repeats <- 10000
for(i in 1:repeats){
  a <- sample(1:180, size=180, replace=TRUE)
  correlations <- cor(wine[a,])
  value[i] <- sum(svd(correlations)$d[1:5])/13
}

## .803 = variance explained by first 5 PCs, from the data
qplot(value, binwidth=0.001) +
    geom_vline(aes(xintercept=0.803,
                   colour="red")) +
    labs(x="Percentage of variance explained by first 5 principal components")

c(mean=mean(value),median=median(value))
## the CI here bootstrap ci
quantile(value,c(0.025,0.975))
