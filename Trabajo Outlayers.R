# Univariate Detection Outlayer

set.seed(3147)
x <- rnorm(100)
summary(x)

# Detección de outlayers
boxplot.stats(x)$out
boxplot(x)

# Creación de un data.frame
y <- rnorm(100)
df <- data.frame(x,y)
rm(x,y)
head(df)

attach(df)
# extracción de indices de outlayers en x
a <- which(x %in% boxplot.stats(x)$out)
a

# extracción de indices de outlayers en y
b <- which(y %in% boxplot.stats(y)$out)
b

# extracción de indices de outlayers en ambos a la vez. Coinciden los indices (2)
c <- intersect(a,b)
c

# Podemos pintarlos
plot(df)
points(df[c,], col="red", pch="+", cex=2.5)

# extracción de indices de outlayers en los dos.
d <- union(a,b)
d

# Podemos pintarlos
plot(df)
points(df[d,], col="red", pch="+", cex=2.5)

detach(df)


# ================================================================================

# Detección de Outlayers con LOF

library(DMwR)

# Cogemos cualquier dato...
iris2 <- iris[,1:4]
outlier.scores <- lofactor(iris2, k=5)
plot(density(outlier.scores))

outliers <- order(outlier.scores, decreasing = TRUE)[1:5]
# Los outlayers
print(outliers)
print(iris2[outliers,])


# ================================================================================

# Detección de Outlayers con Clustering

# Cogemos cualquier dato...
iris2 <- iris[,1:4]

kmeans.result <- kmeans(iris2, centers = 3)
# centros
kmeans.result$centers

# cluster ID's
kmeans.result$cluster

# calculos de distancia
centers <- kmeans.result$centers[kmeans.result$cluster, ]
distances <- sqrt(rowSums((iris2 - centers)^2))
# 5 distancias más largas
outliers <- order(outlier.scores, decreasing = TRUE)[1:5]
# mostrando outlayers
print(outliers)
print(iris2[outliers, ])

#ploteamos
plot(iris2[,c("Sepal.Length", "Sepal.Width")], pch="o", col=kmeans.result$cluster, cex=0.3)
# centros
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=1.5)
#plot outlayers
points(iris2[outliers, c("Sepal.Length", "Sepal.Width")], pch="+", col=4, cex=1.5)
