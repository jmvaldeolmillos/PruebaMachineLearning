# K-MEANS CLUSTERING

iris2 <- iris
iris2$Species <- NULL

# Eliminamos la Species en la del cluster. Creamos los clusters, en total 3
(kmeans.result <- kmeans(iris2, 3))
# Se muestra el resultado.
table(iris$Species, kmeans.result$cluster)

plot(iris2[c("Sepal.Length", "Sepal.Width")], col= kmeans.result$cluster)

#Ploteo de centros
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col= 1:3, pch=8, cex=2)

# ====================================================

# K-MEDIOIDES CLUSTERING
library(fpc)
pamk.result <- pamk(iris2)
# numero de clusters
pamk.result$nc

# Chequeo de clusters
table(pamk.result$pamobject$clustering, iris$Species)
layout(matrix(c(1,2),1,2))
plot(pamk.result$pamobject)
layout(matrix(1))

# Vamos a forzar que los clusters sean 3 (lo suyo)
pamk.result <- pamk(iris2, 3)
table(pamk.result$pamobject$clustering, iris$Species)
layout(matrix(c(1,2),1,2))
plot(pamk.result$pamobject)
layout(matrix(1))

# ====================================================

# Hierarchical Clustering. Con uso de hclust()

idx <- sample(1:dim(iris)[1], 40)
irisSample <- iris[idx,]
irisSample$Species <- NULL
hc <- hclust(dist(irisSample), method = "ave")

# pintamos
plot(hc, hang=-1, labels=iris$Species[idx])
# cortamos en 3 clusters
rect.hclust(hc, k=3)
groups <- cutree(hc, k=3)

# ====================================================

# Clustering basados en Densidad.
library(fpc)
iris2 <- iris[-5]
ds <- dbscan(iris2, eps = 0.42, MinPts = 5)

# comparamos las clases con las originales
table(ds$cluster, iris$Species)
# la fila 0 son ruido o outlayers.

# Mostramos todo
plot(ds, iris2)

# Seleccionamos variables
plot(ds, iris2[c(1,4)])

# Para ver los clusters
plotcluster(iris2, ds$cluster)


# El clustering se puede usar para etiquetar nuevos datos:

# creamos los datos
set.seed(435)
idx <- sample(1:nrow(iris),10)
newData <- iris[idx, -5]
newData<- newData + matrix(runif(10*4, min=0, max=0.2), nrow=10, ncol=4)

# Etiquetamos los nuevos datos:
mypred <- predict(ds, iris2, newData)

# plot Resultados
plot(iris2[c(1,4)], col=1+ds$cluster)
points(newData[c(1,4)], pch="*", col=1+mypred, cex=3)

# chequeo de los resultados
table(mypred, iris$Species[idx])
