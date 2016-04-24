# Random Forest con el paquete RandomForest

# Limitaciones: No maneja missing values. Valores de variables categóricas menos de 32 niveles.
# Opción: Uso de cforest dentro del paquete party que no posee la limitación de las variables categoricas.

set.seed(1234)

# creamoa el training y test con 70% training y 30% test.
ind <- sample(2, nrow(iris), replace = TRUE, prob=c(0.7,0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]

# Arbol de decisión de Species en función de las demás variables:
library(randomForest)
rf <- randomForest(Species ~ ., data=trainData, ntree=100, proximity=TRUE)
table(predict(rf), trainData$Species)

print(rf) # impresión de matriz de confusión

#Confusion matrix:
#           setosa versicolor virginica class.error
#setosa         40          0         0  0.00000000
#versicolor      0         35         3  0.07894737
#virginica       0          2        32  0.05882353

# info
attributes(rf)

#imprimimos el ratio de error
plot(rf) 

# Podemos tambien sacar la importancia de las variables:
importance(rf)
varImpPlot(rf)

# Finalmente predecimos con el test data:
irisPred <- predict(rf, newdata = testData)
table(irisPred, testData$Species)

# imprimimos margen: margen positivo correcta clasificación.
plot(margin(rf, testData$Species))
