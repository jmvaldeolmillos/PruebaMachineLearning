# Arboles de decisión con party: ctree

set.seed(1234)

# creamoa el training y test con 70% training y 30% test.
ind <- sample(2, nrow(iris), replace = TRUE, prob=c(0.7,0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]

# Arbol de decisión de Species en función de las demás variables:
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myFormula, data=trainData)

# Chequear la predicción
table(predict(iris_ctree), trainData$Species)

#            setosa versicolor virginica
# setosa         40          0         0
# versicolor      0         37         3
# virginica       0          1        31

# Print en text, Print en arbol, y Print en arbol simplificado.
print(iris_ctree)
plot(iris_ctree)
plot(iris_ctree, type="simple")

# Ahora trabajamos la predicción sobre test.
testPred <- predict(iris_ctree, newdata = testData)
table(testPred, testData$Species)

# testPred     setosa versicolor virginica
#   setosa         10          0         0
#   versicolor      0         12         2
#   virginica       0          0        14

# Ojo: pueden dar problemas los Missing Values.
# Las variables de test y training tienen que estar todas y ser iguales en ambas.


# =====================================================================================================


# Arboles de decisión con rpart: 1 rpart y 2 prune

set.seed(1234)

# creamoa el training y test con 70% training y 30% test.
ind <- sample(2, nrow(mybf), replace = TRUE, prob=c(0.7,0.3))
trainData <- mybf[ind==1,]
testData <- mybf[ind==2,]

# Arbol de decisión de Species en función de las demás variables:
myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth
bodyfat.rpart <- rpart(myFormula, data=trainData, control=rpart.control(minsplit = 10))

# imprimimos info
attributes(bodyfat.rpart)
print(bodyfat.rpart$cptable)

# imprimimos resultado en texto
print(bodyfat.rpart)

# imprimimos resultado en arbol
plot(bodyfat.rpart)
text(bodyfat.rpart, use.n=T)



# Seleccionamos el arbol con el mínimo error:
opt <- which.min(bodyfat.rpart$cptable[,"xerror"])
cp <- bodyfat.rpart$cptable[opt, "CP"]
bodyfat.prune <- prune(bodyfat.rpart, cp=cp)

# imprimimos resultado en texto
print(bodyfat.prune)

# imprimimos resultado en arbol
plot(bodyfat.prune)
text(bodyfat.prune, use.n=T)

# Predecimos:
DEXfat_pred <- predict(bodyfat.prune, newdata = testData)
xlim <- range(bodyfat$DEXfat)
plot(DEXfat_pred ~ DEXfat, data=testData, xlab="Observed", ylab="Predicted", ylim=xlim, xlim=xlim)
abline(a=0, b=1)
