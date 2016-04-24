# General Linear Regression con GLM
 
data("bodyfat", package="TH.data")
nybf <- bodyfat
myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth
bodyfat.glm <- glm(myFormula, family = gaussian("log"), data = bodyfat)
summary(bodyfat.glm)
 
# Se hace la predicción
pred <- predict(bodyfat.glm, type="response")

plot(bodyfat$DEXfat, pred, xlab="Observed values", ylab="Predicted values")
abline(a=0, b=1)

# Para Logistic Regression.

# family = binomial("logit"), data = bodyfat)
