start <- read.csv(file.choose())
View(start)
summary(start)
start1 <- start[,-4]
start1
summary(start1)
pairs(start1)
attach(start1)
pairs(R.D.Spend,Administration,Marketing.Spend,Profit)
library(psych)
pairs.panels(start1)
cor(start1)
library(corpcor)
cor2pcor(cor(start1))
model <- lm(Profit~.,data = start1)
summary(model)

model1 <- lm(Profit~Administration)
summary(model1)

model2 <- lm(Profit~Marketing.Spend)
summary(model2)

model3 <- lm(Profit~Administration+Marketing.Spend)
summary(model3)

library(car)

vif(model)

avPlots(model,id.n=2,id.cex=0.7)

influenceIndexPlot(model,id.n=3)
influencePlot(model,id.n=3)

modelfinal <- lm(Profit~.,data = start1[-c(46,47,49,50),])
summary(modelfinal)

plot(lm(Profit~.,data = start1[-c(46,47,49,50),]))

library(ggplot2)

ggplot(modelfinal)
qqplot(modelfinal)
residuals(modelfinal)
hist(residuals(modelfinal))






#computer data set.

x <- read.csv(file.choose())
View(x)
summary(x)
x1 <- x[,-c(1,7,8,9)]
x1
attach(x1)
pairs(x1)
library(psych)
pairs.panels(x1)

y <- scale(x1)
View(y)
plot(y)
boxplot(y)
hist(y)
pairs(y)
library(psych)
pairs.panels(y)

cor(y)
library(corpcor)
cor2pcor(cor(y))
z <- lm(price~.,data = x1)
summary(z)
library(car)
vif(z)
avplots(z,id.n=2,id.cex=0.7)
avPlots(z,id.n=2,id.cex=0.7)
influenceIndexPlot(z,id.n=3)
influencePlot(z,id.n=3)

finalmodelprice <- lm(price~.,data = x1[-c(1441,1701)])
summary(finalmodelprice)
plot(finalmodelprice)
hist(residuals(finalmodel))
prd <- predict(finalmodelprice)
View(prd)
write.table(prd,"price.csv")
getwd()

confint(finalmodelprice,level = 0.95)
predictprice <- predict(finalmodelprice,interval = "predict")
View(predictprice)
write.table(predictprice,"fin.csv")
getwd()



##corolla price.
x <- read.csv(file.choose())
View(x)
summary(x)
y <- x[,c(3,4,7,9,13,14,16,17,18)]
View(y)
summary(y)
pairs(y)
library(psych)
pairs.panels(y)
hist(y)
boxplot(y)
cor(y)
library(corpcor)
cor2pcor(cor(y))
attach(y)
model3 <- lm(Price~.,data = y)
summary(model3)

library(car)
vif(model3)
avPlots(model3,id.n=0.2,id.cex=7)
influencePlot(model3,id.cex=3)

confint(model3,level=0.95)
pred3 <- predict(model3,interval="predict")
View(pred3)
write.csv(pred3,"corolla.csv")
getwd()
