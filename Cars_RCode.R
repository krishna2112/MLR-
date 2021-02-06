Cars <- read.csv(file.choose()) # choose the Cars.csv data set
View(Cars)
summary(Cars)

# 7. Find the correlation b/n Output (MPG) & (HP,VOL,SP)-Scatter plot
pairs(Cars)
library(psych)
pairs.panels(Cars)
# 8. Correlation Coefficient matrix - Strength & Direction of Correlation
cor(Cars)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
#install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Cars))

# The Linear Model of interest with all the columns
model.car <- lm(MPG~.,data=Cars)

summary(model.car)

# Multicollinearity check
# Model based on only Volume 
model.carV<-lm(MPG~VOL,data=Cars)
summary(model.carV) # Volume became significant

# Model based on only Weight
model.carW<-lm(MPG~WT,data=Cars)
summary(model.carW) # Weight became significant

# Model based on Volume and Weight
model.carVW<-lm(MPG~VOL+WT,data=Cars)
summary(model.carVW) # Both became Insignificant
 
# Applying VIF function on model built on all inputs
## Variance Inflation factor to check collinearity b/n variables 
influence.measures(model.car)
library(car)
vif(model.car) # Original model
## vif>10 then there exists collinearity among all the variables 

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model.car,id.n=2,id.cex=0.7)
## VIF and AV plot has given us an indication to delete "wt" variable
# So there exists a collinearity problem b/n volume and weight

# Deletion Diagnostics for identifying influential observations
influence.measures(model.car)
library(car)
## plotting Influential measures 
influenceIndexPlot(model.car,id.n=3) # index plots for infuence measures
influencePlot(model.car,id.n=3) # A user friendly representation of the above

# Regression after deleting the 77th observation, which is influential observation
model_1<-lm(MPG~.-WT,data=Cars[-77,])
summary(model_1)

# Regression after deleting the 77th & 71st Observations
model_2<-lm(MPG~.-WT,data=Cars[-c(71,77),])
summary(model_2)



## Final model
plot(lm(MPG~.-WT,data=Cars[-c(77),])) # 77
summary(lm(MPG~.-WT,data=Cars[-c(77),]))
plot(lm(MPG~.-WT,data=Cars[-c(77,79),])) # 77,79
summary(lm(MPG~.-WT,data=Cars[-c(77,79),]))
plot(lm(MPG~.-WT,data=Cars[-c(77,79,80),])) # 77,79,80
summary(lm(MPG~.-WT,data=Cars[-c(77,79,80),]))

# Its not a feasible solution if we remove all the 
# influential values 
# We need to consider other assumptions to likes
# Heteroscadasticity | Normal Distribution of Residuals


finalmodel<-lm(MPG~.-WT,data=Cars[-c(77,79),])
summary(finalmodel)

# Evaluate model LINE assumptions 
plot(finalmodel)

hist(residuals(finalmodel)) # close to normal distribution

### TASK ####### 
## Apply different transformations on input check if they
# are they giving better Adjusted R_Squared and 
# less RMSE also check different plots which we used them
# for evaluating our model

## Apply Ridge and Lasso Regression on this data
# and check whether it is giving better Adjst.R_sqrd 
# and less rmse values are not 

