library(MASS)
housing <- Boston
install.packages("corrplot")
library(corrplot)
library(lattice)
library(ggplot2)

install.packages("caTools")
library(caTools)
library(dplyr)

install.packages("plotly")
library(plotly)


## Prepring the Data | Checking for NA and mussing values and removing them
numberOfNA <- length(which(is.na(housing)==T))
if(numberOfNA>9) {
  housing <- housing[complete.cases(housing),]
}


## Prepare the training and testing data
set.seed(123)
split <- sample.split(housing, SplitRatio = 0.75)
train <- subset(housing,split==TRUE)
test <- subset(housing,split==FALSE)


## This part of the EDA (Exploratory Data Analysis)
## This process is to understand how the variables of the data
str(housing)
head(housing)
summary(housing)


## Variable 'crim' 'rm' and 'black' have a large diffrence between their
## median and mean which indicates lot of outliers in respective variables.

par(mfrow=c(1,4))
boxplot(housing$crim, main='crim', col='Sky Blue')
boxplot(housing$zn, main='zn', col='Sky Blue')
boxplot(housing$rm, main='rm', col='Sky Blue')
boxplot(housing$black, main='black', col='Sky Blue')


## ploting correlation
corrplot(cor(housing))


## trying to find the linearity between 'medv' and other variables
## but its not worth complicating the model a lot for a very small increase in 
## Adjusted R-Squared
## the first row of the plot is the most useful, its indication of how diffrent variable
## impact the median value of homes in 
droplist <- c('chas', 'rad','crim','zn','black')
housingplot <- housing[,!colnames(housing) %in% droplist]
splom(housingplot,col='Sky Blue')


## Build the model and accuracy analysis
## The Null Hypothesis is that the coefficients associated with the variables are zero.
## The alternate hypothesis is that the coefficients are not equal to zero.
## Our key objective is to determine the variable(s) that would give best predictive model.
## fitting simple linear regression is used to fir predictor using all independent variables
lm.fit1 <- lm(medv~.,data=train)
summary(lm.fit1)


## Iterations1 - Variables ‘age’ and ‘indus’ have very high Pr(>|t|) value and low significance 
## hence removing them could give us a better model.
## Iteration2 - As we noticed in EDA ‘lstat’ is non-linear and hence can be squared for a better fit.

lm.fit2 <- lm(medv~.-age-indus+I(lstat^2),data=train)
summary(lm.fit2)


## Residual standard error: 3.991 on 349 degrees of freedom
## Multiple R-squared:  0.8154,	Adjusted R-squared:  0.8091 
## F-statistic: 128.5 on 12 and 349 DF,  p-value: < 2.2e-16


## Iteration3 

lm.fit3 <- lm(medv~.-indus-age-zn+rm*lstat-black+rm*rad+lstat*rad,data=train)
summary(lm.fit3)




## Analysis
residuals <- data.frame('Residuals' = lm.fit3$residuals)
res_hist <- ggplot(residuals, aes(x=Residuals)) + geom_histogram(color='black', fill='skyblue') + ggtitle('Histogram of Residuals')
res_hist

plot(lm.fit3, col='Sky Blue')

test$predicted.medv <- predict(lm.fit3,test)
pl1 <-test %>% 
  ggplot(aes(medv,predicted.medv)) +
  geom_point(alpha=0.5) + 
  stat_smooth(aes(colour='black')) +
  xlab('Actual value of medv') +
  ylab('Predicted value of medv') +
  theme_bw()

ggplotly(pl1)



