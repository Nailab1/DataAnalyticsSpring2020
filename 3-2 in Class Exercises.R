#Validation set Example with Auto dataset
library(ISLR)
set.seed(1)

train=sample(392,196)
#Use the subset option in the lm() function to fit a linear regression using,
#only the observations corresponding to the training set
lm.fit<-lm(mpg~horsepower, data=Auto, subset=train)
#Now we use the predict() function to estimate the response for all 392 observations,
#and we use the mean() function to calculate the MSE of the 196 observations in the validation set
#Note that the -train selects only observations that are not in the training set
attach(Auto)
mean((mpg-predict(lm.fit, Auto))[-train]^2)
#Therefore the estimated test MSE for the linear regression fit is 23.26

#we can use the poly() function to estimate test error for the quadretic and cubic regression
#Quadratic regression line
lm.fit2<-lm(mpg~poly(horsepower,2), data=Auto, subset=train)#Quadratic
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
#MSE = 18.72
#Cubic regression line
lm.fit3<-lm(mpg~poly(horsepower,3), data=Auto, subset=train)#Cubic
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
#MSE= 18.79
set.seed(2)
train=sample(392, 196)
lm.fit<-lm(mpg~horsepower, data=Auto, subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
#MSE = 25.73
lm.fit2<-lm(mpg~poly(horsepower,2), data=Auto, subset=train)#Quad
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
#MSE = 20.43
lm.fit3<-lm(mpg~poly(horsepower,3),data=Auto, subset=train)#cubic
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
#MSE = 20.39
#Evidence that quad works better than linear, but little to prove cube is better than quad
