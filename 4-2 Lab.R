library(e1071)
set.seed(1)
#we now use the svm() function to fit the support vector classifier for a given
#value of the cost parameter
# Here we demonstrate the use of this function on a two-dimensional example so that we can plot the resulting
# decision boundary
#start by generating the observations which belongto two classes
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10),rep(1,10))
x[y==1,]=x[y==1,]+1

#we begin by checking whether the classes are linearly separable
plot(x,col=(3-y))
#They are not, next we fit the support vector classifier
#for the svm() function to perform classification we must encode the response as a factor variable
#create a dataframe w the response coded as a factor
dat<-data.frame(x=x,y = as.factor(y))
svmfit<-svm(y~., data=dat, kernel="linear", cost=10, scale=F)
#The argument scale=FALSE tells the svm() function not to scale each feature to
# have mean zero or standard deviation one;
# depending on the application, one might prefer to use scale=TRUE.

#plot the support vector classiier:
plot(svmfit,dat)
svmfit$index
#this tells us that a linear kernel was used with cost=10, and there were 7 support vectors
#four in one class and three in the other
summary(svmfit)

#using smaller value for cost parameter; larger number of support vectors
svmfit<-svm(y~., data=dat, kernel="linear", cost=0.1, scale=F)
plot(svmfit,dat)
svmfit$index

#tuning
set.seed(1)
tune.out<-tune(svm, y~., data=dat, kernel="linear", ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
#cost=0.1 results in the lowest cross validation error rate
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)
xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=T)
xtest[ytest==1,]=xtest[ytest==1,]+1
testdat=data.frame(x=xtest, y=as.factor(ytest))
ypred<-predict(bestmod,testdat)
table(predict=ypred, truth=testdat$y)

#what if we used cost=0.01?
svmfit<-svm(y~., data=dat, kernel="linear", cost=.01, scale=F)
ypred=predict(svmfit, testdat)
table(predict=ypred, truth=testdat$y)

#consider a situation in which the two classes are linearly separable, can find separating hyperplane
#further seperate the two classes in our simulated data so that they are linearly separable:
x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)

#we fit the support vector classifier and plot the resulting hyperplane
#using a very large value of cost so that no observations are misclassified

dat=data.frame(x=x,y=as.factor(y))
svmfit<-svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit, dat)
#no training erros and only three support vectors used
#However, we can see from the figure that the margin is
#very narrow (because the observations that are not support vectors, indicated as circles, are very
#close to the decision boundary). It seems likely that this model will perform poorly on test data. 

#smaller value of cost:
svmfit<-svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit,dat)
#misclassifed a training observation but we also obtain a much wider margin
#and make use of seven support vectors; likely this model will peform better

##SVM Application to Gene Expression Dataset##
#khan dataset consist of tissue samples corresponding to 4 types of tumors
library(ISLR)
names(Khan)
#this dataset consists of expression measurements for 2,308 genes.
#training dataset = 63 observations, testing dataset= 20 0bservation
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain )
length(Khan$ytest )
table(Khan$ytrain )
table(Khan$ytest )

#using support vector approach to predict cancer subtype using gene expression measurements
#large number of features relative to the number of observations
#this suggest that we should use a linear kernel
dat<-data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out<-svm(y~., data=dat, kernel="linear", cost=10)
summary(out)
#no training errors
data.te=data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te=predict(out, newdata=data.te)
table(pred.te,data.te$y)
#cost=10 yields two test set errors on this data


##Lab1_svm11.R##
#install.packages("kernlab")
library(kernlab)
data(reuters)
is(reuters)
tsv<-ksvm(reuters, rlabels, kernel="stringdot", kpar=list(length=5), cross=3, C=10)
tsv

#Kernlab, svmpath, and klaR
data("iris")
irismodel<-ksvm(Species~., data=iris,
                type="C-bsvc", kernel="rbfdot",
                kpar=list(sigma=0.1), C=10,
                prob.model=T)
irismodel

predict(irismodel, iris[c(3,10,56,68,107,120), -5], type="probabilities")
predict(irismodel, iris[c(3,10,56,68,107,120), -5], type="decision")

k<-function(x,y) {
  (sum(x*y)+1)*exp(0.001*sum((x-y)^2))
}
class(k)<-"kernel"
data("promotergene")
gene<-ksvm(Class~., data=promotergene, kernel=k, C=10, cross=5)
gene


model<-svm(Species~., data=iris_train, method="C-classification", kernel="radial", cost=10, gamma=.1)
summary(model)
