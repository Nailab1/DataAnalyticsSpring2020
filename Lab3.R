library(rpart)
library(rpart.plot)
data("msleep")
str(msleep)
help("msleep")
str(data)
mSleepDF1<-msleep[,c(3,6,10,11)]# 3=vore 6=sleep total 10=brainwt 11=bodywt
str(mSleepDF1)
head(mSleepDF1)

sleepModel_1<-rpart(sleep_total~.,data=mSleepDF1, method='anova')
sleepModel_1

rpart.plot(sleepModel_1, type=3, fallen.leaves=T)
#type=3: Draw separate split labels for the left and right directions.
#fallen.leaves True, default TRUE to position the leaf nodes at the bottom of the graph

rpart.plot(sleepModel_1,type=3,digits=3,fallen.leaves=T)
rpart.plot(sleepModel_1, type=3,digits=4,fallen.leaves=T)

install.packages("C50")
require(C50)

data("iris")
head(iris)
str(iris)
table(iris$Species)

set.seed(9850)
grn<-runif(nrow(iris))
irisrand<-iris[order(grn),]
str(irisrand)
classificationmodel1<-C5.0(irisrand[1:100,-5], irisrand[1:100,5])
classificationmodel1
summary(classificationmodel1)


#performing the prediction using predict() function
prediction1<-predict(classificationmodel1,irisrand[101:150,])
prediction1

#using the confusion matrix to understand our prediction
table(irisrand[101:150,5],prediction1)

plot(classificationmodel1)


#"Digging into Iris"
install.packages("e1071")
library(e1071)
classifier<-naiveBayes(iris[,1:4], iris[,5])
table(predict(classifier,iris[,-5]),iris[,5],dnn=list('predicted','actual'))
classifier$apriori
classifier$tables$Petal.Length

plot(function(x) dnorm(x,1.462,01736640),0,8,col="red", main="Petal Length distribution for the 3 different species")
curve(dnorm(x,4.26,0.4699100), add=T, col="blue")
curve(dnorm(x,5.552,.5518947), add=T,col="green")


#Lab3_ctree1.R
require(rpart)
Swiss_rpart<-rpart(Fertility~Agriculture + Education + Catholic, data=swiss)
rpart.plot(Swiss_rpart, type=3, fallen.leaves=T)
text(Swiss_rpart)

require(party)
treeSwiss<-ctree(Species~.,data=iris)
plot(treeSwiss)

cforest(Species~., data=iris, controls=cforest_control(mtry=2, mincriterion=0))
cforest

treeFert<-ctree(Fertility~Agriculture+Education+Catholic, data=swiss, controls=cforest_control(mtry=2, mincriterion=0))
cforest
treeFert
install.packages("tree")
library(tree)
tr<-tree(Species~.,data=iris)
tr
tr$frame
plot(tr); text(tr)

#Lab3_ctree2.R
fit2M<-ctree(Mileage~Price + Country + Reliability + Type, data=na.omit(cu.summary))
summary(fit2M)
plot(fit2M, uniform=T, main="CI Tree for Mileage"); text(fit2M, use.n=T, all=TRUE, cex=.8)

#Lab3_ctree3.R
summary(kyphosis)
str(kyphosis)
head(kyphosis)
fitK<-ctree(Kyphosis~., data=kyphosis)
plot(fitK, main="Conditional Inference Tree for Kyphosis")
plot(fitK, main="Conditional Inference Tree for Kyphosis", type="simple")
