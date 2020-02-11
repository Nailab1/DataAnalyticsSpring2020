#In-Class Exercise: KNN- Abalone

abalone<-read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE,
                  sep = ",")
colnames(abalone)<-c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight',
                     'rings')
summary(abalone)
str(abalone)
summary(abalone$rings)
#rings is the variable that we want to predict. Break the rings into 3 levels: "young" for less than8
#"adult" for abalones between 8-11, "old" for abalones older than 11
abalone$rings<-as.numeric(abalone$rings)
abalone$rings<-cut(abalone$rings, br=c(-1,8,11,35), labels=c("young","adult", "old"))
abalone$rings<-as.factor(abalone$rings)
summary(abalone$rings)
#remove the "sex" variable in abalone, because KNN requires all numeric variables for prediction
#z<- abalone
aba<-abalone
aba$sex<-NULL

#normalize the data
normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

aba[1:7]<- as.data.frame(lapply(aba[1:7], normalize))
summary(aba$shucked_wieght)

ind<-sample(2,nrow(aba), replace=T, prob=c(0.7,0.3))
KNNtrain<-aba[ind==1,]
KNNtest<-aba[ind==2,]
sqrt(2918)
#make K equal to the sqrt of 2918, the number of observations in the training set
#knn model
library(class)
KNNpred<-knn(train=KNNtrain[1:7], test=KNNtest[1:7], cl=KNNtrain$rings,k=55)
KNNpred
table(KNNpred)

#In-Class Exercise K-Means Iris dataset
library(ggplot2)
head(iris)
str(iris)
summary(iris)
help("sapply")
sapply(iris[,-5],var) #finds variance of last 5 iris columns
summary(iris)

#plot Sepal.Length Vs Sepal.Width using ggplot
ggplot(iris,aes(x = Sepal.Length, y=Sepal.Width, col = Species))+geom_point()
#plot Petal.Length Vs Sepal.Width using ggplot
ggplot(iris,aes(x=Petal.Length, y=Petal.Width, col=Species))+geom_point()

#kmeans clustering

set.seed(300)
k.max<-12
#tot.withinss=Total within-cluster sum of square
#iter.max=max number of iterations allowed
#nstart= if centers is a number, how many random sets should be chosen
wss<-sapply(1:k.max, function(k){kmeans(iris[,3:4],k,nstart=20)$tot.withinss})
wss #within sum of squares
plot(1:k.max, wss, type='b', xlab="Number of clusters(k)", ylab="Within cluster sum of squares")
icluster<-kmeans(iris[,3:4],3,nstart=20)
table(icluster$cluster, iris$Species)

#In-Class Exercises on Trees
library(rpart)
library(rpart.plot)

dim(iris)
s_iris<-sample(150,100)
s_iris

#creating testing and training sets
iris_train<-iris[s_iris,]
iris_test<-iris[-s_iris,]
dim(iris_test)
dim(iris_train)
iris_train

decTreeModel<- rpart(Species~.,iris_train, method="class")
decTreeModel

#plotting the decision tree model using rpart.plot() function
rpart.plot(decTreeModel)
