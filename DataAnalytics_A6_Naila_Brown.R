setwd("C:\\Users\\brownn4\\Documents\\GradSchool\\Grad-Data Analytics")
df<-read.csv("fishdata.csv", header=T)
summary(df)
str(df)
#Removing NA's from Number column
df<-na.omit(df)
summary(df$Number)
max(df$Number)
#Removing 0 length fish and limiting number of fish stocked
df<-subset(df, Number > 100)
summary(df$Number)

which(df$Size..Inches. %in% 0)
df<-df[-c(12290, 13705, 16249, 16418,19594),]
summary(df$Size..Inches.)
# EDA ---------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(gridExtra)
library(plyr)

#creating barplot that shows the number of fish of each species, town, species, county, and waterbody
count(df$Species)
#There are blanks for species; removing those
which(df$Species %in% "")
fish<-df[-c(8096, 8097, 8098, 8099),]
count(fish$Species)
speciesbar<-ggplot(data=fish, aes(x=Species))+geom_bar()+labs(title="Species Stocking Instances")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
speciesbar
speciesbar2<-ggplot(data=fish, aes(x=Species, y=Number))+geom_bar(stat="identity")+labs(title="Number of Fish Stocked")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
speciesbar2
grid.arrange(speciesbar, speciesbar2)
#Reomoving blanks from the rest of the columns
count(fish$Species) #this is good
count(fish$County) #Duplicates here, need to be fixed and blanks
count(fish$Waterbody) #Duplicates here as well, no blanks
count(fish$Town) #Duplicates and blanks
#Removing blanks from Town column (which subsequently removed blank from County Column)
x=which(fish$Town %in% "")
fish<-fish[-c(x),]
count(fish$Town)

count(fish$Month)
#Duplicate removal
fish$Species<-as.factor(tolower(fish$Species))
fish$Town<-as.factor(tolower(fish$Town))
fish$County<-as.factor(tolower(fish$County))
fish$Waterbody<-as.factor(tolower(fish$Waterbody))
str(fish)

#Looking at distribution of fish sizes for each year
#bw2011<-2*IQR(subset(fish, Year=2011)$Size..Inches.)/length(subset(fish, Year=2011))^(1/3)
a=ggplot(data=subset(fish, Year=2011), aes(x=fish$Size..Inches.))+geom_histogram(binwidth=2)+labs(title="Size distribution - 2011")
b=ggplot(data=subset(fish, Year=2012), aes(x=fish$Size..Inches.))+geom_histogram(binwidth=2)+labs(title="Size distribution - 2012")
c=ggplot(data=subset(fish, Year=2013), aes(x=fish$Size..Inches.))+geom_histogram(binwidth=2)+labs(title="Size distribution - 2013")
d=ggplot(data=subset(fish, Year=2014), aes(x=fish$Size..Inches.))+geom_histogram(binwidth=2)+labs(title="Size distribution - 2014")
e=ggplot(data=subset(fish, Year=2015), aes(x=fish$Size..Inches.))+geom_histogram(binwidth=2)+labs(title="Size distribution - 2015")
f=ggplot(data=subset(fish, Year=2016), aes(x=fish$Size..Inches.))+geom_histogram(binwidth=2)+labs(title="Size distribution - 2016")
g=ggplot(data=subset(fish, Year=2017), aes(x=fish$Size..Inches.))+geom_histogram(binwidth=2)+labs(title="Size distribution - 2017")
h=ggplot(data=subset(fish, Year=2018), aes(x=fish$Size..Inches.))+geom_histogram(binwidth=2)+labs(title="Size distribution - 2018")
grid.arrange(a,b,c,d,e,f,g)

#Looking at number of fish stocked over time
#creating basic dataset to create line chart
R<-aggregate(fish$Number, by=fish["Year"], sum)
ggplot(data=R, aes(x=Year, y=x, group=1))+geom_path()+labs(title="Fish Stocking Count Trend")
#Looking at number of fish stocked per month

boxplot(fish$Number)
ggplot(data=fish, aes(x=Number))+geom_histogram(binwidth=500000)+labs(title="Number Distribution")
summary(fish$Number)
ggplot(data=fish, aes(x=Size..Inches.))+geom_histogram()+labs(title="Fish Size Distribution")
ggplot(data=fish, aes(x=County, y=Species, color=Size..Inches., size=Size..Inches.))+geom_point()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title="Species/County Scatterplot")

#Creating heatmap with numeric variables
fish_num<-fish[,c(1,6,8)]
library(corrplot)
corrplot(cor(fish_num),method="circle")
# Modeling and Testing ----------------------------------------------------

#Dummieesssss
fishdf<-fish[,-4]
set.seed(1000)
ran<-sample(nrow(fishdf),.7*nrow(fishdf), replace=F)
Train<-as.data.frame(fishdf[ran,])
Test<-as.data.frame(fishdf[-ran,])


#For the normality test the largest sample that can be taken is 5000
NumNorm<-shapiro.test(Train$Number[0:5000])
NumNorm
#Reject null, data is NOT normally distributed
SizeNorm<-shapiro.test(Train$Size..Inches.[0:5000])
SizeNorm
#This data is also not normally distributed, though it's closer than the number data

#OK Models IG
#Linear
fishlm<-lm(Number~ Year + Waterbody + Month + Size..Inches. + Species, data=Train)
fishlm
summary(fishlm)
plot(resid(fishlm))


fishlm$xlevels[["Waterbody"]]<-union(fishlm$xlevels[["Waterbody"]], levels(factor(Test$Waterbody)))
fishlm$xlevels[["Species"]]<-union(fishlm$xlevels[["Species"]], levels(Test$Species))
lmpred<-predict(fishlm,newdata=Test)
lmpred
library(Metrics)
rmse(Test$Number, lmpred)
actual_predslm<-data.frame(cbind(actuals=Test$Number, prediction=lmpred))
accuracy<-cor(actual_predslm)
accuracy
anova(fishlm)
with(actual_predslm,plot(actuals,prediction))
abline(fishlm)
#Trying linear model with normalized data (Same result)
Testnorm<-Test
Testnorm[,c(5,7)]<-scale(Testnorm[,c(5,7)])
Trainnorm<-Train
Trainnorm[,c(5,7)]<-scale(Trainnorm[,c(5,7)])

fishlm_2<-lm(Number~ Year + Month + Size..Inches.+Waterbody+ Species, data=Trainnorm)
summary(fishlm_2)
fishlm_2$xlevels[["Waterbody"]]<-union(fishlm_2$xlevels[["Waterbody"]], levels(factor(Testnorm$Waterbody)))
lm2pred<-predict(fishlm_2,newdata=Testnorm)
actualpreds2<-data.frame(cbind(actuals=Testnorm$Number, prediction=lm2pred))
acc2<-cor(actualpreds2)
acc2

library(cluster)
library(e1071)
library(factoextra)

clust_train<-Train[,c(1,4,5,6,7)]
clust_test<-Test[,c(1,4,5,6,7)]


#Clustering
clust_train[,c(1,3,5)]<-na.omit(as.data.frame(scale(clust_train[,c(1,3,5)])))
clust_test[,c(1,3,5)]<-na.omit(as.data.frame(scale(clust_test[,c(1,3,5)])))
cluster1<-kmeans(clust_train[,c(1,3,5)], centers=17, nstart=25)                             
table(cluster1$cluster, clust_train$Species)
library(factoextra)
k<-fviz_cluster(cluster1, geom="point", data=clust_train[,c(1,3,5)])
k

cluster1$size
cluster1$centers
#Classification tree
library(rpart)
library(rpart.plot)
dt<-rpart(County~Year+Month+Size..Inches.+Species+Number, data=Train)
summary(dt)
rpart.plot(dt)
#Confusion Matrix
dtm_pred<-predict(dt,newdata=Test,type="class")
confmatrixtree<-table(Test$County, dtm_pred)
confmatrixtree
dtmaccuracy<-sum(diag(confmatrixtree))/sum(confmatrixtree)
library(plyr)



#Kernel density classification
library(klaR)
kdc1<-NaiveBayes(Species~County+Year+Month+Size..Inches.+Waterbody+Number, data=Train, usekernel=T)
kdcpred<-predict(kdc1, newdata=Test)

confmatrixkdc<-table(actual=Test$Species, predicted=kdcpred$class)
cor(confmatrixkdc)
accuracykdc<-sum(diag(confmatrixkdc))/sum(confmatrixkdc)
accuracykdc
plot(kdc1)
