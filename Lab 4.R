#creating a matrix data with random numbers and plotting the matrix using the image() function. You will see there it doesn't have a real pattern in
#the plot
set.seed(12345)
help(par) #par can be used to set or query graphical parameters, parameters can be set by specifying them as arguements
#to par in tag = value form, or by passing them as a list of tagged values
par(mar=rep(0.2,4))
data_Matrix<-matrix(rnorm(400), nrow=40)
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])

#now we can run a hierarchical cluster analysis on the dataset. We will use the heatmap() function that is available in R
help("heatmap")
help(rep)

par(mar=rep(0.2,4))
heatmap(data_Matrix)
#When we run the heatmap() here, we get the dendrograms printed on tboth columns and rows and still there is no real immerging pattern that is interesting to us
#it is because there is no real interesting pattern underlying in the data we generated

#Now we will add a pattern to the data by doing a random coin flip
#we will use the rbinom() function along with a for-loop
help("rbinom")

set.seed(678910)
for (i in 1:40){
  #flipping a coin and getting the data
  coin_Flip<- rbinom(1, size=1, prob=.5)
  #if the coin is "Heads", add a common pattern to that row,
  if(coin_Flip){
    data_Matrix[i, ]<-data_Matrix[i,]+ rep(c(0,3), each=5)
  }
}

#What was done here was the loop went through all the rows and on a random row flipped a coin. During the coin flip, if it turns out to be one (true),
#then a pattern is added to the data in a way that the five of the columns have a mean of zero and the others have a mean of 3
#Now we plot
par(mar=rep(0.2,4))
image(1:10,1:40, t(data_Matrix)[,nrow(data_Matrix):1])
#the right hand five columns have more yellow which means they have a higher value and the left hand are reder which means they have lower value
#it is because some of the rows have a mean of 3 in the right hand side, and some of hte rows have a mean of zero. (mine is the opposite??)

#runing the heatmap function on the data
par(mar=rep(.2,4))
heatmap(data_Matrix)

#taking a closer look at the patterns in rows and columns by looking at the marginal means of the rows and columns
#ten different columns mean and forty diferent rows means

hh<-hclust(dist(data_Matrix))
data_Matrix_Ordered<- data_Matrix[hh$order,]
par(mfrow = c(1,3))
image(t(data_Matrix_Ordered)[, nrow(data_Matrix_Ordered):1]) #original data reordered according to the hierarchical cluster analysis of the rows
plot(rowMeans(data_Matrix_Ordered), 40:1,xlab="The Row Mean", ylab= "Row", pch=19) #mean of each of the rows (40 rows >> 40 dots representing the mean)
plot(colMeans(data_Matrix_Ordered), xlab="Column", ylab="Column Mean", pch = 19) #means of each column

##Lab 1 bronx 1
setwd("C:\\Users\\brownn4\\Documents\\Gradschool\\Grad-Data Analytics")
bronx1<-read.csv("rollingsales_bronx.csv", header=T)
attach(bronx1)
SALE.PRICE<-sub("\\$","",SALE.PRICE)
SALE.PRICE<-as.numeric(gsub(",","", SALE.PRICE))
GROSS.SQUARE.FEET<-as.numeric(gsub(",","", GROSS.SQUARE.FEET)) 
LAND.SQUARE.FEET<-as.numeric(gsub(",","", LAND.SQUARE.FEET)) 
plot(log(GROSS.SQUARE.FEET), log(SALE.PRICE)) 
m1<-lm(log(SALE.PRICE)~log(GROSS.SQUARE.FEET))
summary(m1)
abline(m1,col="red",lwd=2)
plot(resid(m1))

# Model 2

m2<-lm(log(bronx1$SALE.PRICE)~log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2)
plot(resid(m2))
# Suppress intercept - using "0+ ..."
m2a<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2a)
plot(resid(m2a))

# Model 3
m3<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)+factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m3)
plot(resid(m3))

# Model 4
m4<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)*factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m4)
plot(resid(m4))
#

##lab 1 bronx 2
bronx1$SALE.DATE<- as.Date(gsub("[^]:digit:]]","",bronx1$SALE.DATE)) 
bronx1$YEAR.BUILT<- as.numeric(gsub("[^]:digit:]]","",bronx1$YEAR.BUILT)) 
bronx1$ZIP.CODE<- as.character(gsub("[^]:digit:]]","",bronx1$ZIP.CODE)) 

minprice<-10000
bronx1<-bronx1[which(bronx1$SALE.PRICE>=minprice),]
nval<-dim(bronx1)[1]

bronx1$ADDRESSONLY<- gsub("[,][[:print:]]*","",gsub("[ ]+","",trim(bronx1$ADDRESS))) bronxadd<-unique(data.frame(bronx1$ADDRESSONLY, bronx1$ZIP.CODE,stringsAsFactors=FALSE)) names(bronxadd)<-c("ADDRESSONLY","ZIP.CODE") bronxadd<-bronxadd[order(bronxadd$ADDRESSONLY),] duplicates<-duplicated(bronx1$ADDRESSONLY)

for(i in 1:2345) {
  if(duplicates[i]==FALSE) dupadd<-bronxadd[bronxadd$duplicates,1]
}

nsample=450

addsample<-bronxadd[sample.int(dim(bronxadd),size=nsample),]
library(ggmap)
addrlist<-paste(addsample$ADDRESSONLY, "NY", addsample$ZIP.CODE, "US", sep=" ") 
querylist<-geocode(addrlist)

matched<-(querylist$lat!=0 &&querylist$lon!=0) addsample<-cbind(addsample,querylist$lat,querylist$lon) 
names(addsample)<-c("ADDRESSONLY","ZIPCODE","Latitude","Longitude")# correct the column na adduse<-merge(bronx1,addsample)

adduse<-adduse[!is.na(adduse$Latitude),]
mapcoord<-adduse[,c(2,3,24,25)]

table(mapcoord$NEIGHBORHOOD)

mapcoord$NEIGHBORHOOD <- as.factor(mapcoord$NEIGHBORHOOD)
map <- get_map(location = 'Bronx', zoom = 12)
ggmap(map) + geom_point(aes(x = mapcoord$Longitude, y = mapcoord$Latitude, size =1, color=mapcoord$NEIGHBORHOOD), data = mapcoord) +theme(legend.position = "none") 



mapmeans<-cbind(adduse,as.numeric(mapcoord$NEIGHBORHOOD))
colnames(mapmeans)[26] <- "NEIGHBORHOOD" #This is the right way of renaming.

keeps <- c("ZIP.CODE","NEIGHBORHOOD","TOTAL.UNITS","LAND.SQUARE.FEET","GROSS.SQUARE.FEET","SALE.PRICE","Latitude","Longitude") 
mapmeans<-mapmeans[keeps]#Dropping others
mapmeans$NEIGHBORHOOD<-as.numeric(mapcoord$NEIGHBORHOOD) 

for(i in 1:8){
  mapmeans[,i]=as.numeric(mapmeans[,i]) 
}

#Classification
mapcoord$class<as.numeric(mapcoord$NEIGHBORHOOD)
nclass<-dim(mapcoord)[1]
split<-0.8
trainid<-sample.int(nclass,floor(split*nclass))
testid<-(1:nclass)[-trainid]


kmax<-10
knnpred<-matrix(NA,ncol=kmax,nrow=length(testid))
knntesterr<-rep(NA,times=kmax)
for (i in 1:kmax){		# loop over k
  knnpred[,i]<-knn(mapcoord[trainid,3:4],mapcoord[testid,3:4],cl=mapcoord[trainid,2],k=i)
  knntesterr[i]<-sum(knnpred[,i]!=mapcoord[testid,2])/length(testid)
} 
knntesterr

#Clustering
mapobj<-kmeans(mapmeans,5, iter.max=10, nstart=5, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
fitted(mapobj,method=c("centers","classes"))
mapobj$centers
#
library(cluster)
clusplot(mapmeans, mapobj$cluster, color=TRUE, shade=TRUE, labels=2, lines=0) 
#
library(fpc)
plotcluster(mapmeans, mapobj$cluster)
#
mapmeans1<-mapmeans[,-c(1,3,4)]
mapobjnew<-kmeans(mapmeans1,5, iter.max=10, nstart=5, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
fitted(mapobjnew,method=c("centers","classes"))
clusplot(mapmeans1, mapobjnew$cluster, color=TRUE, shade=TRUE, labels=2, lines=0) 
plotcluster(mapmeans1, mapobjnew$cluster)
ggmap(map) + geom_point(aes(x = mapcoord$Longitude, y = mapcoord$Latitude, size =1, color=mapobjnew$cluster), data = mapcoord)#How to change colors?
##Lab 1 kkn 1
install.packages("kknn")
require(kknn)

data(iris)
m<-dim(iris)[1]
val<-sample(1:m, size=round(m/3), replace = F,
            prob =rep(1/m,m))
iris.learn<-iris[-val,]
iris.valid<- iris[val,]
iris.kknn<- kknn(Species~., iris.learn, iris.valid, distance =1, kernel = "triangular")

##Lab 1 knn2
data(ionosphere)
ionosphere.learn<-ionosphere[1:200,]
ionosphere.valid<- ionosphere[-c(1:200),]
fit.knn<- kknn(class~. ionosphere.learn, ionosphere.valid)
table(ionosphere.valid$class, fit.kknn$fit)
(fit.train1 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
                          kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 1))
table(predict(fit.train1, ionosphere.valid), ionosphere.valid$class)
(fit.train2 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
                          kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 2))
table(predict(fit.train2, ionosphere.valid), ionosphere.valid$class)

##Lab 1 knn3
data(swiss)

pairs(~ Fertility + Education + Catholic, data = swiss, subset = Education < 20, main = "Swiss data, Education < 20")

##Lab 1 kmeans 1
data(swiss)
sclass <- kmeans(swiss[2:6], 3) 
table(sclass$cluster, swiss[,1])    

##Lab 1 nyt
nyt1<-read.csv("nyt1.csv")
nyt1<-nyt1[which(nyt1$Impressions>0 & nyt1$Clicks>0 & nyt1$Age>0),]
nnyt1<-dim(nyt1)[1]		# shrink it down!
sampling.rate=0.9
num.test.set.labels=nnyt1*(1.-sampling.rate)
training <-sample(1:nnyt1,sampling.rate*nnyt1, replace=FALSE)
train<-subset(nyt1[training,],select=c(Age,Impressions))
testing<-setdiff(1:nnyt1,training)
test<-subset(nyt1[testing,],select=c(Age,Impressions))
cg<-nyt1$Gender[training]
true.labels<-nyt1$Gender[testing]
classif<-knn(train,test,cg,k=5) #
classif
attributes(.Last.value) 

##Lab 3 ctree 1
require(rpart)
library(rpart.plot)
Swiss_rpart<-rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(Swiss_rpart)
text(Swiss_rpart)
prp(Swiss_rpart)

require(party)
treeSwiss<-ctree(Species ~., data=iris)
plot(treeSwiss)

cforest(Species~., data=iris, controls=cforest_control(mtry=2, mincriterion =0))
treeFert<-ctree(Fertility ~ Agriculture + Education + Catholic, data=swiss)
cforest(Fertility ~ Agriculture+ Education + Catholic, data=swiss, controls=cforest_control(mtry=2, mincriterion=0))
library(tree)
tr<- tree(Species ~., data=iris)
tr
tr$frame
plot(tr)
text(tr)

Tr_test<- rpart(Species~., data=iris)
prp(Tr_test)

##Lab 3 ctree 2
#Conditional inference tree for Mileage
fit2M<- ctree(Mileage~Price+Country+Reliability+Type, data=na.omit(cu.summary))
summary(fit2M)
#Plot tree
plot(fit2M, uniform=T, main="CI Tree for Mileage")
text(fit2M, use.n=T, all=T, cex=.8)

##Lab 3 ctree 3
fitK<- ctree(Kyphosis~Age+Number+Start, data=kyphosis)
plot(fitK, main="Conditional Inference Tree fo Kyphosis")
plot(fitK, main="Conditional Inference Tree for Kyphosis", type= "simple")



#Trees for the Titanic
data(ptitanic)

#Decsion tree
Titanic_rpart<-rpart( survived ~., data = ptitanic)
prp(Titanic_rpart)

#Classification Tree
Titanic_ctree<- ctree(survived~., data= ptitanic)
plot(Titanic_ctree, uniform =T)
text(Titanic_ctree, use.n=T, all=T, cex=.8)

#Hierarchical Cluster Analysis
d<-dist(ptitanic, method="euclidean")
hcl_Titanic<-hclust(d, method="complete")
plot(hcl_Titanic, cex=.8)
text(hcl_Titanic)

#Random Forest
install.packages("randomForest")
library(randomForest)
fitTitanic<-randomForest(survived ~., data=ptitanic, na.action=na.exclude)
plot(fitTitanic)
