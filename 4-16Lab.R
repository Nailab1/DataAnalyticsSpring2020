#Exploratory Factor Analysis Example
setwd("C:\\Users\\brownn4\\Downloads")
data<-read.csv("dataset_exploratoryFactorAnalysis.csv")
install.packages("psych")
library(psych)

corMat<-cor(data)
corMat
solution<-fa(r=corMat, nfactors=2, rotate="oblimin", fm="pa")
solution

#Lab2_fa1
v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
m1<-cbind(v1,v2,v3,v4,v5,v6)
m1
cor(m1)
factanal(m1, factors=3) #varimax is the default
factanal(m1, factors = 3, rotation = "promax") 
#the following shows the g factor as PC1
prcomp(m1)
factanal(~v1+v2+v3+v4+v5+v6, factors = 3, scores = "Bartlett")$scores

#Lab2_fa2
install.packages("Hmisc")
library(Hmisc)
AthleticsData<-spss.get("AthleticsData.sav")
attach(AthleticsData)
names(AthleticsData)
cor(AthleticsData)
prcomp(AthleticsData)

fit.2<-factanal(AthleticsData, factors=3, rotation="varimax")
print(fit.2)

fit.3<-factanal(AthleticsData, factors=3, rotation="varimax")
print(fit.3)
print(fit.3, digits=2, cutoff=.2, sort=T)

install.packages("GPArotation")
library(GPArotation)
fit<-principal(AthleticsData, nfactors=3, rotate="varimax")
fit

#Lab2_fa4
e<-read.csv("EPI_data.csv")
e.keys<-make.keys(e, list(E = c(1, 3, -5, 8, 10, 13, -15, 17, -20, 22, 25, 27,
                                -29, -32, -34, -37, 39, -41, 44, 46, 49, -51, 53, 56),
                          N=c(2, 4, 7, 9, 11, 14, 16, 19, 21, 23, 26, 28, 31, 33, 35, 38, 40,
                              43, 45, 47, 50, 52, 55, 57),
                          L = c(6, -12, -18, 24, -30, 36, -42, -48, -54),
                          I =c(1, 3, -5, 8, 10, 13, 22, 39, -41), 
                          S = c(-11, -15, 17, -20, 25, 27, -29, -32, -37, 44, 46, -51, 53)))
scores<-scoreItems(e.keys, e)
N <- e[abs(e.keys[,"N"]) >0]
E <- e[abs(e.keys[,"E"]) >0]
fa.lookup(e.keys[,1:3],e.dictionary) 

#Lab2_fa5
set.seed(1.234)
N<-200 #number of observations
P<-6 #num of variables
Q<-2 #num of factors

Lambda <- matrix(c(0.7,-0.4, 0.8,0, -0.2,0.9, -0.3,0.4, 0.3,0.7, -0.8,0.1),
                 nrow=P, ncol=Q, byrow=TRUE)
library(mvtnorm)
FF<-rmvnorm(N, mean=c(5,15), sigma=diag(Q))
E<-rmvnorm(N, rep(0,P), diag(P))
X<-FF %*% t(Lambda) + E
Xdf<-data.frame(X)

library(psych)
fa(X, nfactors=2, rotate="varimax")

Xdi<- lapply(Xdf, function(x) cut(x, breaks=c(-Inf, median(x), Inf), ordered=TRUE))
Xdidf<-do.call("data.frame", Xdi)
XdiNum<-data.matrix(Xdidf)

install.packages("polycor")
library(polycor)
pc<-hetcor(Xdidf, ML=T)
faPC<-fa(r=pc$correlations, nfactors=2, n.obs=N, rotate="varimax")
faPC$loadings

faPCdirect<- fa.poly(XdiNum, nfactors=2, rotate="varimax")
faPCdirect$fa$loadings
factor.plot(faPCdirect$fa, cut=0.5)
fa.diagram(faPCdirect)
fa.parallel.poly(XdiNum)
vss(pc$correlations, n.obs=N, rotate="varimax")
library(random.polychor.pa)
random.polychor.pa(data.matrix=XdiNum, nrep=5, q.eigen=.99)