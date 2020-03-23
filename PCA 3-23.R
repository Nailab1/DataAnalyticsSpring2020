#In this lab, we perform PCA on the USArrest data set
data("USArrests")
states=row.names(USArrests)
states
names(USArrests)
# We first briefly examine the data. We notice that the variables have vastly
#different means.
# Note that the apply() function allows us to apply a function-in this case,
#the mean() function
# to each row or column of the data set.
# The second input here denotes whether we wish to compute the mean of
#the rows, 1, or the columns, 2.

apply(USArrests, 2,mean)
apply(USArrests,2,var)
# If we failed to scale the variables before performing PCA,
# then most of the principal components that we observed
would be driven by the Assault variable,
# since it has by far the largest mean and variance. Thus, it is
#important to standardize the variables to have mean zero and
# standard deviation one before performing PCA.

#performing PCA using prcomp() function
#the prcomp() function centers the variables to have mean zero, by using the option scale =TRUE 
#we scale the variables to have standard deviation one
pr.out=prcomp(USArrests, scale=T)
names(pr.out)
#The center & scale components correspond to the means and S>D of the 
#variables that were used for scaling prior to implementing PCA
pr.out$center
pr.out$scale
#The rotation matrix provides the principal component loadings; each col 
#of pr.out$rotation contains the corresponding principal component loading vector
pr.out$rotation
dim(pr.out$x)
#Ploting the first 2 principal components 
biplot(pr.out, scale=0)
#scale=0 ensures that the arrows are scaled to represent the loadings
pr.out$sdev
pr.var<-pr.out$sdev^2
pr.var

#to compute the propotion of variance explained by each principal component we simply
#devide the variance explained by each principal component by the total
#variance explained by all four principal components
pve=pr.var/sum(pr.var)
pve

#PCA w iris dataset
data("iris")
head(iris)
#creating another dataset from the iris dataset that contains the columns from 1 to 4
irisdata1<-iris[,1:4]
irisdata1
principal_components<-princomp(irisdata1,cor=T,score=T)
#cor is a logical value indicating whether the calculation should use the correlation
#matrix or the covariance matrix
#The correlation matrix can only be used if there are no constant variables
#score is a logical value indicating whether he score on each principal component should be calculated
summary(principal_components)
#ploting principal components
plot(principal_components)
#plotting the principal components - line plot
plot(principal_components, type="l")
biplot(principal_components)

#PCA on Boston Dataset
data(Boston, package="MASS")
#PCA
pca_out<-prcomp(Boston, scale=T)
pca_out
plot(pca_out)
biplot(pca_out, scale = 0)
boston_pc<-pca_out$x
boston_pc
summary(boston_pc)
head(boston_pc)
