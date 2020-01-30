attach(EPI_Data)
#Exercise 1: Fitting a distribution beyond histograms
plot(ecdf(EPI), do.points=F, verticals=T)
help("qqnorm")
par(pty="s")
qqnorm(EPI); qqline(EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df=5),x,xlab="Q-Q plot for t dsn")
x2<- seq(30,96,2)
qqplot(qt(ppoints(250), df=5),x,xlab="Q-Q plot")
qqline(x)
qqline(x2)

plot(ecdf(DALY), do.points=F, verticals=T)
qqnorm(DALY); qqline(DALY)
x3<-seq(0,80,1)
qqplot(qt(ppoints(250), df=5),x3,xlab="Q-Q plot for DALY")
qqline(x3)

plot(ecdf(WATER_H), do.points=F, verticals =T)
qqnorm(WATER_H); qqline(WATER_H)
x4<-seq(0,100,1)
qqplot(qt(ppoints(250), df=5),x4,xlab="Q-Q plot for Water_H")

boxplot(EPI,DALY)
boxplot(EPI, ENVHEALTH, ECOSYSTEM, DALY, AIR_H, WATER_H, AIR_E,WATER_E, BIODIVERSITY)

#Multivariate
multivariate<-read.csv("/Users/brownn4/Documents/GradSchool/Grad-Data Analytics/multivariate.csv")
attach(multivariate)
head(multivariate)
mm<-lm(Homeowners~Immigrant)
mm
summary(mm)$coef
plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)
newImmigrantdata<-data.frame(Immigrant=c(0,20))
predict(mm,newImmigrantdata)
abline(mm)
abline(mm,col=3,lwd=3)
attributes(mm)
mm$coefficients

#Slide 23
plot(mtcars$wt, mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt, mtcars$mpg)
qplot(wt,mpg,data=mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
plot(pressure$temperature, pressure$pressure, type ='l')
points(pressure$temperature, pressure$pressure)
lines(pressure$temperature, pressure$pressure/2, col="red")
points(pressure$temperature,pressure$pressure/2, col="blue")

qplot(pressure$temperature, pressure$pressure, geom="line")
qplot(temperature, pressure, data=pressure, geom="line")
ggplot(pressure, aes(x=temperature, y=pressure))+geom_line()+geom_point()
ggplot(pressure, aes(x=temperature, y=pressure))+geom_line()+geom_point()

#Creating Bar graphs
barplot(BOD$demand, names.arg=BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
qplot(factor(cyl), data=mtcars)
ggplot(mtcars, aes(x=factor(cyl))) + geom_bar()

#Creating Histograms
hist(mtcars$mpg)
hist(mtcars$mpg,breaks=10)
hist(mtcars$mpg, breaks=5)
hist(mtcars$mpg, breaks=12)
qplot(mpg,data=mtcars, binwidth=4)
ggplot(mtcars, aes(x=mpg))+geom_histogram(binwidth=4)
ggplot(mtcars, aes(x=mpg))+geom_histogram(binwidth=5)

#Creating Box-plot
plot(ToothGrowth$supp, ToothGrowth$len)
boxplot(len~supp, data=ToothGrowth)
boxplot(len~supp+dose, data=ToothGrowth)
qplot(ToothGrowth$supp, ToothGrowth$len, geom="boxplot")
qplot(supp, len, data=ToothGrowth, geom="boxplot")
ggplot(ToothGrowth, aes(x=supp, y=len))+geom_boxplot()
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom = "boxplot")
qplot(interaction(supp, dose), len, data=ToothGrowth, geom="boxplot")
ggplot(ToothGrowth, aes(x=interaction(supp,dose),y=len))+geom_boxplot()
