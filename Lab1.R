rm(list=ls())
Grump<-read.csv(file.choose(), header = T)
EPI_Data<-read.csv(file.choose(), header=T)

hist(Grump$Num.Settlement.Points, main="Grump Sum sq. Km Histogram")

attach(EPI_Data)
summary(EPI)
fivenum(EPI)
stem(EPI)
hist(EPI_Data$EPI, main="EPI Histogram")
hist(EPI, seq(30.,95.,1.0), prob=T)
lines(density(EPI,na.rm=T,bw=1.))
rug(EPI)
plot(ecdf(EPI), do.points=F, verticals=T)
par(pty='s')
qqnorm(EPI); qqline(EPI)

x<-seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)

#Exercise 1
summary(DALY)
fivenum(DALY)
stem(DALY)
hist(DALY)
hist(DALY,seq(0.,100.,10.0), prob=T)
lines(density(DALY,na.rm=T,bw=10.))
rug(DALY)
plot(ecdf(DALY), do.points=F, verticals=T)
par(pty='s')
qqnorm(DALY); qqline(DALY)

#Comparing distrubitons
boxplot(EPI,DALY)
qqplot(EPI,DALY)

boxplot(EPI,ENVHEALTH,ECOSYSTEM,DALY,AIR_H,WATER_H,AIR_E,WATER_E,BIODIVERSITY)

#Exercise 2
EPILand<-EPI[!Landlock]
ELand<-EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30.,95.,1.0), prob=T)

summary(ELand)
fivenum(ELand)
stem(ELand)
lines(density(ELand, na.rm=T, bw=10.))
rug(Eland)
plot(ecdf(ELand), do.points=F, verticals=T)
par(pty='s')
qqnorm(ELand); qqline(ELand)


EPISurfacewater <- EPI[!No_surface_water]
ESurfacewater<-EPISurfacewater[!is.na(EPISurfacewater)]
hist(ESurfacewater, seq(30.,95.,1.0), prob=T)
#use subset function to filter on region
EPI_South_Asia <- subset(EPI_Data, EPI_regions == "South Asia" )
ggplot(EPI_Data$EPI_regions)


#Grump data
summary(Grump)
detach(EPI)
attach(Grump)
fivenum(Diff00, na.rm=TRUE)
stem(Diff00)
hist(Diff00)
hist(Diff00, seq(-100.,30.,10.0),prob=TRUE)
lines(density(Diff00, na.rm=TRUE, bw=1.))
rug(Diff00)

#Cumulative density function
plot(ecdf(Diff00), do.points=F, verticals=T)
#Quantile-quantile
par(pty="s")
qqnorm(Diff00); qqline(Diff00)
#Make a Q-Q plot
x<-seq(-100,30,10)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

#comparing Grump distributions
boxplot(Diff00,Diff90)
qqplot(Diff00,Diff90)
boxplot(Diff00, Diff90, Diff95)

#Filtering Grump
GrumpRefYear <-Grump[!RefYearFirst,]
GRefYear<-GrumpRefYear[!is.na(GrumpRefYear)]

