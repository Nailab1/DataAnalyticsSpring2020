setwd("C:\\Users\\brownn4\\Documents\\Gradschool\\Grad-Data Analytics")
multivariate<- read.csv("multivariate.csv")
attach(multivariate)

#Scatterplots
plot(Income, Immigrant, main = "Scatterplot")
plot(Immigrant, Homeowners)

#Fitting Linear Models using "lm" function
mm<-lm(Homeowners~Immigrant)
mm
abline(mm)
abline(mm,col=2,lwd=3)
summary(mm)
attributes(mm)
mm$coefficients

HP<-Homeowners/Population
PD<-Population/area
mm<-lm(Immigrant~Income+Population+HP+PD)
summary(mm)

cm<-coef(mm)
cm

# Pipe operator:  %>%
library(dplyr)
df_mtcars <- mtcars
head(df_mtcars)

# nesting 
filter(df_mtcars, mpg > 20) # filter mpg > 20
# we want to get 10 samples of that
sample_n(filter(df_mtcars, mpg > 20), 10)
# now we want to arrange them in the descending order based on the mpg
arrange( sample_n(filter(df_mtcars, mpg >20), 10) ,desc(mpg))
# we can assign this result to a variable called results_mpg
results_mpg <- arrange( sample_n(filter(df_mtcars, mpg >20), 10) ,desc(mpg))
results_mpg

# You can do the above using the Pipe Operator %>%
# dataFrame %>% op1 %>% op2 <$op3
a1<-filter(df_mtcars, mpg>20)
a2<- sample_n(a1,5) #getting a random sample of 5
results_mpg_des<- arrange(a2,desc(mpg))
results_mpg_des

library(dplyr)
df_mtcars %>% filter(mpg>20) %>% sample_n(5) %>% arrange(desc(mpg))
results<- df_mtcars %>% filter(mpg>20) %>% sample_n(5) %>% arrange(desc(mpg))
