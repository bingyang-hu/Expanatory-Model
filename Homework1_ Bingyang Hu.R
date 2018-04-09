rm(list=ls())
setwd("C:/Users/zoey4/Desktop/Academic/Explanaory Model/Homework1")

y<-read.csv('Sporting.csv',header=TRUE,sep=',',stringsAsFactors = FALSE) #Read the data
colnames(y)
dim(y)

#2.
# Construct histograms for each variables.

#Construct a histogtam for Sales.
hist(y$Sales,las=TRUE,col="cadetblue",
     main='Latest one-month sales total \n for 38 stores',
     xlab="latest one-month sales total (dollars)", ylab='frequency',
     cex.lab=1.5, cex.axis=1, cex.main=1.5)

#Construct a histogram for Age.
hist(y$Age,las=TRUE,col="cadetblue",
     main='Median age of customer base \nfor 38 stores',
     xlab="median age of customer base (years)", ylab='frequency',
     cex.lab=1.5, cex.axis=1, cex.main=1.5)

#Construct a histogram for Growth
hist(y$Growth,las=TRUE,col="cadetblue",
     main='Annual popluation growth rate of customer base \nover the past 10 years of 38 stores',
     xlab="annual population growth rate of customer base ",
     ylab='frequency',
     cex.lab=1.5, cex.axis=1, cex.main=1.5)

#Construct a histogram for Income
hist(y$Income,las=TRUE,col="cadetblue",
     main='Median family income of customer base \nfor 38 stores',
     xlab="median family income of customer base(dollars)",ylab='frequency' ,
     cex.lab=1.5, cex.axis=1, cex.main=1.5)

#Construct a histogram for HS
hist(y$HS,las=TRUE,col="cadetblue",
     main='Percentage of customer base with a high school diploma \nfor 38 Stores',
     xlab="percentage of customer base(%)", ylab='frequency',
     cex.lab=1.5, cex.axis=1, cex.main=1.5)

#Construct a hitogram for College
hist(y$College,las=TRUE,col="cadetblue",
     main='Percentage of customer base with a  college dilpoma \nfor 38 Stores',
     xlab="percentage of customer base(%)", ylab='frequency',
     cex.lab=1.5, cex.axis=1, cex.main=1.5)

#Summary statistics for each variable.
summary(y)

#Calculate standard deviation of each variable.
options(scipen=999)  # To disable scientific notation
apply(y,2,sd)   #To calculate standard deviation of each variable.
 

#3.
cor(y) # Calculate correlation coefficient r for all variables


# Construct scatterplots for all variables with sales.


#Construct a scatterplot for Age with Sales

par(mar=c(5, 10, 4, 2))
plot(y$Age, y$Sales, las=TRUE, pch=19, xlab="age (years)", ylab="",
     main="Scatterplot between age and sales", cex.lab=1, cex.axis=1, cex.main=1.5)
mtext("sales (dollars)",side=2,line=7,col="black")

plot(y$Growth, y$Sales, las=TRUE, pch=19, xlab="growth", ylab="",
     main="Scatterplot between growth and sales", cex.lab=1, cex.axis=1, cex.main=1.5)
mtext("sales (dollars)",side=2,line=7,col="black")

plot(y$Income, y$Sales, las=TRUE, pch=19, xlab="income (dollars)", ylab="",
     main="Scatterplot between income and sales", cex.lab=1, cex.axis=1, cex.main=1.5)
mtext("sales (dollars)",side=2,line=7,col="black")

plot(y$HS, y$Sales, las=TRUE, pch=19, xlab="%", ylab="",
     main="Scatterplot between HS % and sales", cex.lab=1, cex.axis=1, cex.main=1.5)
mtext("sales (dollars)",side=2,line=7,col="black")

plot(y$College, y$Sales, las=TRUE, pch=19, xlab="%", ylab="",
     main="Scatterplot between college % and sales", cex.lab=1, cex.axis=1, cex.main=1.5)
mtext("sales (dollars)",side=2,line=7,col="black")

#Construct scatterplot between growth and sales after deleting outliers.
plot(y$Growth[y$Growth<20], y$Sales[y$Growth<20], las=TRUE, pch=19, xlab="growth", ylab="",
     main="scatterplot between growth and sales \nafter deleting outliers", cex.lab=1, cex.axis=1, cex.main=1.5)
mtext("sales (dollars)",side=2,line=7,col="black")

#5.

lm.y<-lm(Sales~HS,data=y) #Fitting a linear model.

summary(lm.y)

lm.y$coefficients #Extract coefficient


#6.
plot(y$HS, y$Sales, las=TRUE, pch=1, xlab="%", ylab="",
     main="Scatterplot between HS % and sales", 
     cex.lab=1, cex.axis=1, cex.main=1.5)
mtext('Sales (dollars)',side=2,line=7,col='black')
abline(coef(lm.y), col="forestgreen", lwd=3) #add the estimated regression line to the scatterplot
legend("topleft", col=c("forestgreen"), lwd=3, bty="n",
       legend=c("estimated"), cex=1.5)  


#7.
range(y$HS)



#10.
#residual plot

plot(y$HS, lm.y$residuals, las=TRUE, pch=19, col="blue", 
     xlab="HS (%)", ylab="", 
     main="Residual Plot", cex.lab=2, cex.axis=1.8, cex.main=1.8)
abline(h=0, lty=2, col="gray70", lwd=2)
mtext("Residuals ",side=2,line=7,cex=2)


#Normal quantile plot
qqnorm(lm.y$residuals, las=TRUE, cex.lab=2, cex.axis=1.8, cex.main=1.8, 
       pch=19, col="red", ylab="",
       main="Normal Q-Q Plot of Residuals")
qqline(lm.y$residuals)
mtext("Sample Quantiles",side=2,line=7,cex=2)

#11.
summary(lm.y)

# extract RMSE
summary(lm.y)$sigma

#13.
new.data <- data.frame("HS"=c(50,75,70)) #set up new data frames 
predict(lm.y, newdata=new.data) # use predict function

#14.
# 95% prediction interval for predictions
temp <- predict(lm.y, newdata=new.data,se.fit=TRUE,interval="prediction", level=0.95)
temp$fit


