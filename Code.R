#Input CSV files
setwd("C:/Users/zoey4/Desktop/Academic/Explanaory Model/Homework3/data/single data file")
literacy<-read.csv('literacy rate.csv',header=TRUE,sep=',',skip=4,stringsAsFactors = FALSE)
electric<-read.csv('Electric power consumption(kWh per capita).csv',header=TRUE,sep=',',skip=4,stringsAsFactors = FALSE)
employment<-read.csv('children in employment total.csv',header=TRUE,sep=',',skip=4,stringsAsFactors = FALSE)
GDP<-read.csv('GDP per capita(current US).csv',header=TRUE,sep=',',skip=4,stringsAsFactors = FALSE)
enrollpre<-read.csv('Gross enrollment ratio pre-primary.csv',header=TRUE,sep=',',skip=4,stringsAsFactors = FALSE)
enrollpri<-read.csv('Gross enrollment ratio primary.csv',header=TRUE,sep=',',skip=4,stringsAsFactors = FALSE)
health<-read.csv('health expenditure per capita.csv',header=TRUE,sep=',',skip=4,stringsAsFactors = FALSE)
internet<-read.csv('Internet User(per 100 people).csv',header=TRUE,sep=',',skip=4,stringsAsFactors = FALSE)
life<-read.csv('Life expectancy.csv',header=TRUE,sep=',',skip=4,stringsAsFactors = FALSE)
mortality<-read.csv('Mortality rate neonatal (per 1000 live births).csv',header=TRUE,sep=',',skip=4,stringsAsFactors = FALSE)
overage<-read.csv('population ages 65 and above (% of total).csv',header=TRUE,sep=',',skip=4,stringsAsFactors = FALSE)
poverty<-read.csv('poverty head ratio at 1.9 per day.csv',header=TRUE,sep=',',skip=4,stringsAsFactors = FALSE)
pricompletion<-read.csv('primary completion rate(total) % of relevant age group.csv',header=TRUE,sep=',',skip=4,stringsAsFactors = FALSE)
education<-read.csv('education expenditure.csv',header=TRUE,sep=',',skip=4,stringsAsFactors = FALSE)

#Extract data of Year 2014 and rename the columns
electric14<-electric[,c("Country.Name","X2014")]
colnames(electric14)<-c("CountryName","electric")

employment14<-employment[,c("Country.Name","X2014")]
colnames(employment14)<-c("CountryName","employment")

GDP14<-GDP[,c("Country.Name","X2014")]
colnames(GDP14)<-c("CountryName","GDP")

enrollpre14<-enrollpre[,c("Country.Name","X2014")]
colnames(enrollpre14)<-c("CountryName","enrollpre")

enrollpri14<-enrollpri[,c("Country.Name","X2014")]
colnames(enrollpri14)<-c("CountryName","enrollpri")

health14<-health[,c("Country.Name","X2014")]
colnames(health14)<-c("CountryName","health")

internet14<-internet[,c("Country.Name","X2014")]
colnames(internet14)<-c("CountryName","internet")

life14<-life[,c("Country.Name","X2014")]
colnames(life14)<-c("CountryName","life")

mortality14<-mortality[,c("Country.Name","X2014")]
colnames(mortality14)<-c("CountryName","mortality")

overage14<-overage[,c("Country.Name","X2014")]
colnames(overage14)<-c("CountryName","overage")

poverty14<-poverty[,c("Country.Name","X2014")]
colnames(poverty14)<-c("CountryName","poverty")

pricompletion14<-pricompletion[,c("Country.Name","X2014")]
colnames(pricompletion14)<-c("CountryName","pricompletion")

literacy14<-literacy[,c("Country.Name","X2014")]
colnames(literacy14)<-c("CountryName","literacy")

education14<-education[,c("Country.Name","X2014")]
colnames(education14)<-c("CountryName","education")


#Combine all data of Year 2014 together
set01<-merge(electric14,employment14,by='CountryName',all.x=TRUE,all.y=TRUE)
set02<-merge(set01,GDP14,by='CountryName',all.x=TRUE,all.y=TRUE)
set03<-merge(set02,enrollpre14,by='CountryName',all.x=TRUE,all.y=TRUE)
set04<-merge(set03,enrollpri14,by='CountryName',all.x=TRUE,all.y=TRUE)
set05<-merge(set04,health14,by='CountryName',all.x=TRUE,all.y=TRUE)
set06<-merge(set05,internet14,by='CountryName',all.x=TRUE,all.y=TRUE)
set07<-merge(set06,life14,by='CountryName',all.x=TRUE,all.y=TRUE)
set08<-merge(set07,mortality14,by='CountryName',all.x=TRUE,all.y=TRUE)
set09<-merge(set08,overage14,by='CountryName',all.x=TRUE,all.y=TRUE)
set10<-merge(set09,poverty14,by='CountryName',all.x=TRUE,all.y=TRUE)
set11<-merge(set10,education14,by='CountryName',all.x=TRUE,all.y=TRUE)
data2014<-merge(set11,pricompletion14,by='CountryName',all.x=TRUE,all.y=TRUE)

#Delete data with missing value
data201401<-data2014[,c(-2,-3,-7,-12,-13)]
cleandata<-data201401[complete.cases(data201401),]

cleandataplusliter<-merge(literacy14,cleandata,by="CountryName",all.y=TRUE)
write.csv(cleandataplusliter,file='2014data.csv')


setwd("C:/Users/zoey4/Desktop/Academic/Explanaory Model/Homework3/data/single data file")
final2014<-read.csv('2014datadeletenoncountires.csv',header=TRUE,sep=',',stringsAsFactors = FALSE)
finaldata<-(final2014[,-1])

###Construct hitogram for 10 variables

par(mfrow=c(2,2))
#pdf("histogram01.pdf", width=16, height=8)
hist(finaldata$literacy,col="cadetblue", las=TRUE, xlab="Adult literacy rate (%)", 
     main="Histogram of Adult Literacy Rate across Countries", cex.main=1.2, cex.axis=1.2, cex.lab=1.2)

hist(finaldata$GDP,col="cadetblue", las=TRUE, xlab="GDP per capita($)", 
     main="Histogram of GDP Per Capita across Countries", cex.main=1.2, cex.axis=1.2, cex.lab=1.2)

hist(finaldata$enrollpre,col="cadetblue", las=TRUE, xlab="Gross enrolment ratio for pre-primary(%)", 
     main="Histogram of Gross Enrollment Ratio for Pre-primary\n across Countries", cex.main=1.2, cex.axis=1.2, cex.lab=1.2)

hist(finaldata$enrollpri,col="cadetblue", las=TRUE, xlab="Gross enrolment ratio for primary(%)", 
     main="Histogram of Gross Enrollment Ratio for Primary\n across Countries", cex.main=1.2, cex.axis=1.2, cex.lab=1.2)
dev.off()

pdf("histogram02.pdf", width=16, height=8)
hist(finaldata$health,col="cadetblue", las=TRUE, xlab="Health expenditure per capita($)", 
     main="Histogram of Health Expenditure Per Capita across Countries", cex.main=1.2, cex.axis=1.2, cex.lab=1.2)

hist(finaldata$internet,col="cadetblue", las=TRUE, xlab="Internet users(/100 people)", 
     main="Histogram of Internet Users Per 100 People across Countries", cex.main=1.2, cex.axis=1.2, cex.lab=1.2)

hist(finaldata$life,col="cadetblue", las=TRUE, xlab="Life expectancy at birth(years)", 
     main="Histogram of Life Expectancy at Birth across Countries", cex.main=1.2, cex.axis=1.2, cex.lab=1.2)

hist(finaldata$mortality,col="cadetblue", las=TRUE, xlab="Mortality rate (/1000 live births)", 
     main="Histogram of Morality Rate across Countries", cex.main=1.2, cex.axis=1.2, cex.lab=1.2)
dev.off()

pdf("histogram03.pdf", width=16, height=8)
hist(finaldata$overage,col="cadetblue", las=TRUE, xlab="Population ages 65 and above(%)", 
     main="Histogram of Population Ages 65 and Above\n across Countries", cex.main=1.2, cex.axis=1.2, cex.lab=1.2)

hist(finaldata$pricompletion,col="cadetblue", las=TRUE, xlab="Primary completion rate(%)", 
     main="Histogram of Primary Completion Rate\n across Countries", cex.main=1.2, cex.axis=1.2, cex.lab=1.2)
dev.off()

####Construct scatterplot to analyze the linear relationship
pdf("scatterplot.pdf", width=16, height=8)
plot(finaldata[,-1],las=TRUE, pch=19, col="firebrick")
dev.off()
cor(finaldata[,-1])

###########a. It seems like there is a curve relationship between literacy and GDP
######Finally decide to transform GDP into 1/GDP.
finaldata01<-finaldata[,-3]
finaldata01$reGDP<-1/(finaldata$GDP)
cor(finaldata01[,-1])

#To make a comparison before and after data comparison
par(mfrow=c(1,2))
plot(finaldata$GDP,finaldata$literacy,las=TRUE, pch=19, col="firebrick")
plot(1/(finaldata$GDP),finaldata$literacy,las=TRUE, pch=19, col="firebrick")
dev.off()

############b. There also is a curve relationship between health and literacy
#Considering the better distrubition and higher correlation, decide to use log health.
finaldata02<-finaldata01[,-5]
finaldata02$LNhealth<-log(finaldata01$health)
plot(finaldata02[,-1])


par(mfrow=c(1,2))
plot(finaldata$health,finaldata$literacy,las=TRUE, pch=19, col="firebrick")
plot(log(finaldata01$health),finaldata01$literacy,las=TRUE, pch=19, col="firebrick")


###Same data transformation for overage

finaldata03<-finaldata02[,-8]
finaldata03$LNoverage<-log(finaldata02$overage)

par(mfrow=c(1,2))
plot(finaldata$overage,finaldata$literacy,las=TRUE, pch=19, col="firebrick")
plot(log(finaldata01$overage),finaldata01$literacy,las=TRUE, pch=19, col="firebrick")

##################################
#Model building

####First model:
lm.1<-lm(literacy~reGDP+enrollpre+internet+pricompletion+enrollpri+mortality+LNoverage+life+LNhealth, 
         data=finaldata03)
summary(lm.1)

#Check multicollinearity
vif(finaldata03[,c("reGDP",'life','internet','enrollpre','enrollpri','mortality','LNoverage','LNhealth')])


require(leaps)

# forward stepwise
forward.step <- regsubsets(literacy~reGDP+enrollpre+internet+pricompletion+enrollpri+mortality+LNoverage+life, 
                           data=finaldata03, method="forward")

summary(forward.step)
round(summary(forward.step)$cp, digits=1)


lm.2<-lm(literacy~mortality+pricompletion+LNoverage,data=finaldata03) # Model using 3 variables
lm.3<-lm(literacy~mortality+pricompletion+LNoverage+life,data=finaldata03) #Model using 4 variables
lm.4<-lm(literacy~mortality+pricompletion+LNoverage+life+internet,data=finaldata03) #Model using 5 variables

#Partial F-test for two pairs of nested models
anova(lm.2,lm.3)
anova(lm.3,lm.4)

summary(lm.2)

#Check regression assumption-----Constant variance
plot(lm.2$fitted, lm.2$residuals, las=TRUE,col="darkolivegreen4", pch=19, xlab="Fitted (%)", ylab="Residuals (%)", 
     main="Residuals vs. Fitted Plot", cex.axis=1.5, cex.lab=1.5, cex.main=1.8)
abline(h=0, lty=2, col="gray50")

#Check regression assumption----- linearity
qqnorm(lm.2$residuals, las=TRUE, col="darkolivegreen4", pch=19, cex.axis=1.5, cex.lab=1.5, cex.main=1.8)
qqline(lm.2$residuals)
#dev.off()


#Use jackknife (resampling) method for external model validation

require(qpcR)
sqrt(PRESS(lm.2)$stat/(64-(3+1))) #Compute RMSE jackknife
PRESS(lm.2)$stat/sum((finaldata03$literacy-mean(finaldata03$literacy))^2) #Compute R square jackknife






