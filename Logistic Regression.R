setwd("C:/Users/zoey4/Desktop/Academic/Explanaory Model/Homework4")
Dating<-read.csv("SpeedDating.csv",header=TRUE,sep=",",stringsAsFactors = FALSE)
head(Dating)
colnames(Dating)

###1. 

length(which(Dating$DecisionM==0 & Dating$DecisionF==0))/nrow(Dating)
length(which(Dating$DecisionM==1 & Dating$DecisionF==0))/nrow(Dating)
length(which(Dating$DecisionM==0 & Dating$DecisionF==1))/nrow(Dating)
length(which(Dating$DecisionM==1 & Dating$DecisionF==1))/nrow(Dating)
# This is the percentage of dates ended with both people wanting a second date
 
###2.
Dating$second.date<-c()
for ( i in 1:nrow(Dating)) {
 if (Dating$DecisionM[i]==1 & Dating$DecisionF[i]==1)
    Dating$second.date[i]<-1
 else
    Dating$second.date[i]<-0
  }   #End for loop

###Consutrut scatterplots for each pair of numerical variables
pdf("jitterplot.pdf",height=8,width=5)
par(mfrow=c(3,3))
col.vector <- rep("black", times=nrow(Dating))
col.vector[Dating$second.date==0] <- "cadetblue"
col.vector[Dating$second.date==1] <- "orange"
plot(jitter(Dating$LikeM),jitter(Dating$LikeF),las=TRUE, pch=1, col=col.vector,xlab="LikeM(1-10)", ylab="LikeF(1-10)",
     main="Scatterplot of LikeF vs. LikeM", cex.lab=1, cex.axis=1, cex.main=1.)
legend("bottomright", legend=c("SeconddateYes", "SeconddateNo"),
        pch=1,col=c("orange", "cadetblue"), bty="n", cex=1)

plot(jitter(Dating$PartnerYesM),jitter(Dating$PartnerYesF),las=TRUE, pch=1, col=col.vector,xlab="PartnetYesM(1-10)", ylab="PartnerYesF(1-10)",
     main="Scatterplot of PartnerYesF  vs. PartnerYesM", cex.lab=1, cex.axis=1, cex.main=1)
legend("bottomright", legend=c("SeconddateYes", "SeconddateNo"),
       pch=1,col=c("orange", "cadetblue"), bty="n", cex=1)

plot(jitter(Dating$AgeM),jitter(Dating$AgeF),las=TRUE, pch=1, col=col.vector,xlab="AgeM (years)", ylab="AgeF (years)",
     main="Scatterplot of AgeF  vs. AgeM", cex.lab=1, cex.axis=1, cex.main=1)
legend("bottomright", legend=c("SeconddateYes", "SeconddateNo"),
       pch=1,col=c("orange", "cadetblue"), bty="n", cex=1)

plot(jitter(Dating$AttractiveM),jitter(Dating$AttractiveF),las=TRUE, pch=1, col=col.vector,xlab="AttractiveM(1-10)", ylab="AttractiveF(1-10)",
     main="Scatterplot of AttractiveF  vs. AttractiveM", cex.lab=1, cex.axis=1, cex.main=1)
legend("bottomright", legend=c("SeconddateYes", "SeconddateNo"),
       pch=1,col=c("orange", "cadetblue"), bty="n", cex=1)

plot(jitter(Dating$SincereM),jitter(Dating$SincereF),las=TRUE, pch=1, col=col.vector,xlab="SincereM(1-10)", ylab="SincereF(1-10)",
     main="Scatterplot of SincereF  vs. SincereM", cex.lab=1, cex.axis=1, cex.main=1)
legend("bottomright", legend=c("SeconddateYes", "SeconddateNo"),
       pch=1,col=c("orange", "cadetblue"), bty="n", cex=1)

plot(jitter(Dating$IntelligentM),jitter(Dating$IntelligentF),las=TRUE, pch=1, col=col.vector,xlab="IntelligentM(1-10)", ylab="IntelligentF(1-10)",
     main="Scatterplot of IntelligentF  vs. IntelligentM", cex.lab=1, cex.axis=1, cex.main=1)
legend("bottomright", legend=c("SeconddateYes", "SeconddateNo"),
       pch=1,col=c("orange", "cadetblue"), bty="n", cex=1)

plot(jitter(Dating$FunM),jitter(Dating$FunF),las=TRUE, pch=1, col=col.vector,xlab="FunM(1-10)", ylab="FunF(1-10)",
     main="Scatterplot of FunF  vs. FunM", cex.lab=1, cex.axis=1, cex.main=1)
legend("bottomright", legend=c("SeconddateYes", "SeconddateNo"),
       pch=1,col=c("orange", "cadetblue"), bty="n", cex=1)

plot(jitter(Dating$AmbitiousM),jitter(Dating$AmbitiousF),las=TRUE, pch=1, col=col.vector,xlab="AmbitiousM(1-10)", ylab="AmbitiousF(1-10)",
     main="Scatterplot of AmbitiousF  vs. AmbitiousM", cex.lab=1, cex.axis=1, cex.main=1)
legend("bottomright", legend=c("SeconddateYes", "SeconddateNo"),
       pch=1,col=c("orange", "cadetblue"), bty="n", cex=1)

plot(jitter(Dating$SharedInterestsM),jitter(Dating$SharedInterestsF),las=TRUE, pch=1, col=col.vector,xlab="SharedInterestsM(1-10)", ylab="SharedInterestsF(1-10)",
     main="Scatterplot of SharedInterestsF  vs. SharedInterestsM", cex.lab=1, cex.axis=1, cex.main=1)
legend("bottomright", legend=c("SeconddateYes", "SeconddateNo"),
       pch=1,col=c("orange", "cadetblue"), bty="n", cex=1)


###3.

###To see the distibutions of each variables.
summary(Dating)
summary(Dating$FunM)
summary(Dating$SharedInterestsM)
summary(Dating$SharedInterestsF)


#Replace value 0 with 1
Dating$FunM[c(which(Dating$FunM==0))]<-1
Dating$SharedInterestsM[c(which(Dating$SharedInterestsM==0))]<-1
Dating$SharedInterestsF[c(which(Dating$SharedInterestsF==0))]<-1

length(which(!complete.cases(Dating)))
summary(Dating)  #To see the number of  missing values in each variable





###No.4
table(Dating$RaceM)
table(Dating$RaceF)
#Remove all missing values.
Datingclean<-Dating[which(complete.cases(Dating)),]


# mosaic plot
temp.1 <- rep("female", times=2*nrow(Dating))
temp.1[1:nrow(Dating)] <- "male"
temp.2 <- rbind(Dating$RaceM,Dating$RaceF)

table(temp.1,temp.2)
mosaicplot(table(temp.1,temp.2), las=TRUE, xlab="GENDER", ylab="RACE",
           main="RACE vs. GENDER",cex.axis=1.4,
           col=c("cadetblue", "firebrick"))
#dev.off()

####No.5
cor(Datingclean[,c(-1,-2,-9,-10,-23)])  #Check the correlation 

require(leaps)
forward.step <- regsubsets(second.date~ LikeM + LikeF+ PartnerYesM+ PartnerYesF+AgeM+AgeF+RaceM+RaceF+
                             AttractiveM+AttractiveF+SincereM+SincereF+IntelligentM+IntelligentF+FunM+FunF+
                             AmbitiousM+AmbitiousF+SharedInterestsM+SharedInterestsF, 
                           data=Datingclean, method="forward")
summary(forward.step)
round(summary(forward.step)$cp, digits=1)

glm.1<-glm(second.date~LikeM + PartnerYesM+ PartnerYesF+RaceF+FunF, data=Datingclean, family=binomial(link="logit"))
summary(glm.1)
glm.2<-glm(second.date~ LikeM + PartnerYesM+ PartnerYesF+FunF, data=Datingclean, family=binomial(link="logit"))
summary(glm.2)
glm.3<-glm(second.date~ LikeM + PartnerYesM+ FunF, data=Datingclean, family=binomial(link="logit"))
summary(glm.3)

anova(glm.2, glm.1, test="LRT")
anova(glm.3,glm.2,test='LRT')

##To test nested model with original data
glm.4<-glm(second.date~ LikeM + PartnerYesM+ PartnerYesF+RaceF+
             FunF, data=Dating, family=binomial(link="logit"))
glm.5<-glm(second.date~ LikeM + PartnerYesM+ PartnerYesF+
             FunF, data=Dating, family=binomial(link="logit"))
glm.6<-glm(second.date~ LikeM + PartnerYesM+ FunF, 
           data=Dating, family=binomial(link="logit"))

summary(glm.4)
summary(glm.5)
summary(glm.6)
anova(glm.6, glm.5, test="LRT")
anova(glm.5, glm.4, test="LRT")


###To check outliers
par(mfrow=c(2,2))
boxplot(Dating$LikeM, col="cadetblue", las=TRUE, main="Box Plot of LikeM", xlab="LikeM", 
        cex.lab=1.8, cex.axis=1.8, cex.main=1.8, horizontal=TRUE)
boxplot(Dating$PartnerYesM, col="cadetblue", las=TRUE, main="Box Plot of PartnerYesM", xlab="PartnerYesM", 
        cex.lab=1.8, cex.axis=1.8, cex.main=1.8, horizontal=TRUE)
boxplot(Dating$PartnerYesF, col="cadetblue", las=TRUE, main="Box Plot of PartnerYesF", 
        xlab="PartnerYesF", cex.lab=1.8, cex.axis=1.8, cex.main=1.8, horizontal=TRUE)
boxplot(Dating$FunF, col="cadetblue", las=TRUE, main="Box Plot of FunF", 
        xlab="FunF", cex.lab=1.8, cex.axis=1.8, cex.main=1.8, horizontal=TRUE)


##Remove all outliers 
glm.7<- glm(second.date~ LikeM + PartnerYesM+ PartnerYesF+FunF, data=Dating, subset=(1:nrow(Dating))[-which(Dating$LikeM<3 & Dating$PartnerYesM<2
                                                                & Dating$DecisionF<2)])
summary(glm.7)

###To check VIF
require(usdm)
vif(Dating[,c("LikeM", "PartnerYesM", "PartnerYesF","FunF")])

###To check sample size
table(Dating$second.date)

#6.
# extract only the observations used to fit glm.5
Datingfinal <- Dating[complete.cases(Dating[,c("LikeM","PartnerYesM","PartnerYesF","FunF")]),]

length(which(Datingfinal$DecisionM==0 & Datingfinal$DecisionF==0))/nrow(Datingfinal)
length(which(Datingfinal$DecisionM==1 & Datingfinal$DecisionF==0))/nrow(Datingfinal)
length(which(Datingfinal$DecisionM==0 & Datingfinal$DecisionF==1))/nrow(Datingfinal)
length(which(Datingfinal$DecisionM==1 & Datingfinal$DecisionF==1))/nrow(Datingfinal)

nrow(Datingfinal)
table(Datingfinal$second.date)


#8.
require(pROC)
roc(response=Datingfinal$second.date, predictor=glm.5$fitted.values,
    plot=TRUE, las=TRUE, 	legacy.axes=TRUE, lwd=5,
    main="ROC for Second DatingAnalysis", cex.main=1.6, cex.axis=1.3, cex.lab=1.3)
legend("bottomright",legend=paste("AUC=", round(auc(response=Datingfinal$second.date, 
                                                    predictor=glm.5$fitted.values), digits=3), sep=" "),bty="n", cex=1.4)
#Calculate AUC
auc(response=Datingfinal$second.date, predictor=glm.5$fitted.values)


# save ROC curve into an object
roc.info <- roc(response=Datingfinal$second.date, predictor=glm.5$fitted.values)

#To get the best threshold for classifiying observation.
coords(roc.info, x="best", ret=c("threshold", "specificity", "sensitivity"))

##To see all threshold and the sensitivity, specificity
pi.range <- t(coords(roc.info, x="all", ret=c("threshold", "specificity", "sensitivity")))
head(pi.range)
tail(pi.range)
dim(pi.range)



#To calculate sensitivity, specificity and accuracy
temp <- Datingfinal
rownames(temp) <- 1:nrow(temp)
temp <- data.frame(temp, "fitted.values"=round(glm.5$fitted.values, digits=3))
actual.seconddate <- rep("SeconddateYes", times=nrow(temp))
actual.seconddate[temp$second.date == 0] <- "SeconddateNo"


# save ROC curve into an object
roc.info <- roc(response=Datingfinal$second.date, predictor=glm.5$fitted.values)
classify.best <- rep("SeconddateYes", times=nrow(temp))
classify.best[temp$fitted.values < coords(roc.info, x="best", ret="threshold")] <- "SeconddateNo"

table(classify.best, actual.seconddate)










