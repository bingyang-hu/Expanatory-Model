setwd("C:/Users/zoey4/Desktop/Academic/Explanaory Model/Homework2")
y <- read.table("wine.dat", header=TRUE, sep=",", stringsAsFactors=FALSE)

#To assign NA into dot.
y$DEGREES[y$DEGREES=='.']<-NA
y$LPRICE2[y$LPRICE2=='.']<-NA 

y<-y[complete.cases(y),] 
rownames(y)<-1:nrow(y)

#Change the type of variables
y$DEGREES<-as.numeric(y$DEGREES)  
y$LPRICE2<-as.numeric(y$LPRICE2)

#To make a scatter plot matrix of all variables in the model
pdf("scatter_matrix.pdf", width=6, height=6)
pairs(y[,-1],las=TRUE, pch=19, col="firebrick")
dev.off()

#(e)Fit two models
#Model 1: LPRICE2 VS TIME_SV
lm.1<-lm(LPRICE2~TIME_SV,data=y)
summary(lm.1)

#Model 2: LPRICE2 VS WRAIN+DEGREES+HRAIN+TIME_SV
lm.2<-lm(LPRICE2~WRAIN+DEGREES+HRAIN+TIME_SV,data=y)
summary(lm.2)

#(h)
##Overall F-test

##Model 1: LPRICE2 VS TIME_SV
anova(lm.1)

#Model 2: LPRICE2 VS WRAIN+DEGREES+HRAIN+TIME_SV
anova(lm.2)

##PRESS
PRESS(lm.1)
PRESS(lm.2)





#########################################2.
p <- read.table("prestige.dat", header=TRUE, sep=",", stringsAsFactors=FALSE)



#b)# Create scatterplot matrix for all quantitative variables.
pdf("scatter_matrix_symbol.pdf", width=6, height=6)
pch.vector <- rep(0, times=nrow(p))
pch.vector[p$type==" "] <-3
pch.vector[p$type=="bc"] <- 6
pch.vector[p$type=="prof"] <- 8
pairs(p[,c("prestige","education", "income", "women")], las=TRUE, pch=pch.vector, col="firebrick")
dev.off()


#Comparing nested model
lm.1<-lm(prestige ~education + income +type, data=p )
summary(lm.1)
lm.2<-lm(prestige ~education + income + women+type, data=p)
summary(lm.2)
anova(lm.1,lm.2)

#c) which professions have missing "type"
p[which(p$type==''),"occupation.group"]



#d)
#Scatterplot between Prestige and Education for Each Type
pch.vector <- rep(0, times=nrow(p))
pch.vector[p$type==" "] <-3
pch.vector[p$type=="bc"] <- 6
pch.vector[p$type=="prof"] <- 8
plot(p$education,p$prestige,pch=pch.vector,col="blue",las=TRUE,
     xlab="Average Education of Occupational incumbents in 1971(years)",
     ylab="Pineo-Porter Prestige Score for Occupation", 
     main="Scatterplot between Prestige and Education for Each Type", 
     cex.main=1.5, cex.axis=1.5, cex.lab=1.5) 

#Scatterplot between Prestige and Income for Each Type
pch.vector <- rep(0, times=nrow(p))
pch.vector[p$type==" "] <-3
pch.vector[p$type=="bc"] <- 6
pch.vector[p$type=="prof"] <- 8
plot(p$income,p$prestige,pch=pch.vector,col="blue",las=TRUE,
     xlab="Average Income of Occupational incumbents in 1971(dollars)",
     ylab="Pineo-Porter Prestige Score for Occupation", 
     main="Scatterplot between Prestige and Income for Each Type", 
     cex.main=1.5, cex.axis=1.5, cex.lab=1.5) 


#e)
lm.3<-lm(prestige ~education + income + type+type*income+type*education, data=p)
summary(lm.3)




#f)
#Construct a histogtam for average income
hist(p$income,las=TRUE,col="pink",
     xlab="Average Income of Occupational Incumbents in 1971(dollars)",
     ylab="Frequency",
     main="Histogram of Average Income \nof Occupational Incumbents in 1971", cex.main=1.5)

#Construct a histogram for log income
hist(log(p$income),las=TRUE,col="pink",
     xlab="Average Log Income of Occupational Incumbents in 1971",
     ylab="Frequency",
     main="Histogram of Log Income \nof Occupational Incumbents in 1971", cex.main=1.5)


#g)
#Fit the model using the log income
p.new<-p
p.new$income<-log(p$income)
lm.4<-lm(prestige ~education + income + type+type*income+type*education, data=p.new)
summary(lm.4)

PRESS(lm.3)
PRESS(lm.4)

anova(lm.3)
anova(lm.4)

