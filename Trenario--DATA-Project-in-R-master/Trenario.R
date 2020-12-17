install.packages("plotly")
library(plotly)
library(mgcv)
library(nlme)
library(vcfR)
install.packages("openxlsx")
install.packages("AER")
install.packages("zoo")
install.packages("lmtest")
install.packages("visualize")
install.packages("orcutt")
install.packages("data.table")
install.packages("dummies")
install.packages("ggplot2")
install.packages('psych')
install.packages('plotrix')
install.packages('ggmap')
install.packages('corrplot')
install.packages('haven')
install.packages("tidyverse")
library('corrplot')
library('ggmap')
library('RColorBrewer')
library('plotrix')
library("openxlsx")
library('psych')
library("ggplot2")
library('visualize')
library("zoo")
library("lmtest")
library("orcutt")
library("AER") 
library("dummies")
library("haven")
library('tidyverse')
library("data.table")

#  Clear Objects
rm(list=ls())
#removing specific object
#rm( x )
######__________________________________________________________________________________________

# Change Directory
setwd("/Users/EfiPaka/Desktop/Econometrics/Data_Project_Trenario/")
#TrenarioData <- read.csv("Data tech - Data B5 - Test B.csv",1)
Pdata<-read.csv("Pdata.csv",1)

#conversion of dates
Pdata$StartDate <- as.Date(Pdata$StartDate, "%d/%m/%Y ") #time
Pdata$EndDate <- as.Date(Pdata$EndDate, "%d/%m/%Y ")
summary(Pdata)

# new messurement and vars
Pdata$satis_sent = (Pdata$learn_sent+Pdata$think_sent+Pdata$curious+Pdata$enjoy_sent+Pdata$more_course+Pdata$cred_sent+Pdata$uncomf_sent+Pdata$intres_sent)/8
Pdata$like_sent = (Pdata$assist_sent+Pdata$uncomf_sent+Pdata$indiff_sent+Pdata$intres_sent+Pdata$amia_sent+Pdata$arrog_sent+Pdata$friend_sent)/7
Pdata$satisfied <- as.numeric(Pdata$satis_sent > 3) # Dummy for satisf
Pdata$NewB<-(as.numeric((Pdata$Age>=25 & Pdata$Age<=31 )& (Pdata$Educ>=2 & Pdata$Educ<4)))
Pdata$A.B<- as.factor(ifelse(Pdata$NewB> 0, "B","A"))
Pdata$gender<- as.factor(ifelse(Pdata$Male> 0, "Male","Female"))
Pdata$time <- c(1:nrow(Pdata))
Pdata$aged<-as.factor(ifelse(Pdata$Age>=35, "Old","Young"))
Pdata$succeed<-as.numeric(Pdata$score>85)
Pdata$succeeded<-as.factor(ifelse(Pdata$score>=85, "85",""))


#checks
mean(Pdata$score,na.rm = TRUE)
mean(Pdata$satis_sent)
summary(Pdata$satis_sent,na.rm = TRUE)
describe(Pdata$Age[Pdata$typeB == 1])
describe((Pdata$score[Pdata$typeB == 1]))
describe(Pdata$Educ[Pdata$typeB == 1])
describe(Pdata$Male[Pdata$typeB == 1])
describe(Pdata$Age[Pdata$typeB == 0])
describe(Pdata$Duration[Pdata$NewB == 0])
describe(Pdata$Duration[Pdata$NewB == 1])

# new SCORE
Pdata$score_modif <- Pdata$score-15  #trying to normalize
mean(Pdata$score_modif,na.rm = TRUE)

#new satis weight

Pdata$satisModf<- (Pdata$satis_sent*0.8) + (Pdata$score_modif*0.2)  # trying to make weights
mean(Pdata$satisModf,na.rm = TRUE)
summary(Pdata$satisModf,na.rm = TRUE)


mean(Pdata$Age)#,na.rm = TRUE)
summary(Pdata$Age)
mean(Pdata$Period,na.rm = TRUE)
#____________________________________________________________________________
#____________________________________________________________________________

# Predictive Models:

#diffrence in diffrences model
Pdata$d2<-as.numeric(Pdata$Period==1)
Pdata$dT <- as.numeric(Pdata$NewB == 1)
Pdata$treat_effect <- Pdata$d2 * Pdata$dT

treat1 <- lm(satis_sent ~ d2 + dT + treat_effect, Pdata)
summary(treat1)

mean(Pdata$satis_sent[Pdata$typeB==0 & Pdata$Period==0]) #bdika

#_____________________________________________________________________________
treat2 <- lm(satis_sent ~  typeB+treat_effect+ Duration+assist_help+feel_sent+friend_sent+empat_sent+indiff_sent, Pdata)
summary(treat2)
#_____________________________________________________________________________

# Regular Linear Model OLS with extra vars

trenario_ols<-lm(satis_sent ~ Age+ Male+Educ+typeB+score+d2+treat_effect, Pdata)
summary(trenario_ols)


#_____________________________________________________________________________
# create a Probability Linear Model
Pdata$satisfied <- as.numeric(Pdata$satis_sent > 3) # Dummy for satisf

LPModel<-lm( satisfied ~Male+Expr+NewB+score, Pdata)
options(scipen=999, digits=4)
summary (LPModel)

LPM<-lm( satisfied ~Male+Expr+NewB+score+time, Pdata)
summary(LPM)
#removed Educ+Age
# create a logit  Model
LogitModel<-glm( satisfied ~ Male + Expr + NewB + score, Pdata, family="binomial" (link="logit"))
summary (LogitModel)

qplot(Pdata$satisfied~Pdata$time)
plot_ly(Pdata, x = ~like_sent, y = ~satisfied, z = ~NewB, type="scatter3d",mode="markers")

#__________________________________________________________________________________

#diffrence in diffrences NEW! model
Pdata$d2<-as.numeric(Pdata$Period==1)
Pdata$dT <- as.numeric(Pdata$NewB == 1)
Pdata$treat_effect <- Pdata$d2 * Pdata$dT
Pdata$effectT<-Pdata$treat_effect

treat4 <- lm(satis_sent ~ d2 + dT + treat_effect, Pdata)
summary(treat4)

treat6 <- lm(satis_sent ~ Period +NewB + effectT, Pdata)
summary(treat6)

#AGE SQAURED model
Pdata$Agesq = (Pdata$Age)^2  #testing for effect

treat5<- lm(satis_sent ~ d2 + dT + treat_effect+ like_sent+Age+Agesq+Male+Educ+score, Pdata)
summary(treat5)
#_____________________________________________________________________________ 


mean(Pdata$satisfied)

cor(Pdata$satisfied,Pdata$typeB)
Pdata$
#_____________________________________________________________________________ 

#_____________________________________________________________________________ ________
  qplot(like_sent,satisfied,data=Pdata,color=typeB ,geom=c("point","smooth"),method="lm",xlab = "Like Sentiment",ylab="Satisfaction sentiment",main ="Satisfaction Effect")
  

#bionominal plot
qplot(score_modif,satis_sent,data=Pdata ,geom=c("point","smooth"),method="glm",method.args=list(family="binomial"),xlab = "Like Sentiment",ylab="Satisfied",main ="Satisfaction Effect")+
theme_economist() + scale_color_solarized(accent="blue","Testing groups:")
  
#character Starting color. One of "yellow","orange","red","magenta","violet","blue","cyan","green"

#
qplot(like_sent,NewB,data=Pdata,color=typeB ,geom=c("point","smooth"),method="gam",xlab = "Like Sentiment",ylab="Satisfaction sentiment",main ="Satisfaction Effect")


qplot(NewB,satisfied,data=Pdata,color=Male ,geom=c("point","smooth"),xlab = "Like Sentiment",ylab="Satisfaction sentiment",main ="Satisfaction Effect")
#we try : Age+Male+Educ+Expr+NewB+score, Pdata
plot(Pdata$satis_sent~Pdata$Agesq , xlim=c(0, 5000), ylim=c(1, 5))


#simple graphs
plot(Pdata$satis_sent~Pdata$like_sent ,main="Satisfaction Effect" 
     ,xlab="Like Sentiment", ylab="Satisfaction Sentiment",xlim=c(0,7), ylim=c(0, 5),col="cornflowerblue",pch=9, cex=c(0.9,0))  #important graph
points(Pdata$satis_sent[Pdata$NewB==1]~Pdata$like_sent[Pdata$NewB==1] ,col="firebrick1",pch=1, cex=c(0.9,0))
points(Pdata$satis_sent[Pdata$NewB==0]~Pdata$like_sent[Pdata$NewB==0] ,col="darkslategray",pch=8, cex=c(0.5,0))


sent_effect <- lm(satis_sent ~  like_sent , Pdata)
summary(sent_effect)
#add regression line
abline( col="deepskyblue3", lwd=3,coef(sent_effect))
abline( col="firebrick", lwd=3,coef(sent_effectB))
abline( col="forestgreen", lwd=3,coef(sent_effectA))

legend(4.5, 1.5, legend=c("Group B5", "All","Group A" ),
       col=c("firebrick", "deepskyblue3","forestgreen"), lty=1:1, cex=0.3)


sent_effectB <- lm(satis_sent[Pdata$NewB==1] ~  like_sent[Pdata$NewB==1] , Pdata)
summary(sent_effectB)



sent_effectA <- lm(satis_sent[Pdata$NewB==0] ~  like_sent[Pdata$NewB==0] , Pdata)
summary(sent_effectA)


#######__________________

#----Part B  ___________________________________________
#Q1
dim(Pdata)

describe(Pdata$typeB[Pdata$typeB==1])


#Q2
describe((Pdata$Duration[Pdata$typeB==1]))
describe((Pdata$Duration[Pdata$typeB==0]))
describe(Pdata$Duration)


#plots
male<-145
female<-(258-male)

plot(Pdata$satis_sent~Pdata$Period)
cor()

A_obs<-Pdata$typeB==0
mediumObs<-(Pdata$Age>=21 & Pdata$Age<=30)
ageGroup<-Pdata[mediumObs, ]
A<-Pdata[A_obs, ]

hist(ageGroup$)
qplot(Pdata$Male,geom="histogram")

#______________ ___ ___ ___ ___ 

# 3D Exploded Pie Chart

slices <- c(56, 44)
lbls <- c("56% Male" ,"44% Female")
pie3D(slices,labels=lbls,explode=0.1 ,shade = 0.8,radius = 2.5,height = 0.3
      ,main="Gender of Participants ",)

young_obs<-(Pdata$Age>=15 & Pdata$Age<=20)
mediumObs<-(Pdata$Age>=21 & Pdata$Age<=25)
studentObs<-(Pdata$Age>=26 & Pdata$Age<=30)
matureObs<-(Pdata$Age>=31 & Pdata$Age<=35)
olderObs<-(Pdata$Age>=36 & Pdata$Age<=40)
crisisAge<-(Pdata$Age>=41 & Pdata$Age<=50)
zkenim<-(Pdata$Age>=51)


qplot(Age,data=Pdata,  fill=I("dodgerblue2"), col=I("grey"), 
      ylab = "Quantity of Obs",xlab = "Ages",main= "Age Distribution",binwidth=0.8)


qplot(Age,data=Pdata,geom = "density",fill=I("midnightblue"), col=I("grey"),  ylab = "Quantity of Obs",xlab = "Ages",main= "Age Distribution- Density")
qplot
str(Pdata)


#score distibution
qplot(Duration,data=Pdata ,fill=I("darkgoldenrod1"), col=I("darkgoldenrod1"),ylab = "Quantity of Obs",xlab = "Duration in Seconds",main= "Grades Distribution",binwidth=6)

qplot(score,data=Pdata ,fill=I("darkgoldenrod1"), col=I("grey"),ylab = "Quantity of Obs",xlab = "Score/Grade",main= "Grades Distribution",binwidth=6.2)
qplot(score,data=Pdata ,fill=I("coral1"), facets = "typeB", col=I("grey"),ylab = "Quantity of Obs",xlab = "Score/Grade",main= "Grades Distribution",binwidth=6.2,labels = TRUE)
qplot(score,data=Pdata,geom = "density",fill=I("midnightblue"), col=I("grey"),  ylab = "Prob",xlab = "Scores",main= "Score Distribution- Density")

ggplot(data=Pdata, aes(Pdata$Duration)) + 
  geom_histogram(breaks=seq(0, 3300, by=10), 
                 col="cyan4", 
                 aes(fill=..count..)) +labs(title="Course Scores Distribution", x="Score/Grade", y="Count")


ggplot(data=Pdata, aes(chol$score)) + 
  geom_histogram(aes(y =density), 
                 breaks=seq(20, 50, by = 2), 
                 col="red", 
                 fill="green"
                 alpha=.2),
  geom_density(col=2) )




# Histogram with density plot
ggplot(Pdata, aes(x=Duration)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6696")+labs(title="Duration of Session", x="Seconds", y="Density")
# Color by groups
ggplot(Pdata, aes(x=Duration, color=typeB, fill=typeB)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  geom_density(alpha=.2) 


mean(Pdata$Duration)


#___________________________________________________________________

cor(Pdata$score,Pdata$enjoy_sent)


PdataSentiment <- Pdata[ -c(1:9,25:36,41:50) ]
matr1 <- cor(PdataSentiment,use = "complete.obs")
round(matr1, 2)

PdataHost <- Pdata[ -c(1:24,35:50) ] #stay 25-34
matr2 <- cor(PdataHost,use = "complete.obs")
round(matr2, 2)

#big corr
PdataBig <- Pdata[ -c(1:9) ]
bigMat <- cor(PdataBig,use = "complete.obs")
round(bigMat, 2)



plot(Pdata$Duration~Pdata$satis_sent , xlim=c(0, 5), ylim=c(0, 5400))
treat3 <- lm(Duration ~  satis_sent+0 , Pdata)
summary(treat3)
abline( col="deepskyblue3", lwd=3,coef(treat3))




B_in_period0<-sum(Pdata$typeB[Pdata$Period==0])
B_in_period0
B_in_period1<-sum(Pdata$typeB[Pdata$Period==1])
B_in_period1


B_in_period00<-sum(Pdata$NewB[Pdata$Period==0])
B_in_period00
B_in_period11<-sum(Pdata$NewB[Pdata$Period==1])
B_in_period11





qplot(period,satis_sent,data=Pdata,color=typeB 
      ,geom=c("point","smooth"),method=lm,xlab = "time",ylab="Satisfaction sentiment",main ="Satisfaction Effect")



mean(Pdata$Age)
mean(Pdata$Age[Pdata$typeB==1])
mean(Pdata$Age[Pdata$typeB==0])
mean(Pdata$Age[Pdata$Male==1])
mean(Pdata$Age[Pdata$Male==0])



#initial tries start_______________________________________________
mean(TrenarioData$Age)

mean(TrenarioData$score[TrenarioData$typeB == 1])

mean(TrenarioData$score[TrenarioData$typeB == 0])

mean(TrenarioData$weight_sent)

cor(TrenarioData$weight_sent,TrenarioData$score)

cor(TrenarioData$weight_sent,TrenarioData$satis_sent)

cor(TrenarioData$weight_sent,TrenarioData$satis_sent)

mean(TrenarioData$total_sent)

cor(TrenarioData$typeB,TrenarioData$weight_sent)

cor(TrenarioData$typeB,TrenarioData$uncomf_sent)

mean(TrenarioData$Male)

cor(TrenarioData$Male,TrenarioData$satis_sent)

cor(TrenarioData$Male,TrenarioData$uncomf_sent)

cor(TrenarioData$weight_sent,TrenarioData$score)

trenario_ols<-lm(satis_sent ~ Age+ Male+ weight_sent+ score+Expr+Educ+typeB, TrenarioData)
summary(trenario_ols)


cor(TrenarioData$typeB,TrenarioData$score)

cor(TrenarioData$typeB,TrenarioData$satis_sent)

#duration
summary(TrenarioData$Duration)
cor(TrenarioData$Duration,TrenarioData$uncomf_sent)


cor(TrenarioData$amia_sent,TrenarioData$uncomf_sent)  ### strong corr
cor(TrenarioData$friend_sent,TrenarioData$uncomf_sent)### strong corr 

cor(TrenarioData$enjoy_sent,TrenarioData$cred_sent) #strong corr
cor(TrenarioData$enjoy_sent,TrenarioData$learn_sent)#strong corr
cor(TrenarioData$enjoy_sent,TrenarioData$think_sent)
cor(TrenarioData$enjoy_sent,TrenarioData$curious)
cor(TrenarioData$enjoy_sent,TrenarioData$more_course)
cor(TrenarioData$enjoy_sent,TrenarioData$empat_sent)


cor(TrenarioData$enjoy_sent,TrenarioData$feel_sent) #weak
cor(TrenarioData$enjoy_sent,TrenarioData$assist_help)
cor(TrenarioData$enjoy_sent,TrenarioData$uncomf_sent)
cor(TrenarioData$enjoy_sent,TrenarioData$indiff_sent)
cor(TrenarioData$enjoy_sent,TrenarioData$intres_sent)
cor(TrenarioData$enjoy_sent,TrenarioData$amia_sent)
cor(TrenarioData$enjoy_sent,TrenarioData$arrog_sent)
cor(TrenarioData$enjoy_sent,TrenarioData$friend_sent)
cor(TrenarioData$learn_sent,TrenarioData$uncomf_sent)

#initial tries end_______________________________________________


par(mfcol= c (1,1))
plot(Pdata$amia_sent, Pdata$uncomf_sent)
plot(Pdata$friend_sent, Pdata$uncomf_sent)
plot(Pdata$enjoy_sent, Pdata$cred_sent)
plot(Pdata$amia_sent, Pdata$uncomf_sent)


cor(Pdata$typeB,Pdata$Duration)

cor(Pdata$satis_sent,Pdata$score_modif,use="pairwise.complete.obs")
mean(Pdata$satis_sent)
mean(Pdata$score)

cor(Pdata[c("learn_sent","think_sent","curious","enjoy_sent","more_course",
            "empat_sent","cred_sent","feel_sent","assist_help","uncomf_sent","indiff_sent","intres_sent",
            "amia_sent","arrog_sent","friend_sent","score")],use="pairwise.complete.obs")



#visualize Correalation 
#correagram CREATIVITY TIME!

corrplot(matr, method = "number",type = "upper")

corrplot(matr2, method = "circle",type = "upper")

corrplot(abs(M),order = "AOE", col = col3(200), cl.lim = c(0, 1))

#first
corrplot(matr2, diag = FALSE, order = "hclust",  
         tl.cex = 0.6, method = "circle", type = "lower",addCoef.col = "black",tl.col = "black", tl.srt = 90,number.cex = .5)

#second
p.mat <- cor.mtest(PdataBig,use = "complete.obs")$p
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(bigMat, method = "color", col = col(200),
         type = "upper", order = "hclust", number.cex = .2,tl.cex = 0.5,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)
#third

corrplot(abs(matr2), order = "AOE", col = col3(200),  cl.lim = c(0, 1))
corrplot(abs(matr2),order = "AOE", cl.lim = c(0, 1))
#tl.cex = 0.5, method = "circle", type = "lower",addCoef.col = "black",tl.col = "black", tl.srt = 90,number.cex = .3)






#new graphs for task III

aggregate(Pdata["satis_sent"],by=list(Periods= Pdata$Period, Types= Pdata$NewB),FUN=mean)

ggplot(Pdata, aes(Period, satis_sent, color = typeB)) +
  geom_jitter() +
  theme_minimal()


#distibution of A/B obs over periods
ggplot(Pdata, aes(Period, satis_sent, color = A.B)) +facet_wrap(~gender)+
  geom_jitter() +labs(title="Satisfaction -Obs distribution over Periods", x="Period", y="Satis_Sent")+
  theme_solarized_2()#+scale_colour_solarized2("groups")

# periods in disctinct
#Pdata$avgeffect<-mean(Pdata$satis_sent[NewB==1])

ggplot(Pdata, aes(time,satis_sent, color = A.B)) +
  stat_summary(geom = 'line' )+
  geom_vline(xintercept = 0.5) + labs(title="Satisfaction Effect - Average treatment Effect", x="Period", y="Satisfaction Sentiment")+
  theme_economist()

# 
#another try:

df$treated <- as.factor(ifelse(df$year > 1993, 1, 0))

t.test(Pdata$satis_sent~Pdata$Period)

Pdata %>% group_by(Period) %>% summarise(t=mean(satis_sent,na.rm=1)) %>% 
  ggplot(aes(Period,t,group=1)) + geom_point() + geom_line()


cor(Pdata$satis_sent,Pdata$time)


#clustering using K-means


iris_cluster <- Pdata
head(iris_cluster)

cls <- kmeans(x = iris_cluster, centers = 3)
iris_cluster$cluster <- as.character(cls$cluster)
head(iris_cluster)

#cluster plot
ggplot() +
  geom_point(data = iris_cluster, 
             mapping = aes(x = score, 
                           y = like_sent, 
                           colour = cluster))
#add centroid
ggplot() +
  geom_point(data = iris_cluster, 
             mapping = aes(x = Period, 
                           y = satis_sent, 
                           colour = cluster)) +
  geom_point(mapping = aes_string(x = cls$centers["score"], 
                                  y = cls$centers[ "like_sent"]),
             color = "red", size = 4)
