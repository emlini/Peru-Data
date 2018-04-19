#Perspetive-taking analysis for Peru data

#last edited 4/19/18
library(ggplot2)
library(dplyr)

#setwd("/Volumes/YARN STASH/Dropbox/Data Analysis Work")
PTdata<- read.csv("Peru Participant Data 180413_EC.csv")
View(PTdata)
PTdata_sub <- subset(PTdata, PTdata$PTL1!='NA') # better way to subset
PT1 <- PTdata_sub$PTL1
PT2 <- PTdata_sub$PTL2
Age <- PTdata_sub$Age.at.Test
Exp <- PTdata_sub$Age.Exposed
Sex <- PTdata_sub$Gender
Dis <- PTdata_sub$Other.Disabilities
Yrs <- PTdata_sub$Number.of.Years.Signing

#check frequency distributions for PT1 and PT2
ggplot(PTdata, aes(x=PT1, y=..count..)) + geom_density()
ggplot(PTdata, aes(x=PT2, y=..count..)) + geom_density()

#test for Normality of PT1 and PT2
shapiro.test(PT1)
shapiro.test(PT2)
#Both are non-normal
#Will consider transformations, but not sure what would work best
#HAVE TO USE ANALYSES THAT DO NOT ASSUME NORMALITY (e.g. Kendall's Tau correlation)

#check distribution of PT1 relative to Age, Age of Exposure, # of Yrs signing, and Sex

ggplot(PTdata, aes(x=Age, y=PT1)) + geom_point() +geom_smooth(method='lm')
cor.test(Age, PT1, method=c("kendall"))  #NOT sig

ggplot(PTdata, aes(x=Exp, y=PT1)) + geom_point() +geom_smooth(method='lm')
cor.test(Exp, PT1, method=c("kendall"))  #NOT sig

ggplot(PTdata, aes(x=Yrs, y=PT1)) + geom_point() +geom_smooth(method='lm')
cor.test(Yrs, PT1, method=c("kendall"))  #NOT sig

ggplot(PTdata, aes(Sex, PT1)) +geom_boxplot()
wilcox.test(PT1~Sex) #NOT sig

ggplot(PTdata_sub, aes(x=Dis, y=PT1)) +geom_boxplot()
wilcox.test(PT1~Dis) #p = .055

#check distribution of PT2 relative to Age, Age of Exposure, # of Yrs signing, and Sex
ggplot(PTdata, aes(x=Age, y=PT2)) + geom_point() +geom_smooth(method='lm')
cor.test(Age, PT2, method=c("kendall")) #SIGNIFICANT

ggplot(PTdata, aes(x=Exp, y=PT2)) + geom_point() +geom_smooth(method='lm')
cor.test(Exp, PT2, method=c("kendall")) #NOT sig

ggplot(PTdata, aes(x=Yrs, y=PT2)) + geom_point() +geom_smooth(method='lm')
cor.test(Yrs, PT2, method=c("kendall")) #p=.067, tau=.224

ggplot(PTdata, aes(Sex, PT2)) + geom_boxplot()
wilcox.test(PT2~Sex) #Significant difference btwn Males 7 Females

ggplot(PTdata_sub, aes(x=Dis, y=PT2)) +geom_boxplot()
wilcox.test(PT2~Dis) #SIGNIFICANT

#Compare PT1 and PT2
ggplot(PTdata_sub, aes(x=PT1, y=PT2)) + geom_jitter() + geom_smooth(method="lm")



#NEXT STEP is to run some MODELS

PT1AgeSexDis <- glm(PT1~Age+Sex+Dis)
summary(PT1AgeSexDis)

PT1ExpSexDis <- glm(PT1~Exp+Sex+Dis)
summary(PT1ExpSexDis)

PT1SexDis <- glm(PT1~Sex+Dis)
summary(PT1SexDis)

PT1Dis <- glm(PT1~Dis)
summary(PT1Dis) #THIS IS THE BEST MODEL, acc to AIC, but pred. not sig.
stargazer(PT1Dis, out="PT1Dis_modeloutput.htm")

PT1DisYrs <- glm(PT1~Dis+Yrs)
summary(PT1DisYrs)

PT1DisAge <- glm(PT1~Dis+Age)
summary(PT1DisAge)

PT1DisExp <- glm(PT1~Dis+Exp)
summary(PT1DisExp)

PT1DisAgeExpYrs <- glm(PT1~Dis+Age+Exp+Yrs)
summary(PT1DisAgeExpYrs)


PT1DisAgeExpSex <- glm(PT1~Dis+Age+Exp+Sex)
summary(PT1DisAgeExpSex)

PT1DisAgeSexYrs <- glm(PT1~Dis+Age+Sex+Yrs)
summary(PT1DisAgeSexYrs)


PT1DisSexExpYrs <- glm(PT1~Dis+Sex+Exp+Yrs)
summary(PT1DisSexExpYrs)


#Models for PT2
PT2AgeSexDis <- glm(PT2~Age+Sex+Dis)
summary(PT2AgeSexDis)
plot(PT2AgeSexDis) #<- CHECK IF RESIDUALS ARE NORMAL
hist(residuals(PT2AgeSexDis))
shapiro.test(residuals(PT2AgeSexDis)) #RESIDUALS NOT NORMALLY DISTRIBUTED
#http://blog.minitab.com/blog/adventures-in-statistics-2/how-important-are-normal-residuals-in-regression-analysis

PT2AgeSexDisYrs <- glm(PT2~Age+Sex+Dis+Yrs)
summary(PT2AgeSexDisYrs)

PT2AgeSex <- glm(PT2~Age+Sex)
summary(PT2AgeSex)

PT2AgeDis <- glm(PT2~Age+Dis)
summary(PT2AgeDis)

PT2AgeSexDisPT1 <- glm(PT2~Age+Sex+Dis+PT1)
summary(PT2AgeSexDisPT1)

PT2Age <- glm(PT2~Age)
summary(PT2Age)
